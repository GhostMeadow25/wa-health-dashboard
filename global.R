# global.R - Data Loading & Global Variables

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(classInt)
  library(stringr)
  library(shinycssloaders)
  library(htmltools)
})

# ── Helper Functions ───────────────────────────────────────────────────────────
`%||%` <- function(x, y) if (is.null(x)) y else x

safe_val <- function(expr, fallback = "N/A") {
  tryCatch({
    val <- expr
    if (is.null(val) || length(val) == 0 || (length(val) == 1 && is.na(val))) fallback
    else as.character(val)
  }, error = function(e) fallback)
}

# input is decimal (0.039 → "3.9%")
format_percent <- function(x, digits = 1) {
  val <- suppressWarnings(as.numeric(x))
  ifelse(is.na(val), "N/A", paste0(round(val * 100, digits), "%"))
}

# input already in % units (3.9 → "3.9%")
format_percent_pct <- function(x, digits = 1) {
  val <- suppressWarnings(as.numeric(x))
  ifelse(is.na(val), "N/A", paste0(round(val, digits), "%"))
}

# safe inside dplyr::mutate
format_number <- function(x) {
  val <- suppressWarnings(as.numeric(x))
  ifelse(is.na(val), "N/A", formatC(val, format = "d", big.mark = ","))
}

# ── Constants ──────────────────────────────────────────────────────────────────
INDICATOR_NAME <- "Children without health insurance"
STATE_NAME     <- "Washington State"
PRIMARY_COLOR  <- "#1B6CA8"
ACCENT_COLOR   <- "#2E9E84"

WA_BOUNDS <- list(sw = c(44.8, -125.5), ne = c(49.8, -116.0))
WA_CENTER <- c(47.4, -120.5)
WA_ZOOM   <- 6

# ── Load County/State Data from Google Sheet CSV ───────────────────────────────
wa_data <- tryCatch({
  filepath <- "data/health_insurance_wa.csv"
  if (!file.exists(filepath)) stop(paste("Data file not found at:", filepath))
  
  data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = TRUE)
  data$Number  <- suppressWarnings(as.numeric(data$Number))
  data$Percent <- suppressWarnings(as.numeric(data$Percent))
  
  required <- c("Location", "Number", "Percent")
  missing  <- setdiff(required, names(data))
  if (length(missing) > 0) stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  if (nrow(data) == 0) stop("Data file is empty")
  
  data <- data %>%
    mutate(
      PctDisplay = format_percent(Percent),
      NumDisplay = format_number(Number),
      ShortName  = gsub(" County$", "", Location)
    )
  data
}, error = function(e) stop(paste("ERROR loading county data:\n", e$message)))

# ── Derived state and county objects ──────────────────────────────────────────
state_row  <- wa_data %>% filter(Location == STATE_NAME)
county_data <- wa_data %>%
  filter(Location != STATE_NAME) %>%
  rename(County = Location)

state_num <- state_row$Number[1]
state_pct <- state_row$Percent[1]

# ── Load Census CSV for county subdivision drill-down ─────────────────────────
wa_cousub_data <- tryCatch({
  filepath <- "data/ACSST5Y2024_S2701-Data.csv"
  if (!file.exists(filepath)) stop(paste("Census CSV not found at:", filepath))
  
  raw <- read.csv(filepath, header = TRUE, stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8-BOM", check.names = TRUE)
  raw <- raw[-1, ]
  rownames(raw) <- NULL
  
  required <- c("NAME", "GEO_ID", "S2701_C04_011E", "S2701_C05_011E")
  missing  <- setdiff(required, names(raw))
  if (length(missing) > 0) stop(paste("Missing Census columns:", paste(missing, collapse = ", ")))
  
  raw$is_suppressed <- raw$S2701_C05_011E == "-"
  raw$unins_num     <- suppressWarnings(as.numeric(raw$S2701_C04_011E))
  raw$unins_pct     <- suppressWarnings(as.numeric(raw$S2701_C05_011E))
  # Leave suppressed rows with NA so they render as grey on the map
  
  raw$cousub_geoid <- sub("^.*US", "", raw$GEO_ID)

  raw$County <- sapply(strsplit(raw$NAME, ", "), function(x) {
    if (length(x) >= 2) x[length(x) - 1] else NA_character_})
  
  # Subdivision name: everything before the first comma
  raw$SubdivName <- sub(",.*$", "", raw$NAME)
  
  raw %>%
    mutate(
      Percent    = ifelse(is_suppressed, NA_real_, unins_pct / 100),
      Number     = ifelse(is_suppressed, NA_real_, unins_num),
      PctDisplay = ifelse(is_suppressed, "Suppressed",
                          format_percent_pct(unins_pct)),
      NumDisplay = ifelse(is_suppressed, "Suppressed",
                          format_number(unins_num)),
      Location   = SubdivName
    )
}, error = function(e) {
  warning(paste("Census CSV error (subdivision map unavailable):", e$message))
  NULL
})

# ── Load County Shapefile ──────────────────────────────────────────────────────
wa_counties <- tryCatch({
  shp_path <- "data/shapefile/tl_2024_us_county.shp"
  if (!file.exists(shp_path)) stop(paste("County shapefile not found:", shp_path))
  shp <- sf::st_read(shp_path, quiet = TRUE) %>% filter(STATEFP == "53")
  if (!"NAMELSAD" %in% names(shp)) stop("County shapefile missing 'NAMELSAD'")
  shp
}, error = function(e) { warning(paste("County shapefile error:", e$message)); NULL })

# ── Load County Subdivision Shapefile ─────────────────────────────────────────
wa_cousub_shp <- tryCatch({
  shp_path <- "data/shapefile/tl_2024_53_cousub.shp"
  if (!file.exists(shp_path)) stop(paste("Cousub shapefile not found:", shp_path))
  shp <- sf::st_read(shp_path, quiet = TRUE)
  if (!"GEOID" %in% names(shp)) stop("Cousub shapefile missing 'GEOID'")
  shp
}, error = function(e) { warning(paste("Cousub shapefile error:", e$message)); NULL })

# ── Dropdown list ──────────────────────────────────────────────────────────────
county_list <- sort(county_data$County)

# ── Global color breaks ────────────────────────────────────────────────────────
# Jenks breakpoints
{
  county_vals  <- county_data$Percent[!is.na(county_data$Percent)] * 100
  cousub_max   <- if (!is.null(wa_cousub_data) && nrow(wa_cousub_data) > 0)
    max(wa_cousub_data$Percent * 100, na.rm = TRUE)
  else max(county_vals)
  n_bins  <- min(5, length(unique(round(county_vals, 2))))
  breaks  <- tryCatch(
    classInt::classIntervals(county_vals, n = n_bins, style = "jenks")$brks,
    error = function(e) pretty(county_vals, n = 5)
  )
  breaks    <- unique(round(breaks, 1))
  breaks[1] <- 0
  breaks[length(breaks)] <- ceiling(max(cousub_max, max(county_vals)) * 10) / 10
  WA_BREAKS <- unique(breaks)
  rm(county_vals, cousub_max, n_bins, breaks)
}