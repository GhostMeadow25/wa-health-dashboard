# R/ui_tabs.R - UI Definitions

custom_css <- tags$head(tags$style(HTML("
  @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;500;600;700&display=swap');
  body, .content-wrapper, .main-footer { font-family: 'Poppins', sans-serif !important; }

  /* ── Header: solid color, no visible text ─────────────────────────────── */
  .skin-blue .main-header .navbar,
  .skin-blue .main-header .logo {
    background-color: #1B6CA8 !important;
    border-bottom: none !important;
  }
  .skin-blue .main-header .logo:hover {
    background-color: #1B6CA8 !important;
    cursor: default;
  }
  /* Hide all text/content inside the header */
  .main-header .logo span,
  .main-header .logo div,
  .main-header .navbar .navbar-custom-menu,
  .main-header .navbar .navbar-left {
    visibility: hidden;
  }
  /* Keep the header bar itself visible but shrink it slightly */
  .main-header { min-height: 36px !important; }
  .main-header .navbar,
  .main-header .logo { min-height: 36px !important; height: 36px !important; }
  .content-wrapper, .main-footer { margin-top: 0 !important; }

  /* ── Page header ──────────────────────────────────────────────────────── */
  .page-header {
    border-bottom: 2px solid #1B6CA8;
    margin-bottom: 18px;
    padding-bottom: 10px;
  }
  .page-header h2 {
    color: #1B6CA8;
    font-size: 22px;
    font-weight: 700;
    margin-bottom: 2px;
  }
  .page-header p { color: #666; font-size: 13px; margin: 0; }

  /* ── Panel boxes ──────────────────────────────────────────────────────── */
  .panel-box {
    background: #ffffff;
    border-radius: 10px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 3px 14px rgba(27,108,168,0.11);
  }

  /* ── Section titles ───────────────────────────────────────────────────── */
  .section-title {
    font-size: 15px;
    font-weight: 700;
    color: #1B6CA8;
    margin-bottom: 12px;
    padding-bottom: 8px;
    border-bottom: 2px solid #1B6CA8;
  }

  /* ── Plot container ───────────────────────────────────────────────────── */
  .plot-container {
    background: #f5f9fc;
    border-radius: 8px;
    padding: 14px;
    border: 1px solid #d4e6f1;
  }

  /* ── Data card: clean white card with blue accent ─────────────────────── */
  .stat-card {
    background: #ffffff;
    border-radius: 10px;
    border-left: 5px solid #1B6CA8;
    padding: 18px 24px;
    box-shadow: 0 3px 14px rgba(27,108,168,0.11);
    display: flex;
    flex-direction: column;
    justify-content: center;
    min-height: 110px;
  }
  .stat-card .stat-value {
    font-size: 42px;
    font-weight: 700;
    color: #1B6CA8;
    line-height: 1.1;
  }
  .stat-card .stat-label {
    font-size: 13px;
    color: #444;
    margin-top: 4px;
  }
  .stat-card .stat-sub {
    font-size: 12px;
    color: #888;
    margin-top: 4px;
  }
  .stat-card .stat-compare {
    font-size: 12px;
    font-weight: 600;
    margin-top: 10px;
    background: #f0f6fb;
    border-radius: 6px;
    padding: 4px 10px;
    display: inline-block;
    color: #1B6CA8;
    width: fit-content;
  }

  /* ── Selector label ───────────────────────────────────────────────────── */
  .selector-label {
    font-weight: 600;
    color: #1B6CA8;
    font-size: 14px;
    margin-bottom: 5px;
  }

  /* ── Citation text ────────────────────────────────────────────────────── */
  .citation-text {
    color: #999;
    font-size: 12px;
    font-style: italic;
    margin-top: 8px;
  }
")))


dashboard_body_content <- function() {
  div(
    style = "padding: 15px;",
    
    # ── Page Header ──────────────────────────────────────────────────────────
    div(class = "page-header",
        h2("Children Without Health Insurance — Washington State"),
        p("2024 ACS 5-Year Estimates | U.S. Census Bureau, American Community Survey (S2701)")),
    
    # ── County Selector + Stat Card ──────────────────────────────────────────
    div(class = "panel-box",
        fluidRow(
          column(3,
                 div(class = "selector-label", "Select a location:"),
                 selectInput("selected_county", NULL,
                             choices  = c("Washington State", county_list),
                             selected = "Washington State",
                             width    = "100%")),
          column(9,
                 uiOutput("datacard_ui"))
        )),
    
    # ── Map + Bar Chart ──────────────────────────────────────────────────────
    fluidRow(
      column(7,
             div(class = "panel-box",
                 div(class = "section-title", icon("map-marker"), " County Map"),
                 div(class = "plot-container",
                     withSpinner(leafletOutput("map_plot", height = "430px"),
                                 type = 6, color = "#1B6CA8", size = 0.8)),
                 div(class = "citation-text",
                     "Uninsured rate among children under 19.
                      Source: ACS 5-Year Estimates 2024 (S2701), U.S. Census Bureau."))),
      
      column(5,
             div(class = "panel-box",
                 div(class = "section-title", icon("bar-chart"), " Counties by Uninsured Rate"),
                 div(class = "plot-container",
                     withSpinner(plotlyOutput("bar_chart", height = "430px"),
                                 type = 6, color = "#1B6CA8", size = 0.8)),
                 div(class = "citation-text",
                     "Red bars = above state average. Green bars = below state average.
                      Dashed line = Washington State average (3.9%).")))
    )
  )
}