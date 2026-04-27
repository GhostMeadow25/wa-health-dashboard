# app.R - Main Application Entry Point

source("global.R")
source("R/ui_tabs.R")
source("R/server.R")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(
      span(class = "logo-lg",
           div(style = "line-height: 1.2;",
               div(style = "font-size: 17px; font-weight: 700; color: #ffffff;",
                   "Washington Children's Health"),
               div(style = "font-size: 11px; font-weight: 500; color: rgba(255,255,255,0.7);",
                   "Insurance Coverage Dashboard"))),
      span(class = "logo-mini", "WA"))),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    custom_css,
    dashboard_body_content()
  )
)

server <- function(input, output, session) {
  setup_server(input, output, session)
}

shinyApp(ui, server)