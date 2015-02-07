library(shiny)
library(shinythemes)
library(shinydashboard)
library(pipeR)
library(zoo)
library(dplyr)
library(dygraphs)
################################################################
#UI component
#Header
dashboard_header <- dashboardHeader(
  title = "JGB Viewer",
  dropdownMenuOutput("messageMenu")
)
#Sidebar
dashboard_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Main",  tabName = "main", icon = icon("dashboard")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )
)
#Body
dashboard_body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(
      tabName = "main",
      fluidRow(
        dygraphOutput("dygraph", width = "80%")
      )
    ),    
    # Second tab content
    tabItem(
      tabName = "about",
      h2("What's this?"),
      p("This is a  Japanese Government Bond(JGB) rate viewer."),
      h2("Code"),
      p("You can download all codes from the following URL on Github.")
      a(href="https://github.com/teramonagi/JGBViewer")
    )
  )
)
#Combine UI components
ui <- dashboardPage(
  dashboard_header,
  dashboard_sidebar,
  dashboard_body
)
################################################################
#Server
server <- function(input, output) {
  HEADER <- paste0(c(1:10,15,20,25,30,40), "Y")
  URL_CURRENT <- "https://www.mof.go.jp/english/jgbs/reference/interest_rate/jgbcme.csv"
  URL_OLD     <- "https://www.mof.go.jp/english/jgbs/reference/interest_rate/historical/jgbcme_all.csv"
  jgb_current <- read.csv(URL_CURRENT, stringsAsFactors=FALSE, na.strings = "-") %>>% setNames(c("Date", HEADER))
  jgb_old <- read.csv(URL_OLD, stringsAsFactors=FALSE, na.strings = "-") %>>% setNames(c("Date", HEADER))
  jgb <- rbind(jgb_old, jgb_current) %>>% read.zoo
  output$dygraph  <- renderDygraph({
    jgb[,"10Y"] %>>%
      dygraph(main="JGB Interest Rate") %>>% 
      dyRangeSelector(dateWindow = c("2012-01-01", as.character(Sys.Date())))
  })
  output$messageMenu <- renderDropdownMenu({
    dropdownMenu(
      type = "messages",
      messageItem(
        from = "Developer",
        message = "This is Beta version!!!"
      )
    )
  })
}
################################################################
#Application
shinyApp(ui, server)