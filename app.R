#Library
library(shiny)
library(shinythemes)
library(shinydashboard)
library(pipeR)
library(zoo)
library(dplyr)
library(dygraphs)
#Constant
HEADER <- paste0(c(1:10,15,20,25,30,40), "Y")
URL_CURRENT <- "http://www.mof.go.jp/english/jgbs/reference/interest_rate/jgbcme.csv"
URL_OLD     <- "http://www.mof.go.jp/english/jgbs/reference/interest_rate/historical/jgbcme_all.csv"
################################################################
#UI component
#Header
dashboard_header <- dashboardHeader(
  title="JGB Viewer(Beta)",
  dropdownMenuOutput("messageMenu")
)
#Sidebar
dashboard_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Main",  tabName="main",  icon=icon("dashboard")),
    menuItem("About", tabName="about", icon=icon("info-circle"))
  )
)
#Body
dashboard_body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(
      tabName = "main",
      fluidRow(
        box(
          title="One term year plot", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
          dygraphOutput("dygraph")
        )
      ),
      fluidRow(
        box(
          title="Choose the term of Interest Rate", status="primary", solidHeader=TRUE, width=8,
          selectInput("term", "", HEADER, selected="10Y")
        )
      )
    ),    
    # Second tab content
    tabItem(
      tabName = "about",
      h2("What's this?"),
      p("This is a Japanese Government Bond(JGB) rate viewer application developed by Shiny and R."),
      h2("Code"),
      p("You can download all codes from the following URL on Github."),
      a(href="https://github.com/teramonagi/JGBViewer", "https://github.com/teramonagi/JGBViewer")
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
  jgb_current <- read.csv(URL_CURRENT, stringsAsFactors=FALSE, na.strings = "-") %>>% setNames(c("Date", HEADER))
  jgb_old <- read.csv(URL_OLD, stringsAsFactors=FALSE, na.strings = "-") %>>% setNames(c("Date", HEADER))
  jgb <- rbind(jgb_old, jgb_current) %>>% read.zoo
  output$dygraph  <- renderDygraph({
    selected_term <- input$term
    jgb[, selected_term] %>>%
      na.locf %>>% 
      na.omit %>>% 
      dygraph(main=paste0("JGB Interest Rate - ", selected_term, " -")) %>>% 
      dyRangeSelector(dateWindow = c(as.character(Sys.Date()-365), as.character(Sys.Date())))
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