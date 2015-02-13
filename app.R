#Library
library(shiny)
library(shinythemes)
library(shinydashboard)
library(pipeR)
library(zoo)
library(dplyr)
library(dygraphs)
library(metricsgraphics)
#Constant
TERMS_INT <- c(1:10,15,20,25,30,40)
TERMS_STR <- paste0(TERMS_INT, "Y")
URL_CURRENT <- "http://www.mof.go.jp/english/jgbs/reference/interest_rate/jgbcme.csv"
URL_OLD     <- "http://www.mof.go.jp/english/jgbs/reference/interest_rate/historical/jgbcme_all.csv"
################################################################
#UI component
#TERMS_STR
dashboard_TERMS_STR <- dashboardHeader(
  title="JGB Viewer(Beta)",
  dropdownMenuOutput("messageMenu")
)
#Sidebar
dashboard_sidebar <- dashboardSidebar(
  #sidebarSearchForm(label = "Doesn't work yet...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem(
      "Charts", icon = icon("bar-chart-o"),
      menuSubItem("Single   time series", tabName="sts", icon=icon("line-chart")),
      menuSubItem("Multiple time series", tabName="mts", icon=icon("line-chart")),
      menuSubItem("Term structure", tabName="ts", icon=icon("line-chart"))
    ),
    menuItem("Data Analysis", icon=icon("th"), tabName="widgets", badgeLabel="soon", badgeColor="green"),
    menuItem("About", tabName="about", icon=icon("info-circle"))
  )
)
#Body
dashboard_body <- dashboardBody(
  tabItems(
    # Single time series
    tabItem(
      tabName = "sts",
      h2("Single time series plot"),
      fluidRow(
        box(
          title="Choose the term of Interest Rate", status="primary", solidHeader=TRUE, width=8,
          selectInput("sts_term", "", TERMS_STR, selected="10Y")
        )
      ),
      fluidRow(
        box(
          title="One term year plot", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
          dygraphOutput("sts_dygraph")
        )
      )
    ),    
    # Multiple time series
    tabItem(
      tabName = "mts",
      h2("Multiple time series plot"),
      fluidRow(
        box(
          title="Terms", status="primary", solidHeader=TRUE, width=12,
          actionButton("mts_selectall", label="Select All"),
          checkboxGroupInput("mts_terms", "", TERMS_STR, selected=TERMS_STR, inline=TRUE)
        )
      ),
      fluidRow(
        box(
          title="Multiple term plot", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
          dygraphOutput("mts_dygraph")
        )
      )
    ),
    # Term structure
    tabItem(
      tabName = "ts",
      h2("Term structureplot"),
      fluidRow(
        box(
          title="Choose the date to show term structure", status="primary", solidHeader=TRUE, width=8,
          #Based on this issue : https://github.com/rstudio/shiny/issues/66
          dateInput('ts_date', 'Date:', value = Sys.Date()),
          sliderInput('ts_slider', 'How many days before today?', min=0, max=10^3, value=0)
        )
      ),
      fluidRow(
        box(
          title="Term structure plot", status="primary", solidHeader=TRUE, width=12,
          metricsgraphicsOutput("ts_metricsgraphics")
        )
      )
    ),
    # About tab content
    tabItem(
      tabName = "about",
      h2("What's this?"),
      p("This is a Japanese Government Bond(JGB) rate viewer application developed by Shiny and R."),
      h2("Code"),
      p("You can download all codes from the following URL on Github."),
      a(href="https://github.com/teramonagi/JGBViewer", "https://github.com/teramonagi/JGBViewer") %>>% p
    )
  )
)
#Combine UI components
ui <- dashboardPage(
  dashboard_TERMS_STR,
  dashboard_sidebar,
  dashboard_body
)
################################################################
#Server
server <- function(input, output, session) {
  # Variables in server 
  mts_selectall <- reactiveValues(pushed = FALSE)
  jgb_current <- read.csv(URL_CURRENT, stringsAsFactors=FALSE, na.strings = "-") %>>% setNames(c("Date", TERMS_STR))
  jgb_old <- read.csv(URL_OLD, stringsAsFactors=FALSE, na.strings = "-") %>>% setNames(c("Date", TERMS_STR))
  jgb <- rbind(jgb_old, jgb_current) %>>% read.zoo
  # Inner functions
  dygraph_plot <- function(selected_term)
  {
    if(length(selected_term) == 0){return(NULL)}
    
    jgb[, selected_term] %>>%
      na.locf %>>% 
      na.omit %>>% {
        if(length(selected_term)==1){
          dygraph(., main=paste0("JGB Interest Rate - ", selected_term, " -")) %>>% 
          dySeries("V1", label=paste0(selected_term, "(%)"))
        } else{
          dygraph(., main="JGB Interest Rate") 
        }  
      } %>>% 
      dyRangeSelector(dateWindow = c(as.character(Sys.Date()-365), as.character(Sys.Date())))  
  }
  # Controller
  output$sts_dygraph <- renderDygraph({input$sts_term  %>>% dygraph_plot})
  output$mts_dygraph <- renderDygraph({input$mts_terms %>>% dygraph_plot})
  output$ts_metricsgraphics <- renderMetricsgraphics({
    jgb_filtered <- jgb[input$ts_date]  
    jgb_filtered <- jgb_filtered[apply(jgb_filtered, 2, function(x)!all(is.na(x)))]
    if(nrow(jgb_filtered) > 0){
      jgb_filtered %>>%
        as.numeric %>>%
        {data.frame(term=TERMS_INT, value=.)} %>>%
        mjs_plot(x=term, y=value) %>>%
        mjs_line() %>>%
        mjs_labs(x="Term(Year)", y="Interest Rate")
    } else{
     NULL 
    }
  })
  observe({
    updateDateInput(session, 'ts_date', value = Sys.Date() - input$ts_slider)
  })
  observe({
    if(input$mts_selectall!=0){mts_selectall$pushed <- TRUE}
  })
  observe({
    if(mts_selectall$pushed){
      all_or_nothing <- if(length(input$mts_terms)!=length(TERMS_STR)){TERMS_STR}
      updateCheckboxGroupInput(session, "mts_terms", choices=TERMS_STR, selected=all_or_nothing, inline=TRUE)
      mts_selectall$pushed <- FALSE
    }    
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