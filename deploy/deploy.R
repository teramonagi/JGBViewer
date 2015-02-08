library("shinyapps")
shinyapps::setAccountInfo(
  name='teramonagi', 
  token=options()$shinyapps.token, 
  secret=options()$shinyapps.secret
)
deployApp(paste0(getwd(), "/.."))