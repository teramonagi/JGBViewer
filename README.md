JGBViewer - web application by Shiny and R - 
=============================================================

## What's this
This is a web application to view Japanese Government Bond(JGB) rate developed by Shiny and R. You can download the code and report an issues on Github:
- https://github.com/teramonagi/JGBViewer

Comments and issue reports are always welcome!

## Installation
You can install JGBViewer package from github using the devtools package

```{r}
library(devtools)
install_github('teramonagi/JGBViewer')
```

## How to run on your PC(local)
You can run this application by the folloinwg procedures:

1. Open JGBViewer.Proj file by RStudio(directory setting purpose mainly)
2. Run the following commands on your R console:
```{r}
library(shiny)
runApp()
```

## Data source
The data of this application is the published data from Ministry of Finance in Japan.
You can access the original data from the following link:
- http://www.mof.go.jp/english/jgbs/reference/interest_rate/index.htm

## Upcoming new features...
- New vizualization
  - Term structure view
  - Daily Change of interest rate view
- Data analysis
  - Principal Component Analysis(PCA)
  - Running Volatility
  
## License
JGBViewer is licensed under the MIT License. 
