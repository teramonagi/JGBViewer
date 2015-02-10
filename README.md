JGBViewer package
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
The data of this application is based on the following link:
- http://www.mof.go.jp/english/jgbs/reference/interest_rate/index.htm

## License
JGBViewer is licensed under the MIT License. 
