## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Examples: How The cointegration based support vector regression model can be applied  
library(ECTSVR)
#taking data finland from the r library
data(finland)
#takaing the two cointegrated variables (4th and 3rd) from the data set
data_example <- finland[,4:3]
#application of ECTSVR model with radial basis kernel function of Epsilon support vector regression model

ECTSVR(data_example,"trace",0.8,2, "radial","eps-regression",verbose = FALSE)

