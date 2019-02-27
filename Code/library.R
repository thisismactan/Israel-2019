#### LIBRARIES ####
if(!require(Hmisc)) {
  install.packages(c("Hmisc", "lubridate", "mvnfast", "readxl", "reshape2", "tidyverse"))
}
require(Hmisc)
require(lubridate)
require(mvnfast)
require(readxl)
require(reshape2)
require(tidyverse)

#### PARTY PALETTE ####
source("Code/party_colors.R")

#### CUSTOM FUNCTIONS ####
logit <- function(x) {
  return(log(x/(1-x)))
}

invlogit <- function(x) {
  return(1/(1+exp(-x)))
}
