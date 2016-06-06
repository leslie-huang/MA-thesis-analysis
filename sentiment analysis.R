# Leslie Huang
# LIWC analysis of FARC communiques

rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/MA/")

libraries <- c("foreign", "utils", "stargazer", "dplyr", "devtools", "quanteda", "quantedaData", "ggplot2", "stringr")
lapply(libraries, require, character.only=TRUE)

spanish_dict <- dictionary(file = "../LIWC/Spanish_LIWC2007_Dictionary.dic", format = "LIWC")

# import FARC communiques