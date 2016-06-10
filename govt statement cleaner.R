# Leslie Huang

rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/MA-thesis-analysis")

libraries <- c("foreign", "utils", "dplyr", "devtools")
lapply(libraries, require, character.only=TRUE)

# read in the statement CSVs
statements <- read.csv("../MA-datasets/govtstatements2012.csv", stringsAsFactors = FALSE)

years <- c(2013, 2014, 2015)

for (i in 1:length(years)) {
  filename <- paste("../MA-datasets/govtstatements", years[i], ".csv", sep = "")
  csv <- read.csv(filename, stringsAsFactors = FALSE)
  statements <- rbind(statements, csv)
}

statements2016 <- read.csv("../MA-datasets/govtstatements2016.csv", stringsAsFactors = FALSE)
statements2016$URL <- NA
statements <- rbind(statements, statements2016)

write.csv(statements, file = "govtstatements.csv")
