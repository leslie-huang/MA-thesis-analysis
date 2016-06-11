# Leslie Huang

rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/MA-thesis-analysis")

libraries <- c("foreign", "utils", "dplyr", "devtools")
lapply(libraries, require, character.only=TRUE)

# read in the statement CSVs
# statements <- read.csv("../MA-datasets/govtstatements2012.csv", stringsAsFactors = FALSE)
# 
# years <- c(2013, 2014, 2015)
# 
# for (i in 1:length(years)) {
#   filename <- paste("../MA-datasets/govtstatements", years[i], ".csv", sep = "")
#   csv <- read.csv(filename, stringsAsFactors = FALSE)
#   statements <- rbind(statements, csv)
# }
# 
# statements2016 <- read.csv("../MA-datasets/govtstatements2016.csv", stringsAsFactors = FALSE)
# statements2016$URL <- NA
# statements <- rbind(statements, statements2016)

s2012 <- read.csv("../MA-datasets/govtstatements2012.csv", stringsAsFactors = FALSE)
s2013 <- read.csv("../MA-datasets/govtstatements2013.csv", stringsAsFactors = FALSE)
s2014 <- read.csv("../MA-datasets/govtstatements2014.csv", stringsAsFactors = FALSE)
s2015 <- read.csv("../MA-datasets/govtstatements2015_1.csv", stringsAsFactors = FALSE)
s2016 <- read.csv("../MA-datasets/govtstatements2016.csv", stringsAsFactors = FALSE)
s2016$URL <- NA
s2014_2 <- read.csv("../MA-datasets/govtstatements2014_2.csv", stringsAsFactors = FALSE)

# deleting non topical statements manually
s2012 <- slice(s2012, -c(5, 6, 10, 15, 16, 17))
s2013 <- slice(s2013, -c(2,3,8,10,11,15,16,17,18,19,20,21,23,29,32,36,37,38,41,43,45,47,48,52,65,66,69,70,71,72,73,74,79,80,82,83,84,85,90,91,92,95,99,101,102,105, 106,108,110,113,115,116,118,121,122,127,129,130,132,133,135,136,137,143,144,146,149,150,152,154,155,157,161,166,167,168,170,173,177,182,188,191))
s2014 <- slice(s2014, -c(6,8,9,10,11,13,17,28,39,41,42))
s2014_2 <- slice(s2014_2, -c(3,4,8,16,21,25,26,32,33,37,39))
s2015 <- slice(s2015, -c(2,4,5,6,15,20,21,23,25))
s2016 <- slice(s2016, -c(5,6,9,11,14,17,20,29,39,42,44,49))

# save the file
statements <- rbind(s2012, s2013, s2014, s2014_2, s2015, s2016)

write.csv(statements, file = "govtstatements.csv")
