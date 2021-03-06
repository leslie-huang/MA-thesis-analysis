# Leslie Huang

rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/MA-thesis-analysis")

libraries <- c("foreign", "utils", "dplyr", "devtools")
lapply(libraries, require, character.only=TRUE)

s2012 <- read.csv("../MA-datasets/govtstatements2012.csv", stringsAsFactors = FALSE)
s2013 <- read.csv("../MA-datasets/govtstatements2013.csv", stringsAsFactors = FALSE)
s2014 <- read.csv("../MA-datasets/govtstatements2014.csv", stringsAsFactors = FALSE)
s2015 <- read.csv("../MA-datasets/govtstatements2015_1.csv", stringsAsFactors = FALSE)
s2016 <- read.csv("../MA-datasets/govtstatements2016.csv", stringsAsFactors = FALSE)
s2016$URL <- NA
s2014_2 <- read.csv("../MA-datasets/govtstatements2014_2.csv", stringsAsFactors = FALSE)
s2015_2 <- read.csv("../MA-datasets/govtstatements2015_2.csv", stringsAsFactors = FALSE)

# delete non topical statements manually
s2012 <- slice(s2012, -c(5, 6, 10, 15, 16, 17))
s2013 <- slice(s2013, -c(2,3,8,10,11,15,16,17,18,19,20,21,23,29,32,36,37,38,41,43,45,47,48,52,65,66,69,70,71,72,73,74,79,80,82,83,84,85,90,91,92,95,99,101,102,105, 106,108,110,113,115,116,118,121,122,127,129,130,132,133,135,136,137,143,144,146,149,150,152,154,155,157,161,166,167,168,170,173,177,182,188,191))
s2014 <- slice(s2014, -c(6,8,9,10,11,13,17,28,39,41,42))
s2014_2 <- slice(s2014_2, -c(3,4,8,16,21,25,26,32,33,37,39))
s2015 <- slice(s2015, -c(2,4,5,6,15,20,21,23,25))
s2016 <- slice(s2016, -c(5,6,9,11,14,17,20,29,39,42,44,49))
s2015_2 <- slice(s2015_2, -c(1,2,5,11,13,14,18,20,22,26,31,32,33,36,37,46,54,63,66,67,71,72,73,77,78,80,81,83,85,88,90,91,92,94,98,101,104,105,106,109,110,111,112,113,114,115,116,117,118,119,120,121,123,124,125,128,129,131,132,134))

# save the file
statements <- rbind(s2012, s2013, s2014, s2014_2, s2015, s2015_2, s2016)

write.csv(statements, file = "govtstatements.csv")
