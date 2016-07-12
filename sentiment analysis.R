# Leslie Huang
# LIWC analysis of FARC communiques

rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/MA-thesis-analysis/")

set.seed(1234)

libraries <- c("foreign", "utils", "stargazer", "dplyr", "devtools", "quanteda", "ggplot2", "stringr", "LIWCalike", "austin", "forecast", "lmtest", "strucchange", "vars", "tseries", "urca", "depmixS4", "rrcov")
lapply(libraries, require, character.only=TRUE)

devtools::install_github("ggbiplot", "vqv")
library(ggbiplot)

# get LIWC dict
spanish_dict <- dictionary(file = "../LIWC/Spanish_LIWC2007_Dictionary.dic", format = "LIWC")

################################################################################## 
################################################################################## 
# get monthly levels of violence
monthly_viol <- read.csv("../MA-datasets/violence_stats.csv", stringsAsFactors = FALSE)
monthly_viol$date <- as.Date(as.yearmon(monthly_viol$date, "%Y-%m"))
monthly_viol[,2:4] <- sapply(monthly_viol[,2:4], function(x) { as.numeric(x)})
monthly_viol <- subset(monthly_viol, select = c(2:4, 1))

# some major dates for plotting
major_violence <- as.Date(c("7/20/13", "1/16/13", "7/29/14", "11/16/14", "4/15/15", "5/31/15", "6/15/15", "6/22/15"), "%m/%d/%y")
major_agree <- as.Date(c("8/26/12", "5/26/13", "11/6/13", "5/16/14", "3/7/15", "6/2/15", "9/23/15"), "%m/%d/%y")

cf_start <- as.Date(c("11/20/12", "12/15/13", "5/16/14", "12/20/14", "7/20/15"), "%m/%d/%y")
cf_end <- as.Date(c("1/20/13", "1/15/14", "5/28/14", "5/22/15", "1/1/16"), "%m/%d/%y")
ceasefires <- data.frame(start = as.Date(c("11/20/12", "12/15/13", "5/16/14", "12/20/14", "7/20/15"), "%m/%d/%y"), end = as.Date(c("1/20/13", "1/15/14", "5/28/14", "5/22/15", "1/1/16"), "%m/%d/%y"))

# df of all dates
dates <- rbind(data.frame(date = major_violence, group = "major_viol"), data.frame(date = major_agree, group = "major_agree"), data.frame(date = cf_start, group = "ceasefire_start"), data.frame(date = cf_end, group = "ceasefire_end"))
dates <- arrange(dates, date)

################################################################################## 
# Some functions to extract sentiment, loess it, and return results

# Function to get raw LIWC measures
liwc_extractor <- function(df) {
  # run liwc
  liwc_results <- liwcalike(df$text, spanish_dict)
  
  # get date metadata
  df_dates <- dplyr::select(df, date)
  date <- as.Date(df_dates[[1]], "%Y-%m-%d")
  
  # extract the measures we want
  liwc_results <- dplyr::select(liwc_results, EmoNeg, EmoPos, Ellos, Muerte)
  
  # make the dataframe
  results_df <- data.frame(cbind(sapply(liwc_results, function(x) {as.numeric(x)})))
  results_df$date <- as.Date(date, "%Y-%m-%d")
  return(results_df)
}

# Function to take a df of raw LIWC values and return loessed values
liwc_loess <- function(liwc_results) {
  date <- as.Date(liwc_results$date, origin = "1970-01-01")
  results_df <- data.frame(cbind(sapply(liwc_results[,1:4], function(x) { loess(x ~ as.numeric(liwc_results$date), control = loess.control(surface = "direct"))$y})))
  results_df$date <- date
  return(results_df)
}

# Function to return loess predictions as a list
loess_lines <- function(liwc_results) {
  
  list_models <- list(sapply(liwc_results[,1:4], function(x) {loess(x ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))}))
  return(list_models)
}

#################################################################################
# import FARC communiques
FARC <- read.csv("../MA-datasets/FARC_communiques.csv", stringsAsFactors = FALSE)

# raw LIWC measures
FARC_raw <- liwc_extractor(FARC)

# loess it
FARC_results <- liwc_loess(FARC_raw)

# get the loess lines for plotting
FARC_lines <- loess_lines(FARC_raw)

#################################################################################
# do the same for joint communiques
joint <- read.csv("../MA-datasets/jointstatements.csv", stringsAsFactors = FALSE)
# delete some empty documents
joint <- filter(joint, text != "")
joint <- slice(joint, -19)

# LIWC estimates
joint_raw <- liwc_extractor(joint)

# loessed point estimates
joint_results <- liwc_loess(joint_raw)

# get the loess lines for plotting
joint_lines <- loess_lines(joint_raw)

#################################################################################
# and the same for govt statements
govt <- read.csv("govtstatements.csv", stringsAsFactors = FALSE)

# LIWC estimates
govt_raw <- liwc_extractor(govt)

# loessed point estimates
govt_results <- liwc_loess(govt_raw)

# get the loess lines for plotting
govt_lines <- loess_lines(govt_raw)
  
#################################################################################
#################################################################################
# Graph it!

# Neg emotion: base graph
base_neg = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "FARC statement")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Joint statement")) +
  geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Joint statement")) +
  geom_point(data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Govt statement")) +
  geom_smooth(method = "loess", se = FALSE, data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Govt statement")) +
  labs(
    x = "Date",
    y = "Percent Neg Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA))

# Neg emotion and major agreements/violence
neg_major <- base_neg +
  ggtitle("Major Events and Percent Negative Emotion Words in Statements") + 
  geom_vline(data = filter(dates, group == "major_agree"), mapping = aes(xintercept = as.numeric(date), color = "Major agreement"), linetype = 2) +
  geom_vline(data = filter(dates, group == "major_viol"), mapping = aes(xintercept = as.numeric(date), color = "Major violence"), linetype = 1)

# Neg emotion and ceasefire dates
neg_cf <- base_neg +
  ggtitle("Ceasefires and Percent Negative Emotion Words in Statements") +
  geom_rect(aes(xmin=cf_start[1], xmax=cf_end[1], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[2], xmax=cf_end[2], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) +
  geom_rect(aes(xmin=cf_start[3], xmax=cf_end[3], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[4], xmax=cf_end[4], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[5], xmax=cf_end[5], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01)

#################################################################################
# Pos emotion: base graph
base_pos = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "FARC statement")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "Joint statement")) +
  geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "Joint statement")) +
  geom_point(data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "Govt statement")) +
  geom_smooth(method = "loess", se = FALSE, data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "Govt statement")) +
  labs(
    x = "Date",
    y = "Percent Pos Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA))

# pos emotion and major agreements
pos_major <- base_pos +
  ggtitle("Major Events and Percent Positive Emotion Words in Statements") +
  geom_vline(data = filter(dates, group == "major_agree"), mapping = aes(xintercept = as.numeric(date), color = "Major agreement"), linetype = 2) +
  geom_vline(data = filter(dates, group == "major_viol"), mapping = aes(xintercept = as.numeric(date), color = "Major violence"), linetype = 1)

# pos emotion and ceasefires
pos_cf <- base_pos +
  ggtitle("Ceasefires and Percent Positive Emotion Words in Statements") +
  geom_rect(aes(xmin=cf_start[1], xmax=cf_end[1], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[2], xmax=cf_end[2], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) +
  geom_rect(aes(xmin=cf_start[3], xmax=cf_end[3], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[4], xmax=cf_end[4], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[5], xmax=cf_end[5], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01)

#################################################################################
#################################################################################
# let's graph 3rd person plural pronouns -- indicator of extremism
base_ellos = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Ellos, color = "FARC statement")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Ellos, color = "Joint statement")) +
  geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Ellos, color = "Joint statement")) +
  geom_point(data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Ellos, color = "Govt statement")) +
  geom_smooth(method = "loess", se = FALSE, data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Ellos, color = "Govt statement")) +
  labs(
    x = "Date",
    y = "Percent 3rd Person Pl Pronouns",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA))

# add major agreements and ceasefires
ellos_major <- base_ellos +
  ggtitle("Major Events and Use of 3rd Person Pl. Pronoun") +
  geom_vline(data = filter(dates, group == "major_agree"), mapping = aes(xintercept = as.numeric(date), color = "Major agreement"), linetype = 2) +
  geom_vline(data = filter(dates, group == "major_viol"), mapping = aes(xintercept = as.numeric(date), color = "Major violence"), linetype = 1)

#################################################################################
#################################################################################
# Let's graph death topic
base_death = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Muerte, color = "FARC statement")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Muerte, color = "Joint statement")) +
  geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Muerte, color = "Joint statement")) +
  geom_point(data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Muerte, color = "Govt statement")) +
  geom_smooth(method = "loess", se = FALSE, data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = Muerte, color = "Govt statement")) +
  labs(
    x = "Date",
    y = "Percent on Death",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA))

#################################################################################
#################################################################################
# Find structural breakpoints

# Function to find breakpoints for each column of a dataframe. Takes one argument: a dataframe whose last column is the date
break_finder <- function(df) {
  # make a list to contain the breakdates
  break_obs <- vector("list", length(df) - 1)
  
  # get breakpoints
  for (i in 1:(length(df)-1) ) {
   break_obs[i] <- (list(breakpoints(df[[i]] ~ 1)$breakpoints))
  }
  return(break_obs)
}

# Function to convert break obs to dates. Takes two arguments: a list of lists (shudder), and an original df
get_breakdate <- function(listoflists, df) {
  # for each list of break obs
  for (i in 1:length(listoflists)) {
    # if it's not empty
    if (!is.na(listoflists[i])) {
      
      # get the observations
      unlisted_obs <- unlist(listoflists[i])
      dates_list <- vector("list", 0)
      
      # and then for each of the observations
      for (j in 1:length(unlisted_obs)) {
        # get the date from the dataframe and add it to the list
        current_date <- df$date[unlisted_obs[j]]
        dates_list <- append(dates_list, current_date)
      }
      listoflists[i] <- list(dates_list)
    }
  }
  
  return(listoflists)
}

FARC_breaks <- get_breakdate(break_finder(FARC_results), FARC_results)
govt_breaks <- get_breakdate(break_finder(govt_results), govt_results)
joint_breaks <- get_breakdate(break_finder(joint_results), joint_results)

# Now get the breakpoints into a list we can graph
# convert breaks to a df
convert_breaks <- function(listoflists) {
  df <- as.data.frame(unlist(listoflists))
  df$group <- NA
  # now fill in by type
  negs <- rep("neg_break", length(listoflists[[1]]))
  poss <- rep("pos_break", length(listoflists[[2]]))
  pp3 <- rep("pp3_break", length(listoflists[[3]]))
  death <- rep("death_break", length(listoflists[[4]]))
  groups <- c(negs, poss, pp3, death)
  df$group <- groups
  df <- na.omit(df)
  colnames(df)[1] <- "date" 
  df$date <- as.Date(df$date, origin = "1970-01-01")
  return(df)
}

# get the dataframes of breaks, sorted by F/G/J stream
FARC_breaks_df <- convert_breaks(FARC_breaks)
govt_breaks_df <- convert_breaks(govt_breaks)
joint_breaks_df <- convert_breaks(joint_breaks)

# Function to make dataframe of all breaks of a type (e.g. neg)
break_sorter <- function(a,b,c,d) {
  df1 <- filter(a, group == d)
  if (nrow(df1) != 0) {
    df1$group <- "FARC"
  }
  df2 <- filter(b, group == d)
  if (nrow(df2) != 0) {
    df2$group <- "govt"
  }  
  df3 <- filter(c, group == d)
  if (nrow(df3) != 0) {
    df3$group <- "joint"
  }  
  df <- rbind(df1, df2, df3)
  if (nrow(df) > 0) {
    return(df)
  }
}

neg_breaks <- break_sorter(FARC_breaks_df, govt_breaks_df, joint_breaks_df, "neg_break")
pos_breaks <- break_sorter(FARC_breaks_df, govt_breaks_df, joint_breaks_df, "pos_break")
pp3_breaks <- break_sorter(FARC_breaks_df, govt_breaks_df, joint_breaks_df, "pp3_break")
death_breaks <- break_sorter(FARC_breaks_df, govt_breaks_df, joint_breaks_df, "death_break")

# let's look at them on the graph
neg_breaks_gg <- base_neg +
  ggtitle("Breakpoints in Negative Emotion") +
  geom_vline(data = filter(neg_breaks, group == "FARC"), mapping = aes(xintercept = as.numeric(date), color = "FARC statement"), linetype = 2) +
  geom_vline(data = filter(neg_breaks, group == "govt"), mapping = aes(xintercept = as.numeric(date), color = "Govt statement"), linetype = 1) +
  geom_vline(data = filter(neg_breaks, group == "joint"), mapping = aes(xintercept = as.numeric(date), color = "Joint statement"), linetype = 3)

pos_breaks_gg <- base_pos +
  ggtitle("Breakpoints in Positive Emotion") +
  geom_vline(data = filter(pos_breaks, group == "FARC"), mapping = aes(xintercept = as.numeric(date), color = "FARC statement"), linetype = 2) +
  geom_vline(data = filter(pos_breaks, group == "joint"), mapping = aes(xintercept = as.numeric(date), color = "Joint statement"), linetype = 3) # no govt breakpoints

ellos_breaks_gg <- base_ellos +
  ggtitle("Breakpoints in Use of 3rd Person Pl. Pronoun") +
  geom_vline(data = filter(pp3_breaks, group == "govt"), mapping = aes(xintercept = as.numeric(date), color = "Govt statement"), linetype = 2)

death_breaks_gg <- base_death +
  ggtitle("Breakpoints in Death") +
  geom_vline(data = filter(death_breaks, group == "FARC"), mapping = aes(xintercept = as.numeric(date), color = "FARC statement"), linetype = 2) +
  geom_vline(data = filter(death_breaks, group == "joint"), mapping = aes(xintercept = as.numeric(date), color = "Joint statement"), linetype = 3) # no govt breakpoints

neg_breaks_gg
pos_breaks_gg
ellos_breaks_gg
deaths_breaks_gg
#################################################################################
#################################################################################
# now let's take a look at trends in violence/military activity

# make our base graph
base_viol = ggplot(monthly_viol, aes(x = as.Date(date, origin = "1970-01-01"), y = FARC_actions, color = "FARC actions")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = monthly_viol, aes(x = as.Date(date, origin = "1970-01-01"), y = deaths_fuerzapublica, color = "Army casualties (excl. wounded)")) +
  geom_smooth(method = "loess", se = FALSE, data = monthly_viol, aes(x = as.Date(date, origin = "1970-01-01"), y = deaths_fuerzapublica, color = "Army casualties (excl. wounded)")) +
  geom_point(data = monthly_viol, aes(x = as.Date(date, origin = "1970-01-01"), y = desmovilizados, color = "Militants demobilized")) +
  geom_smooth(method = "loess", se = FALSE, data = monthly_viol, aes(x = as.Date(date, origin = "1970-01-01"), y = desmovilizados, color = "Militants demobilized"))  +
  labs(
    x = "Date",
    y = "Number of Incidents",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  ggtitle("Level of Violence and Military Actions")

# let's see whether this lines up with ceasefires
viol_cf <- base_viol +
  ggtitle("Level of Violence and Ceasefires") +
  geom_rect(aes(xmin=cf_start[1], xmax=cf_end[1], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[2], xmax=cf_end[2], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) +
  geom_rect(aes(xmin=cf_start[3], xmax=cf_end[3], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[4], xmax=cf_end[4], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[5], xmax=cf_end[5], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01)

# or major events
viol_major <- base_viol +
ggtitle("Major Events and Violence Trends") +
  geom_vline(data = filter(dates, group == "major_agree"), mapping = aes(xintercept = as.numeric(date), color = "Major agreement"), linetype = 2) +
  geom_vline(data = filter(dates, group == "major_viol"), mapping = aes(xintercept = as.numeric(date), color = "Major violence"), linetype = 1)

# let's find the structural breaks ** ignore the "day"
viol_breaks <- get_breakdate(break_finder(monthly_viol), monthly_viol)

convert_vbreaks <- function(listoflists) {
  df <- as.data.frame(unlist(listoflists))
  df$group <- NA
  # now fill in by type
  farc_actions <- rep("farc_action", length(listoflists[[1]]))
  casualties <- rep("casualties", length(listoflists[[2]]))
  desmovilizados <- rep("desmovilizados", length(listoflists[[3]]))
  groups <- c(farc_actions, casualties, desmovilizados)
  df$group <- groups
  df <- na.omit(df)
  colnames(df)[1] <- "date" 
  df$date <- as.Date(df$date, origin = "1970-01-01")
  return(df)
}

# graph structural breaks in violence trends
viol_breaks_list <- convert_vbreaks(viol_breaks)

viol_breaks_gg <- base_viol +
  ggtitle("Breakpoints in Violence") +
  geom_vline(data = filter(viol_breaks_list, group == "farc_action"), mapping = aes(xintercept = as.numeric(date), color = "FARC actions"), linetype = 2) +
  geom_vline(data = filter(viol_breaks_list, group == "casualties"), mapping = aes(xintercept = as.numeric(date), color = "Army casualties (excl. wounded)"), linetype = 2) +
  geom_vline(data = filter(viol_breaks_list, group == "desmovilizados"), mapping = aes(xintercept = as.numeric(date), color = "Militants demobilized"), linetype = 2)

# the graphs
viol_cf
viol_major
viol_breaks_gg

#################################################################################
#################################################################################
# let's check out these time series of data

# ADF tests for stationarity
sapply(monthly_viol[2:4], function(x) {summary(ur.df(na.omit(x), type = "trend", lags = 1))})
# Results: FARC actions / casualties / demobilization: unit root, trend, and drift / unit root, trend, and drift / unit root, no trend, no drift
sapply(monthly_viol[2:4], function(x) {summary(ur.df(na.omit(x), type = "drift", lags = 1))})
# FARC actions / casualties / demobilization: can't reject null / reject null / reject null: unit root, and no drift
sapply(monthly_viol[2:4], function(x) {summary(ur.df(na.omit(x), type = "none", lags = 1))})
# FARC actions / casualties / demobilization: reject null / can't reject / can't reject

# KPSS
sapply(monthly_viol[2:4], function(x) {kpss.test(x, null = "T")})

# check ndiffs: 1 differencing needed for each trend to make stationary
sapply(monthly_viol[2:4], function(x) { ndiffs(x)})

# Now let's see if FARC actions and army casualties are cointegrated.
viol_VAR <- VAR(na.omit(monthly_viol[,2:3]), p = 2, type = "both")

# test for serial correlation of residuals. Increase lags p = 2 in the VAR model to get p val = 0.94: no autocorrelation
serial.test(viol_VAR)

# Now let's run the Johansen cointgration test
summary(ca.jo(na.omit(monthly_viol[,2:3]), type = "trace", K = 2, ecdet = "trend"))
# result: cointegration

# test Granger causality both ways

# significant only when lags = 3, with p = 0.01
grangertest(na.omit(monthly_viol[,2]) ~ na.omit(monthly_viol[,3]), order = 3)

# significant with p = 0.0007
grangertest(na.omit(monthly_viol[,3]) ~ na.omit(monthly_viol[,2]), order = 1)
# result: army deaths are Granger caused by FARC actions

#################################################################################
#################################################################################
# let's look at the means before and after structural breaks

# Function to calculate means in each type of sentiment during "regimes" bounded by structural breaks. Takes two arguments: df of dates with a group ID of break type, and df of loess values
calculate_breakmeans <- function(df, loessed) {
  # need to rename groups so they match up with columns in the loess df
  df$group <- gsub("neg_break", "EmoNeg", df$group)
  df$group <- gsub("pos_break", "EmoPos", df$group)
  df$group <- gsub("pp3_break", "Ellos", df$group)
  df$group <- gsub("death_break", "Muerte", df$group)
  
  # which types of sentiment have breaks?
  groups <- distinct(df, group)$group
  # list to contain the means for each sentiment
  listofmeans <- vector("list", length(groups))

  # for each type of sentiment
  for (i in 1:length(groups)) {
    # get the breakdates for that type
    breaks <- filter(df, group == groups[i])
    # get the name of the type
    senti_name <- groups[i]
    
    # get the correct columns from the loess df, supplied as an argument to the function
    data <- cbind(loessed["date"], loessed[senti_name])
    
    # number of structural breaks
    break_len <- length(breaks[[1]])
    
    # IDs to return
    IDs <- list(senti_name, breaks["date"])
    # Case #1: 2 breaks, 3 regimes
    if (break_len == 2) {
      data1 <- filter(data, date < breaks[1,1])
      mean1 <- mean(unlist(data1[senti_name]))
      
      data2 <- filter(data, date >= breaks[1,1], date < breaks[2,1])
      mean2 <- mean(unlist(data2[senti_name]))
      
      data3 <- filter(data, date >= breaks[2,1])
      mean3 <- mean(unlist(data3[senti_name]))
      
      means <- c(mean1, mean2, mean3)
    }
    
    # Case #2: only 1 break, 2 regimes
    else {
      data1 <- filter(data, date < breaks[1,1])
      mean1 <- mean(unlist(data1[senti_name]))
      
      data2 <- filter(data, date >= breaks[1,1])
      mean2 <- mean(unlist(data2[senti_name]))
      
      means <- c(mean1, mean2)
    }
    
    listofmeans[[i]] <- list(IDs, means)
  }
  return(listofmeans)
}

#################################################################################
#################################################################################
# get all the means of regimes defined by structural breakpoints in emotion. 
FARC_means <- calculate_breakmeans(FARC_breaks_df, FARC_results)
govt_means <- calculate_breakmeans(govt_breaks_df, govt_results)
joint_means <- calculate_breakmeans(joint_breaks_df, joint_results)

neg_breaks_gg
pos_breaks_gg
ellos_breaks_gg
death_breaks_gg

#################################################################################
#################################################################################
# now let's do the same for structural breaks in the violence time series

# modified function
calculate_viol_breakmeans <- function(df, loessed) {
  loessed <- na.omit(loessed)
  df$group <- gsub("farc_action", "FARC_actions", df$group)
  df$group <- gsub("casualties", "deaths_fuerzapublica", df$group)

  # which types of sentiment have breaks?
  groups <- distinct(df, group)$group
  
  # list to contain the means for each sentiment
  listofmeans <- vector("list", length(groups))
  
  # for each type of sentiment
  for (i in 1:length(groups)) {
    # get the breakdates for that type
    breaks <- filter(df, group == groups[i])
    # get the name of the type
    senti_name <- groups[i]
    # get the correct columns from the loess df, supplied as an argument to the function
    data <- cbind(loessed["date"], loessed[senti_name])
    # IDs to return
    IDs <- list(senti_name, breaks["date"])

    data1 <- filter(data, date < breaks[1,1])
    mean1 <- mean(unlist(data1[senti_name]))
    
    data2 <- filter(data, date >= breaks[1,1], date < breaks[2,1])
    mean2 <- mean(unlist(data2[senti_name]))
    
    data3 <- filter(data, date >= breaks[2,1])
    mean3 <- mean(unlist(data3[senti_name]))
      
    means <- c(mean1, mean2, mean3)
    
    listofmeans[[i]] <- list(IDs, means)
  }
  
  return(listofmeans)
}

viol_means <- calculate_viol_breakmeans(viol_breaks_list, monthly_viol)

viol_breaks_gg

#################################################################################
#################################################################################
# Introducing... public opinion
public_op <- read.csv("../MA-datasets/public opinion.csv", stringsAsFactors = FALSE)

public_op <- public_op[,1:3]
public_op$date <- as.Date(as.yearmon(public_op$date, "%Y-%m"))
public_op[,2:3] <- sapply(public_op[,2:3], function(x) { as.numeric(x)})
public_op <- subset(public_op, select = c(2:3, 1))
public_op <- na.omit(public_op)

# graph it
base_opinion = ggplot(public_op, aes(x = as.Date(date, origin = "1970-01-01"), y = santos_positive_image, color = "Positive image of Pres. Santos")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = public_op, aes(x = as.Date(date, origin = "1970-01-01"), y = approve_santos_decision_talks, color = "Approve of negotiations with guerrillas")) +
  geom_smooth(method = "loess", se = FALSE, data = public_op, aes(x = as.Date(date, origin = "1970-01-01"), y = approve_santos_decision_talks, color = "Approve of negotiations with guerrillas")) +
  labs(
    x = "Date",
    y = "Percent Approve/Positive Image",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  ggtitle("Public Opinion")

# public opinion and ceasefires
opinion_cf = base_opinion +
  ggtitle("Public Opinion and Ceasefires") +
  geom_rect(aes(xmin=cf_start[1], xmax=cf_end[1], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[2], xmax=cf_end[2], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) +
  geom_rect(aes(xmin=cf_start[3], xmax=cf_end[3], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[4], xmax=cf_end[4], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01) + 
  geom_rect(aes(xmin=cf_start[5], xmax=cf_end[5], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.01)

# public opinion and major events
opinion_major = base_opinion + 
  ggtitle("Major Events and Public Opinion Trends") +
  geom_vline(data = filter(dates, group == "major_agree"), mapping = aes(xintercept = as.numeric(date), color = "Major agreement"), linetype = 2) +
  geom_vline(data = filter(dates, group == "major_viol"), mapping = aes(xintercept = as.numeric(date), color = "Major violence"), linetype = 1)

# get breakdates in public opinion
opinion_breakd <- get_breakdate(break_finder(na.omit(public_op)), public_op)

#################################################################################
#################################################################################
# Results for demo

# Raw LIWC scores
View(FARC_raw)
View(govt_raw)
View(joint_raw)

# Loessed results
View(FARC_results)
View(govt_results)
View(joint_results)

# Violence, major events, public opinion data
View(monthly_viol)
View(dates)
View(public_op)

# View structural breaks, by type
View(neg_breaks)
View(pos_breaks)
View(pp3_breaks)
View(death_breaks)

# Structural breaks by party, with means of regimes.
# [[1]][[1]] to access the means,  [[1]][[2]] to get the corresponding breakdates
FARC_means
govt_means
joint_means
viol_means

# Base graphs (all loessed)
base_neg
base_pos
base_ellos
base_death
base_viol
base_opinion

# Graphs with ceasefires and major events
neg_cf
neg_major

pos_cf
pos_major

ellos_major

death_major

viol_major
viol_cf

opinion_cf
opinion_major

# Graphs with structural breaks
neg_breaks_gg
pos_breaks_gg
ellos_breaks_gg
death_breaks_gg
viol_breaks_gg

#################################################################################
#################################################################################

# Run hidden Markov model
# let's limit it to just 3 sentiment measures
FARC_results1 <- FARC_results[, -4]
FARC_results1 <- filter(FARC_results1, date >= "2012-01-01")

# formulas
forms1 <- list(FARC_results1$EmoNeg ~ 1, FARC_results1$EmoPos ~ 1, FARC_results1$Ellos ~ 1)

# Find the optimal number of states using BIC -- before fitting
num_states <- seq(2, 10, by = 1)

BIC_vals <- sapply(num_states, function(x) {BIC(depmix(forms1, family = list(gaussian(), gaussian(), gaussian()), nstates = x, data = FARC_results1))})

BIC_df <- data.frame(cbind(num_states, BIC_vals))

# plot the BIC values to select the optimal number of states
BIC_plot <- ggplot(BIC_df, aes(x = num_states, y = BIC_vals)) +
  geom_point() +
  ggtitle("BIC Values for n = 2:10 Latent States HMM")

# add the covariates: monthly violence
# Function takes 1 parameter: a dataframe, and returns one parameter: a dataframe
add_monthlies <- function(df) {
  dates <- df["date"]
  
  # add columns for the monthly data we're adding
  col_names <- c("FARC_actions", "army_casualties", "pres_approve", "peace_approve")
  df[, col_names] <- NA
  for (i in 1:length(dates[[1]])) {
    date <- dates[i, 1]
    year <- format(date, "%Y")
    month <- format(date, "%m")
    
    monthly_date <- as.Date(paste(year, month, "01", sep = "-"))
    
    # get the stats from violence and opinion dfs
    viol <- filter(monthly_viol, date == monthly_date)
    public <- filter(public_op, date == monthly_date)
    
    # write them to new df
    df["FARC_actions"][i, 1] <- as.numeric(viol[1])
    df["army_casualties"][i, 1] <- as.numeric(viol[2])
    df["pres_approve"][i, 1] <- as.numeric(public[1])
    df["peace_approve"][i, 1] <- as.numeric(public[2])
  }
  
  return(df)
}

# run to add violence/public opinion levels to FARC df
FARC_results2 <- add_monthlies(FARC_results1)
govt_results1 <- add_monthlies(govt_results)

forms2 <- list(FARC_results2$EmoNeg ~ 1, FARC_results2$EmoPos ~ 1, FARC_results2$Ellos ~ 1)

# what are BIC values of the fitted models?
BIC_vals2 <- sapply(num_states, function(x) {BIC(fit(depmix(forms2, family = list(gaussian(), gaussian(), gaussian()), nstates = x, data = FARC_results2[,-(6:9)])))})

BIC_df2 <- data.frame(cbind(num_states, BIC_vals2))

# plot the BIC values to select the optimal number of states
BIC_plot2 <- ggplot(BIC_df2, aes(x = num_states, y = BIC_vals2)) +
  geom_point() +
  ggtitle("BIC Values for n = 2:10 Latent States Fitted HMM w/ Covariates")

# what are AIC values of the fitted models?
AIC_vals <- sapply(num_states, function(x) {AIC(fit(depmix(forms2, family = list(gaussian(), gaussian(), gaussian()), nstates = x, data = FARC_results2[,-(6:9)], transitions = list(~ FARC_actions, ~ army_casualties, ~ pres_approve, ~ peace_approve))))})

AIC_df <- data.frame(cbind(num_states, AIC_vals))

# plot the BIC values to select the optimal number of states
AIC_plot <- ggplot(AIC_df, aes(x = num_states, y = AIC_vals)) +
  geom_point() +
  ggtitle("AIC Values for n = 2:10 Latent States Fitted HMM w/ Covariates")

# run the model for states n = 3
mod <- depmix(forms2, family = list(gaussian(), gaussian(), gaussian()), nstates = 3, data = FARC_results2[,-(6:9)], transitions = list(~ FARC_actions, ~ army_casualties, ~ pres_approve, ~ peace_approve))
hmm_mod <- fit(mod)
summary(hmm_mod)

# what state are we in at a given time t?
head(posterior(hmm_mod)) # graph this later



#################################################################################
#################################################################################
# Transition model: sentiment-responds-to-sentiment

# Classify overall sentiment as "high" or "low": Compare negative and positive emotion proportions within a given document. 1 = high, 0 = low
FARC_results3 <- FARC_results
FARC_results3["sentiment_level"] <- as.numeric(FARC_results3$EmoNeg <= FARC_results3$EmoPos)
FARC_results3["side"] <- "FARC"
govt_results3 <- govt_results
govt_results3["side"] <- "govt"
govt_results3["sentiment_level"] <- as.numeric(govt_results3$EmoNeg <= govt_results3$EmoPos)

# combine them into one stream
transition_chain <- rbind(FARC_results3, govt_results3)
# and sort by date
transition_chain <- transition_chain[order(as.Date(transition_chain$date, format = "%Y-%m-%d")), ]
# and filter to the peace process
transition_chain <- filter(transition_chain, date > "2012-01-01")

# pairwise comparison to gauge "responsiveness": how often are the parties giving statements at t and t-1 different?

get_responsiveness <- function(df) {
  # put boolean of pairwise comparisons here
  vect <- rep(NA, (length(df[ , 1]) - 1))
  
  for (i in 2:length(df[ , 1])) {
    side_t <- df["side"][i, ]
    side_t_1 <- df["side"][i-1, ]
    
    vect[i] <- side_t == side_t_1
  }
  
  return(vect)
}

pairwise_responsiveness <- get_responsiveness(transition_chain)


#################################################################################
#################################################################################
# PCA analysis
FARC_corpus <- corpus(FARC$text, docvars = FARC_results$dates)

govt_corpus <- corpus(govt$text, docvars = govt_results$dates)

all_corpora <- FARC_corpus + govt_corpus

# docvars for inserting: sides and dates
sides <- c(rep("FARC", length(FARC_corpus[, 1])), rep("govt", length(govt_corpus[, 1])))
pca_dates <- c(FARC_results$date, govt_results$date)

# make dfm
all_dfm <- dfm(all_corpora, language = "spanish", stem = TRUE, ignoredFeatures = stopwords("spanish"))

# run PCA
statements_PCA <- prcomp(all_dfm, center = TRUE, scale. = TRUE)
summary(statements_PCA)

# plot it
plot(statements_PCA, type = "l", main="PCA of FARC and Govt Statements")
# first 2 PCs account for ~10% of variance. Could be better...

# create graph of PC1 and PC2
PC_graph <- ggbiplot(statements_PCA, obs.scale = 1, var.scale = 1, groups = sides)
PC_graph <- PC_graph + theme(legend.direction = "horizontal", legend.position = "top")
PC_graph

# Graph PC1 as a time series:

# collect date and side metadata with PC1 values
statements_PC1_2 <- data.frame(statements_PCA$x[1:length(statements_PCA$x[,1]),1:2])
statements_PC1_2["date"] <- pca_dates
statements_PC1_2["side"] <- sides
colnames(statements_PC1_2) <- c("PC1", "PC2", "date", "side")

PC1_gg <- ggplot(filter(statements_PC1_2, side == "FARC"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC1, color = "FARC")) +
  geom_jitter() +
  geom_point(data = filter(statements_PC1_2, side == "govt"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC1, color = "Govt")) +
  ggtitle("Plot of First Principal Components over Time") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA))

# Graph PC2
# collect date and side metadata with PC1 values
PC2_gg <- ggplot(filter(statements_PC1_2, side == "FARC"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC2, color = "FARC")) +
  geom_jitter() +
  geom_point(data = filter(statements_PC1_2, side == "govt"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC2, color = "Govt")) +
  ggtitle("Plot of Second Principal Components over Time") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA))

# FARC statement # 94 is an outlier. Let's try removing it, and rerun PCA
# trimmed_statements_PC1_2 <- filter(statements_PC1_2, PC2 < 231)
FARC_corpus_trimmed <- corpus(FARC$text[-94], docvars = FARC_results$dates[-94])
all_corpora_trimmed <- FARC_corpus_trimmed + govt_corpus
all_dfm_trimmed <- dfm(all_corpora_trimmed, language = "spanish", stem = TRUE, ignoredFeatures = stopwords("spanish"))
statements_PCA_trimmed <- prcomp(all_dfm_trimmed, center = TRUE, scale. = TRUE)
summary(statements_PCA_trimmed)

# plot it: still not great
plot(statements_PCA_trimmed, type = "l", main="PCA of FARC and Govt Statements")

statements_PC1_2_trimmed <- data.frame(statements_PCA_trimmed$x[1:length(statements_PCA_trimmed$x[,1]),1:2])
statements_PC1_2_trimmed["date"] <- pca_dates[-94]
statements_PC1_2_trimmed["side"] <- sides[-94]
colnames(statements_PC1_2_trimmed) <- c("PC1", "PC2", "date", "side")

# Plot PC1 time series
PC1_gg_trimmed <- ggplot(filter(statements_PC1_2_trimmed, side == "FARC"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC1, color = "FARC")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = filter(statements_PC1_2_trimmed, side == "govt"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC1, color = "Govt")) +
  geom_smooth(method = "loess", se = FALSE, data = statements_PC1_2_trimmed, aes(x = as.Date(date, origin = "1970-01-01"), y = PC1, color = "Govt")) +
  ggtitle("Plot of First Principal Components over Time (Outlier Removed)") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  labs(
    x = "Date",
    y = "PC1",
    color = "Legend")

# Graph PC2
# collect date and side metadata with PC1 values
PC2_gg_trimmed <- ggplot(filter(statements_PC1_2_trimmed, side == "FARC"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC2, color = "FARC")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = filter(statements_PC1_2_trimmed, side == "govt"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC2, color = "Govt")) +
  geom_smooth(method = "loess", se = FALSE, data = statements_PC1_2_trimmed, aes(x = as.Date(date, origin = "1970-01-01"), y = PC2, color = "Govt")) +
  ggtitle("Plot of Second Principal Components over Time (Outlier Removed)") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  labs(
    x = "Date",
    y = "PC2",
    color = "Legend")


#################################################################################
#################################################################################
# Robust PCA
rob_pca <- PcaHubert(all_dfm)
# First 2 components account for 65% of variance
print(rob_pca)
summary(rob_pca)

# plot
screeplot(rob_pca, type = "lines", main = "Robust PCA with 10 Components")

# let's plot the time series of PC1
rob_pc1 <- data.frame(rob_pca@scores)
rob_pc1 <- dplyr::select(rob_pc1, PC1)
rob_pc1["side"] <- sides
rob_pc1["date"] <- pca_dates

# Robust PC1 graph
PC1_gg_robust <- ggplot(filter(rob_pc1, side == "FARC"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC1, color = "FARC")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = filter(rob_pc1, side == "govt"), aes(x = as.Date(date, origin = "1970-01-01"), y = PC1, color = "Govt")) +
  geom_smooth(method = "loess", se = FALSE, data = rob_pc1, aes(x = as.Date(date, origin = "1970-01-01"), y = PC1, color = "Govt")) +
  ggtitle("Plot of First Principal Components over Time (Robust PCA)") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) + 
  labs(
    x = "Date",
    y = "PC1",
    color = "Legend")