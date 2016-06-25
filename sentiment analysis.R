# Leslie Huang
# LIWC analysis of FARC communiques

rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/MA-thesis-analysis/")

set.seed(1234)

libraries <- c("foreign", "utils", "stargazer", "dplyr", "devtools", "quanteda", "quantedaData", "ggplot2", "stringr", "LIWCalike", "topicmodels", "lda", "stm", "LDAvis", "austin", "forecast", "lmtest", "strucchange", "vars", "tseries", "urca")
lapply(libraries, require, character.only=TRUE)

# get LIWC dict
spanish_dict <- dictionary(file = "../LIWC/Spanish_LIWC2007_Dictionary.dic", format = "LIWC")

################################################################################## 
################################################################################## 
# get monthly levels of violence
monthly_viol <- read.csv("../MA-datasets/violence_stats.csv", stringsAsFactors = FALSE)


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
import FARC communiques
FARC <- read.csv("../MA-datasets/FARC_communiques.csv", stringsAsFactors = FALSE)

# Function to get raw LIWC measures
liwc_extractor <- function(df) {
  # run liwc
  liwc_results <- liwcalike(df$text, spanish_dict)
  
  # get date metadata
  df_dates <- dplyr::select(df, date)
  date <- as.Date(df_dates[[1]], "%Y-%m-%d")
  
  # extract the measures we want, and lowess them
  neg <- as.numeric(liwc_results$EmoNeg)
  pos <- as.numeric(liwc_results$EmoPos)
  pp3 <- as.numeric(liwc_results$Ellos)
  death <- as.numeric(liwc_results$Muerte)

  # make the dataframe
  results_df <- data.frame(cbind(date, neg, pos, pp3, death))
  results_df$date <- as.Date(date, "%Y-%m-%d")
  return(results_df)
}

# Function to take a df of raw LIWC values and return loessed values
liwc_loess <- function(liwc_results) {

  neg <- loess(liwc_results$neg ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))$y
  pos <- loess(liwc_results$pos ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))$y
  pp3 <- loess(liwc_results$pp3 ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))$y
  death <- loess(liwc_results$death ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))$y
  
  # make the dataframe
  date <- as.Date(liwc_results$date, origin = "1970-01-01")
  results_df <- data.frame(cbind(date, neg, pos, pp3, death))
  results_df$date <- date
  return(results_df)
}

# Function to return loess predictions as a list
loess_lines <- function(liwc_results) {
  
  neg <- loess(liwc_results$neg ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))
  pos <- loess(liwc_results$pos ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))
  pp3 <- loess(liwc_results$pp3 ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))
  death <- loess(liwc_results$death ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))
  
  # make the list
  list_models <- list(neg, pos, pp3, death)
  return(list_models)
}

# raw LIWC measures
FARC_raw <- liwc_extractor(FARC)

# loess it
FARC_results <- liwc_loess(FARC_raw)

# get the lines for plotting
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

# get the lines for plotting
joint_lines <- loess_lines(joint_raw)

#################################################################################
# and the same for govt statements

govt <- read.csv("govtstatements.csv", stringsAsFactors = FALSE)

# LIWC estimates
govt_raw <- liwc_extractor(govt)

# loessed point estimates
govt_results <- liwc_loess(govt_raw)

# get the lines for plotting
govt_lines <- loess_lines(govt_raw)
  
#################################################################################
#################################################################################
# Graph it!

# Neg emotion: base graph
base_neg = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = neg, color = "FARC statement")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = neg, color = "Joint statement")) +
  geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = neg, color = "Joint statement")) +
  geom_point(data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = neg, color = "Govt statement")) +
  geom_smooth(method = "loess", se = FALSE, data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = neg, color = "Govt statement")) +
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
  geom_rect(aes(xmin=cf_start[1], xmax=cf_end[1], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3) + 
  geom_rect(aes(xmin=cf_start[2], xmax=cf_end[2], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3) +
  geom_rect(aes(xmin=cf_start[3], xmax=cf_end[3], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3) + 
  geom_rect(aes(xmin=cf_start[4], xmax=cf_end[4], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3) + 
  geom_rect(aes(xmin=cf_start[5], xmax=cf_end[5], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3)

#################################################################################
# Pos emotion: base graph
base_pos = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pos, color = "FARC statement")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pos, color = "Joint statement")) +
  geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pos, color = "Joint statement")) +
  geom_point(data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pos, color = "Govt statement")) +
  geom_smooth(method = "loess", se = FALSE, data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pos, color = "Govt statement")) +
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
  geom_rect(aes(xmin=cf_start[1], xmax=cf_end[1], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3) + 
  geom_rect(aes(xmin=cf_start[2], xmax=cf_end[2], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3) +
  geom_rect(aes(xmin=cf_start[3], xmax=cf_end[3], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3) + 
  geom_rect(aes(xmin=cf_start[4], xmax=cf_end[4], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3) + 
  geom_rect(aes(xmin=cf_start[5], xmax=cf_end[5], ymin=-Inf, ymax=Inf), fill = "yellow", linetype = 0, alpha = 0.3)

#################################################################################
# let's graph 3rd person plural pronouns from FARC -- indicator of extremism
base_ellos = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pp3, color = "FARC statement")) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_jitter() +
  geom_point(data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pp3, color = "Joint statement")) +
  geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pp3, color = "Joint statement")) +
  geom_point(data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pp3, color = "Govt statement")) +
  geom_smooth(method = "loess", se = FALSE, data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = pp3, color = "Govt statement")) +
  labs(
    x = "Date",
    y = "Percent Pos Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA))


# add major agreements and ceasefires
ellos_major <- base_ellos +
  ggtitle("Major Events and Use of 3rd Person Pl. Pronoun") +
  geom_vline(data = filter(dates, group == "major_agree"), mapping = aes(xintercept = as.numeric(date), color = "Major agreement"), linetype = 2) +
  geom_vline(data = filter(dates, group == "major_viol"), mapping = aes(xintercept = as.numeric(date), color = "Major violence"), linetype = 1)

# run all the graphs
base_neg
base_pos
base_ellos

neg_cf
neg_major
pos_cf
pos_major
ellos_major

#################################################################################
#################################################################################
# Find breakpoints

# Function to find breakpoints for each column of a dataframe. Takes one argument: a dataframe whose first column is the date
break_finder <- function(df) {
  # make a list to contain the dates
  break_obs <- vector("list", length(df) - 1)
  
  # get breakpoints
  for (i in 2:length(df)) {
   break_obs[i - 1] <- (list(breakpoints(df[[i]] ~ 1)$breakpoints))
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

# get the dataframes
FARC_breaks_df <- convert_breaks(FARC_breaks)
govt_breaks_df <- convert_breaks(govt_breaks)
joint_breaks_df <- convert_breaks(joint_breaks)

# now sort by type instead of author
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
  geom_vline(data = filter(neg_breaks, group == "FARC"), mapping = aes(xintercept = as.numeric(date), color = "FARC break"), linetype = 2) +
  geom_vline(data = filter(neg_breaks, group == "govt"), mapping = aes(xintercept = as.numeric(date), color = "Govt break"), linetype = 1) +
  geom_vline(data = filter(neg_breaks, group == "joint"), mapping = aes(xintercept = as.numeric(date), color = "Joint break"), linetype = 3)

pos_breaks_gg <- base_pos +
  ggtitle("Breakpoints in Positive Emotion") +
  geom_vline(data = filter(pos_breaks, group == "FARC"), mapping = aes(xintercept = as.numeric(date), color = "FARC break"), linetype = 2) +
  geom_vline(data = filter(pos_breaks, group == "joint"), mapping = aes(xintercept = as.numeric(date), color = "Joint break"), linetype = 3)


neg_breaks_gg
pos_breaks_gg

#################################################################################
#################################################################################
# Loess out some new data
all_dates <- unique(sort(as.numeric(c(FARC_results$date, joint_results$date, govt_results$date))))

# function that takes a df of raw points, estimates loess, and fills out new data with it
loess_filler <- function(liwc_results) {
  date <- all_dates
  neg <- loess(liwc_results$neg ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))
  pos <- loess(liwc_results$pos ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))
  pp3 <- loess(liwc_results$pp3 ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))
  death <- loess(liwc_results$death ~ as.numeric(liwc_results$date), control=loess.control(surface="direct"))
  
  pred_neg <- predict(neg, newdata = date)
  pred_pos <- predict(pos, newdata = date)
  pred_pp3 <- predict(pp3, newdata = date)
  pred_death <- predict(death, newdata = date)
  
  # make the dataframe
  results_df <- data.frame(cbind(date, pred_neg, pred_pos, pred_pp3, pred_death))
  results_df$date <- as.Date(date, origin = "1970-01-01")
  return(results_df)
}

# predict missing values for our data
FARC_predicted <- loess_filler(FARC_raw)
govt_predicted <- loess_filler(govt_raw)
joint_predicted <- loess_filler(joint_raw)

# where are the structural changes in these data?
pred_FARC_breaks <- get_breakdate(break_finder(FARC_predicted), FARC_predicted)
pred_govt_breaks <- get_breakdate(break_finder(govt_predicted), govt_predicted)
pred_joint_breaks <- get_breakdate(break_finder(joint_predicted), joint_predicted)






#################################################################################
#################################################################################
# Time Series Analysis: Negative Emotion

# looking at the base_neg graph, all 3 sets of data (F, G, and J) appear non-stationary

# linear model
neg_lm <- lm(joint_predicted$pred_neg ~ FARC_predicted$pred_neg + govt_predicted$pred_neg)

# let's run the augmented Dickey-Fuller test

adf.test(pred_F_neg) # DF = -1.47, p = 0.8
adf.test(pred_govt_neg) # DF = -4.66,  p < 0.01
adf.test(pred_joint_neg) # DF = 0.73, p > 0.99

# p-vals don't look great, but no unit roots 

# now let's try ADF with trends and drift
# Trend: tau: gamma=1 phi3: gamma = a2 = 0 phi2: a0 = gamma = a2 = 0

summary(ur.df(y = pred_F_neg, type = "trend", lags = 1)) # can't reject null
summary(ur.df(y = pred_F_neg, type = "drift", lags = 1)) # can't reject null
summary(ur.df(y = pred_F_neg, type = "none", lags = 1)) # there is a unit root

summary(ur.df(y = pred_govt_neg, type = "trend", lags = 1)) # there is trend and drift
summary(ur.df(y = pred_govt_neg, type = "drift", lags = 1))
summary(ur.df(y = pred_govt_neg, type = "none", lags = 1))

summary(ur.df(y = pred_joint_neg, type = "trend", lags = 1)) # there is time trend
summary(ur.df(y = pred_joint_neg, type = "drift", lags = 1)) # there is drift
summary(ur.df(y = pred_joint_neg, type = "none", lags = 1)) # no unit root

# also, let's run the KPSS test
ur.kpss(pred_F_neg)

kpss.test(pred_F_neg, null = "T") # not trend stationary, p < 0.01
kpss.test(pred_govt_neg, null = "T") # trend stationary, p = 0.05
kpss.test(pred_joint_neg, null = "T") # trend stationary, p = 0.018

# so we have non stationary time series (probably). how many times do we have to difference to get stationarity?
ndiffs(pred_F_neg) # 1
ndiffs(pred_govt_neg) # 0!
ndiffs(pred_joint_neg) # 0!

# hmmm. This suggests govt and joint data are already stationary

# Let's take the first differences to find out the max order of integration
d_F_neg <- diff(pred_F_neg, 1)
d_govt_neg <- diff(pred_govt_neg, 1)
d_joint_neg <- diff(pred_joint_neg, 1)

kpss.test(d_F_neg, null = "L") 
kpss.test(d_govt_neg, null = "L")
kpss.test(d_joint_neg, null = "L")
# all results are p > 0.1: can't reject null hypothesis of level stationarity
# so the maximum order of integration is likely I(1) for FARC

#################################################################################
# Granger causality

grangertest(pred_joint_neg ~ pred_F_neg, order = 2)
grangertest(pred_F_neg ~ pred_joint_neg, order = 2)


grangertest(d_F_neg ~ d_joint_neg, order = 2)

#################################################################################
# Fitting a VAR model

# let's get our data into a frame
predicted_negs <- data.frame(all_dates, pred_joint_neg, pred_F_neg, pred_govt_neg)

jf_VAR <- VAR(predicted_negs[,2:3], p = 1, type = "both")
serial.test(jf_VAR, lags.pt = 5) # looks like this model is plagued by serial correlation


#################################################################################
#################################################################################
# topic model

# create corpus
FARC_corp <- corpus(FARC$text, docvars = FARC_results$dates)

FARC_dfm <- dfm(FARC_corp, language = "spanish", stem = TRUE, ignoredFeatures = stopwords("spanish"))

trim_FARC <- quanteda::trim(FARC_dfm, minCount = 30, minDoc = 10)
TM <- LDA(trim_FARC, 30, method = "Gibbs", control = list(burnin = 3, thin = 30, iter = 30, seed = 1234))
top10words <- get_terms(TM, k = 10)
doc_topics <- TM@gamma

LDApost <- posterior(TM)

jsonLDA <- createJSON(phi = LDApost$terms, 
                      theta = LDApost$topics, 
                      doc.length = ntoken(trim_FARC), 
                      vocab = features(trim_FARC), 
                      term.frequency = colSums(trim_FARC))
serVis(jsonLDA, out.dir = "visCollLDA", open.browser = TRUE)

