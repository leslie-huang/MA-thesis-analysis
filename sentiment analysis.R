# Leslie Huang
# LIWC analysis of FARC communiques

rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/MA-thesis-analysis/")

libraries <- c("foreign", "utils", "stargazer", "dplyr", "devtools", "quanteda", "quantedaData", "ggplot2", "stringr", "LIWCalike", "topicmodels", "lda", "stm", "LDAvis", "austin", "forecast", "lmtest", "strucchange" "vars", "tseries", "urca")
lapply(libraries, require, character.only=TRUE)

# get LIWC dict
spanish_dict <- dictionary(file = "../LIWC/Spanish_LIWC2007_Dictionary.dic", format = "LIWC")

# some major dates for plotting
major_violence <- as.Date(c("7/20/13", "1/16/13", "7/29/14", "11/16/14", "4/15/15", "5/31/15", "6/15/15", "6/22/15"), "%m/%d/%y")
major_agree <- as.Date(c("8/26/12", "5/26/13", "11/6/13", "5/16/14", "3/7/15", "6/2/15", "9/23/15"), "%m/%d/%y")
cf_start <- as.Date(c("11/20/12", "12/15/13", "5/16/14", "12/20/14", "7/20/15"), "%m/%d/%y")
cf_end <- as.Date(c("1/20/13", "1/15/14", "5/28/14", "5/22/15", "1/1/16"), "%m/%d/%y")

ceasefires <- data.frame(start = as.Date(c("11/20/12", "12/15/13", "5/16/14", "12/20/14", "7/20/15"), "%m/%d/%y"), end = as.Date(c("1/20/13", "1/15/14", "5/28/14", "5/22/15", "1/1/16"), "%m/%d/%y"))

# dataframe of all dates
dates <- rbind(data.frame(date = major_violence, group = "major_viol"), data.frame(date = major_agree, group = "major_agree"))

################################################################################## import FARC communiques
FARC <- read.csv("../MA-datasets/FARC_communiques.csv", stringsAsFactors = FALSE)

# metadata: get date
FARC_meta <- select(FARC, date)
FARC_dates <- as.Date(FARC_meta[[1]], "%Y-%m-%d")

# run LIWC
liwc_FARC <- liwcalike(FARC$text, spanish_dict)

# neg and pos emotion
FARC_neg <- data.frame(cbind(FARC_dates, as.numeric(liwc_FARC$EmoNeg)))
FARC_neg$FARC_dates <- as.Date(FARC_dates, origin = "1970-01-01")

FARC_pos <- data.frame(cbind(FARC_dates, as.numeric(liwc_FARC$EmoNeg)))
FARC_pos$FARC_dates <- as.Date(FARC_dates, origin = "1970-01-01")

# use lowess
# lowess_F_neg <- lowess(FARC_dates, y = FARC_neg$V2, f = 2/3, iter = 3, delta = 0.01 * diff(range(FARC_dates)))
# 
# lowess_F_pos <- lowess(FARC_dates, y = FARC_pos$V2, f = 2/3, iter = 3, delta = 0.01 * diff(range(FARC_dates)))

FARC_neg$V2 <- loess_F_neg$y
FARC_pos$V2 <- loess_F_pos$y

loess_F_neg <- loess(FARC_neg$V2 ~ as.numeric(FARC_dates), control=loess.control(surface="direct"))
loess_F_pos <- loess(FARC_pos$V2 ~ as.numeric(FARC_dates), control=loess.control(surface="direct"))

#################################################################################
# do the same for joint communiques
joint <- read.csv("../MA-datasets/jointstatements.csv", stringsAsFactors = FALSE)
# delete some empty documents
joint <- filter(joint, text != "")
joint <- slice(joint, -19)

# get metadata: dates
joint_meta <- select(joint, date)
joint_dates <- as.Date(joint_meta[[1]], "%Y-%m-%d")

# run LIWC
liwc_joint <- liwcalike(joint$text, spanish_dict)

# get neg and pos emotion
joint_neg <- as.data.frame(cbind(joint_dates, as.numeric(liwc_joint$EmoNeg)))
joint_neg$joint_dates <- as.Date(joint_neg$joint_dates, origin = "1970-01-01")
joint_pos <- as.data.frame(cbind(joint_dates, as.numeric(liwc_joint$EmoPos)))
joint_pos$joint_dates <- as.Date(joint_neg$joint_dates, origin = "1970-01-01")

# use lowess
# lowess_joint_neg <- lowess(joint_dates, y = joint_neg$V2, f = 2/3, iter = 3, delta = 0.01 * diff(range(joint_dates)))
# lowess_joint_pos <- lowess(joint_dates, y = joint_pos$V2, f = 2/3, iter = 3, delta = 0.01 * diff(range(joint_dates)))

joint_neg$V2 <- loess_joint_neg$y
joint_pos$V2 <- loess_joint_pos$y

loess_joint_neg <- loess(joint_neg$V2 ~ as.numeric(joint_dates), control=loess.control(surface="direct"))
loess_joint_pos <- loess(joint_pos$V2 ~ as.numeric(joint_dates), control=loess.control(surface="direct"))

#################################################################################
# get govt statements

govt <- read.csv("govtstatements.csv", stringsAsFactors = FALSE)

govt_meta <- select(govt, date)
govt_dates <- as.Date(govt_meta[[1]], "%Y-%m-%d")

# run LIWC
liwc_govt <- liwcalike(govt$text, spanish_dict)

govt_neg <- as.data.frame(cbind(govt_dates, as.numeric(liwc_govt$EmoNeg)))
govt_neg$govt_dates <- as.Date(govt_neg$govt_dates, origin = "1970-01-01")
govt_pos <- as.data.frame(cbind(govt_dates, as.numeric(liwc_govt$EmoPos)))
govt_pos$govt_dates <- as.Date(govt_pos$govt_dates, origin = "1970-01-01")

# use lowess
# lowess_govt_neg <- lowess(govt_dates, y = govt_neg$V2, f = 2/3, iter = 3, delta = 0.01 * diff(range(govt_dates)))
# 
# lowess_govt_pos <- lowess(govt_dates, y = govt_pos$V2, f = 2/3, iter = 3, delta = 0.01 * diff(range(govt_dates)))

govt_neg$V2 <- loess_govt_neg$y
govt_pos$V2 <- loess_govt_pos$y

loess_govt_neg <- loess(govt_neg$V2 ~ as.numeric(govt_dates), control=loess.control(surface="direct"))
loess_govt_pos <- loess(govt_pos$V2 ~ as.numeric(govt_dates), control=loess.control(surface="direct"))

#################################################################################
# let's graph negative emotion

# plot(FARC_dates, FARC_neg$V2, xlim = c(as.Date("2011-01-01", "%Y-%m-%d"), as.Date("2016-06-01", "%Y-%m-%d")))

# Neg emotion: base graph
base_neg = ggplot() +
  geom_line(data = FARC_neg, aes(x = FARC_dates, y = V2, color = "FARC statement")) +
  geom_jitter() +
#  geom_point(data = joint_neg, aes(x = joint_dates, y = V2, color = "Joint statement")) +
  geom_line(data = joint_neg, aes(x = joint_dates, y = V2, color = "Joint statement")) +
  geom_line(data = govt_neg, aes(x = govt_dates, y = V2, color = "Govt statement")) +
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
base_pos = ggplot() +
  geom_line(data = FARC_pos, aes(x = FARC_dates, y = V2, color = "FARC statement")) +
  geom_jitter() +
#  geom_point(data = joint_pos, aes(x = joint_dates, y = V2, color = "Joint statement")) +
  geom_line(data = joint_pos, aes(x = joint_dates, y = V2, color = "Joint statement")) +
  geom_line(data = govt_pos, aes(x = govt_dates, y = V2, color = "Govt statement")) +
  labs(
    x = "Date",
    y = "Percent Positive Emotion",
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
FARC_ellos <- data.frame(ell = as.numeric(liwc_FARC$Ellos), date = FARC_dates)
joint_ellos <- data.frame(ell = as.numeric(liwc_joint$Ellos), date = joint_dates)

base_ellos = ggplot() +
  geom_line(data = FARC_ellos, aes(x = date, y = ell, color = "FARC statement")) +
  geom_jitter() +
  # geom_point(data = joint_neg, aes(x = joint_dates, y = V2, color = "Joint statement")) +
  geom_line(data = joint_ellos, aes(x = date, y = ell, color = "Joint statement")) +
  # geom_line(data = govt_neg, aes(x = govt_dates, y = V2, color = "Govt statement")) +
  labs(
    x = "Date",
    y = "Percent 3rd Person Pl Pronoun",
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
# Time Series Analysis: Negative Emotion

# looking at the base_neg graph, all 3 sets of data (F, G, and J) appear non-stationary

# let's impute some values using loess
# first, gather all the dates we need
all_dates <- as.numeric(c(joint_dates, FARC_dates, govt_dates))

# predict data using loess
pred_F_neg <- predict(loess_F_neg, newdata = all_dates)
pred_govt_neg <- predict(loess_govt_neg, newdata = all_dates)
pred_joint_neg <- predict(loess_joint_neg, newdata = all_dates)

# linear model now
neg_lm <- lm(pred_joint_neg ~ pred_F_neg + pred_govt_neg)

# let's run the augmented Dickey-Fuller test anyway, to confirm non-stationarity

adf.test(pred_F_neg)
adf.test(pred_govt_neg)
adf.test(pred_joint_neg)

# joint and govt have p-vals < 0.01. This suggests unit root stationarity. But from the base_neg graph, the data is obviously not stationary

# so let's try ADF with trends and drift

# we already know FARC is non stationary
summary(ur.df(y = pred_F_neg, type = "drift", lags = 1))
summary(ur.df(y = pred_F_neg, type = "trend", lags = 1))
summary(ur.df(y = pred_F_neg, type = "none", lags = 1))

summary(ur.df(y = pred_govt_neg, type = "drift", lags = 1))
summary(ur.df(y = pred_govt_neg, type = "trend", lags = 1))
summary(ur.df(y = pred_govt_neg, type = "none", lags = 1))

summary(ur.df(y = pred_joint_neg, type = "drift", lags = 1))
summary(ur.df(y = pred_joint_neg, type = "trend", lags = 1))
summary(ur.df(y = pred_joint_neg, type = "none", lags = 1))

# also, let's run the KPSS test

#################################################################################
#################################################################################
# topic model

# create corpus
FARC_corp <- corpus(FARC$text, docvars = FARC_meta)

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