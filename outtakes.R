# sentiment analysis outtakes
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

