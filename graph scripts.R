q#################################################################################
#################################################################################
# Graph sentiment -- run other script first

# Neg emotion: base graph
base_neg = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "FARC statement")) +
  geom_smooth(method = "loess", se = FALSE, linetype = 2) +
  geom_jitter() +
  geom_point(data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Joint statement")) +
  geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Joint statement"), linetype = 1) +
  geom_point(data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Govt statement")) +
  geom_smooth(method = "loess", se = FALSE, data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Govt statement"), linetype = 3) +
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

# let's look at structural breaks on the graph
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
death_breaks_gg


#################################################################################
#################################################################################
# now let's take a look at trends in violence/military activity

# make our base graph
base_viol = ggplot(monthly_viol, aes(x = as.Date(date, origin = "1970-01-01"), y = FARC_actions)) +
  geom_jitter() +
  geom_smooth(method = "loess", se = FALSE, aes(color = "Loessed FARC actions"), color = "#000000") +
#  geom_point(data = monthly_viol, aes(x = as.Date(date, origin = "1970-01-01"), y = deaths_fuerzapublica, color = "Army casualties (excl. wounded)"), shape = 5, size = 2) +
#  geom_smooth(method = "loess", se = FALSE, data = monthly_vio, aes(x = as.Date(date, origin = "1970-01-01"), y = deaths_fuerzapublica, color = "Loessed casualties"), color = "#000000") +
#  geom_point(data = monthly_viol, aes(x = as.Date(date, origin = "1970-01-01"), y = desmovilizados, color = "Militants demobilized")) +
#  geom_smooth(method = "loess", se = FALSE, data = monthly_viol, aes(x = as.Date(date, origin = "1970-01-01"), y = desmovilizados, color = "Militants demobilized"))  +
  labs(
    x = "Date",
    y = "Number of Incidents",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  ggtitle("Number of FARC Actions Over Time") +
  theme_bw()

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

### get structural breaks

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
# Public Opinion

# graph it
base_opinion = ggplot(public_op, aes(x = as.Date(date, origin = "1970-01-01"), y = approve_santos_decision_talks), color = "#000000") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_jitter() +
  labs(
    x = "Date",
    y = "% Approve/Have Positive Image",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  ggtitle("Public Opinion: Approval rating of peace talks") +
  theme_bw()

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



#################################################################################
#################################################################################
# Results

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

# Structural breaks by party, with means for each regimes.
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
#################################################################################
#################################################################################
#################################################################################
## Graph output for the paper


#################################################################################
# neg graphs
paper_F_n = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "FARC")) +
  ggtitle("Loessed FARC Negative Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(neg_breaks, group == "FARC"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Neg Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 15))

paper_g_n = ggplot(govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Government")) +
  ggtitle("Loessed Government Negative Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(neg_breaks, group == "govt"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Neg Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 15))

paper_j_n = ggplot(joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Joint")) +
  ggtitle("Loessed Joint Negative Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(neg_breaks, group == "joint"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Neg Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 15))

paper_j_n
paper_g_n
paper_F_n

#################################################################################
# positive 

paper_F_p = ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "FARC")) +
  ggtitle("Loessed FARC Positive Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(pos_breaks, group == "FARC"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Positive Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 15))

paper_g_p = ggplot(govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "Government")) +
  ggtitle("Loessed Government Positive Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(pos_breaks, group == "govt"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Positive Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 15))

paper_j_p = ggplot(joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "Joint")) +
  ggtitle("Loessed Joint Positive Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(pos_breaks, group == "joint"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Positive Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 30))

paper_j_p
paper_g_p
paper_F_p


####

# summary stats for paper

# Function to generate vector of summary stats for a given LIWC output df
liwc_summarizer <- function(df) {
  liwc_results <- liwcalike(df$text, spanish_dict)
  num_docs <- length(liwc_results[, 1])
  
  wc_mean <- mean(as.numeric(liwc_results$WC))
  wc_sd <- sd(as.numeric(liwc_results$WC))
  neg_mean <- mean(as.numeric(liwc_results$EmoNeg))
  neg_sd <- sd(as.numeric(liwc_results$EmoNeg))
  pos_mean <- mean(as.numeric(liwc_results$EmoPos))
  pos_sd <- sd(as.numeric(liwc_results$EmoPos))
  ellos_mean <- mean(as.numeric(liwc_results$Ellos))
  ellos_sd <- sd(as.numeric(liwc_results$Ellos))
  
  results <- c(num_docs,
               wc_mean,
               wc_sd,
               neg_mean, neg_sd,
               pos_mean, pos_sd,
               ellos_mean, ellos_sd
  )
  return(results)
}

FARC_summary <- liwc_summarizer(FARC)
govt_summary <- liwc_summarizer(govt)
joint_summary <- liwc_summarizer(joint)

summary_stats <- cbind(FARC_summary, govt_summary, joint_summary)
row.names(summary_stats) <- c("Num docs", "WC", "WC sd", "Neg mean", "Neg sd", "Pos mean", "Pos sd", "Ellos mean", "ellos sd")
stargazer(summary_stats, title="Summary Statistics of Documents", digits = 2, digit.separator = "")

#########################################################################
FARC_results_melt <- FARC_results[, -4]
FARC_results_melt$id <- rep(1:length(FARC_results[,1]))
FARC_results_melt <- melt(FARC_results_melt, id = c("date", "id"), variable.name = "sentiment_type", value.name = "sentiment_measure")

FARC_melt_gg <- ggplot(data = FARC_results_melt, aes(y = sentiment_measure, x = as.Date(date, origin = "1970-01-01"), group = sentiment_type)) +
  geom_smooth(method = "loess", se = FALSE, aes(linetype = sentiment_type, color = sentiment_type)) +
  labs(
    x = "Date",
    y = "Loessed % of Document") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  scale_colour_manual(name = "Sentiment",
                      labels = c("Neg. emotion", "Pos. emotion", "3rd per. pl."),
                      values = c("grey30", "grey70", "black")) +
  scale_linetype_manual(name = "Sentiment",
                        labels = c("Neg. emotion", "Pos. emotion", "3rd per. pl."),
                        values = c(1,2,3)) +
  ggtitle("FARC: Loessed % of Document Contributed by Sentiment Type") +
  theme_bw()


govt_results_melt <- govt_results[, -4]
govt_results_melt$id <- rep(1:length(govt_results[,1]))
govt_results_melt <- melt(govt_results_melt, id = c("date", "id"), variable.name = "sentiment_type", value.name = "sentiment_measure")

govt_melt_gg <- ggplot(data = govt_results_melt, aes(y = sentiment_measure, x = as.Date(date, origin = "1970-01-01"), group = sentiment_type)) +
  geom_smooth(method = "loess", se = FALSE, aes(linetype = sentiment_type, color = sentiment_type)) +
  labs(
    x = "Date",
    y = "Loessed % of Document") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  scale_colour_manual(name = "Sentiment",
                      labels = c("Neg. emotion", "Pos. emotion", "3rd per. pl."),
                      values = c("grey30", "grey70", "black")) +
  scale_linetype_manual(name = "Sentiment",
                        labels = c("Neg. emotion", "Pos. emotion", "3rd per. pl."),
                        values = c(1,2,3)) +
  ggtitle("Govt: Loessed % of Document Contributed by Sentiment Type") +
  theme_bw()

joint_results_melt <- joint_results[, -4]
joint_results_melt$id <- rep(1:length(joint_results[,1]))
joint_results_melt <- melt(joint_results_melt, id = c("date", "id"), variable.name = "sentiment_type", value.name = "sentiment_measure")

joint_melt_gg <- ggplot(data = joint_results_melt, aes(y = sentiment_measure, x = as.Date(date, origin = "1970-01-01"), group = sentiment_type)) +
  geom_smooth(method = "loess", se = FALSE, aes(linetype = sentiment_type, color = sentiment_type)) +
  labs(
    x = "Date",
    y = "Loessed % of Document") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  scale_colour_manual(name = "Sentiment",
                      labels = c("Neg. emotion", "Pos. emotion", "3rd per. pl."),
                      values = c("grey30", "grey70", "black")) +
  scale_linetype_manual(name = "Sentiment",
                        labels = c("Neg. emotion", "Pos. emotion", "3rd per. pl."),
                        values = c(1,2,3)) +
  ggtitle("Joint: Loessed % of Document Contributed by Sentiment Type") +
  theme_bw()

############################################################################
# melted violence and public opinion graphs


opinion_melt <- public_op
opinion_melt$id <- rep(1:length(opinion_melt[,1]))
opinion_melt <- melt(opinion_melt, id = c("date", "id"), variable.name = "rating_type", value.name = "rating")

opinion_melt_gg <- ggplot(data = opinion_melt, aes(y = rating, x = as.Date(date, origin = "1970-01-01"), group = rating_type)) +
  geom_smooth(method = "loess", se = FALSE, aes(linetype = rating_type, color = rating_type)) +
  labs(
    x = "Date",
    y = "Approval Rating (Loessed)") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  scale_colour_manual(name = "Rating",
                      labels = c("Approval of Santos", "Approval of peace talks"),
                      values = c("grey50", "black")) +
  scale_linetype_manual(name = "Rating",
                        labels = c("Approval of Santos", "Approval of peace talks"),
                        values = c(1,2)) +
  ggtitle("Public Opinion (loessed)") +
  theme_bw()


violence_melt <- monthly_viol
violence_melt$id <- rep(1:length(violence_melt[,1]))
violence_melt <- melt(violence_melt, id = c("date", "id"), variable.name = "violence_type", value.name = "number_incidents")

violence_melt_gg <- ggplot(data = violence_melt, aes(y = number_incidents, x = as.Date(date, origin = "1970-01-01"), group = violence_type)) +
  geom_smooth(method = "loess", se = FALSE, aes(linetype = violence_type, color = violence_type)) +
  labs(
    x = "Date",
    y = "Number of Incidents (loessed)") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
  scale_colour_manual(name = "Violence Type",
                      labels = c("FARC actions", "Army casualties (excl. wounded)", "Militants demobilized"),
                      values = c("grey30", "grey70", "black")) +
  scale_linetype_manual(name = "Violence Type",
                        labels = c("FARC actions", "Army casualties (excl. wounded)", "Militants demobilized"),
                        values = c(1,2,3)) +
  ggtitle("Levels of Violence (loessed)") +
  theme_bw()

###################################################
# make a table of sample documents for the paper


table_for_paper_F <- cbind(FARC[,-1], FARC_results)
table_for_paper_F <- dplyr::select(table_for_paper_F, date, text, EmoNeg, EmoPos)
colnames(table_for_paper_F) <- c("Date", "Text", "Percent neg. emotion", "Percent pos. emotion")

table_for_paper_g <- cbind(govt, govt_results[,-5])
table_for_paper_g <- dplyr::select(table_for_paper_g, date, text, EmoNeg, EmoPos)
colnames(table_for_paper_g) <- c("Date", "Text", "Percent neg. emotion", "Percent pos. emotion")
