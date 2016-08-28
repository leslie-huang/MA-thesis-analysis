#################################################################################
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
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 20))

paper_g_n = ggplot(govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Government")) +
  ggtitle("Loessed Government Negative Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(neg_breaks, group == "govt"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Neg Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 20))

paper_j_n = ggplot(govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoNeg, color = "Joint")) +
  ggtitle("Loessed Joint Negative Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(neg_breaks, group == "joint"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Neg Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 20))

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
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 20))

paper_g_p = ggplot(govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "Government")) +
  ggtitle("Loessed Government Positive Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(pos_breaks, group == "govt"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Positive Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 20))

paper_j_p = ggplot(govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = EmoPos, color = "Joint")) +
  ggtitle("Loessed Joint Positive Emotion with Optimal Breakpoints") +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_vline(data = filter(pos_breaks, group == "joint"), mapping = aes(xintercept = as.numeric(date)), linetype = 2) +
  labs(
    x = "Date",
    y = "Percent Positive Emotion",
    color = "Legend") +
  scale_x_date(date_minor_breaks = "1 month",
               limits = c(as.Date("2012-01-01", "%Y-%m-%d"), NA)) +
  coord_cartesian(ylim = c(0, 20))

paper_j_p
paper_g_p
paper_F_p


####