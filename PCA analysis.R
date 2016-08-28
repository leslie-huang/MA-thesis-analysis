#################################################################################
#################################################################################
# PCA analysis -- run after running the original sentiment analyis script


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

# what are the loadings?
head(statements_PCA$rotation)

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


#################################################################################
#################################################################################
# FARC statement # 94 is an outlier. Let's try removing it, adding log transformation, and rerun PCA
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

# what words are correlated with PC1 or PC2?
PC_df <- fortify(statements_PCA$rotation)
words <- row.names(PC_df)
PC_df <- cbind(words, PC_df)

# top words associated with PC1
PC_df1 <- arrange(PC_df, PC1)
View(PC_df1)

# top words associated with PC2
PC_df2 <- arrange(PC_df, PC2)
View(PC_df2)

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
