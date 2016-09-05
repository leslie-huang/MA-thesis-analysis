# bootstrap the transition matrix for FARC

# 3 possibilities
# toS1  toS2  toS3
# fromS1 0.762 0.070 0.168
# fromS2 0.404 0.596 0.000
# fromS3 0.502 0.042 0.456
# 
# St1           4.969  2.576          11.569  9.138
# St2           0.494  0.305           2.666  1.623
# St3           1.412  0.523           8.553  1.974
# 
# toS1  toS2 toS3
# fromS1 0.792 0.208  0.0
# fromS2 0.518 0.482  0.0
# fromS3 0.100 0.000  0.9
# 
# St1           0.501  0.292           2.851  1.746
# St2           5.480  3.257           3.455  2.784
# St3           1.967  1.065          11.843  6.332
# 
# toS1  toS2  toS3
# fromS1 0.553 0.201 0.245
# fromS2 0.064 0.406 0.530
# fromS3 0.083 0.383 0.534
# 
# 
# St1           0.389  0.241           2.043  1.116
# St2           4.904  2.553          11.578  8.927
# St3           1.101  0.477           6.603  2.729

df <- matrix(data = NA, nrow = 1000, ncol = 3)

for (i in 1:1000) {
  # get the unique set of intercepts that corresponds with each transition matrix
  results <- summary(fit(depmix(forms1, family = list(gaussian(), gaussian()), nstates = 3, data = FARC_results1)))
  intercepts <- sort(results[1:3])
  
  df[i, ] <- intercepts
}

View(df)

unique(df)

# there are actually only 3 different sets: some are different bc of rounding error
sum(df[,3] == 5.480)
sum(df[,3] == 4.969)
sum(df[,3] == 4.904)

# the first case occurs 596/1000 times
# so we want 
# 
# toS1  toS2 toS3
# fromS1 0.792 0.208  0.0
# fromS2 0.518 0.482  0.0
# fromS3 0.100 0.000  0.9
# 
# 22331233
# St1           0.501  0.292           2.851  1.746
# St2           5.480  3.257           3.455  2.784
# St3           1.967  1.065          11.843  6.332
# 


# do the same for the government

df2 <- matrix(data = NA, nrow = 1000, ncol = 3)

for (i in 1:1000) {
  # get the unique set of intercepts that corresponds with each transition matrix
  results <- summary(fit(depmix(forms1_govt, family = list(gaussian(), gaussian()), nstates = 3, data = govt_results1)))
  intercepts <- sort(results[1:3])
  
  df2[i, ] <- intercepts
}

View(df2)

unique(df2)

# unique values for column 1 (rounded)
# 1.65 1.87 0.70 1.08

df2_rounded <- round(df2, digits = 2)
unique(df2_rounded)

sum(df2_rounded[,1] == 1.66 | df2_rounded[,1] == 1.65)
sum(df2_rounded[,1] == .70)
sum(df2_rounded[,1] == 1.08)
sum(df2_rounded[,1] == 1.87 | df2_rounded[,1] == 1.88)
