getwd()
setwd("/Users/massadraza/Desktop/mr2207_HW3")
getwd()

install.packages("rpart") 
install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)
library(rpart)
library(rpart.plot)


df <- read.csv("ClassicHit.csv")

# Part 1 -----> Languages of Data: Translating Raw Chaos into Meaning

table(df$Genre)
table(df$Year)
table(df$Year, df$Genre)
table(df$Year, df$Artist)

summary(df)
summary(df$Duration)
summary(df$Loudness)

subsetAfter2000 <- subset(df, Year >= 2000)
subsetBefore2000 <- subset(df, Year < 2000)

tapply(df$Popularity, df$Genre, mean)
tapply(df$Track, df$Artist, length)

df$Energy_Dance <- df$Energy + df$Danceability
summary(df$Energy_Dance)

# Part 2 ------> Exploratory Data Analysis: Seeing the Invisible

hist(df$Loudness,
     main = "Histogram of Loudness",
     xlab = "Loudness (dB)",
     col = "lightblue",
     border = "black")

genre_counts <- table(df$Genre)

barplot(genre_counts, 
        main = "Bar Plot of Genre Counts",
        xlab = "Genre",
        ylab = "Count",
        las = 2,
        col = "lightgreen")

avg_loudness <- tapply(df$Loudness, df$Genre, mean, na.rm = TRUE)

barplot(avg_loudness,
        main = "Average Loudness by Genre", 
        xlab = "Genre",
        ylab = "Average Loudness (dB)",
        col = "orange",
        border = "black",
        las = 2)

# Part 3 -------> Fooled By Data: When Randomness Plays Tricks

plot(df$Energy, df$Popularity,
     main = "Energy vs Original Popularity",
     xlab = "Energy", ylab = "Popularity",
     pch = 1, col = "blue", cex = 0.4)

df$Popularity_Shuffled <- sample(df$Popularity)

plot(df$Energy, df$Popularity_Shuffled,
     main = "Energy vs Shuffled Popularity",
     xlab = "Energy", ylab = "Popularity (Shuffled)",
     pch = 1, col = "red", cex = 0.4)

# Part 4 --------> Central Limit Theorem & Confidence Intervals

# Compute the confidence interval for the mean length of a song (in milliseconds) produced after (and including) the year 2000
songsAfter2000 <- subset(df, Year >= 2000)
numeric_durations <- as.numeric(songsAfter2000$Duration)
n <- 100
num_samples <- 500

sample_means <- replicate(num_samples, mean(sample(numeric_durations, n, replace = TRUE)))

hist(sample_means, 
     breaks = 30,
     col = "skyblue",
     probability = FALSE,
     main = "Sampling Distribution of Sample Means (n = 100)",
     xlab = "Sample Mean Duration (ms)")

randomSample <- songsAfter2000[sample(nrow(songsAfter2000), n, replace = FALSE), ]

n <- nrow(randomSample)
m <- mean(randomSample$Duration)
s <- sd(randomSample$Duration)

z_score <- 1.96

sem <- s / (n^(1/2))
moe <- z_score * sem
round(c(n = n, mean = m, sd = s, SEM = sem, MOE = moe, LCL = m - moe, UCL = m + moe), 2)

# Part 5 ------> Hypothesis Testing: When Suspicion Meets Statistics
# Null Hypothesis: There is no difference in the mean popularity of songs before and after the 2000s
# Alternative Hypothesis: The mean popularity of songs after the 2000s (includes the year 2000) are greater than before the 2000s (does not include the year 2000).

# Create a categorical variable where before is before 2000 and after is after (and including ) the year 2000

df$status <- ifelse(df$Year < 2000, "before", "after") # Engineered a before after column for the purposes of this test
df$Popularity <- as.numeric(as.character(df$Popularity))
df$status <- as.character(df$status)
mean_popularity_after2000 <- mean(df[df$Year >= 2000, ]$Popularity)
mean_popularity_before2000 <- mean(df[df$Year < 2000, ]$Popularity)
obs_difference <- mean_popularity_after2000 - mean_popularity_before2000
obs_difference
z_test_from_data(df, 'status', 'Popularity', 'before', 'after')

# Part 6 --------> Independence & Difference of Proportions (Chi-Square)
# Two Categorical variables to be tested: Artist and Genre
# Null Hypothesis: Artist and Genre are independent
# Alternative Hypothesis: Artist and Genre are not independent.
chisq1 <- chisq.test(df$Artist, df$Genre)
chisq1
chisq1$expected
chisq1$observed

# Part 7 -------> Multiple Hypothesis Testing: The False Discovery Jungle
meanEnergy_PerArtist <- tapply(df$Energy, df$Artist, mean)
n_artist <- sum(!is.na(meanEnergy_PerArtist))
m_artist <- 5 # Limiting ourselves to just 5 pairs of artists b/c the dataset contains 3000+ artists
artist_signifiance <- 0.05 / m_artist
cat("Number of artist groups = ", n_artist, "\nNumber of comparisons = ", m_artist, 
    "\nSignificance level for artist = ", artist_signifiance, "\n")

# Null Hypothesis: The average value of energy is the same when artist is 10,000 Maniacs than when artist is 3 Doors Down
# Alternative Hypothesis: The average value of energy is different when artist is 10,000 Maniacs than when artist is 3 Doors Down

p_val_1 <- permutation_test(df, 'Artist', 'Energy', 10000, '10,000 Maniacs', '3 Doors Down')
p_val_2 <- permutation_test(df, 'Artist', 'Energy', 10000, 'Dead Kennedys', 'Crash Test Dummies')
p_val_3 <- permutation_test(df, 'Artist', 'Energy', 10000, 'DEVO', 'Dinosaur Jr.')
p_val_4 <- permutation_test(df, 'Artist', 'Energy', 10000, 'Gin Blossoms', 'Green Day')
p_val_5 <- permutation_test(df, 'Artist', 'Energy', 10000, 'King Crimson', 'Korn')

if(p_val_1 < 0.05){
  print("PASS 1 w/o bonferroni")
}
if(p_val_2 < 0.05){
  print("PASS 2 w/o bonferroni")
}
if(p_val_3 < 0.05){
  print("PASS 3 w/o bonferroni")
}
if(p_val_4 < 0.05){
  print("PASS 4 w/o bonferroni")
}
if(p_val_5 < 0.05){
  print("PASS 5 w/o bonferroni")
}

if(p_val_1 < artist_signifiance){
  print("PASS 1 bonferroni")
}
if(p_val_2 < artist_signifiance){
  print("PASS 2 bonferroni")
}
if(p_val_3 < artist_signifiance){
  print("PASS 3 bonferroni")
}
if(p_val_4 < artist_signifiance){
  print("PASS 4 bonferroni")
}
if(p_val_5 < artist_signifiance){
  print("PASS 5 bonferroni")
}

# Only one test passed after applying the bonferonni coefficient

# Part 8 -------> Bayesian Reasoning: Updating Beliefs
# Belief: Song is highly danceable (defined with a danceability > 0.7)
# Observation: Song is from the 2010s (years between 2010 and 2019 (inclusive))

n_dance <- nrow(df[df$Danceability > 0.7, ])
n_total <- nrow(df)

PriorP <- n_dance / n_total
PriorOdds <- PriorP / (1 - PriorP)
cat("Prior Probability:", round(PriorP, 3), "\n")
cat("Prior Odds:", round(PriorOdds, 3), "\n")

obs <- df$Year >= 2010 & df$Year <= 2019

TP <- nrow(df[df$Year >= 2010 & df$Year <= 2019 & df$Danceability > 0.7, ]) / nrow(df[df$Danceability > 0.7, ])
FP <- nrow(df[df$Year >= 2010 & df$Year <= 2019 & df$Danceability <= 0.7, ]) / nrow(df[df$Danceability <= 0.7, ])
cat("TP:", round(TP, 3), " FP:", round(FP, 3), "\n")
LR <- TP / FP
PostOdds <- LR * PriorOdds
PostP <- PostOdds / (1 + PostOdds)
cat("Likelihood Ratio:", round(LR, 2), "\n")
cat("Posterior Probability (Danceability | Year between 2010 and 2019 (inclusive)):", round(PostP * 100, 2), "%\n")

# Part 9 ------> Prediction Models: From Correlation to Prediction
df$Mode_Categorical <- ifelse(df$Mode == 0, TRUE, FALSE)
# Engineer a Categorical Mode Column for the purposes of R-Part
# True means the track is minor and false means the track is major 
n <- nrow(df)
train_idx <- sample(1:n, size = floor(0.7 * n))
train <- df[train_idx, ]
test <- df[-train_idx, ]

summary(train$Danceability)
summary(train$Duration)

tree_model <- rpart(
  Mode_Categorical ~ Year + Danceability + Duration,
  data = train,
  method = "class",
  control = rpart.control(cp = 0.001, minsplit = 30)
)

print(tree_model)
plot(tree_model, uniform = TRUE, branch = 0.5, margn = 0.1, main = "rpart: Mode Tree")
text(tree_model, use.n = TRUE, cex = 0.7)
rpart.plot(tree_model)

train_pred_class <- predict(tree_model, newdata = train, type = "class")
train_cm <- table(Actual = train$Mode_Categorical, Predicted = train_pred_class)
train_cm
train_acc <- sum(diag(train_cm)) / sum(train_cm)
train_acc

test_pred_class <- predict(tree_model, newdata = test, type = "class")
test_cm <- table(Actual = test$Mode_Categorical, Predicted = test_pred_class)
test_cm
test_acc <- sum(diag(test_cm)) / sum(test_cm)
test_acc

# Part 10 ------> Association Rules & Lift: The Hidden Recipes 
# Analyzing Embedded Rule: Pop & Major -> Danceable
# pop is genre, major is mode, and danceable is defined when danceability > 0.7
df$Mode_Categorical
df$Danceable <- df$Danceability > 0.7
n_songs <- nrow(df)
n_rule_occurances <- sum(df$Genre == "Pop" & df$Mode_Categorical == FALSE & df$Danceable)
support <- n_rule_occurances / n_songs

n_condition_occurrences <- sum(df$Genre == "Pop" & df$Mode_Categorical == FALSE)
confidence <- n_rule_occurances / n_condition_occurrences

cat("Support:", round(support, 4), "(", n_rule_occurances, "songs )\n")
cat("Confidence:", round(confidence, 4), "(", n_rule_occurances, "/", n_condition_occurrences, ")\n")

# Individual Item Frequencies
totalSongs <- nrow(df)
df_items <- data.frame(
  Genre_Pop = as.numeric(df$Genre == "Pop"),
  Mode_Major = as.numeric(df$Mode_Categorical == FALSE),
  Danceable_True = as.numeric(df$Danceable == TRUE)
)

# Calculating Lift
item_freq <- colSums(df_items) / n_songs
print(round(item_freq, 3))
Danceable_TRUE <- as.numeric(df$Danceable == TRUE)
danceable_Freq <- sum(Danceable_TRUE) / totalSongs
avg_rhs_freq <- danceable_Freq
lift <- confidence / avg_rhs_freq
cat("\nLift for Pop & Major -> Danceable", round(lift, 2), "\n")

# Using apriori()
install.packages("arules", type = "source")
library(arules)

# Convert Categorical features into vectors
df_factor <- data.frame(
  Genre = as.factor(df$Genre),
  Mode = as.factor(df$Mode_Categorical),
  Danceable = as.factor(df$Danceable)
)

trans <- as(df_factor, "transactions")
trans
summary(trans)

rules <- apriori(
  trans,
  parameter = list(
    supp = 0.01,
    conf = 0.5,
    minlen = 2
  )
)

length(rules)
inspect(head(rules, 10))
# False -> Track is a major, True -> Track is a minor
# Analyzing rules where lift > 1 (classified as strong)
strong_rules <- subset(rules, lift > 1)
rules_sorted <- sort(strong_rules, by = "lift", decreasing = TRUE)
inspect(head(rules_sorted, 10))

