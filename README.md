# Music Data Mining Study

A comprehensive data mining and statistical analysis project exploring the ClassicHit music dataset using R. This project demonstrates various data science techniques from exploratory analysis to predictive modeling.

## Dataset

- **ClassicHit.csv** - Music dataset containing track information including:
  - Track metadata (Artist, Genre, Year)
  - Audio features (Energy, Danceability, Loudness, Duration, Mode)
  - Popularity metrics

## Analysis Overview

### Part 1: Data Exploration
Basic data manipulation using `table()`, `summary()`, `subset()`, and `tapply()` to understand the structure and distribution of music data.

### Part 2: Exploratory Data Analysis
Visualizations including:
- Histogram of loudness distribution
- Bar plots of genre counts
- Average loudness by genre

### Part 3: Randomness Analysis
Demonstrates the importance of understanding randomness by comparing original vs. shuffled popularity data against energy levels.

### Part 4: Central Limit Theorem & Confidence Intervals
- Sampling distribution of mean song duration (post-2000)
- 95% confidence interval calculation for song length

### Part 5: Hypothesis Testing
Z-test comparing mean popularity of songs before and after the year 2000.

### Part 6: Chi-Square Test
Tests independence between Artist and Genre categorical variables.

### Part 7: Multiple Hypothesis Testing
Permutation tests with Bonferroni correction comparing energy levels across different artist pairs.

### Part 8: Bayesian Reasoning
Calculates posterior probability of high danceability given songs from the 2010s using:
- Prior odds
- Likelihood ratios
- Posterior probability updates

### Part 9: Prediction Models
Decision tree classification (`rpart`) predicting song mode (major/minor) based on:
- Year
- Danceability
- Duration

### Part 10: Association Rules
Market basket analysis using the `arules` package to discover patterns like "Pop & Major -> Danceable" with support, confidence, and lift metrics.

## Requirements

```r
install.packages("rpart")
install.packages("devtools")
install.packages("arules", type = "source")
devtools::install_github("janish-parikh/ZTest")
```

### R Libraries
- `rpart` / `rpart.plot` - Decision trees
- `arules` - Association rule mining
- `HypothesisTesting` - Statistical tests (z-test, permutation test)

## Usage

1. Set working directory to project folder
2. Ensure `ClassicHit.csv` is in the working directory
3. Run `mr2207_HW3.R` sequentially through each part

## Author

Massad Raza
