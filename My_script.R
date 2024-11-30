# Q1 ----
# Install and load required packages
if (!require("Matching")) install.packages("Matching", dependencies = TRUE)
library(Matching)

# Load data
foo <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSY9jLlufY1GjeMh7D2_g1m6olveHLNCerT2C36MTkcjwQCOlZYf8evLMzGOnc252OgXEEasHqcNIcZ/pub?gid=1976818127&single=true&output=csv")

# Handle missing values
foo <- na.omit(foo)

# Check data structure
str(foo)

# PART A: Regression Analysis ----
# Dependent variable: nowtot
# Independent variables: Dems, Repubs, Christian, age, srvlng, demvote
model <- lm(nowtot ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo)

# Display regression results
summary(model)

# Set independent variables and treatment variable
foo$hasgirls <- as.numeric(foo$hasgirls)  # Convert to 0/1
Tr <- foo$hasgirls                        # Treatment variable
X <- cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote)  # Independent variables

# Define outcome variable (Y)
Y <- foo$nowtot  

# Perform Genetic Matching
set.seed(2324)
genout <- GenMatch(Tr = Tr, X = X, pop.size = 20, nboots = 250)  # Modified pop.size and nboots

# Perform matching
mout <- Match(Y = Y, Tr = Tr, X = X, Weight.matrix = genout)

# Summarize matching results
summary(mout)

# Check balance after matching
MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, 
             data = foo, match.out = mout)

# Calculate treatment effect, standard error, and confidence interval
treatment_effect <- mout$est
std_error <- mout$se.standard
confidence_interval <- c(treatment_effect - 1.96 * std_error, treatment_effect + 1.96 * std_error)

# Display results
cat("Treatment Effect Estimate:", treatment_effect, "\n")
cat("Standard Error:", std_error, "\n")
cat("95% Confidence Interval:", confidence_interval, "\n")

# PART (B) ----
# Repeat everything
# Treatment group (2 girls)
foo$Treatment <- ifelse(foo$ngirls == 2, 1, 0)

# Control group (2 boys, total children is 2)
foo <- subset(foo, (foo$ngirls == 2 & foo$totchi == 2) | (foo$ngirls == 0 & foo$totchi == 2))

# Check data structure
str(foo)

# Independent variables for matching
X <- cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote)

# Define outcome variable (Y)
Y <- foo$nowtot

# Perform Genetic Matching
set.seed(2324)
genout <- GenMatch(Tr = foo$Treatment, X = X, pop.size = 20, nboots = 250)

# Perform matching
mout <- Match(Y = Y, Tr = foo$Treatment, X = X, Weight.matrix = genout)

# Summarize matching results
summary(mout)

# Check balance after matching
MatchBalance(Treatment ~ Dems + Repubs + Christian + age + srvlng + demvote,
             data = foo, match.out = mout)

# Calculate treatment effect, standard error, and confidence interval
treatment_effect <- mout$est
std_error <- mout$se.standard
confidence_interval <- c(treatment_effect - 1.96 * std_error, treatment_effect + 1.96 * std_error)

# Display results
cat("Treatment Effect Estimate (Part B):", treatment_effect, "\n")
cat("Standard Error (Part B):", std_error, "\n")
cat("95% Confidence Interval (Part B):", confidence_interval, "\n")

# Q3 ----
# 1. Load Required Packages and Data
# Install and load the sensemakr package
if (!require("sensemakr")) install.packages("sensemakr", dependencies = TRUE)
library(sensemakr)

# Load a dataset (e.g., daughters dataset or custom data)
data_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSY9jLlufY1GjeMh7D2_g1m6olveHLNCerT2C36MTkcjwQCOlZYf8evLMzGOnc252OgXEEasHqcNIcZ/pub?gid=1976818127&single=true&output=csv"
daughters <- read.csv(data_url)

# Data structure
str(daughters)

# 2. Run Initial Regression
# Fit a linear regression model
model <- lm(nowtot ~ Dems + Repubs + Christian + age + srvlng + demvote, data = daughters)

# Display summary
summary(model)

# 3. Run Sensitivity Analysis
# Perform sensitivity analysis with sensemakr
sensitivity_analysis <- sensemakr(
  model = model,
  treatment = "Dems", # Adjust treatment variable
  benchmark_covariates = "Christian", # Example benchmark covariate
  kd = 1:3
)

# Display sensitivity summary
print(sensitivity_analysis)
summary(sensitivity_analysis)

# Plot sensitivity analysis results
plot(sensitivity_analysis)

# Q4 ----
# Load Lalonde observational dataset
lalonde_obs <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR-BQzMNOodmgQSHEtImgpJeENmPS6POrIHsQaTqjP0NXPAt6OzO3scVBR4Az0sgdMJ3CDYuJNdiYMY/pub?gid=1299980585&single=true&output=csv")

# Data structure
str(lalonde_obs)