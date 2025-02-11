# Question 1A
# load the dataset and skip the header row
data <- read.table("birthwt.txt", header = TRUE, stringsAsFactors = FALSE)

# Counting the number of observations in the dataset
num_observations <- nrow(data)

# Printing the result
cat("Number of observations in the dataset:", num_observations, "\n")
# Answer 1A: Number of observations in the dataset: 189 



# Question 1B: Debugged and Improved Version
# Function to classify variable types
classify_variable <- function(variable, data) {
  unique_vals <- length(unique(data[[variable]]))
  
  if (variable %in% c("low", "smoke", "ht", "ui")) {
    # Binary variables
    return(list(Type = "Discrete", Category = "Binary", Levels = unique_vals))
  } else if (variable == "race") {
    # Nominal variable
    return(list(Type = "Discrete", Category = "Nominal", Levels = unique_vals))
  } else if (variable %in% c("ptl", "ftv")) {
    # Ordinal variables
    return(list(Type = "Discrete", Category = "Ordinal", Levels = unique_vals))
  } else if (is.numeric(data[[variable]])) {
    # Continuous variables
    return(list(Type = "Continuous", Category = NA, Levels = NA))
  } else {
    # Fallback for unrecognized types
    return(list(Type = "Unknown", Category = NA, Levels = NA))
  }
}

# Initializing results data frame
results <- data.frame(
  Variable = colnames(data),
  Type = character(length(colnames(data))),
  Category = character(length(colnames(data))),
  Levels = integer(length(colnames(data))),
  Mean = numeric(length(colnames(data))),
  StdDev = numeric(length(colnames(data))),
  Median = numeric(length(colnames(data))),
  stringsAsFactors = FALSE
)

# Looping through variables and populate results
for (var in colnames(data)) {
  classification <- classify_variable(var, data)
  results[results$Variable == var, "Type"] <- classification$Type
  results[results$Variable == var, "Category"] <- classification$Category
  results[results$Variable == var, "Levels"] <- classification$Levels
  
  # Only calculate statistics if the variable is "Continuous" and Type is not NA
  if (!is.na(classification$Type) && classification$Type == "Continuous") {
    results[results$Variable == var, "Mean"] <- mean(data[[var]], na.rm = TRUE)
    results[results$Variable == var, "StdDev"] <- sd(data[[var]], na.rm = TRUE)
    results[results$Variable == var, "Median"] <- median(data[[var]], na.rm = TRUE)
  } else {
    # Assign NA for non-continuous variables
    results[results$Variable == var, "Mean"] <- NA
    results[results$Variable == var, "StdDev"] <- NA
    results[results$Variable == var, "Median"] <- NA
  }
}
# Print the results
print(results)
# Answer 1B:  
#   Variable     Type Category Levels Mean StdDev Median
#1       low Discrete   Binary      3   NA     NA     NA
#2       age  Unknown     <NA>     NA   NA     NA     NA
#3       lwt  Unknown     <NA>     NA   NA     NA     NA
#4      race Discrete  Nominal      4   NA     NA     NA
#5     smoke Discrete   Binary      3   NA     NA     NA
#6       ptl Discrete  Ordinal      5   NA     NA     NA
#7        ht Discrete   Binary      3   NA     NA     NA
#8        ui Discrete   Binary      3   NA     NA     NA
#9       ftv Discrete  Ordinal      7   NA     NA     NA
#10      bwt  Unknown     <NA>     NA   NA     NA     NA



# Question 1C
# Filter for individuals older than 30 who smoke
smokers_over_30 <- data[data$age > 30 & data$smoke == 1, ]

# Count the number of individuals
num_smokers_over_30 <- nrow(smokers_over_30)

# Print the result
cat("Number of individuals older than 30 who smoke:", num_smokers_over_30, "\n")
# Answer 1C: Number of individuals older than 30 who smoke: 8



# Question 1D
# Plotting a histogram for birth weight
hist(data$bwt,
     main = "Histogram of Birth Weight",
     xlab = "Birth Weight (grams)",
     ylab = "Frequency",
     col = "skyblue", 
     border = "black", 
     breaks = 20)

# Addding gridlines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
# Answer 1D:
# This histogram visualizes the distribution of birth weights in grams.
# - X-axis: Represents birth weights, ranging from approximately 1000 grams to 5000 grams.
# - Y-axis: Indicates the frequency of observations in each bin.
# - Key insights:
#   1. The data shows a relatively normal distribution, peaking around 3000 grams.
#   2. There is a noticeable spread, with weights ranging widely, though most fall between 2000 and 4000 grams.
#   3. A few outliers are visible at the lower and higher ends (less than 1500 grams and greater than 4000 grams).



# Question 1G
# Total number of observations
n <- nrow(data)

# Calculate probabilities
# P(A): Probability of low birth weight
p_low <- sum(data$low == 1) / n 
# P(B): Probability of the mother being a smoker
p_smoke <- sum(data$smoke == 1) / n 
# P(A ∩ B): Both conditions true
p_low_and_smoke <- sum(data$low == 1 & data$smoke == 1) / n  

# Calculate P(A ∪ B): Probability of either condition
p_either <- p_low + p_smoke - p_low_and_smoke

# Print the result
cat("Probability of selecting an individual with either low birth weight or a mother who smoked:", p_either, "\n")
# Answer 1G: Probability of selecting an individual with either low birth weight or a mother who smoked: 0.5449735



# Question 1H
# Total number of observations
total_individuals <- nrow(data)

# Count individuals who are white (race == 1) and had more than 3 physician visits (ftv > 3)
white_more_than_3_visits <- nrow(subset(data, race == 1 & ftv > 3))

# Calculate the probability
probability <- white_more_than_3_visits / total_individuals

# Print the result
cat("Probability of selecting a white individual with more than 3 physician visits:", probability, "\n")
#Answer 1H: Probability of selecting a white individual with more than 3 physician visits: 0.01058201




# Question 2A
# Given probabilities
# Probability of having cancer
P_C <- 0.01  
# Probability of a positive exam if cancer is present
P_E_given_C <- 0.9  
# False positive rate
P_E_given_not_C <- 0.08  
# Probability of not having cancer
P_not_C <- 1 - P_C  

# Calculate P(E), the probability of a positive exam
P_E <- (P_E_given_C * P_C) + (P_E_given_not_C * P_not_C)

# Applying Bayes' Theorem
P_C_given_E <- (P_E_given_C * P_C) / P_E

# Print the result
cat("Probability of having cancer given a positive exam:", P_C_given_E, "\n")
# Answer 2A: Probability of having cancer given a positive exam: 0.1020408



# Question 2B
# Given parameters
n <- 20  # Total attempts
k <- 12  # Number of successes
p <- 0.7  # Probability of success

# Calculate the binomial probability
P_X_equals_k <- dbinom(k, size = n, prob = p)

# Print the result
cat("Probability of having exactly 12 successes in 20 attempts:", P_X_equals_k, "\n")
# Answer 2B: Probability of having exactly 12 successes in 20 attempts: 0.1143967



# Question 2C Part 1
# Given parameters
mu <- 200  # Mean cholesterol level
sigma <- 25  # Standard deviation

# Calculate z-scores
z_220 <- (220 - mu) / sigma
z_180 <- (180 - mu) / sigma

# Calculate probabilities using the normal distribution
P_220 <- pnorm(z_220) 
P_180 <- pnorm(z_180)  

# Calculate the probability of being between 180 and 220
P_between <- P_220 - P_180

# Print the result
cat("Probability of cholesterol level between 180 and 220 mg/dL:", P_between, "\n")
# Answer 2C Part 1: Probability of cholesterol level between 180 and 220 mg/dL: 0.5762892 



# Question 2C Part 2
# Calculate the z-scores for Q1 (25%) and Q3 (75%)
z_25 <- qnorm(0.25)
z_75 <- qnorm(0.75)

# Calculate Q1 and Q3
Q1 <- mu + z_25 * sigma
Q3 <- mu + z_75 * sigma

# Calculate the IQR
IQR <- Q3 - Q1

# Print the results
cat("Q1 (25th percentile):", Q1, "\n")
cat("Q3 (75th percentile):", Q3, "\n")
cat("Interquartile range (IQR):", IQR, "\n")
# Answer 2C Part 2:
 # Q1 (25th percentile): 183.1378
 # Q3 (75th percentile): 216.8622 
 # Interquartile range (IQR): 33.72449 



# Ouestion 2C Part 3:
 # Explanation:
#Cholesterol levels outside the IQR are considered outliers.
#Patients with cholesterol levels below the 25th percentile (Q1) or above the 75th percentile (Q3) may require further evaluation for cardiovascular risks.
#Those with cholesterol levels significantly above the mean (e.g., > 220 >220) are at higher risk.
#The distribution can help define thresholds for normal, borderline, and high-risk cholesterol levels.



# Question 2C Part 4:
 #Explanation:
# A smaller standard deviation means the data is less spread out, resulting in a narrower curve.
#More patients would fall closer to the mean, reducing the likelihood of extreme values.
#The range for normal cholesterol levels (e.g., within 1 standard deviation) would shrink from 175–225 mg/dL to 185–215 mg/dL.



# Question 3 Part 1a:
# Loading necessary libraries
library(tidyverse)

# Loading the dataset
data <- read.csv("Breast_cancer_Naive.csv")

# Explore the dataset
# Dimensions of the dataset
dimensions <- dim(data)

# Check for missing values
missing_values <- sum(is.na(data))

# Print results
cat("Dimensions of the dataset (rows, columns):", dimensions, "\n")
cat("Number of missing values in the dataset:", missing_values, "\n")
# Answer 3 Part 1a: Dimensions of the dataset (rows, columns): 569 32 
 # Number of missing values in the dataset: 0 



# Question 3 Part 1b
# Identify numerical columns
numerical_columns <- sapply(data, is.numeric)
numerical_data <- data[, numerical_columns]

# Calculate key statistics
calculate_statistics <- function(column) {
  list(
    Min = min(column, na.rm = TRUE),
    Q1 = quantile(column, 0.25, na.rm = TRUE),
    Median = median(column, na.rm = TRUE),
    Mean = mean(column, na.rm = TRUE),
    Q3 = quantile(column, 0.75, na.rm = TRUE),
    Max = max(column, na.rm = TRUE),
    Range = max(column, na.rm = TRUE) - min(column, na.rm = TRUE),
    StdDev = sd(column, na.rm = TRUE),
    Skewness = skewness(column, na.rm = TRUE)
  )
}

# Apply the function to each numerical column and organize results
statistics <- lapply(numerical_data, calculate_statistics)
statistics_df <- do.call(rbind, lapply(statistics, as.data.frame))
statistics_df <- as.data.frame(statistics_df)
rownames(statistics_df) <- names(numerical_data)

# Print the statistics
print("=== Descriptive Statistics ===")
print(statistics_df)

# Visualize distributions
# (a) Histograms
par(mfrow = c(2, 2)) 
for (feature in names(numerical_data)) {
  hist(numerical_data[[feature]],
       main = paste("Histogram of", feature),
       xlab = feature,
       col = "skyblue",
       breaks = 20,
       border = "black")
}

# (b) Boxplots
par(mfrow = c(1, 1))
for (feature in names(numerical_data)) {
  boxplot(numerical_data[[feature]],
          main = paste("Boxplot of", feature),
          ylab = feature,
          col = "lightgreen",
          horizontal = TRUE)
}

# Answer 3 part 1b
#                        Min          Q1      Median         Mean           Q3
#id                      8.670e+03 8.69218e+05 9.06024e+05 3.037183e+07 8.813129e+06
#radius_mean             6.981e+00 1.17000e+01 1.33700e+01 1.412729e+01 1.578000e+01
#texture_mean            9.710e+00 1.61700e+01 1.88400e+01 1.928965e+01 2.180000e+01
#perimeter_mean          4.379e+01 7.51700e+01 8.62400e+01 9.196903e+01 1.041000e+02
#area_mean               1.435e+02 4.20300e+02 5.51100e+02 6.548891e+02 7.827000e+02
#smoothness_mean         5.263e-02 8.63700e-02 9.58700e-02 9.636028e-02 1.053000e-01
#compactness_mean        1.938e-02 6.49200e-02 9.26300e-02 1.043410e-01 1.304000e-01
#concavity_mean          0.000e+00 2.95600e-02 6.15400e-02 8.879932e-02 1.307000e-01
#concave.points_mean     0.000e+00 2.03100e-02 3.35000e-02 4.891915e-02 7.400000e-02
#symmetry_mean           1.060e-01 1.61900e-01 1.79200e-01 1.811619e-01 1.957000e-01
#fractal_dimension_mean  4.996e-02 5.77000e-02 6.15400e-02 6.279761e-02 6.612000e-02
#radius_se               1.115e-01 2.32400e-01 3.24200e-01 4.051721e-01 4.789000e-01
#texture_se              3.602e-01 8.33900e-01 1.10800e+00 1.216853e+00 1.474000e+00
#perimeter_se            7.570e-01 1.60600e+00 2.28700e+00 2.866059e+00 3.357000e+00
#area_se                 6.802e+00 1.78500e+01 2.45300e+01 4.033708e+01 4.519000e+01
#smoothness_se           1.713e-03 5.16900e-03 6.38000e-03 7.040979e-03 8.146000e-03
#compactness_se          2.252e-03 1.30800e-02 2.04500e-02 2.547814e-02 3.245000e-02
#concavity_se            0.000e+00 1.50900e-02 2.58900e-02 3.189372e-02 4.205000e-02
#concave.points_se       0.000e+00 7.63800e-03 1.09300e-02 1.179614e-02 1.471000e-02
#symmetry_se             7.882e-03 1.51600e-02 1.87300e-02 2.054230e-02 2.348000e-02
#fractal_dimension_se    8.948e-04 2.24800e-03 3.18700e-03 3.794904e-03 4.558000e-03
#radius_worst            7.930e+00 1.30100e+01 1.49700e+01 1.626919e+01 1.879000e+01
#texture_worst           1.202e+01 2.10800e+01 2.54100e+01 2.567722e+01 2.972000e+01
#perimeter_worst         5.041e+01 8.41100e+01 9.76600e+01 1.072612e+02 1.254000e+02
#area_worst              1.852e+02 5.15300e+02 6.86500e+02 8.805831e+02 1.084000e+03
#smoothness_worst        7.117e-02 1.16600e-01 1.31300e-01 1.323686e-01 1.460000e-01
#compactness_worst       2.729e-02 1.47200e-01 2.11900e-01 2.542650e-01 3.391000e-01
#concavity_worst         0.000e+00 1.14500e-01 2.26700e-01 2.721885e-01 3.829000e-01
#concave.points_worst    0.000e+00 6.49300e-02 9.99300e-02 1.146062e-01 1.614000e-01
#symmetry_worst          1.565e-01 2.50400e-01 2.82200e-01 2.900756e-01 3.179000e-01
#fractal_dimension_worst 5.504e-02 7.14600e-02 8.00400e-02 8.394582e-02 9.208000e-02
#Max        Range       StdDev  Skewness
#id                      9.113205e+08 9.113118e+08 1.250206e+08 6.4396595
#radius_mean             2.811000e+01 2.112900e+01 3.524049e+00 0.9374168
#texture_mean            3.928000e+01 2.957000e+01 4.301036e+00 0.6470241
#perimeter_mean          1.885000e+02 1.447100e+02 2.429898e+01 0.9854334
#area_mean               2.501000e+03 2.357500e+03 3.519141e+02 1.6370654
#smoothness_mean         1.634000e-01 1.107700e-01 1.406413e-02 0.4539207
#compactness_mean        3.454000e-01 3.260200e-01 5.281276e-02 1.1838556
#concavity_mean          4.268000e-01 4.268000e-01 7.971981e-02 1.3938008
#concave.points_mean     2.012000e-01 2.012000e-01 3.880284e-02 1.1650124
#symmetry_mean           3.040000e-01 1.980000e-01 2.741428e-02 0.7217877
#fractal_dimension_mean  9.744000e-02 4.748000e-02 7.060363e-03 1.2976191
#radius_se               2.873000e+00 2.761500e+00 2.773127e-01 3.0723468
#texture_se              4.885000e+00 4.524800e+00 5.516484e-01 1.6377733
#perimeter_se            2.198000e+01 2.122300e+01 2.021855e+00 3.4254803
#area_se                 5.422000e+02 5.353980e+02 4.549101e+01 5.4185001
#smoothness_se           3.113000e-02 2.941700e-02 3.002518e-03 2.3022616
#compactness_se          1.354000e-01 1.331480e-01 1.790818e-02 1.8922032
#concavity_se            3.960000e-01 3.960000e-01 3.018606e-02 5.0835502
#concave.points_se       5.279000e-02 5.279000e-02 6.170285e-03 1.4370701
#symmetry_se             7.895000e-02 7.106800e-02 8.266372e-03 2.1835728
#fractal_dimension_se    2.984000e-02 2.894520e-02 2.646071e-03 3.9033041
#radius_worst            3.604000e+01 2.811000e+01 4.833242e+00 1.0973059
#texture_worst           4.954000e+01 3.752000e+01 6.146258e+00 0.4956970
#perimeter_worst         2.512000e+02 2.007900e+02 3.360254e+01 1.1222227
#area_worst              4.254000e+03 4.068800e+03 5.693570e+02 1.8495814
#smoothness_worst        2.226000e-01 1.514300e-01 2.283243e-02 0.4132383
#compactness_worst       1.058000e+00 1.030710e+00 1.573365e-01 1.4657948
#concavity_worst         1.252000e+00 1.252000e+00 2.086243e-01 1.1441794
#concave.points_worst    2.910000e-01 2.910000e-01 6.573234e-02 0.4900213
#symmetry_worst          6.638000e-01 5.073000e-01 6.186747e-02 1.4263764
#fractal_dimension_worst 2.075000e-01 1.524600e-01 1.806127e-02 1.6538237
# Visualisation description
  #Concave Points (Worst): The distribution appears slightly right-skewed, with most of the data concentrated between 0.05 and 0.15. A long tail extends towards higher values (up to 0.3).
  #Symmetry (Worst): The distribution is approximately symmetric, with a peak around 0.25 to 0.3. There are a few outliers on the higher end (above 0.6), which create a slight right-skew.
  #Fractal Dimension (Worst): The distribution is strongly right-skewed, with most of the data concentrated below 0.1. There is a long tail extending up to 0.2.



# 3 part 2a
# Load necessary libraries
library(caret)  
library(e1071) # For Naïve Bayes implementation

# Encode the target variable (if necessary)
data$diagnosis <- as.factor(data$diagnosis) 

# Split the data into training (70%) and testing (30%) sets
set.seed(42)
train_index <- createDataPartition(data$diagnosis, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Print dataset dimensions
cat("Training set dimensions:", dim(train_data), "\n")
cat("Testing set dimensions:", dim(test_data), "\n")
# Answer 3 Part 2a: The preprocessing for Naïve Bayes classification starts by loading the necessary libraries for data manipulation, model training, and evaluation. The target variable, diagnosis, is then encoded as a categorical factor to ensure it is correctly interpreted for classification. To split the dataset into training and testing sets, a 70-30 ratio is used, with a random seed set for reproducibility. The createDataPartition function ensures stratified sampling, maintaining the class distribution across both sets. This results in a well-structured dataset where 70% is used for training and 30% for testing, ensuring a fair and reliable model evaluation.
 # Training set dimensions: 399 32 
 # Testing set dimensions: 170 32 



# Question 3 part 2b:
# Training the Naïve Bayes model
model <- naiveBayes(diagnosis ~ ., data = train_data)

# Predicting on the testing data
predictions <- predict(model, test_data)

# Evaluating model performance
conf_matrix <- confusionMatrix(predictions, test_data$diagnosis)

# Print accuracy, sensitivity, and specificity
accuracy <- conf_matrix$overall["Accuracy"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]

cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

# Identify the highest contributing features
feature_means <- model$tables
# mean per class
class_means <- lapply(feature_means, function(x) apply(x, 2, mean))

# Calculating the difference in means for each feature
feature_diff <- sapply(class_means, function(x) abs(diff(x)))

# Identifying the features with the highest mean difference
highest_contributing_features <- names(feature_diff)[order(feature_diff, decreasing = TRUE)]

# Printing the highest contributing features
cat("\nHighest Contributing Features:\n")
print(head(highest_contributing_features)) 
#Answer 3 Part 2b:Accuracy: 0.8705882, Sensitivity: 0.8598131, Specificity: 0.8888889
 #Highest Contributing Features:"id", "area_worst", "area_mean", "perimeter_worst", "perimeter_mean", "texture_worst"  



# 3 part 2c
# Print the confusion matrix
cat("Confusion Matrix:\n")
print(conf_matrix)

# Interpret FP and FN
cat("\nFalse Positives (FP): Predicted malignant but actually benign.\n")
cat("False Negatives (FN): Predicted benign but actually malignant (high-risk concern).\n")
# Answer 3 Part 2C:
  #False Positives (FP): Predicted malignant but actually benign.
  #False Negatives (FN): Predicted benign but actually malignant (high-risk concern).
  #What does the confusion matrix tell you about the model's performance?
   #The confusion matrix helps evaluate the model's performance by showing how many true positives (malignant correctly predicted), true negatives (benign correctly predicted), false positives (benign predicted as malignant), and false negatives (malignant predicted as benign) there are. Key metrics derived from the matrix include accuracy, sensitivity, and specificity.
  #Potential concerns regarding false positives or false negatives:
   #False Positives (FP): These are benign cases incorrectly predicted as malignant. This could lead to over-treatment, such as unnecessary biopsies or surgeries.
   #False Negatives (FN): These are malignant cases incorrectly predicted as benign. This is a high-risk concern as it means the model misses critical diagnoses, potentially delaying life-saving treatment.




# 3 part 2d
# Load required library
library(pROC)

# Predict probabilities for the positive class
probabilities <- predict(model, test_data, type = "raw")[, 2]  

# Generate the ROC curve
roc_curve <- roc(test_data$diagnosis, probabilities, levels = rev(levels(test_data$diagnosis)))

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for Naïve Bayes Classifier", col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2) 

# Calculate AUC
auc_value <- auc(roc_curve)

# Print the AUC value
cat("AUC (Area Under the Curve):", auc_value, "\n")
#Answer 3 Part 2d: AUC (Area Under the Curve): 0.950675 
 #The AUC (Area Under the Curve) value of 0.950675 indicates that the classifier has a very strong ability to distinguish between benign and malignant cases. Specifically, an AUC of 0.95 means the model has a 95% chance of correctly ranking a randomly chosen malignant case higher than a randomly chosen benign case.
