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
# Descriptive statistics for numerical features
numerical_summary <- data %>% 
  select(where(is.numeric)) %>% 
  summary()

# Print summary statistics
cat("Descriptive statistics for numerical features:\n")
print(numerical_summary)

# Visualize distributions of numerical features
# Create histograms for all numerical features
numerical_features <- data %>% select(where(is.numeric))
par(mfrow = c(2, 3))  
for (col in colnames(numerical_features)) {
  hist(
    numerical_features[[col]], 
    main = paste("Histogram of", col),
    xlab = col, 
    col = "skyblue", 
    border = "black"
  )
}
Descriptive statistics for numerical features:
  > print(numerical_summary)
# Answer 3 part 1b
 #id             radius_mean      texture_mean   perimeter_mean     area_mean     
 #Min.   :     8670   Min.   : 6.981   Min.   : 9.71   Min.   : 43.79   Min.   : 143.5  
 #1st Qu.:   869218   1st Qu.:11.700   1st Qu.:16.17   1st Qu.: 75.17   1st Qu.: 420.3  
 #Median :   906024   Median :13.370   Median :18.84   Median : 86.24   Median : 551.1  
 #Mean   : 30371831   Mean   :14.127   Mean   :19.29   Mean   : 91.97   Mean   : 654.9  
 #3rd Qu.:  8813129   3rd Qu.:15.780   3rd Qu.:21.80   3rd Qu.:104.10   3rd Qu.: 782.7  
 #Max.   :911320502   Max.   :28.110   Max.   :39.28   Max.   :188.50   Max.   :2501.0  
 #smoothness_mean   compactness_mean  concavity_mean    concave.points_mean symmetry_mean   
 #Min.   :0.05263   Min.   :0.01938   Min.   :0.00000   Min.   :0.00000     Min.   :0.1060  
 #1st Qu.:0.08637   1st Qu.:0.06492   1st Qu.:0.02956   1st Qu.:0.02031     1st Qu.:0.1619  
 #Median :0.09587   Median :0.09263   Median :0.06154   Median :0.03350     Median :0.1792  
 #Mean   :0.09636   Mean   :0.10434   Mean   :0.08880   Mean   :0.04892     Mean   :0.1812  
 #rd Qu.:0.10530   3rd Qu.:0.13040   3rd Qu.:0.13070   3rd Qu.:0.07400     3rd Qu.:0.1957  
 #Max.   :0.16340   Max.   :0.34540   Max.   :0.42680   Max.   :0.20120     Max.   :0.3040  
 #fractal_dimension_mean   radius_se        texture_se      perimeter_se       area_se       
 #Min.   :0.04996        Min.   :0.1115   Min.   :0.3602   Min.   : 0.757   Min.   :  6.802  
 #1st Qu.:0.05770        1st Qu.:0.2324   1st Qu.:0.8339   1st Qu.: 1.606   1st Qu.: 17.850  
 #Median :0.06154        Median :0.3242   Median :1.1080   Median : 2.287   Median : 24.530  
 #Mean   :0.06280        Mean   :0.4052   Mean   :1.2169   Mean   : 2.866   Mean   : 40.337  
 #3rd Qu.:0.06612        3rd Qu.:0.4789   3rd Qu.:1.4740   3rd Qu.: 3.357   3rd Qu.: 45.190  
 #Max.   :0.09744        Max.   :2.8730   Max.   :4.8850   Max.   :21.980   Max.   :542.200  
 #smoothness_se      compactness_se      concavity_se     concave.points_se   symmetry_se      
 #Min.   :0.001713   Min.   :0.002252   Min.   :0.00000   Min.   :0.000000   Min.   :0.007882  
 #1st Qu.:0.005169   1st Qu.:0.013080   1st Qu.:0.01509   1st Qu.:0.007638   1st Qu.:0.015160  
 #Median :0.006380   Median :0.020450   Median :0.02589   Median :0.010930   Median :0.018730  
 #Mean   :0.007041   Mean   :0.025478   Mean   :0.03189   Mean   :0.011796   Mean   :0.020542  
 #3rd Qu.:0.008146   3rd Qu.:0.032450   3rd Qu.:0.04205   3rd Qu.:0.014710   3rd Qu.:0.023480  
 #Max.   :0.031130   Max.   :0.135400   Max.   :0.39600   Max.   :0.052790   Max.   :0.078950  
 #fractal_dimension_se  radius_worst   texture_worst   perimeter_worst    area_worst    
 #Min.   :0.0008948    Min.   : 7.93   Min.   :12.02   Min.   : 50.41   Min.   : 185.2  
 #1st Qu.:0.0022480    1st Qu.:13.01   1st Qu.:21.08   1st Qu.: 84.11   1st Qu.: 515.3  
 #Median :0.0031870    Median :14.97   Median :25.41   Median : 97.66   Median : 686.5  
 #Mean   :0.0037949    Mean   :16.27   Mean   :25.68   Mean   :107.26   Mean   : 880.6  
 #3rd Qu.:0.0045580    3rd Qu.:18.79   3rd Qu.:29.72   3rd Qu.:125.40   3rd Qu.:1084.0  
 #Max.   :0.0298400    Max.   :36.04   Max.   :49.54   Max.   :251.20   Max.   :4254.0  
 #smoothness_worst  compactness_worst concavity_worst  concave.points_worst symmetry_worst  
 #Min.   :0.07117   Min.   :0.02729   Min.   :0.0000   Min.   :0.00000      Min.   :0.1565  
 #1st Qu.:0.11660   1st Qu.:0.14720   1st Qu.:0.1145   1st Qu.:0.06493      1st Qu.:0.2504  
 #Median :0.13130   Median :0.21190   Median :0.2267   Median :0.09993      Median :0.2822  
 ##Mean   :0.13237   Mean   :0.25427   Mean   :0.2722   Mean   :0.11461      Mean   :0.2901  
 #3rd Qu.:0.14600   3rd Qu.:0.33910   3rd Qu.:0.3829   3rd Qu.:0.16140      3rd Qu.:0.3179  
 #Max.   :0.22260   Max.   :1.05800   Max.   :1.2520   Max.   :0.29100      Max.   :0.6638  
 #fractal_dimension_worst
 #Min.   :0.05504        
 #1st Qu.:0.07146        
 #Median :0.08004        
 #Mean   :0.08395        
 #3rd Qu.:0.09208        
 #Max.   :0.20750
 # plot: Histogram of fractal_dimension_worst
  # This histogram visualizes the distribution of the 'fractal_dimension_worst' variable.
  # The data appears to be right-skewed, meaning most values are concentrated on the lower end (around 0.05 to 0.15).
  # There are fewer instances of higher fractal dimension values, with a gradual decline in frequency.
  # The peak indicates the most common range of fractal dimension values in the dataset.
  # This kind of distribution is common in biological data, where extreme values occur less frequently.



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
class_means <- lapply(feature_means, function(x) apply(x, 2, mean)) # mean per class

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
probabilities <- predict(model, test_data, type = "raw")[, 2]  # Probabilities for 'malignant'

# Generate the ROC curve
roc_curve <- roc(test_data$diagnosis, probabilities, levels = rev(levels(test_data$diagnosis)))

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for Naïve Bayes Classifier", col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2)  # Add a diagonal reference line

# Calculate AUC
auc_value <- auc(roc_curve)

# Print the AUC value
cat("AUC (Area Under the Curve):", auc_value, "\n")
#Answer 3 Part 2d: AUC (Area Under the Curve): 0.950675 
 #The AUC (Area Under the Curve) value of 0.950675 indicates that the classifier has a very strong ability to distinguish between benign and malignant cases. Specifically, an AUC of 0.95 means the model has a 95% chance of correctly ranking a randomly chosen malignant case higher than a randomly chosen benign case.
