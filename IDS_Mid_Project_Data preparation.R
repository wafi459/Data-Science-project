#Data Pre-processing

install.packages("dplyr")
library(dplyr)

Dataset_mid <- read.csv("D:/Wafi(Spring-25)/Data Science/Project/heart_disease_uci - modified.csv", header = TRUE, sep = ",")
Dataset_mid

View(Dataset_mid)

#Handling missing values
colSums(is.na(Dataset_mid))

Dataset_mid$age[is.na(Dataset_mid$age)] <- mean(Dataset_mid$age, na.rm = TRUE)
Dataset_mid$ca[is.na(Dataset_mid$ca)] <- mean(Dataset_mid$ca, na.rm = TRUE)
colSums(is.na(Dataset_mid))

#Handling invalid values
Dataset_mid$sex
invalid_indices<-grep("F",Dataset_mid$sex)
Dataset_mid$sex[invalid_indices]<-"Female"
Dataset_mid

Dataset_mid$sex
Dataset_mid$fbs
Dataset_mid$exang

#Categorical to Numeric conversion
Dataset_mid$sex <- factor(Dataset_mid$sex, levels = c("Female", "Male"), labels = c(0, 1))
Dataset_mid$fbs <- factor(Dataset_mid$fbs, levels = c("FALSE", "TRUE"), labels = c(0, 1))
Dataset_mid$exang <- factor(Dataset_mid$exang, levels = c("FALSE", "TRUE"), labels = c(0, 1))
Dataset_mid$sex
Dataset_mid$fbs
Dataset_mid$exang

#NOrmalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
Dataset_mid$trestbps_nor<- normalize(Dataset_mid$trestbps)
Dataset_mid$thalch_nor<- normalize(Dataset_mid$thalch)
Dataset_mid$chol_nor<- normalize(Dataset_mid$chol)
Dataset_mid


#Remove dublicate values
Dataset_mid_updated <- distinct(Dataset_mid, age, sex,cp,trestbps, thalch,chol,fbs,restecg,exang,oldpeak,slope,ca,thal,num, .keep_all = TRUE)

Dataset_mid_updated


#Filter
filtered_data <- filter(Dataset_mid, age > 44)
print(head(filtered_data))



#Convert imbalance dataset to balance data
cat("Original counts:\n")
original_counts <- table(Dataset_mid$num)
print(original_counts)

min_size <- min(original_counts)

class0 <- Dataset_mid[Dataset_mid$num == 0, ]
class1 <- Dataset_mid[Dataset_mid$num == 1, ]

set.seed(123)  
if (nrow(class0) > nrow(class1)) {
  
  class0 <- class0[sample(nrow(class0), min_size), ]
} else {
  
  class1 <- class1[sample(nrow(class1), min_size), ]
}


balanced_data <- rbind(class0, class1)

cat("\nBalanced counts:\n")
print(table(balanced_data$num))



#Split  Data set for Training and Testing 
set.seed(123)
split_index <- sample(1:nrow(Dataset_mid), size = 0.7 * nrow(Dataset_mid))

train_data <- Dataset_mid[split_index, ]
test_data  <- Dataset_mid[-split_index, ]

nrow(train_data)
nrow(test_data)


#Compute the Central Tendencies (Mean, Median, Mode)
mean_age <- mean(train_data$age, na.rm = TRUE)
print(paste("Mean of Age:", mean_age))

mean_chol <- mean(train_data$chol, na.rm = TRUE)
print(paste("Mean of Cholesterol:", mean_chol))

median_age <- median(train_data$age, na.rm = TRUE)
print(paste("Median of Age:", median_age))

median_chol <- median(train_data$chol, na.rm = TRUE)
print(paste("Median of Cholesterol:", median_chol))

mode_sex <- names(sort(table(train_data$sex), decreasing = TRUE))[1]
print(paste("Mode of Sex:", mode_sex))

mode_cp <- names(sort(table(train_data$cp), decreasing = TRUE))[1]
print(paste("Mode of Chest Pain Type:", mode_cp))



#Spread (Range, Variance, Standard Deviation) computation
range_age <- range(train_data$age, na.rm = TRUE)
print(paste("Range of Age:", range_age[1], "to", range_age[2]))

var_age <- var(train_data$age, na.rm = TRUE)
print(paste("Variance of Age:", var_age))

sd_age <- sd(train_data$age, na.rm = TRUE)
print(paste("Standard Deviation of Age:", sd_age))



range_chol <- range(train_data$chol, na.rm = TRUE)
print(paste("Range of Cholesterol:", range_chol[1], "to", range_chol[2]))

var_chol <- var(train_data$chol, na.rm = TRUE)
print(paste("Variance of Cholesterol:", var_chol))

sd_chol <- sd(train_data$chol, na.rm = TRUE)
print(paste("Standard Deviation of Cholesterol:", sd_chol))



# Removing outliers
nrow(Dataset_mid)
Q1 <- quantile(Dataset_mid$trestbps, 0.25)
Q3 <- quantile(Dataset_mid$trestbps, 0.75)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR
Dataset_mid <- Dataset_mid[Dataset_mid$trestbps >= lower & Dataset_mid$trestbps <= upper, ]

Q1 <- quantile(Dataset_mid$chol, 0.25)
Q3 <- quantile(Dataset_mid$chol, 0.75)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR
Dataset_mid <- Dataset_mid[Dataset_mid$chol >= lower & Dataset_mid$chol <= upper, ]

nrow(Dataset_mid)
summary(Dataset_mid$trestbps)
summary(Dataset_mid$chol)


