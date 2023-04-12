#install.packages("treemap)
#install.packages("randomForest")
library(dplyr) 
library(ggplot2)
library(treemap)
library(randomForest)
library(caret)
library(e1071)
library(rpart)

CalcTrain <- read.csv("C:/R/calc_case_description_train_set.csv",stringsAsFactors = FALSE)
CalcTest <- read.csv("C:/R/calc_case_description_test_set.csv",stringsAsFactors = FALSE)

CalcTrain <- subset(CalcTrain,select = -c(image.file.path,cropped.image.file.path,
                                          ROI.mask.file.path))

CalcTest <- subset(CalcTest,select = -c(image.file.path,cropped.image.file.path,
                                        ROI.mask.file.path))

CalcTrain_Clean <- data.frame(CalcTrain)
CalcTest_Clean <- data.frame(CalcTest)

str(CalcTrain_Clean)
str(CalcTest_Clean)




#Renaming columns

CalcTrain_Clean <- CalcTrain_Clean %>% rename(breast_density = breast.density, 
                                              left_or_right_breast = left.or.right.breast,
                                              image_view = image.view, abnormality_type = abnormality.type, abnormality_id = abnormality.id,
                                              calc_type = calc.type, calc_distribution = calc.distribution)

CalcTest_Clean <- CalcTest_Clean %>% rename(breast_density = breast.density, 
                                            left_or_right_breast = left.or.right.breast,
                                            image_view = image.view, abnormality_type = abnormality.type, abnormality_id = abnormality.id,
                                            calc_type = calc.type, calc_distribution = calc.distribution)

##Setting Categorical variables

CalcTrain_Clean$left_or_right_breast <- factor(CalcTrain_Clean$left_or_right_breast)
CalcTrain_Clean$image_view <- factor(CalcTrain_Clean$image_view)
CalcTrain_Clean$abnormality_type <- factor(CalcTrain_Clean$abnormality_type)
CalcTrain_Clean$calc_type <- factor(CalcTrain_Clean$calc_type)
CalcTrain_Clean$calc_distribution <- factor(CalcTrain_Clean$calc_distribution)
CalcTrain_Clean$pathology <- factor(CalcTrain_Clean$pathology)


CalcTest_Clean$left_or_right_breast <- factor(CalcTest_Clean$left_or_right_breast)
CalcTest_Clean$image_view <- factor(CalcTest_Clean$image_view )
CalcTest_Clean$abnormality_type <- factor(CalcTest_Clean$abnormality_type)
CalcTest_Clean$calc_type <- factor(CalcTest_Clean$calc_type)
CalcTest_Clean$calc_distribution <- factor(CalcTest_Clean$calc_distribution)
CalcTest_Clean$pathology <- factor(CalcTest_Clean$pathology)

#Summary Statistics
summary(CalcTrain_Clean)
summary(CalcTest_Clean)

#Univariate analysis
hist(CalcTrain_Clean$breast_density)

boxplot(CalcTrain_Clean$subtlety)

ggplot(data = CalcTrain_Clean, aes(x = assessment)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ggtitle("Density Plot of Assessment")

# Frequency table of left.or.right.breast
table(CalcTrain_Clean$left_or_right_breast)

# Relative frequency table of pathology
prop.table(table(CalcTrain_Clean$pathology))

# Summary statistics of breast density by pathology
aggregate(CalcTrain_Clean$breast_density, by = list(CalcTrain_Clean$pathology), FUN = summary)

ggplot(CalcTrain_Clean, aes(x = pathology, y = breast_density)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(x = "Pathology", y = "Breast Density", 
       title = "Distribution of Breast Density by Pathology")

#Summary statistics of assessment by pathology
aggregate(CalcTrain_Clean$assessment, by = list(CalcTrain_Clean$pathology), FUN = summary)

ggplot(CalcTrain_Clean, aes(x = pathology, y = assessment)) +
  geom_violin(fill = "#69b3a2") +
  labs(x = "Pathology", y = "Assessment", 
       title = "Distribution of Assessment by Pathology")

#Correlation analysis
# Select only the numerical variables
numerical_vars <- CalcTrain_Clean %>% 
  select_if(is.numeric)

# Perform correlation analysis
correlation_matrix <- cor(numerical_vars)

# Print the correlation matrix
print(correlation_matrix)

#install.packages("ggcorrplot")
library(ggcorrplot)

cor_matrix <- cor(CalcTrain_Clean[, c("breast_density", "abnormality_id", "assessment", "subtlety")])

ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


#Missing Values analysis
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

CalcTrain_Clean$calc_distribution[is.na(CalcTrain_Clean$calc_distribution)] <- getmode(CalcTrain_Clean$calc_distribution)
CalcTrain_Clean$calc_type[is.na(CalcTrain_Clean$calc_type)] <- getmode(CalcTrain_Clean$calc_type)

colSums(is.na(CalcTrain_Clean))

CalcTest_Clean$calc_distribution[is.na(CalcTest_Clean$calc_distribution)] <- getmode(CalcTest_Clean$calc_distribution)
CalcTest_Clean$calc_type[is.na(CalcTest_Clean$calc_type)] <- getmode(CalcTest_Clean$calc_type)

colSums(is.na(CalcTest_Clean))

#Predictions

set.seed(101)

features <- c("breast_density", "left_or_right_breast", "image_view", 
              "abnormality_type", "calc_type", "calc_distribution", 
              "assessment", "subtlety")
target <- "pathology"



train_data <- CalcTrain_Clean[, c(features, target)]
test_data <- CalcTest_Clean[, c(features, target)]

colSums(is.na(train_data))

#Random Forest
Calc_rf <- randomForest(pathology ~ ., data = train_data)
Calc_rf

Calc_pred <- predict(Calc_rf, data = predtest)
Calc_pred

#Evaluate using Confusion matrix
cm <- confusionMatrix(Calc_pred, train_data$pathology)
cm

accuracy <- cm$overall['Accuracy']
print(accuracy)

#Decision Tree
tree_model <- rpart(subtlety ~ ., data = train_data, method = "class")
tree_model


