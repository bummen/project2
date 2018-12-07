# http://topepo.github.io/caret/index.html
# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization

# Install all relevant packages and establish useful functions
# install.packages("beepr") # for playing sounds
# install.packages("rpart.plot") # for nice tree plots
# install.packages("rpart") # for building recursive trees
require("rpart") 
require("rpart.plot")
require("caret")
require("beepr")
require("Hmisc")
require("PerformanceAnalytics")
require("gridExtra")


# Load data
survey <- read.csv("..\\Survey_Key_and_Complete_Responses_excel.csv")

# Get a first feeling for the data by plotting most important relationships and looking for correlation.
chart.Correlation(as.matrix(survey[, sapply(survey, is.numeric)]), histogram=TRUE, pch=19)
# There's definitely something going on between salary and brand.
# Data in general looks so unnatural, it's hard to believe our firm paid money to that market research agency.

## Another way of looking at correlation
# Show correlation between variables
# corrMat2 <- rcorr(as.matrix(survey[, sapply(survey, is.numeric)]))
# corrMat2
# corrplot(corrMat2$r)
# We observe no correlation whatsoever between variables.

# Inspect & Transform data types, look for missing values
anyNA(survey) # Any missing values? No, in this case.
str(survey)
survey$brand <- gsub(0, "Acer", survey$brand)
survey$brand <- gsub(1, "Sony", survey$brand)
survey$brand <- as.factor(survey$brand)

# Another way of looking at some basic plots.
PloWins = function(i,k) {par(mfrow=c(i,k))} # Easier to change plots per view
PloWins(3,2)
plot(brand~., data = survey)

# Show tree structure of data
tree <- rpart(brand ~ ., data = survey)
rpart.plot(tree)

## A more basic tree plot
# plot(tree)
# text(tree)
# We observe an interaction effect between salary and age. Other variables (credit, zipcode, car, elevel) do not predict brand.

# Let's visualize that a bit better
plot_survey <- ggplot(survey, aes(x = salary, y = age, color = brand)) +
  geom_point() +
  labs(title="Brand Preference in Complete Survey")
plot_survey

# Set Seed
set.seed(123)

# create a 75%/25% train/test split of the dataset
survTrainSize = createDataPartition(survey[,1], p = .75, list = FALSE)
survTrain = survey[survTrainSize,]
survTest = survey[-survTrainSize,]

#10 fold cross validation
survCrossVal = trainControl(method = "repeatedcv", number = 10, repeats = 10)

# train model knn
survTrain_knn = train(brand~salary*age, # * leads to: ~salary, ~age, ~salary x age or train(brand~salary+age+salary:age)
                      data=survTrain, 
                      method = "knn", 
                      trControl = survCrossVal, 
                      preProc = c("center", "scale"), 
                      tuneGrid = expand.grid(
                        k = 45:45 # k = 45 is the result of tuning
                        )
                      )
beep()
survTrain_knn

#make prediction and measure the performance of model, use information for tuning.
testPrediction_knn = predict(survTrain_knn, survTest)
confusionMatrix(testPrediction_knn, survTest$brand)

# k = 45 ==> Accuracy : 0.9248
# k = 51 ==> Accuracy : 0.9232
# k = 60 ==> Accuracy : 0.9244


# train model rf
survCrossVal = trainControl(method = "repeatedcv", number = 10, repeats = 10, search = "grid") # or add search = "grid"
survTrain_rf = train(brand~salary*age, 
                     data=survTrain,  
                     method = "rf", 
                     trControl = survCrossVal, 
                     preProc = c("center", "scale"),
                     tuneGrid = expand.grid(
                       mtry = 2:2
                     )
                     )
shell.exec("https://youtu.be/PzAc_sx9gcI?t=89") # Just to get back one's attention after this long computation (make sure to execute it with previous code)
survTrain_rf

#make prediction and measure the performance of model, use information for tuning.
testPrediction_rf = predict(survTrain_rf, survTest)
confusionMatrix(testPrediction_rf, survTest$brand)

# mtry = 2 ==> Accuracy : 0.91

# train model SVM
survTrain_svm = train(brand~salary*age, 
                      data=survTrain, 
                      method = "svmLinearWeights2", 
                      trControl = survCrossVal, 
                      preProc = c("center", "scale")
                      )
beep()
survTrain_svm

#make prediction and measure the performance of model, use information for tuning.
testPrediction_svm = predict(survTrain_svm, survTest)
confusionMatrix(testPrediction_svm, survTest$brand)

# cost = 1, Loss = L1 and weight = 1 ==> Accuracy : 0.674           


# train model Naive Bayes
survTrain_naiveBayes = train(brand~salary*age,
                             data=survTrain, 
                             method = "naive_bayes", # very slightly better than "nb" from same caret, which is interesting.
                             trControl = survCrossVal, 
                             tuneGrid = expand.grid(
                               usekernel = c(TRUE, FALSE),
                               laplace = 0:5,
                               adjust = seq(0, 5, by = 1)
                               ),
                             preProc = c("center", "scale")
                             )
beep()
survTrain_naiveBayes

#make prediction and measure the performance of model, use information for tuning.
testPrediction_naiveBayes = predict(survTrain_naiveBayes, survTest)
confusionMatrix(testPrediction_naiveBayes, survTest$brand)

# laplace = 0, usekernel = FALSE and adjust = 0 ==> Accuracy : 0.7524

# Proceed to prediction
survey_incomplete = read.csv("..\\SurveyIncomplete.csv")

# Out of curiosity, look at incomplete data set. If some strangely different patters present (which is not the case), some more critical thinking would have to be deployed.
chart.Correlation(as.matrix(survey_incomplete[, sapply(survey_incomplete, is.numeric)]), histogram=TRUE, pch=19)

# Inspect & Transform data types, look for missing values
anyNA(survey_incomplete) # Any missing values? No, in this case.
str(survey_incomplete) 
# survey_incomplete$brand <- as.factor(survey_incomplete$brand)

survey_predicted_incomplete <- survey_incomplete
survey_predicted_incomplete$brand <- predict(survTrain_knn, survey_incomplete)
survey_predicted_incomplete

# For scientific purposes, let's get a tree for the predicted data set
tree_predicted_incomplete <- rpart(brand ~ ., data = survey_predicted_incomplete)
plot(tree, main = "Decision Tree of Predicted Brand Affinity")
text(tree)

# Plot the predicted brand distribution in dependence of salary and age
plot_prediction <- ggplot(survey_predicted_incomplete, aes(x = salary, y = age, color = brand)) +
  geom_point() +
  labs(title="Predicted Brand Preference for Incomplete Survey")
plot_prediction

# Tadaa, almost final line of code: brand distribution in prediction vs. complete survey 
grid.arrange(plot_survey, plot_prediction, ncol=2)

# Create csv-file of result
write.csv(survey_predicted_incomplete, file = "..\\Predicted Brand Preference.csv")