# Practical Machine Learning - Coursera
# Ana Sapata


library(caret)
library(corrplot)
library(rpart)
library(rpart.plot)
library(rattle)
library(e1071)

setwd('/home/anasapata/Personal/datasciencecoursera/ML/ProjectML')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', 'training.csv')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', 'testing.csv')

# Training and testing sets
training <- read.csv('training.csv', header = TRUE, stringsAsFactors = FALSE)
validation <- read.csv('testing.csv', header = TRUE, stringsAsFactors = FALSE)
training$X <- NULL
validation$X <- NULL

head(training)
head(validation)

dim(training)
dim(validation)

# Remove the columns with NA's values
training <- training[, colSums(is.na(training))==0]
validation <- validation[, colSums(is.na(validation))==0]

# Remove first 6 variables
training <- training[,-c(1:6)]
validation <- validation[,-c(1:6)]
# remove variables with values ""
training <- training[,-c(5:13)]
training <- training[,-c(27:32)]
training <- training[,-c(30:38)]
training <- training[,-c(43:51)]

dim(training)
dim(validation)
# Split the training data between testing and training with 30%, 70% resp.
set.seed(2711)
inTrain <- createDataPartition(training$classe, p= 0.7, list = FALSE)
training <- training[inTrain,]
testing <- training[-inTrain,]

# See the correlation between variables
correlation <- cor(training[,-53])
corrplot(correlation, order = "FPC", method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))

# Get the columns with high correlation(over 0.75)
high_cor = findCorrelation(correlation, cutoff=0.75)
# Verify which are the columns with high correlation
names(training)[high_cor]


# 1st - Decision Trees
set.seed(2711)
DTFit1 <- rpart(classe ~ ., data=training, method="class")
#fancyRpartPlot(DTFit1)
png('DTFit1.png')
prp(DTFit1)
dev.off()
# Predictions
predictDTFit1 <- predict(DTFit1, testing, type = "class")
conf_mat_DTFit1 <- confusionMatrix(table(predictDTFit1, testing$classe))
# Accuracy = .743
conf_mat_DTFit1


# 2nd - Random Forest
set.seed(2711)
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
RFFit1 <- train(classe ~ ., data=training, method="rf", trControl = controlRF)
RFFit1$finalModel
# Predictions
predictRFFit1 <- predict(RFFit1, testing)
conf_mat_RFFit1 <- confusionMatrix(table(predictRFFit1, testing$classe))
# Accuracy = 1
conf_mat_RFFit1
plot(RFFit1)


# 3rd - Generalized Boosted Regression Models
set.seed(2711)
controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
GBMFit1 <- train( classe ~. , data = training, method = "gbm", trControl = controlGBM, verbose = FALSE)
GBMFit1$finalModel
print(GBMFit1)
# Predictions
predictGBMFit1 <- predict(GBMFit1, testing)
conf_mat_GBMFit1 <- confusionMatrix(table(predictGBMFit1, testing$classe))
# Accuracy = .9801
conf_mat_GBMFit1


# The model with the high accuracy was RF, so i will apply it to the validation set
res <- predict(RFFit1, validation); res
