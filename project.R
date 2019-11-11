# Practical Machine Learning - Coursera
# Ana Sapata


library(caret)

setwd('/home/anasapata/Personal/datasciencecoursera/ML/Project')
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


# Split the training data between testing and training with 30%, 70% resp.
set.seed(2711)
inTrain <- createDataPartition(training$classe, p= 0.7, list = FALSE)
training <- training[inTrain,]
testing <- training[-inTrain,]

# See the correlation between variables
cor_mat <- cor(training[,-86])
corrplot(cor_mat, order = "FPC", method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))