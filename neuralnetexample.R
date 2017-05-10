#install.packages("neuralnet")
require(neuralnet)
?infert
dim(infert)
View(infert)
#install.packages("MASS")
library(MASS)
set.seed(123)
DataFrame <- Boston
View(Boston)
help("Boston")

str(DataFrame)

hist(DataFrame$medv)
head(DataFrame,3)

apply(DataFrame,2,range)

maxValue<-apply(DataFrame,2,max)

minValue<-apply(DataFrame,2,min)
minValue
?scale

DataFrame<-as.data.frame(scale(DataFrame,center = minValue, scale = maxValue - minValue))

View(DataFrame)

ind<-sample(1:nrow(DataFrame),400)

trainDF<-DataFrame[ind,]

testDF<-DataFrame[-ind,]

allVars<-colnames(DataFrame)

predictorVars <- allVars[!allVars%in%"medv"]

predictorVars <- paste(predictorVars, collapse = "+")

predictorVars

allVars

form = as.formula(paste("medv~",predictorVars,collapse = "+"))

form

neuralModel<-neuralnet(formula =form,hidden = c(4,2),linear.output = T, data=trainDF)

plot(neuralModel)

predictions <- compute(neuralModel, testDF[,1:13])

View(predictions)

str(predictions)

predictions<- (predictions$net.result*max(testDF$medv)-min(testDF$medv))+min(testDF$medv)

actualValues <- (testDF$medv)*(max(testDF$medv)-min(testDF$medv)) + min(testDF$medv)
View(actualValues)
View(testDF$medv)


MSE <- sum((predictions - actualValues)^2)/nrow(testDF)
MSE

plot(testDF$medv, predictions,col="blue", main = "Real vs Predicted", pch = 1, cex = 1)

abline(0,1,col="black")
