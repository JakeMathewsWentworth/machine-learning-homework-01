# Homework 1 - Part 2 - Jake Mathews
######## Problem 2 ########
library(Matrix)
library(DMwR)
library(class)
library(TTR)

kCount = 100

accuracy <- numeric()
error <- numeric()
n <- integer()
wine.train <- read.csv(file = "winedata.csv", as.is = TRUE)
wine.test <- read.csv(file = "winedata_test.csv", as.is = TRUE)
for (i in 1:kCount) {
  n[i] = i
  knearest <- knn(wine.train, wine.test, wine.train[, 1], k = i)
  
  #confusion matrix
  confusionMatrix <- table(wine.test[, 'class'], knearest)
  matrixDiagonal <- diag(confusionMatrix)
  sumTotal <- sum(confusionMatrix)
  sumDiagonal <- sum(matrixDiagonal)
  
  accuracy[i] <- sumDiagonal / sumTotal
  error[i] <- (sumTotal - sumDiagonal) / sumTotal
  print(accuracy[i])
  print(error[i])
}
resultsknn <- cbind(n, accuracy, error)
colnames(resultsknn) <- c("kNN", "Accuracy", "Error")


resultsknn <- cbind(n, accuracy, error)
colnames(resultsknn) <- c("kNN", "Accuracy", "Error")
pdf("kNNaccuracy.pdf")
plot(1:kCount, accuracy, xlab = "k", ylab = "Accuracy")
dev.off()
pdf("kNNerror.pdf")
plot(1:kCount, error, xlab = "k", ylab = "Error")
dev.off()
write.table(
  resultsknn,
  file = "ResultsDigitKNN.csv",
  append = F,
  quote = F,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = F,
  col.names = T
)
