# Homework 1 - Part 3 - Jake Mathews

# Utility methods
edistance <- function(x, y) {
  n = length(x)
  d = 0
  for (i in 1:n) {
    d = d + (x[, i] - y[, i]) ^ 2
  }
  d = sqrt(d)
  return(d)
}

######## Problem 1 ########
knn = 3
data("iris")
colnames(iris)[5] <- "Species.Name"

# Map species to a value
speciesNames = c("setosa", "versicolor", "virginica")
speciesCount = length(speciesNames)
for (speciesIndex in 1:speciesCount) {
  name = speciesNames[speciesIndex]
  iris$Species.Int[iris$Species.Name == name] <- speciesIndex
}

# Seperate training and test sets
trainIrisIndex <-
  sample(1:nrow(iris), as.integer(.7 * nrow(iris)), replace = FALSE)
trainIris <- iris[trainIrisIndex, ]
testIris <- iris[-trainIrisIndex, ]

# Remove species as a column to calculate distance
trainIris.no_species <- trainIris[, -5]
testIris.no_species <- testIris[, -5]

predictions <- integer()
for (testIndex in 1:nrow(testIris)) {
  distances <- numeric()
  test <- testIris.no_species[testIndex, ]
  for (trainIndex in 1:nrow(trainIris)) {
    train <- trainIris.no_species[trainIndex, ]
    distance <- edistance(test, train)
    distances[trainIndex] = distance
  }
  
  # Create a sorted data frame with distances mapped to species
  distances <- data.frame(distances, trainIris$Species.Int)
  colnames(distances) <- c("distance", "species")
  distances.sorted <- distances[order(distances$distance), ]
  
  # The occurance of each species is tallied
  occurance <- integer(speciesCount)
  for (neighborIndex in 1:knn) {
    speciesIndex = distances.sorted[neighborIndex, ]$species
    occurance[speciesIndex] <- occurance[speciesIndex] + 1
  }
  
  # Which ever species occured the most in the k nearest neighbors wins
  # In the event of a tie, the first species in this order wins (setosa, versicolor, virginica)
  predictedSpecies = which.max(occurance)
  predictions[testIndex] = predictedSpecies
}

# Load result into a data frame
results <- data.frame(predictions, testIris$Species.Int)
colnames(results) <- c("predicted", "actual")

# Create confusion matrix
matrixSize = speciesCount ^ 2
confusionMatrix = matrix(integer(matrixSize), nrow = speciesCount, ncol = speciesCount)
rownames(confusionMatrix) <- speciesNames
colnames(confusionMatrix) <- speciesNames

# Fill confusion matrix
for (resultIndex in 1:nrow(results)) {
  result <- results[resultIndex, ]
  confusionMatrix[result$actual, result$predicted] = confusionMatrix[result$actual, result$predicted] + 1
}

matrixDiaganol <- diag(confusionMatrix)
sumTotal <- sum(confusionMatrix)
sumDiagnanol <- sum(matrixDiaganol)

#calculate accuracy
overallAccuracy <- sumDiagnanol / sumTotal
#calculate realtive error
overallError <- (sumTotal - sumDiagnanol) / sumTotal
overall <- data.frame(overallAccuracy, overallError)
colnames(overall) <- c("Accuracy", "Error")
rownames(overall) <- c("Overall")

# Print results
accuracyTable <- matrix(nrow = speciesCount, ncol = 4)
colnames(accuracyTable) <- c("TP", "FN", "FP", "TN")
rownames(accuracyTable) <- speciesNames
for (species in 1:speciesCount) {
  TP = confusionMatrix[species, species]
  FN = sum(confusionMatrix[species, ]) - TP
  FP = sum(confusionMatrix[, species]) - TP
  TN = FN + FP
  accuracyTable[species, ] = c(TP, FN, FP, TN)
}

print("Problem 1")
print(results)
print(accuracyTable)
print(overall)
