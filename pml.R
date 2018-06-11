run <- function() {

    library(caret)

    data <- read.csv("pml-training.csv")
    data <- data[, -c(1,2, 3, 4, 5)]
    data <- data[, -grep("kurtosis_*", names(data))]
    data <- data[, -grep("skewness_*", names(data))]
    data <- data[, -grep("max_*", names(data))]
    data <- data[, -grep("min_*", names(data))]
    data <- data[, -grep("amplitude_*", names(data))]
    data <- data[, -grep("var_*", names(data))]
    data <- data[, -grep("avg_*", names(data))]
    data <- data[, -grep("stddev_*", names(data))]

    inTrain <- createDataPartition(y = data$classe, p = 0.8, list = FALSE)
    training <<- data[inTrain, ]
    testing <<- data[-inTrain, ]

    tc <- trainControl(method = "cv", number = 5)
    print("Calling RF")
    modelFit <- train(classe ~., data = training, method = "rf", trControl = tc,
                      preProcess = c("nzv", "pca"), verbose = FALSE)

    print("Predicting")
    pred <<- predict(modelFit, testing)

    print(postResample(pred = pred, obs = testing$classe))

    return (modelFit)
}