traindf <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testdf <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")


#get all possible predictors
testmodel <- function(){
  
  acc <- c()
  predictor <- c()
  
  set.seed(0524)
  
  cols <- traindf %>%  select(where(~!any(is.na(.))))
  cols <- names(select_if(cols, is.numeric))
  
  for (i in 1:length(cols)) {
    
    pred <- paste0("classe ~",cols[i])
    
    train.control <- trainControl(method = "repeatedcv",  number = 100, repeats = 1)
    model <- train(as.formula(pred), data = traindf, method = "rpart" ,trControl = train.control)
    predict(model, testdf)
    acc <-append(acc, mean(model$results$Accuracy))
    predictor <- append(predictor, cols[i])
    print(paste("Estimation: ",round(i*100/length(cols),2),"%"))
  }
  
  return(data.frame(predictor, acc))

}

p <- testmodel()

train.control <- trainControl(method = "repeatedcv",  number = 10, repeats = 100)
model <- train(classe ~ raw_timestamp_part_1 + yaw_belt , data = traindf, method = "rpart" ,trControl = train.control)
predict(model, testdf)
model$results$Accuracy




