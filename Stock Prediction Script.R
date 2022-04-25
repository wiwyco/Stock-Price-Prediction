#import Libraries
library(quantmod)
library(caret)

#{r echo=TRUE, results='hide'}

#set symbol
#To change symbol, change in both getSymbols call and raw_frame instantiation
getSymbols("GOOG", src = "yahoo")
raw_frame <- GOOG


#Normalization Function
norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#day prediction net
{
  #Build master Dataframe
  print("Building day dataframe...")
  dayframe <- data.frame(day1open = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day1high = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day1low = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day1close = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day1vol = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day1adj = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day0open = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day0high = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day0low = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day0close = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day0vol = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day0adj = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_1open = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_1high = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_1low = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_1close = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_1vol = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_1adj = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_2open = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_2high = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_2low = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_2close = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_2vol = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_2adj = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_3open = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_3high = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_3low = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_3close = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_3vol = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_3adj = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_4open = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_4high = rep(c(0), each = (nrow(raw_frame) - 6)), 
                            day_4low = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_4close = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_4vol = rep(c(0), each = (nrow(raw_frame) - 6)),
                            day_4adj = rep(c(0), each = (nrow(raw_frame) - 6)))
  print("Done.")
  
  pb <- txtProgressBar(min = 0,
                       max = (nrow(raw_frame) - 7), 
                       style = 3,
                       width = 60,
                       char = "#")
  
  #Populates dayframe
  print("Populating Dataframe...")
  for(i in 1:(nrow(raw_frame) - 7)){
    x <- 13
    y <- (nrow(raw_frame) - 6) - i
    
    dayframe[i, 7] <- raw_frame[y - 1, 1]
    dayframe[i, 8] <- raw_frame[y - 1, 2]
    dayframe[i, 9] <- raw_frame[y - 1, 3]
    dayframe[i, 10] <- raw_frame[y - 1, 4]
    dayframe[i, 11] <- raw_frame[y - 1, 5]
    dayframe[i, 12] <- raw_frame[y - 1, 6]
    
    dayframe[i, 1] <- (raw_frame[y, 1] - dayframe[i, 7])
    dayframe[i, 2] <- (raw_frame[y, 2] - dayframe[i, 8])
    dayframe[i, 3] <- (raw_frame[y, 3] - dayframe[i, 9])
    dayframe[i, 4] <- (raw_frame[y, 4] - dayframe[i, 10])
    dayframe[i, 5] <- (raw_frame[y, 5] - dayframe[i, 11])
    dayframe[i, 6] <- (raw_frame[y, 6] - dayframe[i, 12])                                                                                                             
    
    for(j in 3:6){
      dayframe[i, x] <- (raw_frame[y - (j - 1), 1] - dayframe[i, 7])
      dayframe[i, x+1] <- (raw_frame[y - (j - 1), 2] - dayframe[i, 8])
      dayframe[i, x+2] <- (raw_frame[y - (j - 1), 3] - dayframe[i, 9])
      dayframe[i, x+3] <- (raw_frame[y - (j - 1), 4] - dayframe[i, 10])
      dayframe[i, x+4] <- (raw_frame[y - (j - 1), 5] - dayframe[i, 11])
      dayframe[i, x+5] <- (raw_frame[y - (j - 1), 6] - dayframe[i, 12])
      
      x <- x + 6
    }
    
    setTxtProgressBar(pb, i)
  }
  print("Done.")
  
  #Normalizes all colummns
  print("Normalizing Columns...")
  for(i in c(1:6, 13:36)){
    dayframe[,i] <- norm(dayframe[,i])
  }
  print("Done.")
  
  #Control Block
  fitControl <- trainControl(## 10-fold CV
    method = "cv",
    number = 10)
  
  #Test
  # day1open + day1high + day1low + day1close + day1vol + day1adj
  
  pb <- txtProgressBar(min = 0,
                       max = 5, 
                       style = 3,
                       width = 60,
                       char = "#")
  
  #Neural Nets
  {
    setTxtProgressBar(pb, 0)
    set.seed(825)
    day.open_model <- train(day1open~
                          day_1open + day_1high + day_1low + day_1close + day_1vol + day_1adj +
                          day_2open + day_2high + day_2low + day_2close + day_2vol + day_2adj +
                          day_3open + day_3high + day_3low + day_3close + day_3vol + day_3adj +
                          day_4open + day_4high + day_4low + day_4close + day_4vol + day_4adj, 
                          data = dayframe, 
                          method = "nnet", 
                          trControl = fitControl,
                          verbose = FALSE,
                          trace = FALSE)
    #day.open_model
    
    setTxtProgressBar(pb,1)
    
    day.high_model <- train(day1high~
                         day_1open + day_1high + day_1low + day_1close + day_1vol + day_1adj +
                         day_2open + day_2high + day_2low + day_2close + day_2vol + day_2adj +
                         day_3open + day_3high + day_3low + day_3close + day_3vol + day_3adj +
                         day_4open + day_4high + day_4low + day_4close + day_4vol + day_4adj, 
                         data = dayframe, 
                         method = "nnet", 
                         trControl = fitControl,
                         verbose = FALSE,
                         trace = FALSE)
    day.high_model
    
    setTxtProgressBar(pb, 2)
    
    day.low_model <- train(day1low~
                         day_1open + day_1high + day_1low + day_1close + day_1vol + day_1adj +
                         day_2open + day_2high + day_2low + day_2close + day_2vol + day_2adj +
                         day_3open + day_3high + day_3low + day_3close + day_3vol + day_3adj +
                         day_4open + day_4high + day_4low + day_4close + day_4vol + day_4adj, 
                         data = dayframe, 
                         method = "nnet", 
                         trControl = fitControl,
                         verbose = FALSE,
                         trace = FALSE)
    #day.low_model
    
    setTxtProgressBar(pb, 3)
    
    day.close_model <- train(day1close~
                             day_1open + day_1high + day_1low + day_1close + day_1vol + day_1adj +
                             day_2open + day_2high + day_2low + day_2close + day_2vol + day_2adj +
                             day_3open + day_3high + day_3low + day_3close + day_3vol + day_3adj +
                             day_4open + day_4high + day_4low + day_4close + day_4vol + day_4adj, 
                           data = dayframe, 
                           method = "nnet", 
                           trControl = fitControl,
                           verbose = FALSE,
                           trace = FALSE)
    #day.low_model
    
    setTxtProgressBar(pb, 4)
    
    day.vol_model <- train(day1vol~
                         day_1open + day_1high + day_1low + day_1close + day_1vol + day_1adj +
                         day_2open + day_2high + day_2low + day_2close + day_2vol + day_2adj +
                         day_3open + day_3high + day_3low + day_3close + day_3vol + day_3adj +
                         day_4open + day_4high + day_4low + day_4close + day_4vol + day_4adj, 
                         data = dayframe, 
                         method = "nnet", 
                         trControl = fitControl,
                         verbose = FALSE,
                         trace = FALSE)
    #day.vol_model
    
    setTxtProgressBar(pb, 5)
    
    day.adj_model <- train(day1adj~
                         day_1open + day_1high + day_1low + day_1close + day_1vol + day_1adj +
                         day_2open + day_2high + day_2low + day_2close + day_2vol + day_2adj +
                         day_3open + day_3high + day_3low + day_3close + day_3vol + day_3adj +
                         day_4open + day_4high + day_4low + day_4close + day_4vol + day_4adj, 
                         data = dayframe, 
                         method = "nnet", 
                         trControl = fitControl,
                         verbose = FALSE,
                         trace = FALSE)
    day.adj_model
    
    setTxtProgressBar(pb, 6)
  }
}



#Week (5 day) prediction net
{
weekframe <- data.frame(week1open = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week1high = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week1low = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week1close = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week1vol = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week1adj = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week0open = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week0high = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week0low = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week0close = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week0vol = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week0adj = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_1open = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_1high = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_1low = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_1close = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_1vol = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_1adj = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_2open = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_2high = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_2low = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_2close = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_2vol = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_2adj = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_3open = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_3high = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_3low = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_3close = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_3vol = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_3adj = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_4open = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_4high = rep(c(0), each = (nrow(raw_frame) - 36)), 
                       week_4low = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_4close = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_4vol = rep(c(0), each = (nrow(raw_frame) - 36)),
                       week_4adj = rep(c(0), each = (nrow(raw_frame) - 36)))


for(i in 1:3810) {
  weekframe[i,1] <- mean(raw_frame[(nrow(raw_frame) - i):(nrow(raw_frame) - i - 4),1])
  weekframe[i,2] <- max(raw_frame[(nrow(raw_frame) - i):(nrow(raw_frame) - i - 4),2])
  weekframe[i,3] <- min(raw_frame[(nrow(raw_frame) - i):(nrow(raw_frame) - i - 4),3])
  weekframe[i,4] <- mean(raw_frame[(nrow(raw_frame) - i):(nrow(raw_frame) - i - 4),4])
  weekframe[i,5] <- mean(raw_frame[(nrow(raw_frame) - i):(nrow(raw_frame) - i - 4),5])
  weekframe[i,6] <- mean(raw_frame[(nrow(raw_frame) - i):(nrow(raw_frame) - i - 4),6])
  
  
  weekframe[i,7] <- mean(raw_frame[(nrow(raw_frame) - i - 5):(nrow(raw_frame) - i - 9),1])
  weekframe[i,8] <- max(raw_frame[(nrow(raw_frame) - i - 5):(nrow(raw_frame) - i - 9),2])
  weekframe[i,9] <- min(raw_frame[(nrow(raw_frame) - i - 5):(nrow(raw_frame) - i - 9),3])
  weekframe[i,10] <- mean(raw_frame[(nrow(raw_frame) - i - 5):(nrow(raw_frame) - i - 9),4])
  weekframe[i,11] <- mean(raw_frame[(nrow(raw_frame) - i - 5):(nrow(raw_frame) - i - 9),5])
  weekframe[i,12] <- mean(raw_frame[(nrow(raw_frame) - i - 5):(nrow(raw_frame) - i - 9),6])
  
  
  weekframe[i,13] <- mean(raw_frame[(nrow(raw_frame) - i - 10):(nrow(raw_frame) - i - 14),1])
  weekframe[i,14] <- max(raw_frame[(nrow(raw_frame) - i - 10):(nrow(raw_frame) - i - 14),2])
  weekframe[i,15] <- min(raw_frame[(nrow(raw_frame) - i - 10):(nrow(raw_frame) - i - 14),3])
  weekframe[i,16] <- mean(raw_frame[(nrow(raw_frame) - i - 10):(nrow(raw_frame) - i - 14),4])
  weekframe[i,17] <- mean(raw_frame[(nrow(raw_frame) - i - 10):(nrow(raw_frame) - i - 14),5])
  weekframe[i,18] <- mean(raw_frame[(nrow(raw_frame) - i - 10):(nrow(raw_frame) - i - 14),6])
  
  
  weekframe[i,19] <- mean(raw_frame[(nrow(raw_frame) - i - 15):(nrow(raw_frame) - i - 19),1])
  weekframe[i,20] <- max(raw_frame[(nrow(raw_frame) - i - 15):(nrow(raw_frame) - i - 19),2])
  weekframe[i,21] <- min(raw_frame[(nrow(raw_frame) - i - 15):(nrow(raw_frame) - i - 19),3])
  weekframe[i,22] <- mean(raw_frame[(nrow(raw_frame) - i - 15):(nrow(raw_frame) - i - 19),4])
  weekframe[i,23] <- mean(raw_frame[(nrow(raw_frame) - i - 15):(nrow(raw_frame) - i - 19),5])
  weekframe[i,24] <- mean(raw_frame[(nrow(raw_frame) - i - 15):(nrow(raw_frame) - i - 19),6])
  
  
  weekframe[i,25] <- mean(raw_frame[(nrow(raw_frame) - i - 20):(nrow(raw_frame) - i - 24),1])
  weekframe[i,26] <- max(raw_frame[(nrow(raw_frame) - i - 20):(nrow(raw_frame) - i - 24),2])
  weekframe[i,27] <- min(raw_frame[(nrow(raw_frame) - i - 20):(nrow(raw_frame) - i - 24),3])
  weekframe[i,28] <- mean(raw_frame[(nrow(raw_frame) - i - 20):(nrow(raw_frame) - i - 24),4])
  weekframe[i,29] <- mean(raw_frame[(nrow(raw_frame) - i - 20):(nrow(raw_frame) - i - 24),5])
  weekframe[i,30] <- mean(raw_frame[(nrow(raw_frame) - i - 20):(nrow(raw_frame) - i - 24),6])
  
  
  weekframe[i,31] <- mean(raw_frame[(nrow(raw_frame) - i - 25):(nrow(raw_frame) - i - 29),1])
  weekframe[i,32] <- max(raw_frame[(nrow(raw_frame) - i - 25):(nrow(raw_frame) - i - 29),2])
  weekframe[i,33] <- min(raw_frame[(nrow(raw_frame) - i - 25):(nrow(raw_frame) - i - 29),3])
  weekframe[i,34] <- mean(raw_frame[(nrow(raw_frame) - i - 25):(nrow(raw_frame) - i - 29),4])
  weekframe[i,35] <- mean(raw_frame[(nrow(raw_frame) - i - 25):(nrow(raw_frame) - i - 29),5])
  weekframe[i,36] <- mean(raw_frame[(nrow(raw_frame) - i - 25):(nrow(raw_frame) - i - 29),6])
}

for(i in c(1:6, 13:36)){
  weekframe[,i] <- norm(weekframe[,i])
}


#Control Block
fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  repeats = 10)

#Neural Nets
set.seed(825)
week.open_model <- train(week1open~
                      week_1open + week_1high + week_1low + week_1close + week_1vol + week_1adj +
                      week_2open + week_2high + week_2low + week_2close + week_2vol + week_2adj +
                      week_3open + week_3high + week_3low + week_3close + week_3vol + week_3adj +
                      week_4open + week_4high + week_4low + week_4close + week_4vol + week_4adj, 
                    data = weekframe, 
                    method = "nnet", 
                    trControl = fitControl,
                    verbose = FALSE,
                    trace = FALSE)
week.open_model

week.high_model <- train(week1high~
                           week_1open + week_1high + week_1low + week_1close + week_1vol + week_1adj +
                           week_2open + week_2high + week_2low + week_2close + week_2vol + week_2adj +
                           week_3open + week_3high + week_3low + week_3close + week_3vol + week_3adj +
                           week_4open + week_4high + week_4low + week_4close + week_4vol + week_4adj, 
                         data = weekframe, 
                         method = "nnet", 
                         trControl = fitControl,
                         verbose = FALSE,
                         trace = FALSE)
week.high_model

week.low_model <- train(week1low~
                           week_1open + week_1high + week_1low + week_1close + week_1vol + week_1adj +
                           week_2open + week_2high + week_2low + week_2close + week_2vol + week_2adj +
                           week_3open + week_3high + week_3low + week_3close + week_3vol + week_3adj +
                           week_4open + week_4high + week_4low + week_4close + week_4vol + week_4adj, 
                         data = weekframe, 
                         method = "nnet", 
                         trControl = fitControl,
                         verbose = FALSE,
                         trace = FALSE)
week.low_model

week.close_model <- train(week1close~
                           week_1open + week_1high + week_1low + week_1close + week_1vol + week_1adj +
                           week_2open + week_2high + week_2low + week_2close + week_2vol + week_2adj +
                           week_3open + week_3high + week_3low + week_3close + week_3vol + week_3adj +
                           week_4open + week_4high + week_4low + week_4close + week_4vol + week_4adj, 
                         data = weekframe, 
                         method = "nnet", 
                         trControl = fitControl,
                         verbose = FALSE,
                         trace = FALSE)
week.close_model

week.vol_model <- train(week1vol~
                           week_1open + week_1high + week_1low + week_1close + week_1vol + week_1adj +
                           week_2open + week_2high + week_2low + week_2close + week_2vol + week_2adj +
                           week_3open + week_3high + week_3low + week_3close + week_3vol + week_3adj +
                           week_4open + week_4high + week_4low + week_4close + week_4vol + week_4adj, 
                         data = weekframe, 
                         method = "nnet", 
                         trControl = fitControl,
                         verbose = FALSE,
                         trace = FALSE)
week.vol_model

week.adj_model <- train(week1adj~
                           week_1open + week_1high + week_1low + week_1close + week_1vol + week_1adj +
                           week_2open + week_2high + week_2low + week_2close + week_2vol + week_2adj +
                           week_3open + week_3high + week_3low + week_3close + week_3vol + week_3adj +
                           week_4open + week_4high + week_4low + week_4close + week_4vol + week_4adj, 
                         data = weekframe, 
                         method = "nnet", 
                         trControl = fitControl,
                         verbose = FALSE,
                         trace = FALSE)
week.adj_model
}