library(tidyr)
library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(xgboost)

#LOAD DATA ----
setwd("~/Sloan Classes/2021 Spring/AE/Project")

Billboard <- read.csv("charts.csv")
SpotifyTracks <- read.csv("tracks.csv")

#CLEAN DATA ----

##Cleaning Billboard Data, specifically artist to match the artist in Spotify Data
#Separating Artist column based on separator words
separated <- separate(Billboard, artist, c("a", "b", "c", "d","e"), sep = "featuring|Featuring|and|And|&")
#remove spaces after
separated[c ("a", "b","c","d")] <- lapply(separated[c ("a", "b","c","d")], trimws)

#combine artist columns with commas between like Spotify data
combined <- Billboard
combined$artist <- paste0("'", separated$a, "', '", separated$b, "', '", separated$c, "', '", separated$d, "'")

#remove everything after NAs
combined$artist <-gsub("NA.*","",combined$artist)

#remove last quotations and commas
combined$artist = substr(combined$artist,1,nchar(combined$artist)-3)
combined$artist = paste0("[", combined$artist,"]")

#Rename columns to match Spotify data columns
colnames(combined)[3] <- "name"
colnames(combined)[4] <- "artists"

##Clean Spotify data by removing their keys
spotifyMinus <- SpotifyTracks[-c(1,7)]




#Merge Spotify information to billboard
merged <- inner_join(combined, spotifyMinus, by = c("name", "artists"))


#Merge songs based on max weeks.on.board
maxWeeks <- merge(aggregate(weeks.on.board ~ name, merged, max), merged)
maxWeeks <- unique(maxWeeks)
maxWeeks <- maxWeeks[!duplicated(maxWeeks[c("name", "artists")]),]

post2000 = filter(maxWeeks, maxWeeks$date>="2000-01-01")

##Cleanup for Spotify Hits ----

#spotifyminus track = spotify.hit.billboard = if(the song & artist are on combined assign 1 if not assign 0)

merged.clean.up.hit <-subset(merged, select = -c(peak.rank,last.week,weeks.on.board,popularity,rank)) #remove varibales
merged.clean.up.hit$billboard=1 #set billboard as 1
merged.clean.up.hit <- merge(aggregate(date ~ name, merged.clean.up.hit, min), merged.clean.up.hit) #only keep first row when it hit billboard
colnames(merged.clean.up.hit)[2] <- "billboard.date" #renamed date to billboard date
merged.clean.up.hit <- unique(merged.clean.up.hit) #remove dupes

##this isn't working
merged.clean.up.hit2 <- merge(merged.clean.up.hit, spotifyMinus, by=c("name","artists"))
maxWeeks <- merge(aggregate(weeks.on.board ~ name, merged, max), merged)
#up to here


#PREDICITING weeks.on.board----


df.weeks = subset(maxWeeks, select = -c(peak.rank,last.week,rank,popularity))

##1.TRAIN: 2000 to 2005 | TEST:2006 to 2007----
train.2000.to.2005 = filter(df.weeks, df.weeks$date>="2000-01-01", df.weeks$date<="2005-12-31")
test.2006.to.2007 = filter(df.weeks, df.weeks$date>="2006-01-01", df.weeks$date<="2007-12-31")

set.seed(216)
xgb.cv <-  train(weeks.on.board~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                      train.2000.to.2005,
                          method = "xgbTree", objective = "reg:squarederror",
                             trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2000.to.2005 <- xgboost(data = data.matrix(subset(train.2000.to.2005, select=-c(weeks.on.board))), label = train.2000.to.2005$weeks.on.board,
                              nrounds = xgb.cv$bestTune$nrounds, 
                              max_depth = xgb.cv$bestTune$max_depth, 
                              eta = xgb.cv$bestTune$eta, 
                              gamma = xgb.cv$bestTune$gamma, 
                              colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                              min_child_weight = xgb.cv$bestTune$min_child_weight, 
                              subsample = xgb.cv$bestTune$subsample)
                              
summary(mod.xgb.2000.to.2005)

xgb.pred.train = predict(mod.xgb.2000.to.2005, newdata=data.matrix(subset(train.2000.to.2005, select=-c(weeks.on.board))))
xgb.pred.test = predict(mod.xgb.2000.to.2005, newdata=data.matrix(subset(test.2006.to.2007, select=-c(weeks.on.board))))

y.train.2000.to.2005=train.2000.to.2005$weeks.on.board
y.mean.train.2000.to.2005 = mean(y.train.2000.to.2005)
y.test_2000to2005=test.2006.to.2007$weeks.on.board
y.mean.test_test_2006to2007= mean(y.test_2000to2005)

SSETrain = sum((xgb.pred.train - y.train.2000.to.2005)^2)
SSTTrain = sum((y.train.2000.to.2005 - y.mean.train.2000.to.2005)^2)
XGB.R2.2000.to.2005 = 1 - SSETrain/SSTTrain
XGB.R2.2000.to.2005

SSETest = sum((xgb.pred.test - y.test_2000to2005)^2)
SSTTest = sum((y.test_2000to2005 - y.mean.train.2000.to.2005)^2)
XGB.OSR2.2006.to.2007 = 1 - SSETest/SSTTest
XGB.OSR2.2006.to.2007

summary_r <- data.frame("2000.to.2005", XGB.R2.2000.to.2005, "2006.to.2007", XGB.OSR2.2006.to.2007,"weeks.on.board")   
colnames(summary_r) <- c('Test_Data','R2','Train_Data', 'OSR2','y')


##2.TRAIN: 2005 to 2010 | TEST:2011 to 2012----
train.2005.to.2010 = filter(df.weeks, df.weeks$date>="2005-01-01", df.weeks$date<="2010-12-31")
test.2011.to.2012 = filter(df.weeks, df.weeks$date>="2011-01-01", df.weeks$date<="2012-12-31")

set.seed(216)
xgb.cv <-  train(weeks.on.board~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2005.to.2010,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2005.to.2010 <- xgboost(data = data.matrix(subset(train.2005.to.2010, select=-c(weeks.on.board))), label = train.2005.to.2010$weeks.on.board,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2005.to.2010)

xgb.pred.train = predict(mod.xgb.2005.to.2010, newdata=data.matrix(subset(train.2005.to.2010, select=-c(weeks.on.board))))
xgb.pred.test = predict(mod.xgb.2005.to.2010, newdata=data.matrix(subset(test.2011.to.2012, select=-c(weeks.on.board))))

y.train.2005.to.2010=train.2005.to.2010$weeks.on.board
y.mean.train.2005.to.2010 = mean(y.train.2005.to.2010)
y.test_2005to2010=test.2011.to.2012$weeks.on.board
y.mean.test_test_2011to2012= mean(y.test_2005to2010)

SSETrain = sum((xgb.pred.train - y.train.2005.to.2010)^2)
SSTTrain = sum((y.train.2005.to.2010 - y.mean.train.2005.to.2010)^2)
XGB.R2.2005.to.2010 = 1 - SSETrain/SSTTrain
XGB.R2.2005.to.2010

SSETest = sum((xgb.pred.test - y.test_2005to2010)^2)
SSTTest = sum((y.test_2005to2010 - y.mean.train.2005.to.2010)^2)
XGB.OSR2.2011.to.2012 = 1 - SSETest/SSTTest
XGB.OSR2.2011.to.2012

summary_r <- summary_r%>% add_row(Test_Data = "2005.to.2010", 
                        R2 = XGB.R2.2005.to.2010,   
                          Train_Data = "2011.to.2012",   
                              OSR2 = XGB.OSR2.2011.to.2012,
                                y = "weeks.on.board")



##3.TRAIN: 2010 to 2015 | TEST:2016 to 2017----
train.2010.to.2015 = filter(df.weeks, df.weeks$date>="2010-01-01", df.weeks$date<="2015-12-31")
test.2016.to.2017 = filter(df.weeks, df.weeks$date>="2016-01-01", df.weeks$date<="2017-12-31")

set.seed(216)
xgb.cv <-  train(weeks.on.board~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2010.to.2015,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2010.to.2015 <- xgboost(data = data.matrix(subset(train.2010.to.2015, select=-c(weeks.on.board))), label = train.2010.to.2015$weeks.on.board,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2010.to.2015)

xgb.pred.train = predict(mod.xgb.2010.to.2015, newdata=data.matrix(subset(train.2010.to.2015, select=-c(weeks.on.board))))
xgb.pred.test = predict(mod.xgb.2010.to.2015, newdata=data.matrix(subset(test.2016.to.2017, select=-c(weeks.on.board))))

y.train.2010.to.2015=train.2010.to.2015$weeks.on.board
y.mean.train.2010.to.2015 = mean(y.train.2010.to.2015)
y.test_2010to2015=test.2016.to.2017$weeks.on.board
y.mean.test_test_2016to2017= mean(y.test_2010to2015)

SSETrain = sum((xgb.pred.train - y.train.2010.to.2015)^2)
SSTTrain = sum((y.train.2010.to.2015 - y.mean.train.2010.to.2015)^2)
XGB.R2.2010.to.2015 = 1 - SSETrain/SSTTrain
XGB.R2.2010.to.2015

SSETest = sum((xgb.pred.test - y.test_2010to2015)^2)
SSTTest = sum((y.test_2010to2015 - y.mean.train.2010.to.2015)^2)
XGB.OSR2.2016.to.2017 = 1 - SSETest/SSTTest
XGB.OSR2.2016.to.2017

summary_r <- summary_r %>% add_row(Test_Data = "2010.to.2015", 
                      R2 = XGB.R2.2010.to.2015,   
                      Train_Data = "2016.to.2017",   
                      OSR2 = XGB.OSR2.2016.to.2017,
                      y = "weeks.on.board")





##4.TRAIN: 2015 to 2019 | TEST:2020 to 2021----
train.2015.to.2019 = filter(df.weeks, df.weeks$date>="2015-01-01", df.weeks$date<="2019-12-31")
test.2020.to.2021 = filter(df.weeks, df.weeks$date>="2020-01-01", df.weeks$date<="2021-12-31")

set.seed(216)
xgb.cv <-  train(weeks.on.board~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2015.to.2019,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2015.to.2019 <- xgboost(data = data.matrix(subset(train.2015.to.2019, select=-c(weeks.on.board))), label = train.2015.to.2019$weeks.on.board,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2015.to.2019)

xgb.pred.train = predict(mod.xgb.2015.to.2019, newdata=data.matrix(subset(train.2015.to.2019, select=-c(weeks.on.board))))
xgb.pred.test = predict(mod.xgb.2015.to.2019, newdata=data.matrix(subset(test.2020.to.2021, select=-c(weeks.on.board))))

y.train.2015.to.2019=train.2015.to.2019$weeks.on.board
y.mean.train.2015.to.2019 = mean(y.train.2015.to.2019)
y.test_2015to2019=test.2020.to.2021$weeks.on.board
y.mean.test_test_2020to2021= mean(y.test_2015to2019)

SSETrain = sum((xgb.pred.train - y.train.2015.to.2019)^2)
SSTTrain = sum((y.train.2015.to.2019 - y.mean.train.2015.to.2019)^2)
XGB.R2.2015.to.2019 = 1 - SSETrain/SSTTrain
XGB.R2.2015.to.2019

SSETest = sum((xgb.pred.test - y.test_2015to2019)^2)
SSTTest = sum((y.test_2015to2019 - y.mean.train.2015.to.2019)^2)
XGB.OSR2.2020.to.2021 = 1 - SSETest/SSTTest
XGB.OSR2.2020.to.2021

summary_r <- summary_r %>% add_row(Test_Data = "2015.to.2019", 
                                   R2 = XGB.R2.2015.to.2019,   
                                   Train_Data = "2020.to.2021",   
                                   OSR2 = XGB.OSR2.2020.to.2021,
                                   y = "weeks.on.board")



##5.TRAIN: 2014 to 2019 | TEST:2020 to 2021----
train.2014.to.2019 = filter(df.weeks, df.weeks$date>="2014-01-01", df.weeks$date<="2019-12-31")
test.2020.to.2021 = filter(df.weeks, df.weeks$date>="2020-01-01", df.weeks$date<="2021-12-31")

set.seed(216)
xgb.cv <-  train(weeks.on.board~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2014.to.2019,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2014.to.2019 <- xgboost(data = data.matrix(subset(train.2014.to.2019, select=-c(weeks.on.board))), label = train.2014.to.2019$weeks.on.board,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2014.to.2019)

xgb.pred.train = predict(mod.xgb.2014.to.2019, newdata=data.matrix(subset(train.2014.to.2019, select=-c(weeks.on.board))))
xgb.pred.test = predict(mod.xgb.2014.to.2019, newdata=data.matrix(subset(test.2020.to.2021, select=-c(weeks.on.board))))

y.train.2014.to.2019=train.2014.to.2019$weeks.on.board
y.mean.train.2014.to.2019 = mean(y.train.2014.to.2019)
y.test_2014to2019=test.2020.to.2021$weeks.on.board
y.mean.test_test_2020to2021= mean(y.test_2014to2019)

SSETrain = sum((xgb.pred.train - y.train.2014.to.2019)^2)
SSTTrain = sum((y.train.2014.to.2019 - y.mean.train.2014.to.2019)^2)
XGB.R2.2014.to.2019 = 1 - SSETrain/SSTTrain
XGB.R2.2014.to.2019

SSETest = sum((xgb.pred.test - y.test_2014to2019)^2)
SSTTest = sum((y.test_2014to2019 - y.mean.train.2014.to.2019)^2)
XGB.OSR2.2020.to.2021 = 1 - SSETest/SSTTest
XGB.OSR2.2020.to.2021

summary_r <- summary_r %>% add_row(Test_Data = "2014.to.2019", 
                                   R2 = XGB.R2.2014.to.2019,   
                                   Train_Data = "2020.to.2021",   
                                   OSR2 = XGB.OSR2.2020.to.2021,
                                   y = "weeks.on.board")




##6.TRAIN: 2013 to 2018 | TEST:2019 to 2021----
train.2013.to.2018 = filter(df.weeks, df.weeks$date>="2013-01-01", df.weeks$date<="2018-12-31")
test.2019.to.2021 = filter(df.weeks, df.weeks$date>="2019-01-01", df.weeks$date<="2021-12-31")

set.seed(216)
xgb.cv <-  train(weeks.on.board~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2013.to.2018,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2013.to.2018 <- xgboost(data = data.matrix(subset(train.2013.to.2018, select=-c(weeks.on.board))), label = train.2013.to.2018$weeks.on.board,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2013.to.2018)

xgb.pred.train = predict(mod.xgb.2013.to.2018, newdata=data.matrix(subset(train.2013.to.2018, select=-c(weeks.on.board))))
xgb.pred.test = predict(mod.xgb.2013.to.2018, newdata=data.matrix(subset(test.2019.to.2021, select=-c(weeks.on.board))))

y.train.2013.to.2018=train.2013.to.2018$weeks.on.board
y.mean.train.2013.to.2018 = mean(y.train.2013.to.2018)
y.test_2013to2018=test.2019.to.2021$weeks.on.board
y.mean.test_test_2019to2021= mean(y.test_2013to2018)

SSETrain = sum((xgb.pred.train - y.train.2013.to.2018)^2)
SSTTrain = sum((y.train.2013.to.2018 - y.mean.train.2013.to.2018)^2)
XGB.R2.2013.to.2018 = 1 - SSETrain/SSTTrain
XGB.R2.2013.to.2018

SSETest = sum((xgb.pred.test - y.test_2013to2018)^2)
SSTTest = sum((y.test_2013to2018 - y.mean.train.2013.to.2018)^2)
XGB.OSR2.2019.to.2021 = 1 - SSETest/SSTTest
XGB.OSR2.2019.to.2021

summary_r <- summary_r %>% add_row(Test_Data = "2013.to.2018", 
                                   R2 = XGB.R2.2013.to.2018,   
                                   Train_Data = "2019.to.2021",   
                                   OSR2 = XGB.OSR2.2019.to.2021,
                                   y = "weeks.on.board")





##7.TRAIN: 2000 to 2018 | TEST:2019 to 2021----
train.2000.to.2018 = filter(df.weeks, df.weeks$date>="2000-01-01", df.weeks$date<="2018-12-31")
test.2019.to.2021 = filter(df.weeks, df.weeks$date>="2019-01-01", df.weeks$date<="2021-12-31")

set.seed(216)
xgb.cv <-  train(weeks.on.board~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2000.to.2018,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2000.to.2018 <- xgboost(data = data.matrix(subset(train.2000.to.2018, select=-c(weeks.on.board))), label = train.2000.to.2018$weeks.on.board,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2000.to.2018)

xgb.pred.train = predict(mod.xgb.2000.to.2018, newdata=data.matrix(subset(train.2000.to.2018, select=-c(weeks.on.board))))
xgb.pred.test = predict(mod.xgb.2000.to.2018, newdata=data.matrix(subset(test.2019.to.2021, select=-c(weeks.on.board))))

y.train.2000.to.2018=train.2000.to.2018$weeks.on.board
y.mean.train.2000.to.2018 = mean(y.train.2000.to.2018)
y.test_2000to2018=test.2019.to.2021$weeks.on.board
y.mean.test_test_2019to2021= mean(y.test_2000to2018)

SSETrain = sum((xgb.pred.train - y.train.2000.to.2018)^2)
SSTTrain = sum((y.train.2000.to.2018 - y.mean.train.2000.to.2018)^2)
XGB.R2.2000.to.2018 = 1 - SSETrain/SSTTrain
XGB.R2.2000.to.2018

SSETest = sum((xgb.pred.test - y.test_2000to2018)^2)
SSTTest = sum((y.test_2000to2018 - y.mean.train.2000.to.2018)^2)
XGB.OSR2.2019.to.2021 = 1 - SSETest/SSTTest
XGB.OSR2.2019.to.2021

summary_r <- summary_r %>% add_row(Test_Data = "2000.to.2018", 
                                   R2 = XGB.R2.2000.to.2018,   
                                   Train_Data = "2019.to.2021",   
                                   OSR2 = XGB.OSR2.2019.to.2021,
                                   y = "weeks.on.board")


#PREDICITING peak.rank ----

df.peak = subset(maxWeeks, select = -c(weeks.on.board,last.week,rank,popularity))

## 1.TRAIN: 2000 to 2005 | TEST:2006 to 2007 ----
train.2000.to.2005 = filter(df.peak, df.peak$date>="2000-01-01", df.peak$date<="2005-12-31")
test.2006.to.2007 = filter(df.peak, df.peak$date>="2006-01-01", df.peak$date<="2007-12-31")

set.seed(216)
xgb.cv <-  train(peak.rank~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2000.to.2005,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2000.to.2005 <- xgboost(data = data.matrix(subset(train.2000.to.2005, select=-c(peak.rank))), label = train.2000.to.2005$peak.rank,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2000.to.2005)

xgb.pred.train = predict(mod.xgb.2000.to.2005, newdata=data.matrix(subset(train.2000.to.2005, select=-c(peak.rank))))
xgb.pred.test = predict(mod.xgb.2000.to.2005, newdata=data.matrix(subset(test.2006.to.2007, select=-c(peak.rank))))

y.train.2000.to.2005=train.2000.to.2005$peak.rank
y.mean.train.2000.to.2005 = mean(y.train.2000.to.2005)
y.test_2000to2005=test.2006.to.2007$peak.rank
y.mean.test_test_2006to2007= mean(y.test_2000to2005)

SSETrain = sum((xgb.pred.train - y.train.2000.to.2005)^2)
SSTTrain = sum((y.train.2000.to.2005 - y.mean.train.2000.to.2005)^2)
XGB.R2.2000.to.2005 = 1 - SSETrain/SSTTrain
XGB.R2.2000.to.2005

SSETest = sum((xgb.pred.test - y.test_2000to2005)^2)
SSTTest = sum((y.test_2000to2005 - y.mean.train.2000.to.2005)^2)
XGB.OSR2.2006.to.2007 = 1 - SSETest/SSTTest
XGB.OSR2.2006.to.2007

summary_r <- summary_r %>% add_row(Test_Data = "2000.to.2005", 
                                   R2 = XGB.R2.2000.to.2005,   
                                   Train_Data = "2006.to.2007",   
                                   OSR2 = XGB.OSR2.2006.to.2007,
                                   y = "peak.rank")


## 2.TRAIN: 2005 to 2010 | TEST:2011 to 2012----
train.2005.to.2010 = filter(df.peak, df.peak$date>="2005-01-01", df.peak$date<="2010-12-31")
test.2011.to.2012 = filter(df.peak, df.peak$date>="2011-01-01", df.peak$date<="2012-12-31")

set.seed(216)
xgb.cv <-  train(peak.rank~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2005.to.2010,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2005.to.2010 <- xgboost(data = data.matrix(subset(train.2005.to.2010, select=-c(peak.rank))), label = train.2005.to.2010$peak.rank,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2005.to.2010)

xgb.pred.train = predict(mod.xgb.2005.to.2010, newdata=data.matrix(subset(train.2005.to.2010, select=-c(peak.rank))))
xgb.pred.test = predict(mod.xgb.2005.to.2010, newdata=data.matrix(subset(test.2011.to.2012, select=-c(peak.rank))))

y.train.2005.to.2010=train.2005.to.2010$peak.rank
y.mean.train.2005.to.2010 = mean(y.train.2005.to.2010)
y.test_2005to2010=test.2011.to.2012$peak.rank
y.mean.test_test_2011to2012= mean(y.test_2005to2010)

SSETrain = sum((xgb.pred.train - y.train.2005.to.2010)^2)
SSTTrain = sum((y.train.2005.to.2010 - y.mean.train.2005.to.2010)^2)
XGB.R2.2005.to.2010 = 1 - SSETrain/SSTTrain
XGB.R2.2005.to.2010

SSETest = sum((xgb.pred.test - y.test_2005to2010)^2)
SSTTest = sum((y.test_2005to2010 - y.mean.train.2005.to.2010)^2)
XGB.OSR2.2011.to.2012 = 1 - SSETest/SSTTest
XGB.OSR2.2011.to.2012

summary_r <- summary_r%>% add_row(Test_Data = "2005.to.2010", 
                                  R2 = XGB.R2.2005.to.2010,   
                                  Train_Data = "2011.to.2012",   
                                  OSR2 = XGB.OSR2.2011.to.2012,
                                  y = "peak.rank")



##3.TRAIN: 2010 to 2015 | TEST:2016 to 2017----
train.2010.to.2015 = filter(df.peak, df.peak$date>="2010-01-01", df.peak$date<="2015-12-31")
test.2016.to.2017 = filter(df.peak, df.peak$date>="2016-01-01", df.peak$date<="2017-12-31")

set.seed(216)
xgb.cv <-  train(peak.rank~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2010.to.2015,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2010.to.2015 <- xgboost(data = data.matrix(subset(train.2010.to.2015, select=-c(peak.rank))), label = train.2010.to.2015$peak.rank,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2010.to.2015)

xgb.pred.train = predict(mod.xgb.2010.to.2015, newdata=data.matrix(subset(train.2010.to.2015, select=-c(peak.rank))))
xgb.pred.test = predict(mod.xgb.2010.to.2015, newdata=data.matrix(subset(test.2016.to.2017, select=-c(peak.rank))))

y.train.2010.to.2015=train.2010.to.2015$peak.rank
y.mean.train.2010.to.2015 = mean(y.train.2010.to.2015)
y.test_2010to2015=test.2016.to.2017$peak.rank
y.mean.test_test_2016to2017= mean(y.test_2010to2015)

SSETrain = sum((xgb.pred.train - y.train.2010.to.2015)^2)
SSTTrain = sum((y.train.2010.to.2015 - y.mean.train.2010.to.2015)^2)
XGB.R2.2010.to.2015 = 1 - SSETrain/SSTTrain
XGB.R2.2010.to.2015

SSETest = sum((xgb.pred.test - y.test_2010to2015)^2)
SSTTest = sum((y.test_2010to2015 - y.mean.train.2010.to.2015)^2)
XGB.OSR2.2016.to.2017 = 1 - SSETest/SSTTest
XGB.OSR2.2016.to.2017

summary_r <- summary_r %>% add_row(Test_Data = "2010.to.2015", 
                                   R2 = XGB.R2.2010.to.2015,   
                                   Train_Data = "2016.to.2017",   
                                   OSR2 = XGB.OSR2.2016.to.2017,
                                   y = "peak.rank")





##4.TRAIN: 2015 to 2019 | TEST:2020 to 2021----
train.2015.to.2019 = filter(df.peak, df.peak$date>="2015-01-01", df.peak$date<="2019-12-31")
test.2020.to.2021 = filter(df.peak, df.peak$date>="2020-01-01", df.peak$date<="2021-12-31")

set.seed(216)
xgb.cv <-  train(peak.rank~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2015.to.2019,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2015.to.2019 <- xgboost(data = data.matrix(subset(train.2015.to.2019, select=-c(peak.rank))), label = train.2015.to.2019$peak.rank,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2015.to.2019)

xgb.pred.train = predict(mod.xgb.2015.to.2019, newdata=data.matrix(subset(train.2015.to.2019, select=-c(peak.rank))))
xgb.pred.test = predict(mod.xgb.2015.to.2019, newdata=data.matrix(subset(test.2020.to.2021, select=-c(peak.rank))))

y.train.2015.to.2019=train.2015.to.2019$peak.rank
y.mean.train.2015.to.2019 = mean(y.train.2015.to.2019)
y.test_2015to2019=test.2020.to.2021$peak.rank
y.mean.test_test_2020to2021= mean(y.test_2015to2019)

SSETrain = sum((xgb.pred.train - y.train.2015.to.2019)^2)
SSTTrain = sum((y.train.2015.to.2019 - y.mean.train.2015.to.2019)^2)
XGB.R2.2015.to.2019 = 1 - SSETrain/SSTTrain
XGB.R2.2015.to.2019

SSETest = sum((xgb.pred.test - y.test_2015to2019)^2)
SSTTest = sum((y.test_2015to2019 - y.mean.train.2015.to.2019)^2)
XGB.OSR2.2020.to.2021 = 1 - SSETest/SSTTest
XGB.OSR2.2020.to.2021

summary_r <- summary_r %>% add_row(Test_Data = "2015.to.2019", 
                                   R2 = XGB.R2.2015.to.2019,   
                                   Train_Data = "2020.to.2021",   
                                   OSR2 = XGB.OSR2.2020.to.2021,
                                   y = "peak.rank")



##5.TRAIN: 2014 to 2019 | TEST:2020 to 2021----
train.2014.to.2019 = filter(df.peak, df.peak$date>="2014-01-01", df.peak$date<="2019-12-31")
test.2020.to.2021 = filter(df.peak, df.peak$date>="2020-01-01", df.peak$date<="2021-12-31")

set.seed(216)
xgb.cv <-  train(peak.rank~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2014.to.2019,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2014.to.2019 <- xgboost(data = data.matrix(subset(train.2014.to.2019, select=-c(peak.rank))), label = train.2014.to.2019$peak.rank,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2014.to.2019)

xgb.pred.train = predict(mod.xgb.2014.to.2019, newdata=data.matrix(subset(train.2014.to.2019, select=-c(peak.rank))))
xgb.pred.test = predict(mod.xgb.2014.to.2019, newdata=data.matrix(subset(test.2020.to.2021, select=-c(peak.rank))))

y.train.2014.to.2019=train.2014.to.2019$peak.rank
y.mean.train.2014.to.2019 = mean(y.train.2014.to.2019)
y.test_2014to2019=test.2020.to.2021$peak.rank
y.mean.test_test_2020to2021= mean(y.test_2014to2019)

SSETrain = sum((xgb.pred.train - y.train.2014.to.2019)^2)
SSTTrain = sum((y.train.2014.to.2019 - y.mean.train.2014.to.2019)^2)
XGB.R2.2014.to.2019 = 1 - SSETrain/SSTTrain
XGB.R2.2014.to.2019

SSETest = sum((xgb.pred.test - y.test_2014to2019)^2)
SSTTest = sum((y.test_2014to2019 - y.mean.train.2014.to.2019)^2)
XGB.OSR2.2020.to.2021 = 1 - SSETest/SSTTest
XGB.OSR2.2020.to.2021

summary_r <- summary_r %>% add_row(Test_Data = "2014.to.2019", 
                                   R2 = XGB.R2.2014.to.2019,   
                                   Train_Data = "2020.to.2021",   
                                   OSR2 = XGB.OSR2.2020.to.2021,
                                   y = "peak.rank")




##6.TRAIN: 2013 to 2018 | TEST:2019 to 2021----
train.2013.to.2018 = filter(df.peak, df.peak$date>="2013-01-01", df.peak$date<="2018-12-31")
test.2019.to.2021 = filter(df.peak, df.peak$date>="2019-01-01", df.peak$date<="2021-12-31")

set.seed(216)
xgb.cv <-  train(peak.rank~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2013.to.2018,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2013.to.2018 <- xgboost(data = data.matrix(subset(train.2013.to.2018, select=-c(peak.rank))), label = train.2013.to.2018$peak.rank,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2013.to.2018)

xgb.pred.train = predict(mod.xgb.2013.to.2018, newdata=data.matrix(subset(train.2013.to.2018, select=-c(peak.rank))))
xgb.pred.test = predict(mod.xgb.2013.to.2018, newdata=data.matrix(subset(test.2019.to.2021, select=-c(peak.rank))))

y.train.2013.to.2018=train.2013.to.2018$peak.rank
y.mean.train.2013.to.2018 = mean(y.train.2013.to.2018)
y.test_2013to2018=test.2019.to.2021$peak.rank
y.mean.test_test_2019to2021= mean(y.test_2013to2018)

SSETrain = sum((xgb.pred.train - y.train.2013.to.2018)^2)
SSTTrain = sum((y.train.2013.to.2018 - y.mean.train.2013.to.2018)^2)
XGB.R2.2013.to.2018 = 1 - SSETrain/SSTTrain
XGB.R2.2013.to.2018

SSETest = sum((xgb.pred.test - y.test_2013to2018)^2)
SSTTest = sum((y.test_2013to2018 - y.mean.train.2013.to.2018)^2)
XGB.OSR2.2019.to.2021 = 1 - SSETest/SSTTest
XGB.OSR2.2019.to.2021

summary_r <- summary_r %>% add_row(Test_Data = "2013.to.2018", 
                                   R2 = XGB.R2.2013.to.2018,   
                                   Train_Data = "2019.to.2021",   
                                   OSR2 = XGB.OSR2.2019.to.2021,
                                   y = "peak.rank")





##7.TRAIN: 2000 to 2018 | TEST:2019 to 2021----
train.2000.to.2018 = filter(df.peak, df.peak$date>="2000-01-01", df.peak$date<="2018-12-31")
test.2019.to.2021 = filter(df.peak, df.peak$date>="2019-01-01", df.peak$date<="2021-12-31")

set.seed(216)
xgb.cv <-  train(peak.rank~ 
                   danceability+
                   duration_ms+
                   explicit+
                   energy+
                   key+
                   loudness+
                   mode+
                   speechiness+
                   acousticness+
                   instrumentalness+
                   liveness+
                   valence+
                   tempo+
                   time_signature,
                 train.2000.to.2018,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=5))



xgb.cv$results
xgb.cv$bestTune

mod.xgb.2000.to.2018 <- xgboost(data = data.matrix(subset(train.2000.to.2018, select=-c(peak.rank))), label = train.2000.to.2018$peak.rank,
                                nrounds = xgb.cv$bestTune$nrounds, 
                                max_depth = xgb.cv$bestTune$max_depth, 
                                eta = xgb.cv$bestTune$eta, 
                                gamma = xgb.cv$bestTune$gamma, 
                                colsample_bytree = xgb.cv$bestTune$colsample_bytree, 
                                min_child_weight = xgb.cv$bestTune$min_child_weight, 
                                subsample = xgb.cv$bestTune$subsample)

summary(mod.xgb.2000.to.2018)

xgb.pred.train = predict(mod.xgb.2000.to.2018, newdata=data.matrix(subset(train.2000.to.2018, select=-c(peak.rank))))
xgb.pred.test = predict(mod.xgb.2000.to.2018, newdata=data.matrix(subset(test.2019.to.2021, select=-c(peak.rank))))

y.train.2000.to.2018=train.2000.to.2018$peak.rank
y.mean.train.2000.to.2018 = mean(y.train.2000.to.2018)
y.test_2000to2018=test.2019.to.2021$peak.rank
y.mean.test_test_2019to2021= mean(y.test_2000to2018)

SSETrain = sum((xgb.pred.train - y.train.2000.to.2018)^2)
SSTTrain = sum((y.train.2000.to.2018 - y.mean.train.2000.to.2018)^2)
XGB.R2.2000.to.2018 = 1 - SSETrain/SSTTrain
XGB.R2.2000.to.2018

SSETest = sum((xgb.pred.test - y.test_2000to2018)^2)
SSTTest = sum((y.test_2000to2018 - y.mean.train.2000.to.2018)^2)
XGB.OSR2.2019.to.2021 = 1 - SSETest/SSTTest
XGB.OSR2.2019.to.2021

summary_r <- summary_r %>% add_row(Test_Data = "2000.to.2018", 
                                   R2 = XGB.R2.2000.to.2018,   
                                   Train_Data = "2019.to.2021",   
                                   OSR2 = XGB.OSR2.2019.to.2021,
                                   y = "peak.rank")





###############EXTRA CODE ----

#mod.xgb.2000.to.2018.final.model <- mod.xgb.2000.to.2018$finalModel #We retrieve the best model!

mat <- xgb.importance (model = mod.xgb.2000.to.2018)
xgb.plot.importance (importance_matrix = mat) 
mat$Importance%>%sum()#This sums to 1. So the Gain column gives us the the relative importance
