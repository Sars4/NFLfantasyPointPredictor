# Change to where you have it saved
setwd('C:\\Users\\logan\\Desktop\\fantasyFootballPred')


# this function will quickly assemble the data into our desired format by intelligently
# merging 2 data tables and then dropping certain columns
getdata <- function(year, prediction=FALSE, positive_y=FALSE) {
  yr0rush <- read.csv(paste(year,"rush.csv", sep = ""),stringsAsFactors=FALSE, sep=",")
  yr0rec <- read.csv(paste(year,"rec.csv", sep = ""),stringsAsFactors=FALSE, sep=",")
  if(!prediction){
    year = year + 1
    yr1rush <- read.csv(paste(year,"rush.csv", sep = ""),stringsAsFactors=FALSE, sep=",")
    yr1rec <- read.csv(paste(year,"rec.csv", sep = ""),stringsAsFactors=FALSE, sep=",")
  }
  
  
  n = nrow(yr0rush)
  
  Tgt <- rep(0,n)
  Rec <- rep(0,n)
  Ctch. <- rep(0,n)
  RecYds <- rep(0,n)
  Y.R <- rep(NA,n)
  RecTD <- rep(0,n)
  Rec1D <- rep(0,n)
  Y.Tgt <- rep(NA,n)
  R.G <- rep(0,n)
  RecY.G <- rep(0,n)
  yr0fPts <- rep(0,n)
  if(!prediction){
    yr1fPts <- rep(0,n)
    g <- rep(0,n)
  }
  
  if(!prediction){
    data = cbind(yr0rush, Tgt, Rec, Ctch., RecYds, Y.R, RecTD, Rec1D, Y.Tgt, R.G, RecY.G, yr0fPts, g, yr1fPts)
  } else {
    data = cbind(yr0rush, Tgt, Rec, Ctch., RecYds, Y.R, RecTD, Rec1D, Y.Tgt, R.G, RecY.G, yr0fPts)
  }
  
  for (i in 1:n) {
    tag = data[i,16]
    
    #fill in recieving data
    rowNum = which(yr0rec$Tag == tag)
    if (length(rowNum)) {
      data[i,17] = yr0rec[rowNum, 8]
      data[i,18] = yr0rec[rowNum, 9]
      data[i,19] = yr0rec[rowNum, 10]
      data[i,20] = yr0rec[rowNum, 11]
      data[i,21] = yr0rec[rowNum, 12]
      data[i,22] = yr0rec[rowNum, 13]
      data[i,23] = yr0rec[rowNum, 14]
      data[i,24] = yr0rec[rowNum, 16]
      data[i,25] = yr0rec[rowNum, 17]
      data[i,26] = yr0rec[rowNum, 18]
    }
    
    #fill in yr0 fantasy data
    data[i,27] = data[i,9]/10 + data[i,10]*6 - data[i,15]*1.2 + data[i,18] + data[i,20]/10 + data[i,22]*6
    
    #fill in yr1 fantasy data and games
    if (!prediction){
      points = 0
      rowNum = which(yr1rush$Tag == tag)
      if (length(rowNum)) {
        data[i,28] = yr1rush[rowNum, 6]
        points = points + yr1rush[rowNum,9]/10 + yr1rush[rowNum,10]*6 - yr1rush[rowNum,15]*1.2
        
      }
      rowNum = which(yr1rec$Tag == tag)
      if (length(rowNum)) {
        points = points + yr1rec[rowNum,9] + yr1rec[rowNum,11]/10 + yr1rec[rowNum,13]*6
      }
      data[i,29] = points
    }
    
  }
  
  #drop anyone who's not an RB
  data = subset(data,Pos=='RB')
  
  #drop anyone who didn't do anything the next year
  if (!prediction) {
    data = subset(data,g!="0") # how does this work with or without the quotes
  }
  
  #drop unnecessary columns
  if (!prediction) {
    data = subset(data, select = -c(Rk,Pos,Lng,Tag,g))
  } else {
    data = subset(data, select = -c(Rk,Pos,Lng,Tag))
  }
  
  #erase percentage signs
  data$Ctch. <- gsub("\\%","",data$Ctch.)
  
  #convert strings to ints
  for (i in 3:23) {
    data[,i]<-as.numeric(data[,i])
  }
  if(!prediction){
    data[,24]<-as.numeric(data[,24])
  }
  
  #convert NA to 0
  data[is.na(data)] = 0
  
  #drop non-positive y
  if (!prediction && positive_y) {
    data = subset(data,yr1fPts>0)
  }
  
  return(data)
  
}



# this method will train a regression model and return the weight vector
train <- function(X, y) {
  X <- as.matrix(cbind(rep(1,nrow(X)),X))
  y <- as.vector(y)
  a = solve(t(X) %*% X, t(X) %*% y, tol = 1e-23)
  return(a)
}


# this method will compute the train and test error
error <- function(xTrain, yTrain, xTest, yTest) {
  a = train(xTrain, yTrain)
  
  X <- as.matrix(cbind(rep(1,nrow(xTrain)),xTrain))
  y <- as.vector(yTrain)
  
  yhat = X %*% a
  error = y - yhat
  sse = sum(error*error)
  trainMSE = sse/length(error)
  
  X <- as.matrix(cbind(rep(1,nrow(xTest)),xTest))
  y <- as.vector(yTest)
  
  yhat = X %*% a
  error = y - yhat
  sse = sum(error*error)
  testMSE = sse/length(error)
  
  return(c(trainMSE, testMSE))
}



# this method will return a table displaying the training error and the testing
# error resulting from leaving each attribute out
attribute_table <- function(xTrain, yTrain, xTest, yTest) {
  
  Attribute <- colnames(xTrain)
  trainMSE <- rep(0,ncol(xTrain))
  testMSE <- rep(0,ncol(xTrain))
  
  for (i in 1:ncol(xTrain)){
    vec = error(xTrain[,-i], yTrain, xTest[,-i], yTest)
    trainMSE[i] = vec[1]
    testMSE[i] = vec[2]
  }
  
  df <- data.frame(Attribute, trainMSE, testMSE)
  vec = error(xTrain, yTrain, xTest, yTest)
  df <- rbind(data.frame(Attribute = "None", trainMSE = vec[1], testMSE = vec[2]), df)
  
  return(df)
  
}















# --------------------------------------------------------------------------- #
# here is a naive first attempt at the model.  We do not normalize the data
# or add any seasonal/polynomial/logarithmic transformations


# create our train, test, and predict data
d2011 = getdata(2011)
d2012 = getdata(2012)
d2013 = getdata(2013)
d2014 = getdata(2014)
d2015 = getdata(2015)
d2016 = getdata(2016)
d2017 = getdata(2017)
d2018 = getdata(2018)
d2019 = getdata(2019)
d2020 = getdata(2020)
d2021 = getdata(2021, prediction = TRUE)


Train = rbind(d2011, d2012, d2013, d2014, d2015, d2016, d2017, d2018)
Test = rbind(d2019, d2020)

xTrain = Train[,3:23]
yTrain = Train[,24]
xTest = Test[,3:23]
yTest = Test[,24]
xPredict = d2021[,3:23]

a = train(xTrain, yTrain)
a # this is the solution to the normal equation
table = attribute_table(xTrain[,-21], yTrain, xTest[,-21], yTest)
# drop yr0fPts since it's a linear combination of 6 other attributes
# otherwise, this process will deem all 7 columns irrelevant
table[rev(order(table$trainMSE)),]# table of errors using leave-one-out regression, sorted by Train MSE
table[rev(order(table$testMSE)),]# table of errors using leave-one-out regression, sorted by Test MSE


#predicting
X=as.matrix(cbind(rep(1,nrow(xPredict)),xPredict))
yHat = X %*% a
prediction = cbind(d2021[,1], yHat)
colnames(prediction) <- c("Name", "ProjPts")
prediction = as.data.frame(prediction)
prediction[,2]<-as.numeric(prediction[,2])
prediction[rev(order(prediction$ProjPts)),] #prediction on 2021 data


# --------------------------------------------------------------------------- #
# here is a second attempt with a min-max normalization on X and a logarithmic
# transformation on y




# create our train, test, and predict data
d2011 = getdata(2011, positive_y=TRUE)
d2012 = getdata(2012, positive_y=TRUE)
d2013 = getdata(2013, positive_y=TRUE)
d2014 = getdata(2014, positive_y=TRUE)
d2015 = getdata(2015, positive_y=TRUE)
d2016 = getdata(2016, positive_y=TRUE)
d2017 = getdata(2017, positive_y=TRUE)
d2018 = getdata(2018, positive_y=TRUE)
d2019 = getdata(2019, positive_y=TRUE)
d2020 = getdata(2020, positive_y=TRUE)
d2021 = getdata(2021, prediction = TRUE)


Train = rbind(d2011, d2012, d2013, d2014, d2015, d2016, d2017, d2018)
Test = rbind(d2019, d2020)

xTrain = Train[,3:23]
yTrain = Train[,24]
xTest = Test[,3:23]
yTest = Test[,24]
xPredict = d2021[,3:23]


#normalize
constants = matrix(rep(0,42), nrow=21)
X = rbind(xTrain, xTest, xPredict)
for (i in 1:21){
  constants[i,] = c(min(X[,i]),max(X[,i]))
}

minMax <- function(df) {   # normalize
  for (i in 1:21) {
    min = constants[i,1]
    max = constants[i,2]
    df[,i] = (df[,i] - min) / (max - min)
  }
  return(df)
}

xTrain <- minMax(xTrain)
xTest <- minMax(xTest)
xPredict <- minMax(xPredict)



a = train(xTrain, log(yTrain))
a # this is the solution to the normal equation
table = attribute_table(xTrain[,-21], log(yTrain), xTest[,-21], log(yTest))
# drop yr0fPts since it's a linear combination of 6 other attributes
# otherwise, this process will deem all 7 columns irrelevant
table[rev(order(table$trainMSE)),]# table of errors using leave-one-out regression, sorted by Train MSE
table[rev(order(table$testMSE)),]# table of errors using leave-one-out regression, sorted by Test MSE


#predicting
X=as.matrix(cbind(rep(1,nrow(xPredict)),xPredict))
yHat = X %*% a
prediction = cbind(d2021[,1], exp(yHat))
colnames(prediction) <- c("Name", "ProjPts")
prediction = as.data.frame(prediction)
prediction[,2]<-as.numeric(prediction[,2])
prediction[rev(order(prediction$ProjPts)),] #prediction on 2021 data





# --------------------------------------------------------------------------- #
# here is a third attempt with a standardization on X and a
# logarithmic transformation on y




# create our train, test, and predict data
d2011 = getdata(2011, positive_y=TRUE)
d2012 = getdata(2012, positive_y=TRUE)
d2013 = getdata(2013, positive_y=TRUE)
d2014 = getdata(2014, positive_y=TRUE)
d2015 = getdata(2015, positive_y=TRUE)
d2016 = getdata(2016, positive_y=TRUE)
d2017 = getdata(2017, positive_y=TRUE)
d2018 = getdata(2018, positive_y=TRUE)
d2019 = getdata(2019, positive_y=TRUE)
d2020 = getdata(2020, positive_y=TRUE)
d2021 = getdata(2021, prediction = TRUE)


Train = rbind(d2011, d2012, d2013, d2014, d2015, d2016, d2017, d2018)
Test = rbind(d2019, d2020)

xTrain = Train[,3:23]
yTrain = Train[,24]
xTest = Test[,3:23]
yTest = Test[,24]
xPredict = d2021[,3:23]


#normalize
constants = matrix(rep(0,42), nrow=21)
X = rbind(xTrain, xTest, xPredict)
for (i in 1:21){
  constants[i,] = c(mean(X[,i]),sd(X[,i]))
}

standardization <- function(df) {   # normalize
  for (i in 1:21) {
    mean = constants[i,1]
    sd = constants[i,2]
    df[,i] = (df[,i] - mean) / sd
  }
  return(df)
}

xTrain <- standardization(xTrain)
xTest <- standardization(xTest)
xPredict <- standardization(xPredict)


a = train(xTrain, log(yTrain))
a # this is the solution to the normal equation
table = attribute_table(xTrain[,-21], log(yTrain), xTest[,-21], log(yTest))
# drop yr0fPts since it's a linear combination of 6 other attributes
# otherwise, this process will deem all 7 columns irrelevant
table[rev(order(table$trainMSE)),]# table of errors using leave-one-out regression, sorted by Train MSE
table[rev(order(table$testMSE)),]# table of errors using leave-one-out regression, sorted by Test MSE


#predicting
X=as.matrix(cbind(rep(1,nrow(xPredict)),xPredict))
yHat = X %*% a
prediction = cbind(d2021[,1], exp(yHat))
colnames(prediction) <- c("Name", "ProjPts")
prediction = as.data.frame(prediction)
prediction[,2]<-as.numeric(prediction[,2])
prediction[rev(order(prediction$ProjPts)),] #prediction on 2021 data




#TODO:


#determine correlation with every stat
#add polynomial data

