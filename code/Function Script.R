####################################################
####         Stock Prediction Function         #####
####################################################
#
# Function Input: date as <"YYYY-MM-DD">
# Function Output: 505 x 3 dataframe-
#   rows of stocks, same-day and 5 day predictions
#
#####################################################

## Load packages
if(!("ggplot2" %in% rownames(installed.packages()))){
  install.packages("ggplot2")
}
if(!("quantmod" %in% rownames(installed.packages()))){
  install.packages("quantmod")
}
if(!("rvest" %in% rownames(installed.packages()))){
  install.packages("rvest")
}
if(!("lubridate" %in% rownames(installed.packages()))){
  install.packages("lubridate")
}
if(!("caret" %in% rownames(installed.packages()))){
  install.packages("caret")
}
if(!("plyr" %in% rownames(installed.packages()))){
  install.packages("plyr")
}
if(!("dplyr" %in% rownames(installed.packages()))){
  install.packages("dplyr")
}
if(!("scales" %in% rownames(installed.packages()))){
  install.packages("scales")
}
if(!("randomForest" %in% rownames(installed.packages()))){
  install.packages("randomForest")
}
if(!("mgcv" %in% rownames(installed.packages()))){
  install.packages("mgcv")
}

library(ggplot2)
library(quantmod)
library(rvest)
library(lubridate)
library(caret)
library(plyr)
library(dplyr)
library(scales)
library(randomForest)
library(mgcv)

# load data- must change working directory to current file location

load("../data/long.Rda")
long$time = ymd(long$time)
long$time <- as.Date(long$time)

# load symbols
load("../symb.Rda")

# load fitted models
load("../models.Rda")

##### FUNCTION
datefunc <- function(date){
  day <- as.Date(date)
  if(date<=Sys.Date()){ # if date is today or before:
    if(wday(day) %in% c(1,7)){ # if weekend, error message
      stop("Please enter a weekday")
    }else if(date<="2015-11-13"){ # take subset of current data, get dataframe
      new.long <- subset(long, time<=day)
    }else{ # acquire new data and concatenate
      data <- new.env()
      getSymbols(symb[,1],env=data,from="2015-11-16",to=day)
      symb[,1]<-gsub("-", "_", symb[,1])
      ########## process data
      dat.list <- as.list(data) # dat.list is a list of time series objects
      
      # remove columns from list that are unwanted
      dat.list = lapply(dat.list, function(x){
        #### subsets just open, closed, and volume columns
        cn = colnames(x)
        res = grep("Open|Close|Volume", cn, value = TRUE)
        x = x[, res]
        x
      }) 
      
      # reduce list into wide dataframe
      alldf = Reduce(function(...) 
        merge(..., all = TRUE), 
        dat.list)
      
      alldf = as.data.frame(alldf)
      alldf$time = rownames(alldf)
      colnames(alldf) = gsub("(.*)[.](Open|Close|Volume)", "\\2.\\1", colnames(alldf))
      
      # fix strange naming (stock BF.B -> BF_B)
      colnames(alldf) = gsub("(.*)[.](.*)[.](.*)", "\\1.\\2_\\3", colnames(alldf))
      
      # create super-long dataframe
      new.long = stats::reshape(alldf, direction = "long", 
                                idvar = "time", 
                                timevar = "stock",
                                varying = grep("[.]", colnames(alldf), value = TRUE))
      new.long = merge(new.long,symb,all=T,by.x="stock", by.y="symb")
      new.long = new.long[ order(new.long[,1], new.long[,2]), ]
      rownames(new.long) = NULL
      
      #rbind additional days from my data
      new.long <- rbind(subset(long,select=c("stock","time","Open","Close","Volume","sector"),
                               time>=day-10), # need to bind previous data so we can create lagged variables
                        new.long)
      new.long <- new.long[order(new.long[,1], new.long[,2]),]
      
      
    }
    
    ###### begin data processing
    # proper date formatting
    new.long$time = ymd(new.long$time)
    new.long$weekday = wday(new.long$time, label=T)
    new.long$month = month(new.long$time, label=T)
    new.long$year = year(new.long$time)
    
    # number of days within stock- useful for lag variables
    new.long$numday<-ave(new.long$stock, new.long$stock, FUN=seq_along)
    
    # reverse dates, reversed numbering column
    new.long = new.long[order(new.long[,1], rev(new.long[,2])), ]
    new.long$numdayrev <- ave(new.long$stock, new.long$stock, FUN=seq_along)
    
    # return to original numbering
    new.long = new.long[order(new.long[,1],new.long[,2]), ]
    
    ####################
    # Create Variables
    ####################
    # current day return (also what we will want to be predicting)
    new.long$return0 <- (new.long$Close-new.long$Open) / new.long$Open
     
    # lagged returns (returns yesterday, two, three days ago and 1 week ago)
    new.long$return1minus <- c(NA,new.long$return0[1:(nrow(new.long)-1)])
    new.long$return1minus[which(new.long$numday %in% c(1))]<-NA
    new.long$return2minus <- c(NA,NA,new.long$return0[1:(nrow(new.long)-2)])
    new.long$return2minus[which(new.long$numday %in% c(1:2))]<-NA
    new.long$return3minus <- c(NA,NA,NA,new.long$return0[1:(nrow(new.long)-3)])
    new.long$return3minus[which(new.long$numday %in% c(1:3))]<-NA
    new.long$returnwkminus <-c(NA,NA,NA,NA,NA,new.long$return0[1:(nrow(new.long)-5)])
    new.long$returnwkminus[which(new.long$numday %in% c(1:5))]<-NA
    
    # lagged volumes (volumes from yesterday and two days ago)
    new.long$volume1minus <- c(NA,new.long$Volume[1:(nrow(new.long)-1)])
    new.long$volume1minus[which(new.long$numday %in% c(1))] <- NA 
    
    new.long$volume2minus <- c(NA,NA,new.long$Volume[1:(nrow(new.long)-2)])
    new.long$volume2minus[which(new.long$numday %in% c(1:2))] <- NA 
    
    # lagged close (close yesterday)
    new.long$close1minus <- c(NA,new.long$Close[1:(nrow(new.long)-1)])
    new.long$close1minus[which(new.long$numday %in% c(1))] <- NA 
    
    # edit variable types
    new.long$sector <- factor(new.long$sector)
    new.long$time <- as.Date(new.long$time)
    
    ####################
    # Run Models
    ####################
    comp.new.long.0<-subset(new.long,time==day)
    comp.new.long.0<- comp.new.long.0[complete.cases(comp.new.long.0[,1:18]),]
    comp.new.long.5 <- subset(new.long,time==day)
    comp.new.long.5<- comp.new.long.5[complete.cases(comp.new.long.5),]
    
    ## 0
    glmpred <- predict(glm0,comp.new.long.0)
    rfpred <- predict(rf0,comp.new.long.0)
    pred0 <- predict(combModFit0,data.frame(glmpred=glmpred,rfpred=rfpred), interval="prediction")
    
    comp.new.long.0$pred0 <- pred0[,1]
    comp.new.long.0$predlwr0 <- pred0[,2]
    comp.new.long.0$predupr0 <- pred0[,3]
    
    ## 5
    glmpred <- predict(glm5,comp.new.long.5)
    rfpred <- predict(rf5,comp.new.long.5)
    pred5 <- predict(combModFit5,data.frame(glmpred=glmpred,rfpred=rfpred), interval="prediction")
    
    comp.new.long.5$pred5 <- pred5[,1]
    comp.new.long.5$predlwr5 <- pred5[,2]
    comp.new.long.5$predupr5 <- pred5[,3]
    
    ### get predicted and true ranks by day
    # return0
    comp.new.long.0<-comp.new.long.0[order(comp.new.long.0[,'time'],-comp.new.long.0[,'predlwr0']), ]
    comp.new.long.0$predrank0<- ave(as.character(comp.new.long.0$time), as.character(comp.new.long.0$time), FUN=seq_along)
    
    # return5
    comp.new.long.5<-comp.new.long.5[order(comp.new.long.5[,'time'],-comp.new.long.5[,'predlwr5']), ]
    comp.new.long.5$predrank5<- ave(as.character(comp.new.long.5$time), as.character(comp.new.long.5$time), FUN=seq_along)
    
    finaldf<-merge(subset(comp.new.long.0,select=c("stock","predrank0")),
                   subset(comp.new.long.5,select=c("stock","predrank5")),
                   all=T, by=c("stock"))
    return(finaldf)
    
    
    
  }else{ # error message
    stop("Please enter today's date or a past date")
  }
}