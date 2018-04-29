library(randomForest)
library(AUC)
library(glmnet)


####data import
basicstats <- read.csv("~/Downloads/NBAbasicstats1718.csv")
advancedstats <- read.csv("~/Downloads/NBAadvancedstats1718.csv")
salary <- read.csv("~/Downloads/NBAsalary1718.csv")

####data prep
salary <- salary[,1:2]
salary <- aggregate(salary$season17_18,by=list(salary$Player),sum)
names(salary) <- c("Player","Amount")
allstats <- merge(basicstats,advancedstats,by="Player") #merge on player name
allstats <- allstats[-c(2:5,7)] #remove duplicates

names(allstats) <- c("Player","GS","FGM",
                     "FGA","FGPct","ThreePM","ThreePA",
                     "ThreePtPct","TwoPM","TwoPA","TwoPtPct",
                     "eFGPct","FTM","FTA","FTPct","ORB","DRB",
                     "TRB","AST","STL","BLK","TOV","PF","PPG",
                     "Position","Age","Team","GP","MP","PER",
                     "TSPct","ThreePAr","FTr","ORBPct","DRBPct",
                     "TRBPct","ASTPct","STLPct","BLKPct","TOVPct",
                     "Usage","OWinS","DWinS","TotWS","WSper48",
                     "OBPM","DBPM","BPM","VORP") #rename columns cleanly
allstats <- allstats[-27] #remove Team Name


allstats <- subset(allstats,allstats$GP > quantile(allstats$GP,.1)) #remove bottom 10% of games played
allstats <- subset(allstats,allstats$MP > quantile(allstats$MP,.1)) #remove bottom 10% of minutes played from that subset (trying to remove these small contracts here)
full <- merge(allstats,salary)
names(full)[ncol(full)] <- "Salary"
full <- full[which(rowSums(is.na(full)) ==0),]
####split data
sample_ind <- sample(1:nrow(full),.75*nrow(full),F)
train <- full[sample_ind,]
test <- full[-sample_ind,]



####model fitting
set.seed(4)
rffit500 <- randomForest(formula=Salary~.-Player,
                        data=train,
                        ntree=500, 
                        importance=TRUE)
rffit1000 <- randomForest(formula=Salary~.-Player,
                         data=train,
                         ntree=1000, 
                         importance=TRUE)

####predictions
predrf500 <- predict(rffit500,test)
predrf1000 <- predict(rffit1000,test)
sqrt(mean((predrf500-test$Salary)^2)) #test MSE for ntree=500
sqrt(mean((predrf1000-test$Salary)^2)) #test MSE for ntree=1000
#ntree=500 yields a better performance in this iteration so we will go with that

####analysis
fullwithpredictions <- data.frame(full,predict(rffit500,full))
names(fullwithpredictions)[length(fullwithpredictions)] <- "PredSalary"
condensed <- data.frame(fullwithpredictions$Player,fullwithpredictions$Salary,fullwithpredictions$PredSalary)
condensed[,4] = condensed[,3]-condensed[,2]
names(condensed) <- c("Player","ActualSalary","PredSalary","Difference")
condensed$Player[which.max(condensed$Difference)] #Josh Richardson
condensed$Player[which.min(condensed$Difference)] #Mike Conley

varImpPlot(rffit500)
decMSE <- c("age","gs","fgm","fga","ppg","tov","vorp","ftm","mp","2pa")
decIMP <- c("gs","age","vorp","fgm","ppg","fga","mp","totws","ftm","fta")
intersect(decMSE,decIMP)
#in both top 10s: Age, GS, FGM, FGA, PPG, VORP, FTM, MP

#most overpaid:
#Conley, Curry, Greg Monroe, Ryan Anderson, Al Horford, Paul Millsap, Solomon Hill

#most underpaid:
#Josh Richardson, Spencer Dinwiddie, Kyle Anderson, Ben Simmons, Nikola Jokic, Lou Williams, Reggie Bullock


####now we'll do the same but with log(salary)

####split data
full$logSalary <- log(full$Salary)
sample_ind2 <- sample(1:nrow(full),.75*nrow(full),F)
train <- full[sample_ind2,]
test <- full[-sample_ind2,]

####model fitting
set.seed(4)
rffit500 <- randomForest(formula=logSalary~.-Player-Salary,
                         data=train,
                         ntree=500, 
                         importance=TRUE)
rffit1000 <- randomForest(formula=logSalary~.-Player-Salary,
                          data=train,
                          ntree=1000, 
                          importance=TRUE)


predrf500 <- predict(rffit500,test)
predrf1000 <- predict(rffit1000,test)
sqrt(mean((predrf500-test$logSalary)^2)) #test MSE for ntree=500
sqrt(mean((predrf1000-test$logSalary)^2)) #test MSE for ntree=1000

## ntree=1000 yields a better performance in this iteration so we will go with that


fullwithpredictions2 <- data.frame(full,exp(predict(rffit1000,full)))
names(fullwithpredictions2)[length(fullwithpredictions2)] <- "PredSalary"
condensed2 <- data.frame(fullwithpredictions2$Player,fullwithpredictions2$Salary,fullwithpredictions2$PredSalary)
condensed2[,4] = condensed2[,3]-condensed2[,2]
names(condensed2) <- c("Player","ActualSalary","PredSalary","Difference")
condensed2$Player[which.max(condensed2$Difference)] #Will Barton
condensed2$Player[which.min(condensed2$Difference)] #Conley
varImpPlot(rffit1000)
decMSE2 <- c("age","gs","fgm","ppg","tov","fga","trb","drb","mp","totws")
decIMP2 <- c("age","gs","mp","ppg","fgm","fga","ftpct","ftm","totws","tov")
intersect(decMSE2,decIMP2)
#in both top 10s: age, gs, fgm, ppg, tov, fga, mp, totws

