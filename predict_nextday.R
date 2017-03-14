library(quantmod)
library(foreach)
library(caret)
library(data.table)
library(Quandl)
library(doParallel)

Sys.setenv(TZ='America/New_York')
"%+%" <- function(x,y) {paste(x,y,sep="")}
source("functions_features.R")
source("functions_eval.R")
source("functions_general.R")

runner = data.frame()
runSymbols=c("GSPC","NDX","DJI","AAPL","MSFT","FTSE","GDAXI")
targetSymbol = runSymbols = "FTSE"  

for (targetSymbol in runSymbols) {

  
#--------------# LOAD STOCK-----------
print("starting " %+% targetSymbol)
sf = read.csv("stocks_day/" %+% targetSymbol %+% ".csv")
names(sf)

#----------prep dates
sf %>% dim
sf$date = sf$date %>% as.character %>% as.Date

#-------target
target = "target_1.pure"
sf$target.pure = sf[,target]
sf$target.bin <- ifelse(sf[,target] > 0, 1, 0)
sf$target.sym <- ifelse(sf[,target] > 0, 1, -1)
sf$target <- cut(sf[,target], breaks=c(-Inf,0, Inf), labels=c("down_stay","up_change"))

#--------ascending
sf = sf %>% arrange(date)

#-------------SHIFTS all JAPAN AND AUSTRALIA STOCKS 1 day to future (Time Zone!)
JAP_AUST = sf[,names(sf) %like% "AXJO|N225"]
JAP_AUST = sapply(JAP_AUST,FUN=shift,n=1,type="lead")
sf = sf[,!names(sf) %like% "AXJO|N225"]
sf= cbind(sf,JAP_AUST)

# SHIFT next day of GDAXI and FTSE as predictor (Time ZONE!)
if (!targetSymbol %in% c("FTSE","GDAXI")) {
sf$f.shift_GDAXI = shift(sf$f.dayoc_GDAXI,1,type="lead") 
sf$f.shift_FTSE = shift(sf$f.dayoc_FTSE,1,type="lead") 
sf=sf[-nrow(sf),]
}


#----------build meta features (has NAs)
#sf = sf %>% arrange(desc(date) )
#print("....build META features for " %+% targetSymbol)
#META = build_features_leadindic(sf[,predictorNames],sf$target.pure,windows=c(21),thres=c(0.6,0.7,0.8),maxlines=nrow(sf) )
#sf=cbind(sf,META)

#-----------select where target is available
sf = sf [!is.na(sf$target),]
sf %>% dim

#----------fill NAs with last value
first0NA = which(apply(sf[,names(sf) %like% "f."],1,function(x) sum(is.na(x)) == 0 ))[1] %>% names %>% as.numeric
sf = sf[first0NA:nrow(sf),]
for (n in names(sf)[names(sf) %like% "f."]) {if (sum(is.na(sf[,n])) > 0) sf[,n] = na.locf(sf[,n]) }
sf %>% dim


#----cross.cor (ordered ASC: cross.cor.lag > 0 means past data ------# ASC: lag moves past data down to future, lead moves future data up to past
sf = sf %>% arrange(date) 
#Find_Max_CCF(sf$f.tec_roc.usd.Close,sf$target.bin,sf$date)
d=sf
ccor=lapply(d[,names(d) %like% "f."],Find_Max_CCF,d$target.sym,d$date) 
ccor=lapply(names(ccor),function(x) {
  ccor[[x]]$var = x
  ccor[[x]]
}) %>% do.call("rbind",.) %>% arrange(desc(abs(cor)))
ccor %>% filter(lag <=0)

#------select features
#features = ccor[ccor$lag == 0,]$var[1:50]
#features = names(sf)[names(sf) %like% "f.shift"]
predictorNames = names(sf)[names(sf) %like% "f.shift|f.sameday|AXJO|f_mom_3|f_mom_7"]

#--------------find best gap with test class balance
gap=250
best_gap= (foreach(i=c(1:600)) %do% {mean(sf$target.bin[(1+i):(1+i+gap)]) } %>% do.call("c",.) / 0.5 -1) %>% abs 
best_gap=which(best_gap <= 0.05 )[1]
best_gap_end=best_gap+gap
print("...testing on data where prob of going up is...")
print(mean(sf$target.bin[best_gap:best_gap_end]))

#----------------tunegrids
tuneGrids=list()
tuneGrids[["glm"]] = glm_grid=expand.grid(.alpha = seq(1,1,1),.lambda = seq(0,0.2,0.025))
tuneGrids[["rf"]] = rfTuneGrid <- expand.grid(mtry = c(2:6))
tuneGrids[["gbm"]] = expand.grid(n.trees = c(50),interaction.depth = c(30),shrinkage = 0.1,n.minobsinnode=1)

#----reverse
sf = sf %>% arrange(date) %>% rev_me
sf %>% dim
#--------------let's model
sf %>% dim
train.days=1200
test.days=2
back=0
best_gap=0
testseq=seq(0,260,test.days) + best_gap
result=list()
#for (back in c(best_gap:best_gap_end)) {

for (back in testseq ) { 
  
  print(back)
  
  #print(back)
  ro=back 
  split.te <- c((1+ro):(test.days+ro))
  split.tr <- c((test.days+ro+1):(test.days+ro+1+train.days))
  test <- sf[split.te,]
  train <-sf[split.tr,] 
  
  # ascending
  test <- test %>% arrange(date)
  train <-train %>% arrange(date)
  
  #feature selection long
  d=train
  ccor=lapply(d[,names(d) %like% "f."],Find_Max_CCF,d$target.sym,d$date)
  ccor=lapply(names(ccor),function(x) {
    ccor[[x]]$var = x
    ccor[[x]]
  }) %>% do.call("rbind",.) %>% arrange(desc(abs(cor)))
  featuresC = ccor[ccor$lag == 0,]$var[1:2]
  features = c(featuresC,predictorNames) %>% unique
  
  # time slice cross validation
  train_n = train %>% nrow
  window = train_n - 10
  horizon = 2
  myTimeControl <- trainControl(method = "timeslice",initialWindow = window ,horizon = horizon,fixedWindow = TRUE,allowParallel = T,classProbs = TRUE)
  trControlNone = trainControl(method = "none",allowParallel = TRUE,classProbs = TRUE)
  
  #fix zero var
  #nzv <- nearZeroVar(train, saveMetrics = TRUE) %>% subset(.,zeroVar==T) %>% rownames
  #train[,nzv]=train[,nzv] %>% sapply(function(x) sample(c(x-0.00001,x,x+0.00001), length(x), replace=TRUE)  )
  
  #-------glmnet elastic net: mix of L1 Lasso and L2 regularization
  glmnet <- train(train[,features], train[,"target"],method='glmnet',trControl=myTimeControl
                          #,preProcess=c("center","scale")
                          ,metric = "Accuracy"
                          #,trace=TRUE
                          ,tuneGrid=tuneGrids[["glm"]]
                           )
  probs_glm <- predict(object=glmnet, test[,features], type='prob')[,2]
  probs = probs_glm
  
  #--------------final
  t=test[,names(test) %like% "date|target.bin|target$",]
  t$f_hour = 0
  t$probs = probs
  #print(score(na.omit(t),targetSymbol))
  result = rbind(result,t)
  print(score(result,targetSymbol,train.days))
}

#-----prep results
result$stock = targetSymbol
result = result[order(result$date),] 
kpi=score(result,targetSymbol,train.days)
kpi$model = "glmnet_Feb17"
print(kpi)


#-----save results
saveRDS(result,"results_probs/" %+% targetSymbol %+% ".rds")
runner = rbind(runner,kpi)
load_kpi=read.table("results/results_nextday.csv",header=T)
load_kpi = rbind(load_kpi,kpi)
write.table(load_kpi,"results/results_nextday.csv",row.names = F)

}
#---------------------cluster
# stopCluster(cl)
# rm(cl)
# if (!exists("cl")) {
#   cl <- makeCluster(4,outfile=getwd() %+% "/logs/clusterwork_" %+% as.Date(Sys.time()) %+% ".txt")
#   registerDoParallel(cl)
# }

#-----predict
# FTSE 20Feb
live=data.frame(AXJO.Open=5805.80,AXJO.Close=5795.10,N225.Open=19161.33,N225.Close=19251.08)
live=build_features_samedayOC(live,names(live) %>% get_name() %>% unique) %>% as.data.frame %>% t
predict(object=glmnet, live, type='prob')[,2]





