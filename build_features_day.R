library(quantmod)
library(foreach)
library(caret)
library(data.table)
library(Quandl)
library(doParallel)
library(dplyr)

Sys.setenv(TZ='America/New_York')
"%+%" <- function(x,y) {paste(x,y,sep="")}
source("functions_features.R")
source("functions_eval.R")
source("functions_general.R")

#--------------load day stock file with all stocks, order desc by date
sp = unique(as.vector(read.table("http://algotrade.glueckert.net/public/stockpair_symbs.txt")$V1))
s=readRDS("stocks_day/stocks_day.rds")

#--------SETINGS: pick symbols to run
allSymbols = s[,names(s) %like% "Close"] %>% names %>% get_name
runSymbols = allSymbols[ allSymbols %in% gsub("\\^","",sp)] 
runSymbols=c("GSPC","NDX","DJI","AAPL","MSFT","FTSE","GDAXI")
targetSymbol=c("FTSE")
#---------------------------------


#--------------load all possible dates MSFT
ALL_DATES = index(getSymbols("MSFT",src="yahoo",auto.assign = F)) %>% as.Date 
ALL_DATES = data.frame(date=rev(ALL_DATES[ALL_DATES >= min(s$date) & ALL_DATES <= max(s$date) ]))


for (targetSymbol in runSymbols) {
  
  print("-----------------------------------start building features "%+% targetSymbol)
 
  #-------reorder descending
  s %>% dim
  s <- s %>% arrange(desc(date))

  #------ Open and Close rates same day 9-12 and 12-16 (only for US stocks)
  dmin = readRDS("stocks_minute/GSPC.rds") %>% filter(hour(datetime) == 12 & minute(datetime) == 0 ) %>% head(1) # DUMMY!!!
  tryCatch({ dmin = readRDS("stocks_minute/" %+% targetSymbol %+% ".rds")} ,error=function(e) NULL)
  dmin %>% dim
  dmin = dmin %>% filter(hour(datetime) == 12 & minute(datetime) == 0 )
  dmin$target_12 = dmin[,6]
  TS_OPCLO = merge(s,dmin[,c("date","target_12")],by = "date",all.x=T)
  TS_OPCLO$target_Open = TS_OPCLO[,targetSymbol %+% ".Open"]
  TS_OPCLO$target_Close = TS_OPCLO[,targetSymbol %+% ".Close"]
  TS_OPCLO = TS_OPCLO %>% arrange(desc(date))
  
  #-------same day difference 9:30 - 12:00 [f.sameday]
  TS_OPCLO$f.sameday_912 = (TS_OPCLO$target_12 / TS_OPCLO$target_Open - 1) * 100
  TS_OPCLO=TS_OPCLO[,c("target_Open","target_Close","target_12","f.sameday_912")]
  
  #-------same day difference Open Close [f.dayoc]
  OPCLO = build_features_samedayOC(allSymbols,s)
  
  #-------momentum [f.mom]
  MOM = build_features_momentum(OPCLO[,"f.dayoc_" %+% targetSymbol,drop=F],days=c(3,7,14,21))
 
  #--------min max [f.dayminmax]
  MINMAX = build_features_samedayMinMax(allSymbols,s)
  
  # #------specific technial indicators [ f.tec]
  TEC_IND =data.frame()
  target_s = s[,(!names(s) %like% "f\\." & names(s) %like% targetSymbol)]
  target_s=target_s %>% sapply(Hmisc::impute,'random')              # impute missings
  rownames(target_s) = s[,"date"] %>% as.character
  target_s = target_s %>% as.xts                                     # convert XTS
  print("....build technical indidcators...")
  for (day in s$date) {
    day = day %>% as.Date
    ti = extract_tec_indicators(targetSymbol,stock.ohcl=target_s,day=day,nback=14,daysselect=c(1))
    TEC_IND=rbind(TEC_IND,ti)
  }

  #------general percentage feature [f.perc]
  PERC = build_features_day_perc(s, days=c(1,2,3,4,5,7,14,21),symbs=names(s)[names(s) %like% "Close|Volume"],offLimitsSymbols=NA, roundByScaler=10)
  
  #------percentage change for tec indicaotrs [f.perc_f.tec]
  PERC_TEC = build_features_day_perc(TEC_IND, days=c(1,2,3,4,5,7,14,21),symbs=names(TEC_IND),offLimitsSymbols=NA, roundByScaler=10)
  
  #--------day features [f.time_s]
  F_TIME = build_features_dwm(s$date) 
  F_TIME = F_TIME[,!names(F_TIME) %like% "month"]
  
  #------Google Trends
  
  #------Twitter
  
  #-------PutCall Options
  
  
  #------COMBINE
  s4 = cbind(s[,names(s) %like% "date" | names(s) %like% targetSymbol],TS_OPCLO,OPCLO,MOM,MINMAX,PERC
             ,TEC_IND,PERC_TEC
             ,F_TIME)
  
  #-----order asc
  s4 = s4 %>% arrange(date)

  #----------remove 0 variance features or 20% NAs
  nzv <- nearZeroVar(s4, saveMetrics = TRUE)
  Nas = sapply(s4,function(x) length(which(is.na(x)))) 
  nzv$nas = Nas
  nzv$var = rownames(nzv)
  nzv[nzv$zeroVar == TRUE | nzv$nzv  == TRUE | nzv$nas > 0,]
  keep.col = nzv[nzv$nzv == FALSE & nzv$zeroVar == FALSE  & nzv$nas <= 130,]$var
  s5 = s4[,keep.col]
  print("cleaning up...")
  print("before :" %+% dim(s5))
  keep.row = apply(s5,1,function(x) length(which(is.na(x)))) %>% as.vector < 40
  sum(!keep.row)
  s5 = s5[keep.row,]
  print("final :" %+% dim(s5))
  
  #------merge with ALL_DATES
  s6 = merge(ALL_DATES,s5,by.x='date',all.x=T) 
  print("merge :" %+% dim(s6))
  

  #----------Cut beginning NAs
  first0NA = which(apply(s6,1,function(x) length(which(is.na(x)))) %>% as.vector == 0 )[1]
  s6 = s6[first0NA:nrow(s6),]
  s7=s6
  
  #------------TARGET Next day
  s7$target_1.pure <- s7[,"f.dayoc_" %+% targetSymbol]
  s7$target_1.pure = shift(s7$target_1.pure,1,type="lead")
  #s7[1:10,names(s7) %like% "target|date|GSPC.Close|GSPC.Open|f.dayoc_GSPC"]
  
  #------------TARGET Next day 12-16
  s7$target_1_1216.pure = (s7$target_Close / s7$target_12 - 1) * 100
  s7$target_1_1216.pure = shift(s7$target_1_1216.pure,1,type="lead")
  #s7[1:10,names(s7) %like% "target|date|GSPC.Close|GSPC.Open|f.dayoc_GSPC"]
  
  
  #------------TARGET 7 days
  dates = s7[,"date",drop=F]
  dates$date7 = dates$date + 7
  price = s7[s7$date %in% dates$date7 ,c("date","target_Close")]
  new = merge(dates,price,by.x = "date7",by.y = "date",all.x = TRUE )
  s7$target_7.pure = (new$target_Close / s7$target_Close - 1) * 100

  
  #--------------Save
  write.table(s7, file = "stocks_day/" %+% targetSymbol %+% ".csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "utf-8")
  
}
  