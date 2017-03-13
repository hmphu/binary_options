source("functions_general.R")
setwd("/var/www/vhosts/algotrade.glueckert.net/httpdocs/binary_options")
source("../algotrade/functions_dailyquotes.R")

# http://www.qmatix.com/XLQSymbolGuide.htm

sp = unique(as.vector(read.table("http://algotrade.glueckert.net/public/stockpair_symbs.txt")$V1))
all_stocks = as.vector(read.table("http://algotrade.glueckert.net/public/allstock_symbs.txt")$V1)
all_symbs = unique(c(all_stocks,sp)) 

#-----------SETTINGS
years = c(2010,2017)
symbs = sp
windows = c(5,15,60)
y0=min(years) %+% "-01-01"
y1=max(years) %+% "-12-31"
#-------------------

#------------minute data
symbs = c("GSPC","NDX","DJI","AAPL","MSFT")
symbs = c("AAPL","MSFT")
for (symb in symbs) {
  min_data=api_stock_rates(symb,y0,y1,historyType=0,intradayMinutes=1,prefix=symb%+%".",varnames="",batch=c(y0,y1,30))
  saveRDS(min_data,"stocks_minute/" %+% symb %+% ".rds")
  print(symb %+% "")
}


# #------------EOD data ActiveTIck
# years = c(2012,2016)
# y0=min(years) %+% "-01-01"
# y1=max(years) %+% "-12-31"
# draw=foreach (symb= all_symbs) %do% {
#  Sys.sleep(1)
#  api_stock_rates(symb,y0,y1,historyType=1,intradayMinutes=0,prefix=symb%+%".",varnames="",batch=NA)
# }


#------------EOD data YAHOO quantmod
draw=foreach (symb= all_symbs,.errorhandling = "remove") %do% {
  print(symb)
  s=getSymbols(symb, src = "yahoo",auto.assign = F)
  s=s[as.Date(index(s))  >= as.Date("2010-01-01")]
  s
}
draw=draw[!is.null(draw)]
draw = draw %>% lapply(function(x) {
  datex = index(x) %>% as.Date
  x = x %>% as.data.frame
  x$date = datex
  x
})

# combine
d1 = draw[sapply(draw,nrow) %>% unlist > 1000]
d1 = Reduce(function(x, y) merge(x, y, all=T,by.x="date", by.y="date"), d1, accumulate=F)

# remove "^"
names(d1) = gsub("\\^","",names(d1))

# eliminate stocks with few rows / days
NAs = d1[,names(d1) %like% "Close"] %>% sapply(function(x) sum(is.na(x))) %>% as.data.frame
keep = rownames(NAs)[NAs <= 100] %>% get_name
d1 = d1[,(d1 %>% names %>% get_name %in% keep ) | names(d1) %like% "date",]

# attach MSFT date vector to normalize dates
ALL_DATES = index(getSymbols("MSFT",src="yahoo",auto.assign = F)) %>% as.Date 
ALL_DATES = data.frame(date=rev(ALL_DATES[ALL_DATES >= min(d1$date) & ALL_DATES <= max(d1$date) ]))
d2 = merge(ALL_DATES,d1,by.x='date',all.x=T) 

#replace 0 Volume with NA
d2[d2 == 0] <- NA

# remove NA rows
NAs.row = apply(d2,1,function(x) {length(which(is.na(x)))})

saveRDS(d2,"stocks_day/stocks_day.rds")

d2[,names(d2) %like% "N225|date"] %>% tail

#--------
getSymbols("^N225", src = "yahoo",auto.assign = F) %>% tail(1)
getSymbols("^GDAXI", src = "yahoo",auto.assign = F) %>% tail(1)
getSymbols("^AXJO", src = "yahoo",auto.assign = F) %>% tail(1)
getSymbols("^FTSE", src = "yahoo",auto.assign = F) %>% tail(5)
