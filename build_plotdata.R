source("functions_general.R")
setwd("/var/www/vhosts/algotrade.glueckert.net/httpdocs/binary_options")
source("../algotrade/functions_dailyquotes.R")

#--------------------load minute data
all_stocks=list()
for (f in list.files("stocks_minute"))  {
  print("load " %+% f)
  all_stocks[[strsplit(f,"\\.")[[1]][1]]] = readRDS("stocks/" %+% f)
}

av_rows1 = sapply(all_stocks,nrow) 
av_rows1 %>% print

#-------------Select year and time 09:30 to 16:00
all_stocks=all_stocks %>% lapply(function(x) x %>% filter(datetime %>% year >= 2015 & as.numeric(format(datetime,"%H")) %in% c(9:15)) ) 

#-------------------Merge keep all
fin = Reduce(function(x, y) merge(x, y, all=T,by.x="datetime", by.y="datetime"), all_stocks, accumulate=F)
fin = fin[,!grepl("date\\.y$|date\\.x$",names(fin)) ]

#------drop days with missing minutes
keep = aggregate(datetime ~ date,data=fin, FUN=length) %>% filter(.,datetime == 390)
fin = fin %>% filter(.,date %in% keep$date)
fin  = fin %>% select(.,matches("Close|datetime"))

#-------------------build targets minutes
tars = target_minute(fin %>% select(.,matches("Close|datetime")),window=c(60,15,5))
tars%>% head
full = cbind(fin[,"datetime",drop=F],tars)
full %>% head

#------------------build direction changes
n = full %>% names %gp% "Close"
dchan=apply(full[,n],2,direction_change)
colnames(dchan) = colnames(dchan) %+% ".flip"
full=cbind(full,dchan)

#-----build up down
#updown=apply(full[,n],2,create_updown)
#colnames(updown) = colnames(updown) %+% ".dir"
#dummies <- dummyVars( ~. , data = updown)
#updown=predict(dummies,updown)
#full=cbind(full,updown)
#full=cbind(full,updown)
#dim(full)

#-----------------create times
timestuff = data.frame(year=as.numeric(format(fin$datetime,"%Y")),date=as.Date(fin$datetime),hour = as.numeric(format(fin$datetime,"%H"))
                       , month =  as.numeric(format(fin$datetime,"%Y")) %+% "-" %+% as.numeric(format(fin$datetime,"%m")))
full=cbind(timestuff,full)
full %>% head

av_days_year = (full$date%>% unique%>% length / full$year%>% unique %>% length) %>% ceiling


#------A create Aggregation per hour MAD%
agg_mad=aggregate(. ~ year + hour, data = full, FUN=mad,na.action="na.omit")
agg_mad=melt(agg_mad,id.vars=c("year","hour"),measure.vars=n)
agg_mad$lag=as.numeric(gsub("\\D", "", agg_mad$variable)) 
agg_mad=agg_mad[order(agg_mad$lag),]
agg_mad %>% head
#agg_mad = data.table(agg_mad)
saveRDS(agg_mad,"agg_mad.rds")

#------B create Aggretaion # flips
agg_sum =aggregate(. ~ year + hour, data = full, FUN=sum,na.action="na.omit")
agg_sum=melt(agg_sum,id.vars=c("year","hour"),measure.vars= agg_sum %>% names %gp% ("flip"),value.name="flip")
agg_sum$lag=as.numeric(gsub("\\D", "", agg_sum$variable)) 
agg_sum=agg_sum[order(agg_sum$lag),]
agg_sum %>% nrow
#agg_sum = data.table(agg_sum)
saveRDS(agg_sum,"agg_sum.rds")

#---- C updown
agg_ud = melt(full,id.vars=c("year","hour","date"),measure.vars=n,value.name="dir")
agg_ud$direction = ifelse(is.na(agg_ud$value), NA,  ifelse(agg_ud$value > 0 ,"up","down") )
agg_ud$lag=as.numeric(gsub("\\D", "", agg_ud$variable)) 
agg_ud %>% head
#saveRDS(agg_ud,"agg_ud.rds")

pmap = expand.grid(plot=c("mad","sum","ud"),filt_minute = c(60,15,5),filt_size = c(0,1,2,3,4),filt_year = c(2015,2016))
pmap$key = apply(pmap,1,function(x) paste(x,collapse="") %>% gsub(" ","",.))

if (unique(pmap$key) %>% length == nrow(pmap) ) print("keys ok") else warning("keys broken")
plots=list()
for (key in pmap$key) {
  i = which(pmap$key == key)
  print( pmap[i,])
  if (pmap[i,"plot"] == "mad") {
      out = filter(agg_mad, lag== pmap[i,"filt_minute"]  & year==pmap[i,"filt_year"]) 
      #ggplot(out, aes(x=hour, y=value, colour = variable, group=variable)) + geom_line()
   }
  if (pmap[i,"plot"] == "sum") {
      out = filter(agg_sum, lag== pmap[i,"filt_minute"]  & year== pmap[i,"filt_year"])
      out[out$hour == 9,]$value = out[out$hour == 9,]$value / 30 / av_days_year
      out[out$hour >= 10,]$value = out[out$hour  >= 10,]$value / 60 / av_days_year
      ggplot(out, aes(x = hour, y = value, colour = variable, group=variable)) + geom_line()
  }
  if (pmap[i,"plot"] == "ud") {
      out = filter(agg_ud, lag== pmap[i,"filt_minute"] & abs(value) >= pmap[i,"filt_size"] & year==pmap[i,"filt_year"])
      if (nrow(out) > 0) out = aggregate(value  ~ hour + variable + direction, out, FUN=length,na.action="na.omit")  %>% arrange(.,value)
      ggplot(out, aes(x = direction, y = value,colour = variable,group=variable)) + geom_line() + facet_grid(. ~ hour)
      
  }
  plots[[key]] = out
}
plots
pmap$n = sapply(plots,nrow)
pmap 



stock_names = strsplit(agg_ud$variable %>% unique %>% as.character,"\\.") %>% lapply(.,head,1) %>% unlist
saveRDS(stock_names,"stock_names.rds")



