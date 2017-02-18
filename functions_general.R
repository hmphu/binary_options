library(quantmod)
library(foreach)
library(data.table)
library(Quandl)
library(caret)
library(magrittr)
library(dplyr)

#library(tidyverse)

"%+%" <- function(x,y) {paste(x,y,sep="")}
"%g%" <- function(x,y) {x[grepl(y,x)]}
"%gp%" <- function(x,y) {x[grepl(y,x,perl=TRUE)]}

sprint = function(x) {
  v = ""
  for (e in x) v=v %+% "-" %+% e
  v
}


get_name = function(v){
  strsplit(v,"\\.") %>% lapply(.,head,1) %>% unlist
}

rev_me = function(x) {
  if (is(x)[1] == "data.frame") 
    return(x[rev(1:nrow(x)),])
  else if (is(x)[1] == "vector") 
    return(rev(x))
  else
    return(x)
}

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

