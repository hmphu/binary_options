library(ROCR)
#https://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/


pROC = function(pred, fpr.stop) {
  perf <- performance(pred,'tpr','fpr')
  for (iperf in seq_along(perf@x.values)){
    ind = which(perf@x.values[[iperf]] <= fpr.stop)
    perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
    perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
  }
  return(perf)
}


opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}



score = function(t,targetSymbol,n_train) {
  count_months = elapsed_months(min(t$date),max(t$date))
  days.test = t %>% nrow
  pred <- prediction(t$probs, as.factor(t$target))
  perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
  auc = performance(pred, measure = "auc")@y.values[[1]]
  proc.perf = pROC(pred, fpr.stop=0.2)
  cut_perfect=opt.cut(proc.perf, pred)[3,]
  cutoffs = c(0.5,0.6,cut_perfect) %>% as.numeric
  t$pred = NA
  t = t[!is.na(t$probs),]
  

  out = data.frame()
  for (cut in cutoffs) {
    
    #cut = 0.5
    tf = t
    if (tf %>% filter(probs >= cut) %>% nrow > 0) tf[tf$probs >= cut ,]$pred = "up_change"
    if (tf %>% filter(probs < (1-cut)) %>% nrow > 0) tf[tf$probs < (1-cut) ,]$pred = "down_stay"
    tf = tf[!is.na(tf$pred),]
    if (tf %>% nrow >= 2) {
              tf = na.omit(tf)
              u = c("up_change","down_stay")
              cm=confusionMatrix(table(factor(tf$pred %>% as.character,u), factor(tf$target %>% as.character,u)))
              hit = tf[tf$pred == tf$target,] %>% nrow
              miss = tf[tf$pred != tf$target,] %>% nrow
              money= hit * 20 * 0.8 - miss*20*1
              res=data.frame(months=count_months,stock=targetSymbol,n.train=n_train,auc=auc,cutoff=cut,
                         accuracy=cm$overall["Accuracy"],
                         pos.pre.val=cm$byClass["Pos Pred Value"],
                         neg.pre.val=cm$byClass["Neg Pred Value"],
                         precision=cm$byClass["Precision"],
                         days.test=days.test,
                         days.trade=tf %>% na.omit %>% nrow,
                         money20=money,
                         money20.day=round(money/days.test,1),
                         trend.up=mean(t$target.bin)
              )
    }
    out = rbind(out,res)
  }
  rownames(out) = NULL
  out
}


simulate_returns = function(best,cutoff,MONEY_TRADE) {
  best$pred = ifelse(best$probs >= cutoff,1,ifelse(best$probs < (1-cutoff),0,NA))
  best$hit = 0
  best[which(best$target.bin != best$pred),"hit"] = -MONEY_TRADE
  best[which(best$target.bin == best$pred),"hit"] = MONEY_TRADE*0.8
  invested = sum(!is.na(best$pred)) * MONEY_TRADE
  trades = sum(!is.na(best$pred))
  best$cum= cumsum(best$hit) + invested
  best$delta = (Delt(best$cum,k=-1) *100) %>% as.vector
  best$delta[1] = ((invested + best$hit[1]) / invested - 1) * 100
  best
}


