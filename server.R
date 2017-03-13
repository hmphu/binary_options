library(ggplot2)
library(scales)
library(reshape2)
library(quantmod)
library(markdown)
library(DT)
library(caret)
library(magrittr)
library(shinyBS)
library(dplyr)
library(plotly)
library(shinyjs)

#---------------------FUNCTIONS
source("functions_general.R")
source("functions_eval.R")

find_top = function(df,top) {
  if ((nrow(df)) > (top) )
    return(df=rbind(df %>% head(top/2),df %>% tail(top/2)))
  else
    return(df)
}

shuffle = function(x,k) {
  n <- length(x)
  x[order(rep(seq_len(ceiling(n/k)), each=k, length.out=n) + runif(n))]
}

scale_end = function(t,to) {
  from=to[1]
  to=to[2]
  fact = to - t[length(t)]
  fs = seq(0,fact,length.out = length(t))
  t=t+fs
  t
}
calc_win = function(alerts,tradesize,bin_win,bin_loss,hit) {
  trades_win = trunc(alerts*hit)
  trades_loss = alerts - trades_win 
  win=trades_win * tradesize * bin_win
  loss=trades_loss * tradesize * bin_loss
  out = round(win - loss,0)
  return(out)
}

create_kpis=function(invested,rate_return,DAYS_TRADING_YEAR) {
  out=list()
  multiple = round(1+rate_return/100,2)
  total=round(invested*multiple,0)
  profit=total-invested
  profit_day=profit/DAYS_TRADING_YEAR
  profit_month=round(profit/12,0)
  out=list(invested=invested,total=total,profit=profit,multiple=multiple,profit_day=profit_day,
           profit_month=profit_month)
  out
}

"%+%" <- function(x,y) {paste(x,y,sep="")}
rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
"%g%" <- function(x,y) {x[grepl(y,x)]}
"%gp%" <- function(x,y) {x[grepl(y,x,perl=TRUE)]}



#-------------------------STATIC VARIABLES
# http://www.barclayhedge.com/research/indices/ghs/Hedge_Fund_Index.html
DAYS_TRADING_YEAR = 250
SP5 = read.csv("benchmark.csv")
SP5$delta= c(0,na.omit(Delt(SP5$GSPC.Close,k=1))) + 1
SP5 = SP5[seq(1,nrow(SP5),length.out = DAYS_TRADING_YEAR),]
SP5$delta = SP5$GSPC.Close/SP5$GSPC.Close[1]
SP5$delta2=SP5[shuffle(c(1:nrow(SP5)),10),]$delta
plots=readRDS("plots.rds")

# load modelling results
load_kpi=read.table("results/results_nextday.csv",header=T) %>% na.omit %>% arrange(desc(money20.day))
load_kpi[,4:9]=load_kpi[,4:9] %>% sapply(round,2)

# pick best model
best = readRDS("results_probs/" %+% load_kpi[1,"stock"] %+% ".rds")
best=simulate_returns(best,cutoff=load_kpi[1,"cutoff"],MONEY_TRADE=20)

# calculate kpis best model
sharp=sqrt(best$delta %>% length) * (mean(best$delta)/sd(best$delta) )
invested = best$cum[1]
multiple=( best$cum %>% tail(1) / invested *100 - 100) %>% round(0) %+% "%"
trades = sum(!is.na(best$pred))

#-------------------JAVASCRIPT

function(input, output, session) {
  
#-------------------wanrnings
  # suppress warnings  
  storeWarn<- getOption("warn")
  options(warn = -1) 

#----------------events


#-------------reactive functions----
plots_re =  reactive({
  
  filt_minute = input$i_lag
  filt_size =  input$i_thres
  filt_year = input$i_year
  settings= filt_minute %+% filt_size %+% filt_year
  plot_mad=plots[["mad"%+% settings]] 
  plot_sum=plots[["sum"%+% settings]] 
  plot_ud=plots[["ud"%+% settings]] 
  print(input$i_stock)
  if (!is.null(input$i_stock)) {
    plot_mad=plot_mad %>% filter(variable %>% as.character %>% get_name %in% input$i_stock )
    plot_sum=plot_sum %>% filter(variable %>% as.character %>% get_name %in% input$i_stock )
    plot_ud=plot_ud%>% filter(variable %>% as.character %>% get_name %in% input$i_stock )
  }
  list(plot_mad=plot_mad,plot_sum=plot_sum,plot_ud=plot_ud)
})
  
#modal_input = reactiveValues()
#modal_input$invest = 5000
observeEvent(input$BUT_startsim, {
  toggleModal(session, "MODAL_sim", toggle = "close")
  updateSelectInput(session, "i_trade", selected = input$i_modal_invest)
})
  
list_invest =  reactive({ 
      #---binary profit
      out = list()
      tradesize = input$i_trade
      alert_day = input$i_alert_day
      alerts = alert_day * DAYS_TRADING_YEAR
      hit = input$i_hit/100
      bin_win = input$i_bin_win/100
      bin_loss = input$i_bin_loss/100
      invested=alerts*tradesize
      profit=calc_win(alerts,tradesize,bin_win,bin_loss,hit)
      total=invested+profit
      multiple=round(total/invested,2)
      profit_day=round(profit/DAYS_TRADING_YEAR,2)
      profit_month=round(profit/12,0)
      out$kpis = list(invested=invested,total=total,profit=profit,multiple=multiple,profit_day=profit_day,
               profit_month=profit_month)
      
      #----hedge fund and sp500
      out$sp500 = create_kpis(invested,input$i_sp500,DAYS_TRADING_YEAR)
      out$hedge = create_kpis(invested,input$i_hedge,DAYS_TRADING_YEAR)
      
      #---plot
      
      o=data.frame(hit = rbinom(DAYS_TRADING_YEAR,1,input$i_hit/100))
      o$money = -input$i_trade * input$i_bin_loss/100
      o[o$hit == 1,]$money = input$i_trade * input$i_bin_win/100
      o=o[sample(1:nrow(o)),]
      o$hit=NULL
      o$money_cum=cumsum(o$money)
      o$money_cum = o$money_cum + invested
      o$binary = o$money_cum/o$money_cum[1]
      
      o$binary = scale_end(o$binary, to=c(1,out$kpis$multiple))
      o$sp500 = scale_end(SP5$delta, to=c(1,out$sp500$multiple))
      o$hedge = scale_end(SP5$delta2, to=c(1,out$hedge$multiple))
      o$day = c(1:DAYS_TRADING_YEAR)
      out$sim_plot = o
      out
      })

    

#--------------Outputs
  output$k_invested2 <- renderText({"Invest: $" %+% list_invest()$kpis$invested})
  output$k_invested <- renderText({list_invest()$kpis$invested})
  output$k_total <- renderText({list_invest()$kpis$total})
  output$k_profit <- renderText({list_invest()$kpis$profit})
  output$k_multiple <- renderText({ (list_invest()$kpis$multiple*100-100) %+% "%"})
  output$k_profit_month <- renderText({list_invest()$kpis$profit_month})
  
  output$sp_invested <- renderText({list_invest()$sp500$invested})
  output$sp_total <- renderText({list_invest()$sp500$total})
  output$sp_profit <- renderText({list_invest()$sp500$profit})
  output$sp_multiple <- renderText({ (list_invest()$sp500$multiple*100-100) %+% "%"})
  output$sp_profit_month <- renderText({list_invest()$sp500$profit_month})

  output$he_invested <- renderText({list_invest()$hedge$invested})
  output$he_total <- renderText({list_invest()$hedge$total})
  output$he_profit <- renderText({list_invest()$hedge$profit})
  output$he_multiple <- renderText({ (list_invest()$hedge$multiple*100-100) %+% "%"})
  output$he_profit_month <- renderText({list_invest()$hedge$profit_month})

  
#--------------Rendering
  
  output$example_plot <- renderPlotly({
    #ggplot(data=list_invest()$sim_plot, aes(x=day,binary)) + geom_line() + xlab("day") + ylab("money") 
    #theme_set(theme_gray(base_size = 12))
    ggplot(list_invest()$sim_plot, aes(x=day)) + geom_line(aes(y = binary, colour = "binary")) + geom_line(aes(y = sp500, colour = "sp500"))+ geom_line(aes(y = hedge, colour = "hedge funds"))
  })
  
  
  output$example_summary <- renderPrint({
    summary(cars)
  })
  
  output$example_text <- renderText({
    paste('stuf pasted')
  })
  
  output$table_kpis <- DT::renderDataTable({
    DT::datatable(
  load_kpi,rownames= FALSE,options = list(pageLength = 5)
    )
  })
  
  output$table_benchmark <- DT::renderDataTable({
    DT::datatable(
      read.table("results/table_benchmark.csv",header=T),rownames= FALSE
    )
  })
  
  
  output$table_cars <- renderTable({
    table(cars)
  })

  output$plot_mad <- renderPlotly({
    #theme_set(theme_gray(base_size = 18))
    plots_re()$plot_mad %>% plot_ly(x = ~hour, y = ~value, color = ~variable, mode = "lines")
    #ggplot(data=plots_re()$plot_mad, aes(x=hour, y=value, colour = variable, group=variable)) + geom_line()
    #ggplotly(A) 
  })
  
  output$plot_sum <- renderPlotly({
    #theme_set(theme_gray(base_size = 18))
    #ggplot(data=plots_re()$plot_sum, aes(x=hour, y=value, colour = variable, group=variable)) + geom_line()
    plots_re()$plot_sum %>% plot_ly(x = ~hour, y = ~value, color = ~variable, mode = "lines")
    #ggplotly(B) 
  })
  
  output$plot_ud2 <- renderPlotly({
    #ggplot(data = plots_re()$plot_ud, aes(x = direction, y = value, colour = variable, group=variable)) + geom_line() + facet_grid(. ~ hour)
    plots_re()$plot_ud %>% plot_ly(x = ~direction, y = ~value, color = ~variable, mode = "lines")
    #ggplotly(C)     
    
     })
  output$plot_ud <- renderPlotly({
    ggplot(plots_re()$plot_ud, aes(x = direction, y = value,colour = variable,group=variable)) + geom_line() + facet_grid(. ~ hour)
    #plots_re()$plot_ud %>% plot_ly(x = ~direction, y = ~value, color = ~variable, mode = "lines")
    #ggplotly(C)     
    
  })
  output$sharp<- renderPlotly({

    best %>% plot_ly(x = ~date, y = ~cum, mode = "lines") %>% layout(title = "trades: " %+% trades %+% ", investment: " %+% invested %+% ", ROI: " %+% multiple %+% ", SHARP-RATIO = " %+% round(sharp,2) )
    
  })
 
}
