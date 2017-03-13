Dataincubator Capstone project: Automated binary option trading (work in progress)
===============================================================================
https://glueckert.shinyapps.io/binary_options/

**Goal**: predict hourly or daily stock direction using machine learning and perform API based automated trading on binary option platforms. 
**Result**: up to 0.7 accuracy yielding  20%+ ROI at sharp-ratio of 5+

The webapp shows:

* explantion and resources regarding binary options
* example simulation for binary option trading
* visualizations of hourly directional stock movements
* backetesting results and benchmark
* dashboard of live trading results (wor in progress)

Data used

* for end-of-day data: yahoo finance via [quantmod package](https://cran.r-project.org/web/packages/quantmod/quantmod.pdf)
* for minute data: [activetick API](http://wwww.activetick.com)

Key directories and files
===============================================================================
`/build_stocks.R`
retrieves stock data for daily rates and minute rates

`/build_plotdata.R`
builds 5-minute, 15-minute, 60-minute directional stock movements for visualizations in the *stock directions* tab

`/build_features_day.R`
calculates features for next-day analysis using a mix of technical indicators (R-quantmod) and sliding window approach

Tech stack
===============================================================================
Shiny-R application deployed to <a href="http://www.shinyapps.io/" target="_blank">shinyapps.io</a>

Analysis and machine learning
  * R

Web & Graphics
  * R Shiny
  * Bootstrap
  * Custom CSS, HTML tweaks
  * JQuery / Javascript

Sources 
===============================================================================
Stock trading papers
* http://cs229.stanford.edu/proj2012/ShenJiangZhang-StockMarketForecastingusingMachineLearningAlgorithms.pdf
* https://pdfs.semanticscholar.org/4ecc/55e1c3ff1cee41f21e5b0a3b22c58d04c9d6.pdf

Trading tutorials
* https://www.toptal.com/machine-learning/s-p-500-automated-trading
* http://francescopochetti.com/stock-market-prediction-part-ii-feature-generation/

Live data feeds
* https://www.quora.com/What-are-some-good-APIs-to-get-real-time-stock-quotes
* https://www.programmableweb.com/news/96-stocks-apis-bloomberg-nasdaq-and-etrade/2013/05/22
* http://quant.stackexchange.com/questions/32336/api-or-service-to-get-german-dax-and-uk-ftse-historical-and-or-live-minute-data
* http://quant.stackexchange.com/questions/18215/where-to-get-long-time-historical-intraday-data?rq=1

About binary options
* http://www.investopedia.com/articles/active-trading/061114/guide-trading-binary-options-us.asp
* http://www.investopedia.com/terms/b/binary-option.asp

Stock symbol guide:
* http://www.qmatix.com/XLQSymbolGuide.htm


