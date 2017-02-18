Dataincubator Capstone project: Automated binary option trading (work in progress)
===============================================================================
https://glueckert.shinyapps.io/binary_options/

Goal:

* predict hourly/daily stock direction using machine learning and perform API based automated trading on binary option platforms. 

The webapp shows:

* explantion and resources regarding binary options
* example simulation for binary option trading
* visualizations of hourly directional stock movements
* backetesting results and benchmark
* dashboard of live trading results (wor in progress)

Data used

* for end-of-day data: yahoo finance via [quantmod package](https://cran.r-project.org/web/packages/quantmod/quantmod.pdf)
* for minute data: [activetick API](http://wwww.activetick.com)

See below for [details](#details) and [tech stack](#tech-stack)

Details
===============================================================================
Similar countries to your top choice are found using machine learning (affinity
clustering). See the notebook `/code/python-explore/affinitypropagation.ipynb`


Key directories
===============================================================================

`/dir1`

explanation dir1

`/dir2`

explanation dir2

Tech stack
===============================================================================
Briefly, this is a Python-Flask web application with a PostgreSQL database and bokeh-driven data explorer, all deployed to Heroku. The web page is styles with <a href="http://getbootstrap.com" target="_blank">Bootstrap</a>. <a href="{{ url_for('static', filename='ico/favicon.ico') }}" target="_blank">Favicon</a> is my own, created on <a href="https://www.fiftythree.com" target="_blank">Paper for iOS</a> and converted with <a href="http://www.favicon.cc/" target="_blank">favicon.cc</a></p>

Analysis and machine learning
  * R
  * Python

Web & Graphics
  * Bootstrap
  * Custom CSS, HTML tweaks

Databases
* None used, just saved as CSVs

Methodology notes
===============================================================================
global market hyptothesis:

* global market
* global market
* global market

feature generation:

* features
* features
* features

modelling:

* model
* model
* model

Sources 
===============================================================================
Papers on stock trading:
* http://data.worldbank.org/data-catalog/CPIA

Other sources
* http://data.worldbank.org/data-catalog/CPI
