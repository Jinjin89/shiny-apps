library(shiny)
library(shinydashboard)

# loading require components
ui_list <- list()
server_list <- list()
fun_list <- list() # functions

# loading general utils
source('libs/00.R')
source('libs/read_data.R')
source('libs/stats.R')

# stats(UI and server)
source('apps/stats/pairwise_comparison.R')

# run app
shiny::runApp('apps',port = 3838,host='0.0.0.0')
