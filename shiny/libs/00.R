# install packages for temporal analysis
fun_loading_required_pkg <- function(package_name) {
  if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
    install.packages(package_name,repos = 'https://mirrors.pku.edu.cn/CRAN/')
    library(package_name, character.only = TRUE)
  } 
}

# loading all required pacakges in order
fun_loading_required_pkg('bslib')
fun_loading_required_pkg('shiny')
fun_loading_required_pkg('shinydashboard')
fun_loading_required_pkg('readxl')
fun_loading_required_pkg('data.table')
fun_loading_required_pkg('DT')
fun_loading_required_pkg('dplyr')
fun_loading_required_pkg('magrittr')