#!/usr/bin/env Rscript
options(shiny.port = 18888)

####################################
# function consolidate
####################################
ft_loading_required_pkg <- function(package_name) {
  if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
    install.packages(package_name,repos = 'https://mirrors.pku.edu.cn/CRAN/')
    library(package_name, character.only = TRUE)
  } 
}

ft_read_data <- function(input_file,input_text,header,sheet_name,aggr,group){
  tryCatch(
    {
      # 1) reading data
      if(input_text!=''){
        message('**   loading data using input_text')
        df = data.table::fread(input_text,header = header)
      }else if(grepl('\\.xlsx$|\\.xls$',input_file)){
        message('**   loading data using readxl',', header: ',header,', sheet: ',sheet_name)
        df = readxl::read_excel(input_file,sheet = sheet_name,col_names=header)
      }else if(grepl('\\.$csv|\\.txt$|\\.tsv$',input_file)){
        message('**  loading data using fread')
        df = data.table::fread(input_file,header = header)
      }else{
        return('error in data.format!')
      }
      message('**  glance data')
      print(head(df))
      
      # 2) data pp
      all_variables = colnames(df)
      
      if(aggr){
        message('**    aggaragated data:')
        if(group %in% all_variables){
          message('**    group name: ',group,': ',length(as.character(df[[group]])))
          value_var =dplyr::setdiff(all_variables,group)[1]
          message('**    value name: ',value_var,": ",length(as.numeric(df[[value_var]])))
          length(as.character(df[[group]]))
          
          df_clean <- data.frame(
            group = as.character(df[[group]]),
            value = as.numeric(df[[value_var]])
          )
        }else{
          message('**    group name not found!')
          return('group name not found')
        }
      }else{
        df_clean = as.data.frame(matrix(ncol = 2,nrow = 0))
        colnames(df_clean) <- c("group",'value')
        # 1) get each data
        message('**    variables found: ',length(all_variables))
        for(each_var in all_variables){
          df_clean <- rbind(
            df_clean,
            data.frame(value = df[[each_var]]) %>% 
              dplyr::mutate(group = each_var) %>% 
              dplyr::select(group,value)
          )
        }
      }
      # final data set
      df_clean$group = as.character(df_clean$group)
      df_clean$value = as.numeric(df_clean$value)
      df_clean <- 
        df_clean %>% 
        dplyr::filter(!is.na(.$group)) %>% 
        dplyr::filter(!is.na(.$value))
      message('**    df_clean records: ',nrow(df_clean))
      
      return(df_clean)
    },
    error = function(e) return('error in reading data!')
  )
}


ft_test <- function(input_df,methods){
  tryCatch(
    {
      
      
      all_variables = input_df$group %>% unique
      init_res = utils::combn(all_variables,2) %>% 
        t %>% 
        as.data.frame() %>% 
        magrittr::set_colnames(c('group1','group2')) %>% 
        dplyr::mutate(group1_median=NA,group2_median=NA,pval = NA,methods = methods)
      for(i in (1:nrow(init_res))){
        
        group1_name = init_res$group1[i]
        group2_name = init_res$group2[i]
        group1_value = input_df %>% dplyr::filter(group == group1_name) %>% dplyr::pull(value)
        group2_value = input_df %>% dplyr::filter(group == group2_name) %>% dplyr::pull(value)
        message('**    current comparsion: ',group1_name,' - ',group2_name)
        init_res[i,'group1_median'] <- median(group1_value)
        init_res[i,'group2_median'] <- median(group2_value)
        
        if(methods == 't-test'){
          message('**    perform using t-test')
          res = stats::t.test(x = group1_value,y = group2_value)
          init_res[i,'pval'] <- res$p.value
        }else if(methods == 'Wilcoxon'){
          message('**    perform using Wilcoxon')
          res = stats::wilcox.test(x = group1_value,y = group2_value)
          init_res[i,'pval'] <- res$p.value
          
        }else{
          message('to be contined')
        }
      }
      return(init_res)
    },
    error = function(e){return('test error!')}
  )
}

#df_test = fread('Desktop/test_aggr.tsv') %>% na.omit() %>% set_colnames(c('group','value'))
#ft_test(df_test,'t-test')
#ft_test(df_test,'Wilcoxon')

####################################
# install and loading required packages
####################################
ft_loading_required_pkg('bslib')
ft_loading_required_pkg('shiny')
ft_loading_required_pkg('shinydashboard')
ft_loading_required_pkg('readxl')
ft_loading_required_pkg('data.table')
ft_loading_required_pkg('DT')
ft_loading_required_pkg('dplyr')
ft_loading_required_pkg('magrittr')


####################################
# UI
####################################

ui <- fluidPage(
  # title
  titlePanel("pairwise comparison analysis"),
  
  fluidRow(
    # Input panel: left column with 4 units of width (out of 12)
    column(3, 
           # For example, a file input
           fileInput('file', 'Choose File', accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx")),
           textInput('sheet_name','sheet name(for excel file):',value = 'Sheet1'), 
           textAreaInput("textArea", "or paste data beblow:",placeholder= "paste data here..."),
           
           # methods
           div(style = "display: flex; justify-content: space-between;",
               checkboxInput('header','Header',TRUE),
               checkboxInput('aggregated','aggregated',FALSE), 
               # data types
           ),
           
           textInput('group','group(for aggregated):',value = 'group',placeholder = 'column name of group'),
           selectInput('methods','which test to use',choices = c("t-test",'Wilcoxon')),
           hr(),
           checkboxInput('data','showing data'),
           #checkboxInput('dataSummary','showing data summary'),
           
           # run
           actionButton("runButton", "Run Analysis"),
           
           # message
           hr(),
           textOutput('infos'),
           tags$head(tags$style("#infos{color: red;}"))
    ),
    #column(1),
    # Results panel: Right column with 8 units of width (out of 12)
    column(7,
           # 1) results
           dataTableOutput('results',width = '90%'),  
           hr(),
           # 2) show data summary
           #dataTableOutput('table1'),
           
           # 3) show input data
           dataTableOutput('data_raw')
           
    ),
    column(1)
  )
)

####################################
# server
####################################

server <- function(input, output,session){
  observeEvent(
    input$runButton,
    {
      #######################################
      # 1) loading data infos
      #######################################
      # message info
      message('******************************************')
      infos = ''
      # 1) getting
      if(length(input$file) > 0){
        data_file = normalizePath(input$file$datapath)  
        message('*    file is stored at: ', data_file)
      }else{
        data_file = NULL
      }
      # 1) loading data
      data_raw <- ft_read_data(
        input_file = data_file,
        input_text = input$textArea,
        header = input$header,
        sheet_name = input$sheet_name,
        group = input$group,
        aggr = input$aggregated)
      
      if(!is.data.frame(data_raw)){
        infos = paste0(infos,data_raw)
      }else{
        if(input$data){
          output$data_raw <- 
            DT::renderDataTable(
              data_raw,
              rownames=F,
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              class = "display")
        }
      }
      
      # 2) data sorting
      message('*     is data aggregated: ', input$aggregated)
      
      #######################################
      # 2) test step
      #######################################
      stat_test_res = ft_test(data_raw,methods = input$methods)
      if(is.data.frame(stat_test_res)){
        output$results = DT::renderDataTable(
          stat_test_res,
          extensions = 'Buttons',
          options = list(
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          class = "display")
      }else{
        infos = paste0(infos,' ',stat_test_res)
        stat_test_res = data.frame(error = 'on test error')
        output$results <- DT::renderDataTable(stat_test_res)
        message('*     test error')
      }
      
      #######################################
      # 3) final data
      #######################################
      message(infos,'\n')
      output$infos = renderText(infos)
    }
  )
}

# run the applications
shinyApp(ui = ui, server = server)
