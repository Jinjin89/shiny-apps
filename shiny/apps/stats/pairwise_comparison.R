# funs 

# UI
ui_list$ui_sidebar_pairwise_comparison <- 
  menuItem(
    text="params/non-params test",
    tabName = "stats_pairwise_comparison")

ui_list$ui_body_pairwise_comparison <- 
  tabItem(
    tabName = "stats_pairwise_comparison",
    h2("stats_pairwise_comparison"), 
    # >> data panel
    box(
      # box params
      width = 5,
      # data
      textAreaInput(
        "data_pairwise_input",
        "Paste data beblow:",
        #value = 'a	b	c\n1	4	8\n2	5	9\n3	6	10\n7	12\n5\n5',
        placeholder= "paste data here...",
        height = "150px"),
      
      div(style = "display: flex",
          checkboxInput('data_pairwise_header','Header',TRUE),
          HTML("&nbsp;&nbsp;"),
          checkboxInput('data_pairwise_wide','wide',TRUE)
      ),
      
      textInput('data_pairwise_group','group(for aggregated):',value = 'group',placeholder = 'column name of group'),
      selectInput('data_pairwise_method','which test to use',choices = c("t-test",'Wilcoxon')),
      actionButton("data_pairwise_run", "Run Analysis")
    ),
    # >> results panel
    box(
      width = 7,
      shiny::uiOutput('res_pairwise')
    )
  )

# functions for server
server_list$running_error <- function(error_message = '') {
  renderUI(
    tagList(
      h3("Runing error, pls check the input!"),
      if(error_message!=''){
        h4(paste0("Error: ",error_message))
      }
    )
    )}
server_list$res_pairwise_placeholder <- renderUI(h3("Paste data on the left panel, then click `Run analysis`"))
server_list$fun_pairwise <-function(input_text,header,wide,group,test_method){
  tryCatch(
    {
      # read data
      message("> start analysis...")
      df_read <-  data.table::fread(input_text,header = header,fill=T) # reading the data
      message(">>> input data ")
      print(df_read)
      all_variables <- colnames(df_read)
      # wide pivot into longer format
      if(wide){
        df_read <- tidyr::pivot_longer(df_read,cols = dplyr::all_of(all_variables),names_to = 'group',values_to = 'value')
        group_col <-  'group'
        value_col <-  'value'
      }else{
        group_col <- group
        value_col <- dplyr::setdiff(all_variables,group_col)[1]
        df_read <- df_read %>% dplyr::select(dplyr::all_of(c(group_col,value_col)))
      }
      # make data clean
      suppressWarnings({
        df_read[[value_col]] <- as.numeric(df_read[[value_col]])
        df_read <- na.omit(df_read)
      })
      message(">>> clean data ")
      print(df_read)
      message(">>> clean data(group): ", group_col)
      message(">>> clean data(value): ", value_col)
      # reget all vars
      all_variables <- unique(df_read[[group_col]])
      if(length(all_variables) <2){
        return(list(
          status = F,
          data = 'Too few groups'
        ))
      }
      # all comparison
      message(">> starting comparison, here is all the possible combinations:")
      all_possible_comb <- utils::combn(all_variables,2) %>% 
        t %>% 
        as.data.frame() %>% 
        set_colnames(c("Group1","Group2")) %>% 
        mutate(Group1_median = NA,Group2_median = NA,pvalue =NA)
      #print(all_possible_comb)
      # run stats
      for(i in 1:nrow(all_possible_comb)){
        message(">>> analysis loop: ",i)
        x_name <- all_possible_comb$Group1[i]
        y_name <- all_possible_comb$Group2[i]

        x_val <- df_read[[value_col]][df_read[[group_col]] == x_name]
        y_val <- df_read[[value_col]][df_read[[group_col]] == y_name]
        
        pval <- fun_list$fun_stats_test(x_val,y_val,test_method)
        
        # merge data into res
        all_possible_comb[i,'Group1_median'] <- median(x_val)
        all_possible_comb[i,'Group2_median'] <- median(y_val)
        all_possible_comb[i,'pvalue'] <- pval

      }
      print(all_possible_comb)
      return(list(status = T,
                  data = all_possible_comb,
                  group_df = dplyr::count(df_read,!!as.name(group_col))))
    },
    error = function(e) return(list(status = F,data = "running error"))
  )
}

# 
