library(shiny)
library(shinydashboard)
function(input, output) {
    ############################
    # pairwise comparison
    ############################
    output$res_pairwise <- server_list$res_pairwise_placeholder
    
    observeEvent(input$data_pairwise_run, {
        # results
        input_data = isolate(input$data_pairwise_input)
        current_res <- server_list$fun_pairwise(
            input_text = input_data,
            header = input$data_pairwise_header,
            wide = input$data_pairwise_wide,
            group = input$data_pairwise_group,
            test_method = input$data_pairwise_method
        )
        if(current_res$status){
            output$res_pairwise <-  renderUI({
                tagList(
                    h3("Results :"),
                    hr(),
                    DT::datatable(
                        current_res$data,
                        extensions = 'Buttons',
                        options = list(
                            paging = TRUE,
                            searching = TRUE,
                            fixedColumns = TRUE,
                            ordering = TRUE,
                            dom = 'Bfrtip',
                            buttons = c('copy')
                        ),),
                    hr(),
                    h3("Group sample size :"),
                    DT::datatable(current_res$group_df),
                )
            })
        }else{
            output$res_pairwise <-  server_list$running_error(current_res$data)
        }
        
    })
}
