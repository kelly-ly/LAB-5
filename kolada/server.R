library("assignment5Package")

shinyServer(
  function(input, output, session){
    library(assignment5Package)
    kolada<-kolada_api$new()
    
    func_input <- reactive({
      switch(input$functions,
             "Search By Title" = kolada$search_with_title(input$title_search_type, input$title_input_str),
             "Search By ID" = kolada$search_with_id(input$id_search_type, input$id_input_str),
             "Search Data" = kolada$search_data(input$data_input_kpi, input$data_input_municipality, input$data_input_year),
             "Search OU" = kolada$search_ou(input$ou_input_kpi, input$ou_input_ou, input$ou_input_year))
    })
    
    output$result <- renderDataTable({
      func_input()
    })
    output$summary <- renderText({
      paste("Function: ", input$functions, "\nNumber of results: ", nrow(func_input()))
    })
  }
)