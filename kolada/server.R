shinyServer(
  function(input, output){
    func_input <- reactive({
      switch(input$functions,
             "Search Area" = search_area(input$area_name),
             "Search KPI" = search_KPI(input$KPI_str),
             "Search Enheter" = search_enheter(input$id_str),
             "Get Data" = get_data(input$exact))
    })
    
    output$result <- renderTable({
      functions <- func_input()
      print(functions)
    })
    
    output$summary <- renderText({
      paste("Function: ", input$functions, "\nNumber of results: ", nrow(func_input()))
    })
  
  }
)

