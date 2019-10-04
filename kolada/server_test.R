shinyServer(
  function(input, output){
    library(assignment5Package)
    k1<-kolada_api$new()
    func_input <- reactive({
      switch(input$functions,
             "Search Area" = k1$search_area(input$area_name),
             "Search KPI" = k1$search_KPI(input$KPI_str),
             "Search Enheter" = k1$search_enheter(input$id_str),
             "Get Data" = k1$get_data(input$exact))
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

