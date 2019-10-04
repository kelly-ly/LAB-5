shinyUI(
  pageWithSidebar(
    headerPanel("Kolada API"),
    sidebarPanel(
      selectInput("functions", "Please chose package founction:",
                  choices = c("Search Area", 
                              "Search KPI",
                              "Search Enheter",
                              "Get Data"
                  )),
      conditionalPanel(condition = "input.functions == 'Search Area'",
                       textInput("area_name", "Area Name", "")),
      conditionalPanel(condition = "input.functions == 'Search KPI'",
                       textInput("KPI_str", "KPI", "")),
      conditionalPanel(condition = "input.functions == 'Search Enheter'",
                       selectizeInput("id_str", label = "Municipality ID", choices = NULL)
                       ),
      conditionalPanel(condition = "input.functions == 'Get Data'",
                       selectizeInput("search_type", label = "Select Search type:", choices = c("Exact", "Peryear", "Municipality")),
                       textInput("data_kpi", label = "KPI:", ""),
                       conditionalPanel(condition = "input.search_type == 'Exact'",
                                        selectInput("data_peryear", label = "Select Year:", choices = 1960:2019),
                                        selectizeInput("data_permunicipality", label = "Municipality ID", choices = NULL)),
                       conditionalPanel(condition = "input.search_type == 'Peryear'",
                                        selectInput("data_peryear", label = "Select Year:", choices = 1960:2019)),
                       conditionalPanel(condition = "input.search_type == 'Municipality'",
                                        selectizeInput("data_permunicipality", label = "Municipality ID", choices = NULL))
                       )
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      tableOutput("result"),
      tags$head(tags$style("#summary{font-size: 24px; font-style:italic; color: red;}"))
    )
  )
)
