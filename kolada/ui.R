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
                       selectizeInput("id_str", label = "Municipality ID", choices = get_all_municipality()$id)
                       ),
      conditionalPanel(condition = "input.functions == 'Get Data'",
                       textInput("exact", "Exact", ""),
                       selectInput("peryear", label = "Select Year:", choices = 1960:2019),
                       selectizeInput("permunicipality", label = "Municipality ID", choices = get_all_municipality()$id))
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      tableOutput("result"),
      tags$head(tags$style("#summary{font-size: 24px; font-style:italic; color: red;}"))
    )
  )
)