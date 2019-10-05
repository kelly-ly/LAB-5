shinyUI(
  pageWithSidebar(
    headerPanel("Kolada API"),
    sidebarPanel(
      selectInput("functions", "Please chose package founction:",
                  choices = c("Search By Title", 
                              "Search By ID",
                              "Search Data",
                              "Search OU")),
      
      conditionalPanel(condition = "input.functions == 'Search By Title'",
                       selectInput("title_search_type", "Type:", choices = c("kpi", "kpi_groups", "municipality", "municipality_groups", "ou")),
                       textInput("title_input_str", "Title:", ""),
                       helpText("Example:", br(),
                                "kpi: Personalkostnader;", br(),
                                "kpi_groups: funktionsnedsättning;", br(),
                                "municipality: Linköping;", br(),
                                "municipality_groups: Linköping,", br(),
                                "ou: Linköping")),
      
      conditionalPanel(condition = "input.functions == 'Search By ID'",
                       selectInput("id_search_type", "Type:", choices = c("kpi", "kpi_groups", "municipality", "municipality_groups", "ou")),
                       textInput("id_input_str", "ID:", "N00005"),
                       helpText("Example:", br(),
                                "kpi: N00005;", br(),
                                "kpi_groups: G2KPI110397;", br(),
                                "municipality: 0580;", br(),
                                "municipality_groups: G123885,", br(),
                                "ou: V15E011400101.")),
      
      conditionalPanel(condition = "input.functions == 'Search Data'",
                       textInput("data_input_kpi", "KPI:", "N00945"),
                       textInput("data_input_municipality", "Municipality:", "1860"),
                       textInput("data_input_year", "Year:", "2009,2007")),
      
      conditionalPanel(condition = "input.functions == 'Search OU'",
                       textInput("ou_input_kpi", "KPI:", "N15033,N15030"),
                       textInput("ou_input_ou", "OU:", "V15E144001301,V15E144001101"),
                       textInput("ou_input_year", "Year:", "2009,2008,2007"))

    ),
    mainPanel(
      verbatimTextOutput("summary"),
      dataTableOutput("result"),
      tags$head(tags$style("#summary{font-size: 24px; font-style:italic; color: red;}"))
    )
    )
)
