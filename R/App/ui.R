source(here::here("R","theme.R"))

## Start UI ----
ui <- dashboardPage(

  ## * HEADER ----
  header = dashboardHeader(

    title = "Data quality",

    # insert a logo on the top-right corner
    tags$li(a(
      href = "http://www.riskwave.net",
      img(
        src = "riskwave-logo.png",
        title = "Company Home", height = "25px", width = "80px"
      ),
      style = "padding-top:10px; padding-bottom:10px;"
    ),
    class = "dropdown"
    )
  ),

  ## * SIDEBAR ----
  sidebar = dashboardSidebar(

      sidebarMenu(

      menuItem("Home", tabName = "home", icon = icon("info-circle")),
      menuItem("Load File", tabName = "load_file", icon = icon("upload")),
      menuItem("Dataset Glimpse", tabName = "dataset_glimpse", icon = icon("search")),
      menuItem("Multivariate Analysis", tabName = "multivariate_analysis", icon = icon("database"),
               menuSubItem(
                  text = "Numerical distributions",
                  tabName = "numerical_distributions",
                  icon = icon("angle-right")
                ),
                menuSubItem(
                  text = "Categorical distributions",
                  tabName = "categorical_distributions",
                  icon = icon("angle-right")
                ),
                menuSubItem(
                  text = "Correlations",
                  tabName = "correlations",
                  icon = icon("angle-right")
                ),
                menuSubItem(
                  text = "Outliers",
                  tabName = "outliers",
                  icon = icon("angle-right")
                )
      ),
      menuItem("Univariate Analysis", tabName = "univariate_analysis", icon = icon("poll")),
      menuItem("Quality Cheker", tabName = "quality_checker", icon = icon("check-circle"))
    )
  ),

  ## * BODY ----
  body = dashboardBody(

    customTheme,
    
    # Attach css custom sheet to the body
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # Define UI for each table
    tabItems(
            
      ## ** load file ----
      tabItem(
        tabName = "home",
        
          fluidRow(
              
              column(5, style='padding-left:50px; padding-right:0px; padding-top:120px; padding-bottom:50px',
                     
                    offset = 0,
                    tags$div(img(src = "importance-quality.png", width="800", height="800"), style="text-align: center;")
              ),
            
              column(5, style='padding-left:50px; padding-right:0px; padding-top:120px; padding-bottom:50px',
                    offset = 1,
                    tags$div(img(src = "six-dimensions.png", width="800", height="800"), style="text-align: center;")
              )
            
          )
        ),

      
      
      ## ** load file ----
      tabItem(
          tabName = "load_file",
      
      fluidRow(
        column(7,
               
          box(
             title = span(icon("caret-right"), "Please upload a file"),     
             status = "info",
             solidHeader = F, 
             width = 12,
            
            # checkbox if file has header 
            fileInput("file1", "Choose CSV File",
                      multiple = F,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
      
            # horizontal line
            tags$hr(),
      
            # input: checkbox if file has header 
            checkboxInput("header", "Header", TRUE),
      
            # input: select separator 
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
      
            # input: select quotes
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
      
            # horizontal line 
            tags$hr(),
      
            # Input: Select number of rows to display
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
            
            )
          ),
        
        column(5,
          box(
           title = span(icon("caret-right"), "Show dataset"),     
           status = "info",
           solidHeader = F, 
           width = 12,
           tableOutput("contents")
          )
        )
      )
    ),
    


      ## ** dataset glimpse ----
      tabItem(
          tabName = "dataset_glimpse",
          
          fluidRow(
            column(3, 
                   
                  box(
                    title = span(icon("caret-right"), "Basic Description"),
                    status = "info",
                    solidHeader = F,
                    width = 12,
                    withSpinner(plotOutput("basic_description", height = 450), type=7)
                  ),
                  
                  box(
                    title = span(icon("caret-right"), "Funded Amount by Time"),
                    status = "info",
                    solidHeader = F,
                    width = 12,
                    withSpinner(plotOutput("check_types", height = 450), type=7)
                  )
            ),

            column(3,
                  
                  box(
                    title = span(icon("caret-right"), "Percentange of missing values"),
                    status = "info",
                    solidHeader = F,
                    width = 12,
                    withSpinner(plotOutput("missing_values", height = 983), type=7)
                  )
            ),
            
            column(6,
                   
                  box(
                    title = span(icon("caret-right"), "Funded Amount by Time"),
                    status = "info",
                    solidHeader = F,
                    width = 12,
                    withSpinner(uiOutput("data_summary", height = 983), type=7)
                  )
            ),

          ))
      ),
       
      ## ** multivariate_analysis ----
      tabItem(
          tabName = "multivariate_analysis",
      ),
      
      ## ** numerical_distributions ---- 
      tabItem(
          tabName = "numerical_distribution",
      ),
      
      ## ** categorical_distributions ----
      tabItem(
          tabName = "categorical_distribution",
      ),
        
      ## ** correlations ----
      tabItem(
          tabName = "correlations",
      ),
       
      ## ** outliers ----        
      tabItem(
          tabName = "outliers",
      ),
       
      ## ** univariate_analysis ----  
      tabItem(
          tabName = "univariate_analysis",
      ),
       
      ## ** quality_checker ----
      tabItem(
          tabName = "quality_checker",
      )
        
      
    )

  )
