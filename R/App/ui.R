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
      
      ## ** home ----
      tabItem(
          tabName = "home",
      ),
      
      ## ** dataset glimpse ----
      tabItem(
          tabName = "dataset_glimpse",
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
)