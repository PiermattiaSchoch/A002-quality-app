## Theme  ----
# Options for spinner
options(spinner.color = "#4b6cb7", spinner.color.background = "#ffffff", spinner.size = 1)

# Create custom theme
customTheme <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Roboto"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"

  ### header
  ,logoBackColor = "#182848" # cambia header alto sx
  

  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"

  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"

  # ,headerButtonBackColor = "#182848"
  # ,headerButtonIconColor = "rgb(255, 255, 255)"          # colore icona 
  # ,headerButtonBackColorHover = "rgb(255, 255, 255)"
  # ,headerButtonIconColorHover = "#182848"                # hover colore
  # 
  # #,headerBackColor = "rgb(238,238,238)" # header dopo icona
  # ,headerBackColor = cssGradientThreeColors(
  #   direction = "right"
  #   ,colorStart = "#182848"
  #   ,colorMiddle = "#4b6cb7"
  #   ,colorEnd = "#4b6cb7"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 50
  #   ,colorEndPos = 100
  # )
  # ,headerBoxShadowColor = "rgb(208, 207, 252)" # ombra sotto
  # ,headerBoxShadowSize = "2px 2px 2px"
  # 
  
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "#182848"
    ,colorMiddle = "#4b6cb7"
    ,colorEnd = "#4b6cb7"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 2

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"

  ,sidebarUserTextColor = "rgb(255,255,255)"

  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"

  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "#4b6cb7"  #violetto linee
  ,sidebarTabBorderWidth = 1

  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(255,255,255)"
    ,colorMiddle = "rgb(255,255,255)"
    ,colorEnd = "rgb(255,255,255)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"

  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(255,255,255)"
    ,colorMiddle = "rgb(255,255,255)"
    ,colorEnd = "rgb(255,255,255)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "#4b6cb7"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"

  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "#4b6cb7"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"

  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5

  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5

  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"

  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"

  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1

)


### creating custom logo object
customLogo <- shinyDashboardLogoDIY(

  boldText = "Linked Finance"
  ,mainText = ""
  ,textSize = 16
  ,badgeText = "Demo"
  ,badgeTextColor = "#4b6cb7"
  ,badgeTextSize = 2
  ,badgeBackColor = "white"
  ,badgeBorderRadius = 5

)


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