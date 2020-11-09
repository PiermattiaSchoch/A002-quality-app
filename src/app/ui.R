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

      menuItem("Intro", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Load File", tabName = "load_file", icon = icon("upload")),
      #menuItem("Dataset Glimpse", tabName = "dataset_glimpse", icon = icon("search")),
      menuItem("Multivariate Analysis", tabName = "multivariate_analysis", icon = icon("database"),startExpanded = TRUE,

               menuSubItem(
                  text = "Data Summary",
                  tabName = "data-summary",
                  icon = icon("angle-right")
                ),

                menuSubItem(
                  text = "Missing Values",
                  tabName = "missing-values",
                  icon = icon("angle-right")
                ),

               menuSubItem(
                  text = "Numerical",
                  tabName = "numerical",
                  icon = icon("angle-right")
                ),
                menuSubItem(
                  text = "Categorical",
                  tabName = "categorical",
                  icon = icon("angle-right")
                ),
                menuSubItem(
                  text = "Correlations",
                  tabName = "correlations",
                  icon = icon("angle-right")
                )
      ),
      menuItem("Univariate Analysis", tabName = "univariate_analysis", icon = icon("poll"), startExpanded = TRUE,

               menuSubItem(
                  text = "Numerical variables",
                  tabName = "numerical_variables",
                  icon = icon("angle-right")
                ),

                menuSubItem(
                  text = "Categorical variables",
                  tabName = "categorical_variables",
                  icon = icon("angle-right")
                )
               ),
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

      ## ** introduction ----
      tabItem(
        tabName = "introduction",

          fluidRow(

              column(5, style='padding-left:50px; padding-right:0px; padding-top:120px; padding-bottom:50px',

                    offset = 0,
                    tags$div(img(src = "importance-quality.png", width="820", height="800"), style="text-align: center;")
              ),

              column(5, style='padding-left:50px; padding-right:0px; padding-top:120px; padding-bottom:50px',
                    offset = 1,
                    tags$div(img(src = "six-dimensions.png", width="820", height="800"), style="text-align: center;")
              )

          )
        ),



      ## ** load file ----
      tabItem(
          tabName = "load_file",

      fluidRow(
        column(6,

          box(
             #title = span(icon("caret-right"), "Please upload a file"),
             status = "info",
             solidHeader = F,
             width = 12,

             helpText(icon("caret-right"),"Please upload a file (Default max. file size is 5MB)"),

             # checkbox if file has header
             fileInput("file1", "",
                      multiple = F,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

             # horizontal line
             tags$hr(),


             helpText(icon("caret-right"),"Format your data"),
             br(),

              # input: checkbox if file has header
              checkboxInput("header", "Header", TRUE),

              # input: select separator
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ";"),

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
                           choices = c(Head = "head"),
                           selected = "head")

              ),

            box(
             #title = span(icon("search"), "Show formatted dataset"),
             status = "info",
             solidHeader = F,
             width = 12,

             helpText(icon("search"), "Check how the dataset has been imported"),
             reactableOutput("contents", height = 350)
          )
        ),


        column(6,

              # Dynamic valueBoxes
              valueBoxOutput("Dataframe"),

              valueBoxOutput("Variables"),

              valueBoxOutput("Observations"),

              box(
                    title = span(icon("caret-right"), "Basic Description"),
                    status = "info",
                    solidHeader = F,
                    width = 12,
                    withSpinner(plotOutput("basic_description", height = 390), type=7)
                  ),

              box(
                title = span(icon("caret-right"), "Data Types"),
                status = "info",
                solidHeader = F,
                width = 12,
                withSpinner(plotOutput("check_types", height = 345), type=7)
              )


              )
      )

    ),



      ## ** dataset glimpse ----
      # tabItem(
      #     tabName = "dataset_glimpse",
      #
      #     fluidRow(
      #       column(3,

                  # box(
                  #   title = span(icon("caret-right"), "Basic Description"),
                  #   status = "info",
                  #   solidHeader = F,
                  #   width = 12,
                  #   withSpinner(plotOutput("basic_description", height = 450), type=7)
                  # ),

                  # box(
                  #   title = span(icon("caret-right"), "Data Types"),
                  #   status = "info",
                  #   solidHeader = F,
                  #   width = 12,
                  #   withSpinner(plotOutput("check_types", height = 450), type=7)
                  # )
      #      ),

      #       column(3,
      #
      #             box(
      #               title = span(icon("caret-right"), "Percentange of missing values"),
      #               status = "info",
      #               solidHeader = F,
      #               width = 12,
      #               withSpinner(plotOutput("missing_values", height = 983), type=7)
      #             )
      #       ),
      #
      #       column(6,
      #
      #             box(
      #               title = span(icon("caret-right"), "Data Summary"),
      #               status = "info",
      #               solidHeader = F,
      #               width = 12,
      #               withSpinner(uiOutput("data_summary", height = 983), type=7)
      #             )
      #       )
      #
      #     )
      # ),

      ## ** data summary ----
      tabItem(
          tabName = "data-summary",

          fluidRow(

            column(6,
                   offset = 3,
            box(
                title = span(icon("caret-right"), "Data Summary"),
                status = "info",
                solidHeader = F,
                width = 12,
                withSpinner(uiOutput("data_summary", height = 983), type=7)
                )
            )
          )
      ),

      ## ** missing-values ----
      tabItem(
          tabName = "missing-values",

          fluidRow(

                column(8,
                       offset = 2,
                  box(
                    title = span(icon("caret-right"), "Percentange of missing values"),
                    status = "info",
                    solidHeader = F,
                    width = 12,
                    withSpinner(plotOutput("missing_values", height = 983), type=7)
                  )
                )

          )
      ),


      ## ** numerical analysis ----
      tabItem(
          tabName = "numerical",


            fluidRow(

            tabBox(
             title = tagList(shiny::icon("gear"), "Click on tabs for different visualizations"),
             id = "tabset1",
             side="right",
             width = 12,
             selected = "Histograms",
             tabPanel("Outliers (Mean Absolute Deviation)",withSpinner(plotOutput("outliers_mad", height = 983), type=7)),
             tabPanel("Outliers (Z-score)",withSpinner(plotOutput("outliers_zscore", height = 983), type=7)),
             tabPanel("QQ-plots",withSpinner(plotOutput("qqplots", height = 983), type=7)),
             tabPanel("Densities",withSpinner(plotOutput("densities", height = 983), type=7)),
             tabPanel("Histograms",withSpinner(plotOutput("numerical_histograms", height = 983), type=7))
            )
          )

      ),

      ## ** categorical_distributions ----
      tabItem(
          tabName = "categorical",

         fluidRow(

            tabBox(
             title = tagList(shiny::icon("gear"), "Click on tabs for different visualizations"),
             id = "tabset2",
             side="right",
             width = 12,
             selected = "Frequencies",
             tabPanel("Pie-charts",withSpinner(highchartOutput("pie"), type=7)),
             tabPanel("Frequencies",withSpinner(plotOutput("frequencies", height = 1200), type=7))
            )
          )

      ),

      ## ** correlations ----
      tabItem(
          tabName = "correlations",

           fluidRow(

            box(
             title = span(icon("caret-right"), "Correlations"),
             status = "info",
             solidHeader = F,
             width = 12,
             withSpinner(plotOutput("corr", height = 983), type=7)
            )
          )
      ),

      ## ** outliers ----
      tabItem(
          tabName = "outliers",

         # fluidRow(
         #
         #    box(
         #     title = span(icon("caret-right"), "Correlations"),
         #     status = "info",
         #     solidHeader = F,
         #     width = 12,
         #     withSpinner(plotOutput("corr", height = 983), type=7)
         #    )
         #  )
      ),

      ## ** univariate_analysis ----
      tabItem(
          tabName = "numerical_variables",

          fluidRow(
            column(2,

            box(
              title = "",
              status = "info",
              solidHeader = F,
              width = 12,
              selectInput("univariate_num", span(icon("caret-right"), "Pick a variable"), ""),
            )
            ),
            column(5,

            box(
              title = "",
              status = "info",
              solidHeader = F,
              width = 12,
              withSpinner(highchartOutput("hists", height = 400), type=7)
            )
            ),

            column(5,

            box(
              title = "",
              status = "info",
              solidHeader = F,
              width = 12,
              withSpinner(highchartOutput("dens", height = 400), type=7)
            )
            )


            )
          ),

        tabItem(
          tabName = "categorical_variables",

          fluidRow(
            column(2,

            box(
              title = "",
              status = "info",
              solidHeader = F,
              width = 12,
              selectInput("univariate_cat", span(icon("caret-right"), "Pick a variable"), ""),
            )
            ),

            column(5,

            box(
              title = "",
              status = "info",
              solidHeader = F,
              width = 12,
              withSpinner(highchartOutput("freq"), type=7)
            )
            ),

            column(5,

            box(
              title = "",
              status = "info",
              solidHeader = F,
              width = 12,
              withSpinner(highchartOutput("pie_hc"), type=7)
            )
            )
            )
          ),

      ## ** quality_checker ----
      tabItem(
          tabName = "quality_checker",


          fluidRow(

              column(5, style='padding-left:50px; padding-right:0px; padding-top:120px; padding-bottom:50px',

                    offset = 0,

                    actionButton(inputId = "email1",
                    icon = icon("envelope", lib = "font-awesome"),
                    a("Contact Admin", href="mailto:piermattia.schoch@riskwave.net")),

                    hr(),
                    tags$div(img(src = "data-valid.png", width="700", height="700"), style="text-align: center;")
              )


      )


    )

  )
)
)
