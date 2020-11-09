#source(here::here("R", "libs.R"))

# Load dataset
#df = readRDS(here::here("data","dataset.rds"))

library(here)
library(styler)
library(devtools)

# Data manipulation 
library(dplyr)
library(tidyverse)

# Data visualization 
library(ggplot2)
#library(highcharter)

# Shiny
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(reactable)


library(htmltools)


#########################

# 
# theData = df
# theData_df = data.frame(theData)
# aggregate = as.data.frame(theData_df %>% 
#                               group_by_at(names(theData_df)[2], .groups = 'drop') %>%
#                               mutate(count = n())  %>%
#                               select(all_of(names(theData_df)[2]), count) %>%
#                               distinct_at(vars(names(theData_df)[2]), .keep_all = T) %>%
#                               ungroup() %>%
#                               slice(1:8))
# 
#  hc_fun2(aggregate, "job", names(aggregate)[2], type="pie")


# Import utils
#source(here::here("R","utils","utils-multivariate.R"))

#devtools::install_github("laresbernardo/lares")
#library(lares)
library(data.table)
library(tidyquant)
#library(DataExplorer)
library(visdat)
library(summarytools)

## missing values ----
plot_missing_mycolor = function (data, 
                                 group = list(Good = 0.05, Okay = 0.4, Poor = 0.8, Scarce =  1), 
                                 geom_label_args = list(), 
                                 title = NULL,
                                 ggtheme = theme_gray(), 
                                 theme_config = list(legend.position = c("bottom"))) {
  
        pct_missing <- Band <- NULL
        missing_value <- data.table(profile_missing(data))
        group <- group[sort.list(unlist(group))]
        
        invisible(lapply(seq_along(group), function(i) {
        
          if (i == 1) {
          missing_value[pct_missing <= group[[i]], `:=`(Band,
             names(group)[i])]
        } else {
        missing_value[pct_missing > group[[i - 1]] & pct_missing <= 
         group[[i]], `:=`(Band, names(group)[i])]
        }
        }))
        
        output <- ggplot(missing_value, aes_string(x = "feature", 
        y = "num_missing", fill = "Band")) + geom_bar(stat = "identity") + 
        scale_fill_manual("Band", values = c("Good"="green2","Okay"="gold","Poor"="darkorange","Scarce"="firebrick2")) + coord_flip() + xlab("Features") + 
        ylab("Missing Rows")
        geom_label_args_list <- list(mapping = aes(label = paste0(round(100 * pct_missing, 2), "%")))
        output <- output + do.call("geom_label", c(geom_label_args_list, geom_label_args))
        class(output) <- c("single", class(output))
        plotDataExplorer(plot_obj = output, title = title, ggtheme = ggtheme, theme_config = theme_config)
        }


## Plot a series of histograms ----

library(gridExtra)
library(ggcorrplot)

green = "#018F83"
positano = "#DE9F06"
darkblu = "#334357"

plot_series_hist <- function(df, xvar){
    
     df$title = toupper(xvar)
    
     p = ggplot(df, aes_string(x=xvar)) +
        geom_histogram(bins=30, color=positano, fill=green) +
          theme_tq() +
          scale_color_tq() +
          facet_wrap(~title)+
          labs(title="", x="", y="") +
          theme(axis.text = element_text(family="Courier", colour=darkblu, size=12, face="bold"),
                strip.text.x = element_text(size = 14, colour = "white", angle = 0),
                strip.background =element_rect(fill=green))
     
     print(p)
}

## Plot a series of densities ----
plot_series_qq <- function(xvar){
    
     dataset = dataset %>% select_if(is.numeric)    
  
     dataset$title = toupper(xvar)
    
     p = ggplot(dataset, aes_string(sample=xvar)) +
          geom_density(position = "identity", color=blue_end, fill=blue_start) +
                    theme_tq() +
                      scale_color_tq() +
                        facet_wrap(~ title)+
                          labs(title="", x="", y="") +
                            theme(axis.text = element_text(family="Courier", colour=darkblue, size=10, face="bold"),
                                  strip.text.x = element_text(size = 14, colour = "white", angle = 0),
                                   strip.background =element_rect(fill=green))
     
     print(p)
}

## Plot a series of qqplot ----
plot_series_qq <- function(xvar){
    
     dataset = dataset %>% select_if(is.numeric)    
  
     dataset$title = toupper(xvar)
    
     p = ggplot(dataset, aes_string(sample=xvar)) +
          geom_qq(distribution=qnorm, fill=green) +
                  geom_qq_line(line.p = c(0.25, 0.75), col = yellow) +
                    theme_tq() +
                      scale_color_tq() +
                        facet_wrap(~ title)+
                          labs(title="", x="", y="") +
                            theme(axis.text = element_text(family="Courier", colour=darkblue, size=10, face="bold"),
                                  strip.text.x = element_text(size = 14, colour = "white", angle = 0),
                                   strip.background =element_rect(fill=green))
     
     print(p)
}



## Plot a series of frequency plots ----
plot_series_freq = function(var_x){
  
       dataset = dataset %>% select_if(is.character)
       
       dataset$title = toupper(var_x)
  
       var_x_expr = enquo(var_x)

       if((nrow(dataset)!=length(unique(dataset[[var_x]])))) {
    
       data = dataset %>%
                select(all_of(var_x), title) %>%
                  group_by_at(1, .groups = 'drop') %>%
                    mutate(count = n()) %>% 
                      arrange(desc(count)) %>% 
                        distinct(!!var_x, .keep_all=T) %>% 
                          ungroup() %>% 
                            slice_max(count, n=20)
  
     p1 <-  ggplot(data, aes(x=fct_reorder(!! rlang::ensym(var_x), count), y=count))

     
     p1 <- p1 +
            geom_bar(stat="identity",fill=green, width=.5) +
              theme_tq() +
              facet_grid(. ~ title)+
              scale_x_discrete(label=function(x) abbreviate(x, minlength=10)) + 
              coord_flip()+
              labs(title="", x="", y="") +
              theme(axis.text = element_text(family="Courier", colour=darkblu, size=12, face="bold"),
                     axis.text.x = element_text(angle=0, hjust=1),
                    strip.text.x = element_text(size = 14, colour = "white", angle = 0),
                    strip.background =element_rect(fill=green))
  
    print(p1)  
  }
}

## Outliers ---- 

# Z-score
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

# Mean abs error
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

## Plot a series of outliers 
plot_series_outliers = function(data){
  
  data[,2] = as.character(data[,2])
  data = data[order(data[,1]),]

   p = data %>%
        ggplot(aes(x= "", y=data[,1])) +
          geom_violin() +
            geom_jitter(position=position_jitter(0.2), alpha=0.5, aes(color = data[,2]))+
              scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red"), 
                                 name="",
                                 labels=c("Outlier", "Not outlier")) +
              theme_tq() +
                facet_grid(. ~ title)+
                  labs(title="", x="", y="") +
                    theme(axis.text = element_text(family="Courier", colour=darkblu, size=12, face="bold"),
                          strip.text.x = element_text(size = 14, colour = "white", angle = 0))

   print(p) 
}

## Highcharter plots ----

hc_fun2 <- function(df, x, y, type) {
  x <- enexpr(x)
  y <- enexpr(y)
  hchart(df, 
         type = type,
         hcaes(!!x, !!y),
         name = "ciao"
  )
} 


#########################

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
            
            column(8, 
                   offset = 2,
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

##############


options(shiny.maxRequestSize=30*1024^2)
library(visdat)


server <- function(input, output, session) {
  
  ## * get dataframe ----
  
  dataset = reactive({
    
    req(input$file1)

    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    
    #df = df[1:1000,]
    
    df[1:120, 2 ] = NA
    df[200:222, 12 ] = NA
    df[341:1000, 9 ] = NA
    df[100:1000, 13 ] = NA

    df = data.frame(df)

    nums_cols = c("age","duration","cons.price.idx","cons.conf.idx","euribor3m", "nr.employed", "emp.var.rate")
    #nums_cols = c("age", "duration")
    
    df[, (names(df) %in% nums_cols)] = sapply(df[, (names(df) %in% nums_cols)], as.numeric)
    df[, !(names(df) %in% nums_cols)] = sapply(df[, !(names(df) %in% nums_cols)], as.factor)
    
    return(df)
    
    
  })
  
  ## * load file ----
  output$contents <- renderReactable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    
    react_df = data.frame(dataset())
  
    react_df[is.na(react_df)] = "NA"    
    
    return(reactable(react_df))
    
  })
  
  
 
  output$Dataframe <- renderValueBox({
    
    valueBox(
      paste0(as.character(substitute(dataset()))), 
      "Dataframe", 
      icon = icon("folder-open"),
      color = "green",
      width = 10
    )
  })
  
  output$Variables <- renderValueBox({
    
    df = data.frame(dataset())
    
    valueBox(
      paste0(ncol(df)), 
      "variables", 
      icon = icon("columns"),
      color = "light-blue",
      width = 10
    )
    
  })
  
  output$Observations <- renderValueBox({
    
    df = data.frame(dataset())

    valueBox(
      paste0(nrow(df)), 
      "observations", 
      icon = icon("list", lib = "glyphicon"),
      color = "maroon",
      width = 10
    )
  })
  
  
  output$basic_description = renderPlot({
    
    df_str(dataset(), return = "plot", quiet = F) + labs(title="")
    
    })
  

  output$check_types = renderPlot({
    
    check_types = vis_dat(dataset())
    check_types 
    # check_types + 
    #             coord_flip() + 
    #             labs(y="\n Observations \n") +
    #             scale_y_discrete(position = 'right', limits = c(0, nrow(dataset())))+
    #             scale_x_discrete(position = 'bottom')+
    #             theme(legend.position="bottom")
    
  })
  
  
  ## * multivariate analysis ----
  
  ## ** data summary ----
  output$data_summary <- renderUI({
    
    st_options(dfSummary.varnumbers	=T,
               dfSummary.labels.col = T,
               display.labels=T,
               dfSummary.valid.col=T, 
               dfSummary.na.col = T,
               headings = F,
               round.digits=1,
               bootstrap.css = FALSE,
               footnote = NA)                     
    
    out <- print(dfSummary(dataset(), 
                           graph.magnif = 0.8),
                           method = 'render',
                           max.tbl.height = 950)
  })
  
  ## ** missing values ----
  
  output$missing_values = renderPlot({
    
    plot_missing_mycolor(dataset(),
                         ggtheme=theme_tq(),
                         title="",
                         theme_config = list(legend.position = 'none')) +
                         labs(x="",y="")
    })
              
  ## ** numerical analysis ----

  output$numerical_histograms = renderPlot({
    
    data = dataset()
    nums <- unlist(lapply(data, is.numeric))  
    data = data[ , nums]
    
    blue_start = "#182848"
    blue_end = "#4b6cb7"
    
    list = NULL
    for(name in names(data)){
      
      data$title = rep(toupper(name), times=nrow(data))
      
      p = ggplot(data, aes_string(x=name)) +
          geom_histogram(bins=30, color=blue_end, fill=blue_start) +
            theme_tq() +
            scale_color_tq() +
            facet_wrap(~title)+
            labs(title="", x="", y="") +
            theme(axis.text = element_text(family="Courier", colour=darkblu, size=12, face="bold"),
                  strip.text.x = element_text(size = 14, colour = "white", angle = 0),
                  strip.background =element_rect(fill=darkblu))
  
  
      list[[name]] = p
    
    }
    
    grid.arrange(grobs=list)
    
  })
  
  
  output$densities = renderPlot({
  
    data = dataset()
    nums <- unlist(lapply(data, is.numeric))  
    data = data[ , nums]
    
    blue_start = "#182848"
    blue_end = "#4b6cb7"
    
    list = NULL
    for(name in names(data)){
      
      data$title = rep(toupper(name), times=nrow(data))
      
      p = ggplot(data, aes_string(x=name)) +
          geom_density(position = "identity", color=blue_end, fill=blue_start) +
            theme_tq() +
            scale_color_tq() +
            facet_wrap(~title)+
            labs(title="", x="", y="") +
            theme(axis.text = element_text(family="Courier", colour=darkblu, size=12, face="bold"),
                  strip.text.x = element_text(size = 14, colour = "white", angle = 0),
                  strip.background =element_rect(fill=darkblu))
  
  
      list[[name]] = p
    
    }
    
    grid.arrange(grobs=list)
    
  })
  
   
  output$qqplots = renderPlot({
  
    data = dataset()
    nums <- unlist(lapply(data, is.numeric))  
    data = data[ , nums]
    
    blue_start = "#182848"
    blue_end = "#4b6cb7"
    
    list = NULL
    for(name in names(data)){
      
      data$title = rep(toupper(name), times=nrow(data))
      
      p = ggplot(data, aes_string(sample=name)) +
          geom_qq(distribution=qnorm, fill=blue_end) +
                  geom_qq_line(line.p = c(0.25, 0.75), col = blue_start) +
                    theme_tq() +
                      scale_color_tq() +
                        facet_wrap(~ title)+
                          labs(title="", x="", y="") +
                            theme(axis.text = element_text(family="Courier", colour=darkblu, size=10, face="bold"),
                                  strip.text.x = element_text(size = 14, colour = "white", angle = 0),
                                   strip.background =element_rect(fill=darkblu))
     
  
  
      list[[name]] = p
    
    }
    
    grid.arrange(grobs=list)
    
  })
  
   output$outliers_mad = renderPlot({
  
     # take reactive dataset
     data = dataset()
     
     # take a data frame with outliers according to MAD function 
     outliers_logical = data %>% transmute_if(is.numeric, isnt_out_mad)

     # bind the two data frames data
     data = as.data.frame(cbind(data %>% select_if(is.numeric), outliers_logical))
     data = data[,order(names(data))]
      
     # build a list 
     data_list = lapply(seq(1, ncol(data), by=2), function(i) data[i: pmin((i+1), ncol(data))])
     # add a title 
     data_list = lapply(data_list, function(x) cbind(x, title = toupper(colnames(x)[1])))
     
     # produce a plot list 
     plot_list = lapply(data_list, plot_series_outliers)
     
     # arrange in a grid layout
     grid.arrange(grobs=plot_list)
    
  })
  

  output$outliers_zscore = renderPlot({
  
     # take reactive dataset
     data = dataset()
     
     # take a data frame with outliers according to MAD function 
     outliers_logical = data %>% transmute_if(is.numeric, isnt_out_z)

     # bind the two data frames data
     data = as.data.frame(cbind(data %>% select_if(is.numeric), outliers_logical))
     data = data[,order(names(data))]
      
     # build a list 
     data_list = lapply(seq(1, ncol(data), by=2), function(i) data[i: pmin((i+1), ncol(data))])
     # add a title 
     data_list = lapply(data_list, function(x) cbind(x, title = toupper(colnames(x)[1])))
     
     # produce a plot list 
     plot_list = lapply(data_list, plot_series_outliers)
     
     # arrange in a grid layout
     grid.arrange(grobs=plot_list)
    
  }) 
   
     
  ## ** categorical analysis ----
  
  output$frequencies = renderPlot({
  
    data = dataset()
    fact <- unlist(lapply(data, is.character))  
    data = data[ , fact]
    
    blue_start = "#182848"
    blue_end = "#4b6cb7"
    
    list = NULL
    for(name in names(data)){
      
      data$title = rep(toupper(name), times=nrow(data))
      
      temp = data %>%
              select(all_of(name)) %>% 
              pull()

      data_temp = data %>% 
                  group_by(temp, .groups = 'drop') %>%
                  mutate(count = n()) %>% 
                  select(temp, count, title) %>% 
                  distinct(temp, .keep_all = T) %>% 
                  ungroup() %>% 
                  arrange(desc(count)) %>% 
                  slice(1:8)
                
      p = data_temp %>% 
              ggplot(aes(x=fct_reorder(temp, count), y=count)) +
              geom_bar(stat="identity",colour=blue_end, fill=blue_start, width=.5) +
              #coord_flip() +
              theme_tq() +
              scale_color_tq() +
              facet_wrap(~title)+
              labs(title="", x="", y="") +    
              geom_text(
              aes(label = temp),
              position = position_dodge(width = 1),
              vjust = -0.5, size = 3
              ) + 
              theme(axis.title = element_text(family="Helvetica", size=12, colour=darkblu),
                    plot.title = element_text(hjust=0.5, size=18, face="bold" ,colour=darkblu),
                    axis.text = element_text(family="Courier", colour=darkblu, size=10, face="bold"),
                    axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"), angle=0, vjust=0.5),
                    axis.text.x = element_blank())
        
  
      list[[name]] = p
    
    }
    
    grid.arrange(grobs=list)

  })
  

  # output$pie = renderHighchart({
  # 
  #   data = dataset()
  #   fact <- unlist(lapply(data, is.character))  
  #   data = data[ , fact]
  #   
  #   blue_start = "#182848"
  #   blue_end = "#4b6cb7"
  #   
  #   list = NULL
  #   for(name in names(data)){
  #     
  #     temp = data %>%
  #             select(all_of(name)) %>% 
  #             pull()
  # 
  #     data_temp = data %>% 
  #                 group_by(temp, .groups = 'drop') %>%
  #                 mutate(count = n()) %>% 
  #                 select(temp, count) %>% 
  #                 distinct(temp, .keep_all = T) %>% 
  #                 ungroup() %>% 
  #                 slice(1:8)
  #               
  #     hc <- data_temp %>%
  #             hchart(
  #             "pie", hcaes(x = temp, y = count),
  #              name = name
  #           )
  #       
  # 
  #     list[[name]] = hc
  #   
  #   }
  #   
  #  hw_grid(list, rowheight = 500, add_htmlgrid_css = F)
  # 
  # })
  
  
    ## ** correlations ----
  output$corr = renderPlot({
    
  dataset = dataset()
      
  corr_data = dataset %>% select_if(is.numeric) %>% select_if(~!all(is.na(.)))
  
  corr <- round(cor(corr_data, use="complete.obs"), 1)
  
  # Plot
  ggcorrplot(corr, hc.order = TRUE,
             type = "upper",
             lab = TRUE,
             lab_size = 6,
             method="square",
             colors = c("#FB2E41", "white", "#182848"),
             lab_col = "white",
             title="Correlogram of dataset",
             ggtheme=theme_bw) +
             labs(title="")

    
    
  })
  
  
  ## **  Observer to create a dataframe when users go to univariate section  ----
  
  observe({
    inFile<-input$file1
    print(inFile)
    if(is.null(inFile))
      return(NULL)
    dt = read.csv(inFile$datapath, header=input$header, sep=input$sep)
    dt = dt %>% select_if(is.numeric)
    ## Decide later what to do with the data, here we just fill
    updateSelectInput(session, "univariate_num", choices = names(dt))
  })
  
    observe({
    inFile<-input$file1
    print(inFile)
    if(is.null(inFile))
      return(NULL)
    dt = read.csv(inFile$datapath, header=input$header, sep=input$sep)
    dt = dt %>% select_if(is.character)
    ## Decide later what to do with the data, here we just fill
    updateSelectInput(session, "univariate_cat", choices = names(dt))
  })
  
  
  ## ** numerical variables ----
  
  dataset_univariate_num = reactive({
    
    req(input$file1)

    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    
    df =  df[, input$univariate_num]

    return(df)
    
    
  })
    

  output$hists = renderHighchart({
    
    hc <- hchart(
          dataset_univariate_num(), 
          color = "#B71C1C", name = input$univariate_num
    )
    
  })  
  
  output$dens = renderHighchart({
    
    hc <- hchart(
          density(dataset_univariate_num()), 
          color = "#B71C1C", name = input$univariate_num
    )

  })
  
  ## ** categorical variables ----
      
  dataset_univariate_cat = reactive({
    
    req(input$file1)

    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    
    df =  df[, input$univariate_cat]
    df = sort(df)

    return(df)
    
  })
  
  aggregate = reactive({

    theData = dataset_univariate_cat()
    theData_df = data.frame(dataset_univariate_cat())
    aggr = as.data.frame(theData_df %>%
                              group_by_at(names(theData_df)[1], .groups = 'drop') %>%
                              mutate(count = n())  %>%
                              select(all_of(names(theData_df)[1]), count) %>%
                              distinct_at(vars(names(theData_df)[1]), .keep_all = T) %>%
                              ungroup() %>%
                              arrange(desc(count)) %>% 
                              slice(1:8))
    
    #names(aggr)[1] = "inputvar"

  })
  
  output$freq = renderHighchart({
    
    x = dataset_univariate_cat()
    hchart(x, type = "column", name=input$univariate_cat)
    
  })
  
  

  output$pie_hc = renderHighchart({


    x = dataset_univariate_cat()
    hchart(x, type = "pie", name=input$univariate_cat)

  })
    
  
}


# Import ui and server
#source(here::here("R", "App", "ui.R"))
#source(here::here("R", "App", "server.R"))

# Run app
shinyApp(
  ui = ui,
  server = server
)

