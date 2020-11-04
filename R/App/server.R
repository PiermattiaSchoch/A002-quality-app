options(shiny.maxRequestSize=30*1024^2)
library(visdat)


server <- function(input, output, session) {
  
  ## ** get dataframe ----
  
  dataset = reactive({
    
    req(input$file1)

    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    
    return(df)
    
    
  })
  
  ## ** load file ----
  output$contents <- renderReactable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    react_df = data.frame(dataset())
    
    if(input$disp == "head") {
      return(reactable(react_df))
    }
    else {
      return(dataset())
    }

  })
  
  
  output$Dataframe <- renderValueBox({
    valueBox(
      "bank", 
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
  
  
  ## ** dataset glimpse ----
  output$basic_description = renderPlot({
    
    df_str(dataset(), return = "plot", quiet = F) + labs(title="")
    
    })
  
  output$missing_values = renderPlot({
    
    plot_missing_mycolor(dataset(),
                         ggtheme=theme_tq(),
                         title="",
                         theme_config = list(legend.position = 'none')) +
                         labs(x="",y="")}
    )

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
  
  output$data_summary <- renderUI({
    
    st_options(dfSummary.varnumbers	=F,
               dfSummary.labels.col = F,
               display.labels=T,
               dfSummary.valid.col=F, 
               dfSummary.na.col = F,
               headings = F,
               round.digits=1,
               bootstrap.css = FALSE,
               footnote = NA)                     
    
    out <- print(dfSummary(dataset(), 
                           graph.magnif = 0.7),
                           method = 'render',
                           max.tbl.height = 950)
              
  })
  
  ## ** numerical distributions  ----

  output$numerical_histograms = renderPlot({
    
    dataset = dataset()
    
    list = lapply(names(dataset %>% select_if(is.numeric)), function(xvar) {
    
    dataset$title = toupper(xvar)
    
    p = ggplot(dataset, aes_string(x=xvar)) +
        geom_histogram(bins=30, color=positano, fill=green) +
          theme_tq() +
          scale_color_tq() +
          facet_wrap(~title)+
          labs(title="", x="", y="") +
          theme(axis.text = element_text(family="Courier", colour=darkblu, size=12, face="bold"),
                strip.text.x = element_text(size = 14, colour = "white", angle = 0),
                strip.background =element_rect(fill=green))
     
    print(p)
    
    })
    
    grid.arrange(grobs=list)

  })
  
}