options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
  
  
  ## ** get dataframe ----
  
  dataset = reactive({
    
    req(input$file1)

    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    df
    
    
  })
  
  
  
  ## ** load file ----
  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    if(input$disp == "head") {
      return(head(dataset()))
    }
    else {
      return(dataset())
    }

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
    check_types = check_types + 
                coord_flip() + 
                labs(y="\n Observations \n") +
                scale_y_discrete(position = 'right', limits = c(0, nrow(dataset())))+
                scale_x_discrete(position = 'bottom')+
                theme(legend.position="bottom")
    
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
  
}