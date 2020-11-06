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
  
  observe({
    inFile<-input$file1
    print(inFile)
    if(is.null(inFile))
      return(NULL)
    dt = read.csv(inFile$datapath, header=input$header, sep=input$sep)
    ## Decide later what to do with the data, here we just fill
    updateSelectInput(session, "univariate", choices = names(dt))
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
  

  output$pie = renderHighchart({
  
    data = dataset()
    fact <- unlist(lapply(data, is.character))  
    data = data[ , fact]
    
    blue_start = "#182848"
    blue_end = "#4b6cb7"
    
    list = NULL
    for(name in names(data)){
      
      temp = data %>%
              select(all_of(name)) %>% 
              pull()

      data_temp = data %>% 
                  group_by(temp, .groups = 'drop') %>%
                  mutate(count = n()) %>% 
                  select(temp, count) %>% 
                  distinct(temp, .keep_all = T) %>% 
                  ungroup() %>% 
                  slice(1:8)
                
      hc <- data_temp %>%
              hchart(
              "pie", hcaes(x = temp, y = count),
               name = name
            )
        
  
      list[[name]] = hc
    
    }
    
   hw_grid(list, rowheight = 500, add_htmlgrid_css = F)

  })
  
  
    ## ** correlations ----
  output$corr = renderPlot({
    
  dataset = dataset()
      
  corr_data = dataset %>% select_if(is.numeric) %>% select_if(~!all(is.na(.)))
  
  corr <- round(cor(corr_data, use="complete.obs"), 1)
  
  # Plot
  ggcorrplot(corr, hc.order = TRUE,
             type = "lower",
             lab = TRUE,
             lab_size = 3,
             method="circle",
             colors = c("#FB2E41", "white", "#304857"),
             title="Correlogram of dataset",
             ggtheme=theme_bw)

    
    
  })
  
  ## ** univariate analysis ----
  
  dataset_univariate = reactive({
    
    req(input$file1)

    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    
    df =  df[, input$univariate]
    
    return(df)
    
    
  })
    
  
  output$hists = renderHighchart({
    
  if(is.numeric(dataset_univariate())){
   
    hc <- hchart(
          dataset_univariate(), 
          color = "#B71C1C", name = input$univariate
  )
    
  }
  })  
  
  output$dens = renderHighchart({
    
  if(is.numeric(dataset_univariate())){
   
    hc <- hchart(
          density(dataset_univariate()), 
          color = "#B71C1C", name = input$univariate
  )
    
  }
  })  
    
  
}