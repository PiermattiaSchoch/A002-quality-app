#devtools::install_github("laresbernardo/lares")
library(lares)
library(data.table)
library(tidyquant)
library(DataExplorer)
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


