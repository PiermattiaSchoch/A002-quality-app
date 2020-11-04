library(gridExtra)
green = "#018F83"
positano = "#DE9F06"
darkblu = "#334357"

## Plot a series of histograms
plot_series_hist <- function(xvar){
    
     dataset = dataset %>% select_if(is.numeric)    
  
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
}
