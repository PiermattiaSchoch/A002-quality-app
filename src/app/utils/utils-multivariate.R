#' Profile missing values
#'
#' Analyze missing value profile
#' @param data input data
#' @keywords profile_missing
#' @return missing value profile, such as frequency, percentage and suggested action.
#' @import data.table
#' @export profile_missing
#' @seealso \link{plot_missing}
#' @examples
#' profile_missing(airquality)

profile_missing <- function(data) {
	## Declare variable first to pass R CMD check
	feature <- num_missing <- pct_missing <- group <- NULL
	## Check if input is data.table
	is_data_table <- is.data.table(data)
	## Detect input data class
	data_class <- class(data)
	## Set data to data.table
	if (!is_data_table) data <- data.table(data)
	## Extract missing value distribution
	missing_value <- data.table(
		"feature" = names(data),
		"num_missing" = sapply(data, function(x) {sum(is.na(x))})
	)
	missing_value[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
	missing_value[, pct_missing := num_missing / nrow(data)][]
	## Set data class back to original
	if (!is_data_table) class(missing_value) <- data_class

	missing_value
}


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
        #plotDataExplorer(plot_obj = output, title = title, ggtheme = ggtheme, theme_config = theme_config)
        }


## Plot a series of histograms ----
plot_series_hist <- function(df, xvar){

     df$title = toupper(xvar)

     p = ggplot(df, aes_string(x=xvar)) +
        geom_histogram(bins=30, color=positano, fill=green) +
          theme_tq() +
          scale_color_tq() +
          facet_wrap(~title)+
          labs(title="", x="", y="") +
          theme(axis.text = element_text(family="Courier", colour="#334357", size=12, face="bold"),
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
                            theme(axis.text = element_text(family="Courier", colour="#334357", size=10, face="bold"),
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
                            theme(axis.text = element_text(family="Courier", colour="#334357", size=10, face="bold"),
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
              theme(axis.text = element_text(family="Courier", colour="#334357", size=12, face="bold"),
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
                    theme(axis.text = element_text(family="Courier", colour="#334357", size=12, face="bold"),
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

