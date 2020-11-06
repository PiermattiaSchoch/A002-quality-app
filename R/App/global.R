source(here::here("R", "libs.R"))

# Load dataset
df = readRDS(here::here("data","dataset.rds"))
data = df
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

      hc <- data_temp %>%
              hchart(
              "pie", hcaes(x = temp, y = count),
               name = name
            )


      list[[name]] = hc

    }

   hw_grid(list, rowheight = 500)


# Import utils
source(here::here("R","utils","utils-multivariate.R"))

# Import ui and server
source(here::here("R", "App", "ui.R"))
source(here::here("R", "App", "server.R"))

# Run app
shinyApp(
  ui = ui,
  server = server
)

