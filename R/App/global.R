source(here::here("R", "libs.R"))

# Load dataset
df = readRDS(here::here("data","dataset.rds"))

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
source(here::here("R","utils","utils-multivariate.R"))

# Import ui and server
source(here::here("R", "App", "ui.R"))
source(here::here("R", "App", "server.R"))

# Run app
shinyApp(
  ui = ui,
  server = server
)

