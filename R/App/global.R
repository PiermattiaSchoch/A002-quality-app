source(here::here("R", "libs.R"))

# Load dataset
#dataset = readRDS(here::here("data","dataset.rds"))

# Import utils
source(here::here("R","utils","utils-dataset-glimpse.R"))

# Import ui and server
source(here::here("R", "App", "ui.R"))
source(here::here("R", "App", "server.R"))

# Run app
shinyApp(
  ui = ui,
  server = server
)
