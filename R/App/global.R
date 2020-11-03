source(here::here("R", "libs.R"))

# Import ui and server
source(here::here("R", "App", "ui.R"))
source(here::here("R", "App", "server.R"))

# Run app
shinyApp(
  ui = ui,
  server = server
)
