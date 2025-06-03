# app.R

source("global.R")  # Global variables and libraries

shinyApp(
  ui = source("ui.R")$value,
  server = source("server.R")$value
)
