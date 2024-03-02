# Load libraries
library(shiny)

# Load files
source("global.R")
source("ui.R")
source("server.R")

# Run the Shiny app
shinyApp(ui = ui, server = server)