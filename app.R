# app.R
# This file now serves as the main entry point for the Shiny application.

# Source the global script which loads packages and defines data processing functions.
# global.R is the heart of the application's backend logic.
source("global.R")

# Source the user interface definition.
# ui.R defines the layout and appearance of the web application.
source("ui.R")

# Source the server-side logic.
# server.R contains the instructions for how the app should react to user input.
source("server.R")

# Run the Shiny application by combining the UI and server components.
shinyApp(ui = ui, server = server)