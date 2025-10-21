# app.R
source("ui.R")
source("server.R")

# Chargement intelligent des packages
if (system.file(package = 'pacman') == "") {
  install.packages("pacman")
}
library(pacman)

pacman::p_load(
  devtools,      # développement
  rio,           # import/export
  here,          # gestion des chemins
  tidyverse,     # data management & viz
  flexdashboard, # dashboards
  shiny,         # applications interactives
  plotly,        # graphiques interactifs
  powerjoin,     # jointures intelligentes
  lubridate,     # gestion des dates
  DT,            # tableaux interactifs
  dplyr,
  stringr,
  openxlsx,
  readxl,
  tidyverse,
  shinydashboard
)

shinyApp(ui = ui, server = server)