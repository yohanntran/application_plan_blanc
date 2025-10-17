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
  readxl,
  shinydashboard
)

# ---- Source de données globale ----
source("global.R") # (ton script contenant les calculs de Base_PB etc.)

ui <- dashboardPage(
  
  # === En-tête ===
  dashboardHeader(title = "PLAN BLANC"),
  
  # === Sidebar ===
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableaux", tabName = "tableaux", icon = icon("table"))
    ),
    
    div(style = "margin-left: 15px;",
        h4("Fichier SMS du :", style = "text-decoration: underline;"),
        textOutput("date_fichier", inline = TRUE)
    ),
    
    br(),
    
    # Bloc Disponibilité
    div(style = "margin-left: 15px;",
        h4("Disponibilité :", style = "text-decoration: underline;"),
        tags$ul(
          tags$li("maintenant : ", textOutput("nb_personne_disponible", inline = TRUE)),
          tags$li("dans 30min : ", textOutput("nb_personne_30min", inline = TRUE)),
          tags$li("dans 1h : ", textOutput("nb_personne_1h", inline = TRUE)),
          tags$li("dans 3h : ", textOutput("nb_personne_3h", inline = TRUE)),
          tags$li("dans 6h : ", textOutput("nb_personne_6h", inline = TRUE)),
          tags$li("dans 12h : ", textOutput("nb_personne_12h", inline = TRUE))
        )
    ),
    
    br(),
    
    # Bloc Particularité
    div(style = "margin-left: 15px;",
        h4("Particularité :", style = "text-decoration: underline;"),
        tags$ul(
          tags$li("pas enregistré : ", textOutput("nb_personne_pas_present", inline = TRUE)),
          tags$li("réponse incorrecte : ", textOutput("nb_personne_incorrect", inline = TRUE))
        )
    ),
    
    br(),
    
    # Bloc Disponibles dans l’heure
    div(style = "margin-left: 15px;",
        h4("Disponibles dans l’heure :", style = "text-decoration: underline;"),
        tags$ul(
          tags$li("Gap : ", textOutput("dispo_h_gap", inline = TRUE)),
          tags$li("Sisteron : ", textOutput("dispo_h_sisteron", inline = TRUE)),
          tags$li("Briançon : ", textOutput("dispo_h_briancon", inline = TRUE))
        )
    ),
    
    br(),
    
    # Bloc Réponses
    div(style = "margin-left: 15px;",
        h4("Réponses :", style = "text-decoration: underline;"),
        textOutput("pourcentage_reponse_total")
    ),
    
    br(),
    
    # Bloc Personnel dans l’annuaire
    div(style = "margin-left: 15px;",
        h4("Personnel dans l’annuaire :", style = "text-decoration: underline;"),
        textOutput("nb_annuaire")
    ),
    
    br(),
    
    # Import fichier SMS
    fileInput(
      "file_sms", 
      "Importez les réponses SMS",
      accept = c(".xls"),
      buttonLabel = "Parcourir...",
      placeholder = "Aucun fichier sélectionné"
    )
  ),
  
  # === Corps principal ===
  dashboardBody(
    tabItems(
      tabItem(tabName = "tableaux",
              fluidRow(
                column(
                  width = 12,
                  tabsetPanel(
                    id = "onglets_tables",
                    tabPanel("Toutes les réponses", DTOutput("table_total")),
                    tabPanel("ASH", DTOutput("table_ash")),
                    tabPanel("Aide soignant", DTOutput("table_aide")),
                    tabPanel("IDE", DTOutput("table_ide")),
                    tabPanel("Médecin", DTOutput("table_medecin")),
                    tabPanel("ARM", DTOutput("table_arm")),
                    tabPanel("Cadre", DTOutput("table_cadre")),
                    tabPanel("Chirurgien", DTOutput("table_chirurgien")),
                    tabPanel("IBODE", DTOutput("table_ibode")),
                    tabPanel("Sécurité", DTOutput("table_securite")),
                    tabPanel("Pilote", DTOutput("table_pilote")),
                    tabPanel("Autres", DTOutput("table_autres")),
                    tabPanel("Non enregistré", DTOutput("table_non_enregistre")),
                    tabPanel("Réponse incorrecte", DTOutput("table_reponse_incorrect"))
                  )
                )
              )
      )
    )
  )
)
