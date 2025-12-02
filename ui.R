ui <- fluidPage(
  useShinyjs(),
  
  # ---- Page de connexion ----
  div(
    id = "login_page",
    style = "
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      height: 100vh;
      background-color: #f8f9fa;
    ",
    box(
      title = "Connexion",
      width = 3,
      status = "primary",
      solidHeader = TRUE,
      textInput("username", "Nom d'utilisateur"),
      passwordInput("password", "Mot de passe"),
      actionButton(
        "submit_login", "Se connecter",
        class = "btn btn-primary btn-block",
        style = "color: white; font-weight: bold;"
      ),
      br(),
      textOutput("login_message")
    )
  ),
  
  # ---- Contenu du dashboard caché ----
  div(
    id = "app_content",
    style = "display:none;",  # caché au départ
    dashboardPage(
      dashboardHeader(title = "PLAN BLANC",
                      tags$li(class = "dropdown",
                              actionButton(
                                "logout_button", 
                                "Déconnexion", 
                                class = "btn btn-default btn-sm",
                                style = "margin-top: 8px; margin-right: 8px;"
                              ))
      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Réponses SMS", tabName = "tableaux", icon = icon("table"))
        ),
        
        tagList(
          div(style = "padding: 0 15px;",
              h4("Date de l'import SMS :", style = "text-decoration: underline;"),
              textOutput("date_fichier", inline = TRUE),
              hr(),
              
              h4("Disponibilité :", style = "text-decoration: underline;"),
              tags$ul(
                tags$li("Sur place : ", textOutput("nb_personne_disponible", inline = TRUE)),
                tags$li("Dans 30 min : ", textOutput("nb_personne_30min", inline = TRUE)),
                tags$li("Dans 1h : ", textOutput("nb_personne_1h", inline = TRUE)),
                tags$li("Dans 3h : ", textOutput("nb_personne_3h", inline = TRUE)),
                tags$li("Dans 6h : ", textOutput("nb_personne_6h", inline = TRUE)),
                tags$li("Dans 12h : ", textOutput("nb_personne_12h", inline = TRUE))
              ),
              hr(),
              
              h4("Particularités :", style = "text-decoration: underline;"),
              tags$ul(
                tags$li("Non enregistré : ", textOutput("nb_personne_pas_present", inline = TRUE)),
                tags$li("Réponse incorrecte : ", textOutput("nb_personne_incorrect", inline = TRUE))
              ),
              hr(),
              
              h4("Disponibles dans l’heure :", style = "text-decoration: underline;"),
              tags$ul(
                tags$li("Gap : ", textOutput("dispo_h_gap", inline = TRUE)),
                tags$li("Sisteron : ", textOutput("dispo_h_sisteron", inline = TRUE)),
                tags$li("Briançon : ", textOutput("dispo_h_briancon", inline = TRUE))
              ),
              hr(),
              
              h4("Taux de réponse :", style = "text-decoration: underline;"),
              textOutput("pourcentage_reponse_total"),
              br(),
              h4("Annuaire :", style = "text-decoration: underline;"),
              textOutput("nb_annuaire"),
              hr()
          )
        ),
        
        fileInput("file_sms", "Importer réponses SMS (.xls)",
                  accept = c(".xls", "application/vnd.ms-excel"),
                  buttonLabel = "Parcourir...", placeholder = "Aucun fichier"),
        
        fileInput("file_annuaire", "Importer annuaire (.xlsx)",
                  accept = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                  buttonLabel = "Parcourir...", placeholder = "Aucun fichier")
      ),
      dashboardBody(
        useShinyjs(),
        
        tags$head(
          tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico")
        ),
        
        tags$style(HTML("
          .container-fluid {
            padding-right: 0px;
            padding-left: 0px;
          }
        ")),
        
        div(id = "app_content",
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
    )
  )
)