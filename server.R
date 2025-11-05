# server.R

server <- function(input, output, session) {
  
  # --- Reactive Values ---
  # Use a reactiveValues object to store data that can be accessed and updated
  # throughout the app. This is the central state of the application.
  rv <- reactiveValues()
  
  # --- Logging Function ---
  # A simple logger to record file import events. This is useful for debugging.
  log_import <- function(fichier, statut, message_text = "") {
    # Use the absolute path for the log file.
    log_file <- file.path("/srv/DB", "log.log")
    # log_file <- file.path("//NETUSERS/Partages/Intra-Service/CHB/dim/Statistiques/APP PLAN BLANC/DB", "log.log")
    
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    tryCatch({
      cat(paste0("[", timestamp, "] ", fichier, " - ", statut, " - ", message_text, "\n"),
          file = log_file, append = TRUE)
    }, error = function(e) {
      message(paste("Failed to write to log file:", e$message))
    })
  }

  # --- Data Loading Function ---
  # This function calls the main data processing function from global.R and
  # handles potential errors, ensuring the app doesn't crash.
  charger_donnees <- function() {
    # Wrap the data processing in a tryCatch block to handle any unexpected errors.
    resultats <- tryCatch({
      traitement_donnees_complet(simulation = simulation) # Call the main processing function
    }, error = function(e) {
      # If an error occurs, log it and return a list of empty/zeroed values.
      message(paste("⚠️ Erreur critique lors du traitement des données:", e$message))
      showNotification("Erreur critique lors du traitement des données. Vérifiez les fichiers et les logs.", type = "error", duration = 10)
      
      # Return a default 'empty' state for the application.
      empty_df <- data.frame()
      list(
        Base_PB_total = empty_df, Base_PB_incorrect = empty_df, pas_present = empty_df,
        Base_PB_aide_soignant = empty_df, Base_PB_ash = empty_df, Base_PB_medecin = empty_df,
        Base_PB_arm = empty_df, Base_PB_cadre = empty_df, Base_PB_chir = empty_df,
        Base_PB_pilot = empty_df, Base_PB_secu = empty_df, Base_PB_ibode = empty_df,
        Base_PB_ide = empty_df, Base_PB_autre = empty_df,
        nombre_personnel = 0, nb_personne_total = 0, pourcentage_reponse_total = "0/0 (0%)",
        dispo_h_gap = 0, dispo_h_briancon = 0, dispo_h_sisteron = 0, dispo_dans_h_total = 0,
        nb_personne_disponible = 0, nb_personne_30min = 0, nb_personne_1h = 0,
        nb_personne_3h = 0, nb_personne_6h = 0, nb_personne_12h = 0,
        nb_personne_incorrect = 0, nb_personne_pas_present = 0
      )
    })
    
    # Populate the reactiveValues with the results from the data processing.
    for (name in names(resultats)) {
      rv[[name]] <- resultats[[name]]
    }
  }

  # --- Initial Data Load ---
  # Load data once when the application starts.
  charger_donnees()

  # --- File Import Observers ---
  
  # Observer for 'annuaire.xlsx' file upload.
  observeEvent(input$file_annuaire, {
    req(input$file_annuaire)
    info <- input$file_annuaire
    
    # Robust validation for the uploaded file.
    if (tolower(tools::file_ext(info$name)) != "xlsx") {
      showNotification("Erreur : Le fichier doit être au format .xlsx.", type = "error")
      log_import(info$name, "ÉCHEC", "Format incorrect")
      return()
    }
    
    # Attempt to copy the file and reload data.
    tryCatch({
      # Check for 'Contacts' sheet before committing.
      sheets <- readxl::excel_sheets(info$datapath)
      if (!"Contacts" %in% sheets) {
        showNotification("Erreur : La feuille 'Contacts' est introuvable dans le fichier.", type = "error")
        log_import(info$name, "ÉCHEC", "Feuille 'Contacts' absente")
        return()
      }
      
      file.copy(info$datapath, annuaire_path, overwrite = TRUE)
      log_import(info$name, "SUCCÈS", "Annuaire importé")
      showNotification("Nouvel annuaire importé avec succès !", type = "message")
      charger_donnees() # Reload all data
      
    }, error = function(e) {
      showNotification(paste("Erreur à la lecture du fichier annuaire:", e$message), type = "error")
      log_import(info$name, "ÉCHEC", e$message)
    })
  })
  
  # Observer for 'reponsesSMS.xls' file upload.
  observeEvent(input$file_sms, {
    req(input$file_sms)
    info <- input$file_sms
    
    # Validation for .xls format.
    if (tolower(tools::file_ext(info$name)) != "xls") {
      showNotification("Erreur : Le fichier doit être au format .xls.", type = "error")
      log_import(info$name, "ÉCHEC", "Format incorrect")
      return()
    }

    tryCatch({
      # A simple read test to ensure file is not corrupt.
      readxl::read_excel(info$datapath, n_max = 1)
      
      file.copy(info$datapath, reponsesSMS_path, overwrite = TRUE)
      log_import(info$name, "SUCCÈS", "SMS importés")
      showNotification("Nouvelles réponses SMS importées avec succès !", type = "message")
      
      # Update the file modification date displayed in the UI.
      rv$last_modif <- file.info(reponsesSMS_path)$mtime
      
      charger_donnees() # Reload all data
      
    }, error = function(e) {
      showNotification(paste("Erreur à la lecture du fichier SMS:", e$message), type = "error")
      log_import(info$name, "ÉCHEC", e$message)
    })
  })

  # --- UI Outputs ---

  # Update the text displaying the last import date.
  output$date_fichier <- renderText({
    # Initialize the date on app start.
    if (is.null(rv$last_modif) && file.exists(reponsesSMS_path)) {
      rv$last_modif <- file.info(reponsesSMS_path)$mtime
    }
    if (!is.null(rv$last_modif) && !is.na(rv$last_modif)) {
      format(rv$last_modif, "%d/%m/%Y %H:%M:%S")
    } else {
      "Aucun fichier importé"
    }
  })
  
  # An observer to update all sidebar text outputs at once when rv changes.
  observe({
    output$nb_personne_disponible <- renderText({ rv$nb_personne_disponible })
    output$nb_personne_30min      <- renderText({ rv$nb_personne_30min })
    output$nb_personne_1h         <- renderText({ rv$nb_personne_1h })
    output$nb_personne_3h         <- renderText({ rv$nb_personne_3h })
    output$nb_personne_6h         <- renderText({ rv$nb_personne_6h })
    output$nb_personne_12h        <- renderText({ rv$nb_personne_12h })
    output$nb_personne_incorrect  <- renderText({ rv$nb_personne_incorrect })
    output$nb_personne_pas_present<- renderText({ rv$nb_personne_pas_present })
    output$nb_personne_total      <- renderText({ rv$nb_personne_total })
    output$dispo_h_gap            <- renderText({ rv$dispo_h_gap })
    output$dispo_h_briancon       <- renderText({ rv$dispo_h_briancon })
    output$dispo_h_sisteron       <- renderText({ rv$dispo_h_sisteron })
    output$pourcentage_reponse_total <- renderText({ rv$pourcentage_reponse_total })
    output$nb_annuaire            <- renderText({ rv$nombre_personnel })
  })

  # --- DataTable Rendering ---

  # A centralized function to render the DataTables with consistent styling.
  render_dt_table <- function(data) {
    # Ensure data is not null or empty before rendering.
    validate(need(is.data.frame(data) && nrow(data) > 0, "Aucune donnée disponible pour cette catégorie."))
    
    # Déterminer l’index de la colonne Contact si elle existe
    contact_idx <- which(colnames(data) == "Contact")
    column_defs <- list()
    if (length(contact_idx) == 1) {
      column_defs <- list(list(visible = FALSE, targets = contact_idx - 1)) # JS index starts at 0
    }
    
    dt <- datatable(
      data,
      extensions = 'Buttons',
      class = 'cell-border compact',
      selection = "none",
      rownames = FALSE,
      filter = list(position = 'top', clear = FALSE),
      options = list(
        scrollX = TRUE,
        pageLength = -1, # Show all rows
        dom = 'Bfrtip',
        buttons = c('excel', 'pdf'),
        columnDefs = column_defs,  # Appliquer la suppression de colonne si nécessaire
        language = list(
          search = "Rechercher :", lengthMenu = "Afficher _MENU_ éléments",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ éléments",
          infoEmpty = "Aucun élément à afficher",
          infoFiltered = "(filtré de _MAX_ éléments)",
          zeroRecords = "Aucun enregistrement trouvé",
          paginate = list(first = "Premier", previous = "Précédent", `next` = "Suivant", last = "Dernier")
        )
      )
    )
    
    # Apply conditional row coloring based on availability, if the column exists.
    if ("Disponibilité" %in% colnames(data)) {
      dt <- dt %>% formatStyle(
        'Disponibilité',
        target = 'row',
        backgroundColor = styleEqual(
          c("sur place", "disponible en <30 min", "disponible dans l'heure", "disponible dans les 3h",
            "disponible dans les 6h", "disponible dans les 12h", "réponse incorrecte", "indisponible"),
          c("#FFFFFF", "#F0EDCF", "#FDE767", "#F3B95F", "#EE9322", "#C08261", "#D83F31", "#E0E0E0")
        )
      )
    }
    return(dt)
  }

  # An observer to render all DataTables.
  observe({
    output$table_total          <- renderDT({ render_dt_table(rv$Base_PB_total) })
    output$table_ash            <- renderDT({ render_dt_table(rv$Base_PB_ash) })
    output$table_aide           <- renderDT({ render_dt_table(rv$Base_PB_aide_soignant) })
    output$table_ide            <- renderDT({ render_dt_table(rv$Base_PB_ide) })
    output$table_medecin        <- renderDT({ render_dt_table(rv$Base_PB_medecin) })
    output$table_arm            <- renderDT({ render_dt_table(rv$Base_PB_arm) })
    output$table_cadre          <- renderDT({ render_dt_table(rv$Base_PB_cadre) })
    output$table_chirurgien     <- renderDT({ render_dt_table(rv$Base_PB_chir) })
    output$table_ibode          <- renderDT({ render_dt_table(rv$Base_PB_ibode) })
    output$table_securite       <- renderDT({ render_dt_table(rv$Base_PB_secu) })
    output$table_pilote         <- renderDT({ render_dt_table(rv$Base_PB_pilot) })
    output$table_autres         <- renderDT({ render_dt_table(rv$Base_PB_autre) })
    output$table_non_enregistre <- renderDT({ render_dt_table(rv$pas_present) })
    output$table_reponse_incorrect <- renderDT({ render_dt_table(rv$Base_PB_incorrect) })
  })
  
  
  # Valeur réactive pour savoir si l'utilisateur est connecté
  user_logged_in <- reactiveVal(FALSE)
  
  # Charger identifiants depuis le fichier .env (JSON)
  credentials <- jsonlite::fromJSON(".env")
  
  # ---- UI dynamique ----
  output$app_ui <- renderUI({
    if (user_logged_in()) {
      dashboard_content  # Affiche le dashboard complet
    } else {
      login_page         # Affiche la page de login
    }
  })
  
  # ---- Observer la soumission du formulaire de login ----
  observeEvent(input$submit_login, {
    req(input$username, input$password)
    
    user_index <- which(credentials$username == input$username)
    #pour créer un password il faut utiliser bcrypt::hashpw(as.character(password))
    if (length(user_index) == 1 &&
        bcrypt::checkpw(input$password, credentials$password[user_index])) {
      
      user_logged_in(TRUE)
      shinyjs::hide("login_page")
      shinyjs::show("app_content")
      output$login_message <- renderText("")
      
      # 🔹 Log connexion réussie
      log_import("auth", "SUCCESS", paste("Connexion réussie :", input$username))
      
      # Stocker la connexion dans localStorage pour restaurer après refresh
      shinyjs::runjs("localStorage.setItem('user_logged_in', 'true');")
      
    } else {
      output$login_message <- renderText("Identifiants incorrects ❌")
      
      # 🔹 Log tentative échouée
      log_import("auth", "ERROR", paste("Échec connexion :", input$username))
      
      # Vider le champ mot de passe après erreur
      shinyjs::runjs("document.getElementById('password').value = ''")
    }
  })
  
  # ---- Déconnexion ----
  observeEvent(input$logout_button, {
    user_logged_in(FALSE)
    shinyjs::show("login_page")
    shinyjs::hide("app_content")
    
    # 🔹 Log déconnexion
    log_import("auth", "INFO", "Utilisateur déconnecté")
    
    # Supprimer la connexion locale
    shinyjs::runjs("localStorage.removeItem('user_logged_in');")
  })
  
  # ---- Restaurer la session après refresh ----
  observe({
    shinyjs::runjs("
    if(localStorage.getItem('user_logged_in') === 'true') {
      Shiny.setInputValue('restore_login', true, {priority: 'event'});
    }
  ")
  })
  
  observeEvent(input$restore_login, {
    user_logged_in(TRUE)
    shinyjs::hide("login_page")
    shinyjs::show("app_content")
    
    # 🔹 Log restauration session
    log_import("auth", "INFO", "Session restaurée après refresh")
  })
  
}



