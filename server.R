server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  
  log_import <- function(fichier, statut, message_text = "") {
    # Fichier de log unique pour les imports
    log_file <- file.path("..", "DB", "log.log")
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(paste0("[", timestamp, "] ", fichier, " - ", statut, " - ", message_text, "\n"),
        file = log_file, append = TRUE)
  }

  # --- Quand un fichier annuaire est importé ---
  observeEvent(input$file_annuaire, {
    req(input$file_annuaire)
    fichier <- input$file_annuaire$name
    
    # Vérification stricte de l'extension
    ext <- tolower(tools::file_ext(fichier))
    if (ext != "xlsx") {
      showNotification(
        "Erreur : veuillez importer un fichier Excel au format .xlsx uniquement.",
        type = "error",
        duration = 5
      )
      log_import(fichier, "ÉCHEC", "Mauvais format (non .xlsx)")
      return(NULL)
    }
    
    # Vérification de la signature du fichier
    raw_header <- readBin(input$file_annuaire$datapath, what = "raw", n = 4)
    sig <- as.integer(raw_header)
    
    sig_zip <- as.integer(as.raw(c(0x50, 0x4B, 0x03, 0x04))) # vrai .xlsx
    sig_xls <- as.integer(as.raw(c(0xD0, 0xCF, 0x11, 0xE0))) # ancien .xls
    
    if (identical(sig, sig_xls)) {
      showNotification(
        "Erreur : ce fichier semble être un ancien format .xls renommé en .xlsx.",
        type = "error",
        duration = 6
      )
      log_import(fichier, "ÉCHEC", "Ancien .xls renommé")
      return(NULL)
    }
    
    if (!identical(sig, sig_zip)) {
      showNotification(
        "Erreur : le fichier ne correspond pas à un vrai fichier Excel (.xlsx).",
        type = "error",
        duration = 6
      )
      log_import(fichier, "ÉCHEC", "Signature invalide")
      return(NULL)
    }
    
    # Vérification que la feuille 'Contacts' existe
    sheets <- tryCatch({
      readxl::excel_sheets(input$file_annuaire$datapath)
    }, error = function(e) {
      showNotification(
        "Erreur : le fichier Excel est illisible ou corrompu.",
        type = "error",
        duration = 6
      )
      log_import(fichier, "ÉCHEC", "Fichier illisible")
      return(NULL)
    })
    
    if (!"Contacts" %in% sheets) {
      showNotification(
        "Erreur : le fichier doit contenir une feuille nommée 'Contacts'.",
        type = "error",
        duration = 6
      )
      log_import(fichier, "ÉCHEC", "Feuille 'Contacts' absente")
      return(NULL)
    }
    
    # Test de lecture pour s'assurer que le contenu est exploitable
    tryCatch({
      readxl::read_excel(input$file_annuaire$datapath, sheet = "Contacts", n_max = 1)
    }, error = function(e) {
      showNotification(
        paste("Erreur : la feuille 'Contacts' est illisible ou corrompue.", e$message),
        type = "error",
        duration = 6
      )
      log_import(fichier, "ÉCHEC", paste("Feuille illisible :", e$message))
      return(NULL)
    })
    
    # Copie du fichier validé
    dest_file <- file.path("..", "DB", "annuaire.xlsx")
    file.copy(input$file_annuaire$datapath, dest_file, overwrite = TRUE)
    
    # Recharger le contexte et les données
    source("global.R", local = TRUE)
    charger_donnees()
    
    # Notification de succès
    showNotification(
      "Nouvel annuaire importé avec succès !",
      type = "message",
      duration = 5
    )
    log_import(fichier, "SUCCES", "Import réussi")
  })
  
  
  
  
  # --- Initialisation de la date du fichier existant ---
  fichier_sms <- file.path("..","DB","reponsesSMS.xls")
  if (file.exists(fichier_sms)) {
    rv$last_modif <- file.info(fichier_sms)$mtime
  } else {
    rv$last_modif <- NA
  }
  
  # --- Output texte pour afficher la date ---
  output$date_fichier <- renderText({
    req(rv$last_modif)
    format(rv$last_modif, "%d/%m/%Y %H:%M:%S")
  })
  
  # --- Fonction interne pour charger les données ---
  charger_donnees <- function() {
    resultats <- tryCatch(
      {
        # Essaye le traitement normal
        traitement_donnees_complet()
      },
      error = function(e) {
        message("⚠️ Erreur lors du traitement des données, création de tableaux vides...")
        # Création de listes et data.frames vides
        empty_df <- data.frame()
        list(
          Base_PB                = empty_df,
          Base_PB_incorrect      = empty_df,
          pas_present            = empty_df,
          pas_present_a_ajouter  = empty_df,
          Base_PB_total          = empty_df,
          Base_PB_aide_soignant  = empty_df,
          Base_PB_ash            = empty_df,
          Base_PB_medecin        = empty_df,
          Base_PB_arm            = empty_df,
          Base_PB_cadre          = empty_df,
          Base_PB_chir           = empty_df,
          Base_PB_pilot          = empty_df,
          Base_PB_secu           = empty_df,
          Base_PB_ibode          = empty_df,
          Base_PB_ide            = empty_df,
          Base_PB_autre          = empty_df,
          nombre_personnel       = 0,
          nb_personne_total      = 0,
          nb_personne_incorrect  = 0,
          pourcentage_reponse_total = "0/0 (0%)",
          dispo_h_gap            = 0,
          dispo_h_briancon       = 0,
          dispo_h_sisteron       = 0,
          dispo_dans_h_total     = 0,
          nb_personne_disponible = 0,
          nb_personne_30min      = 0,
          nb_personne_1h         = 0,
          nb_personne_3h         = 0,
          nb_personne_6h         = 0,
          nb_personne_12h        = 0,
          nb_personne_no_reponse = 0,
          nb_personne_pas_present= 0
        )
      }
    )
    
    # Mise à jour des reactiveValues
    rv$Base_PB                <- resultats$Base_PB
    rv$Base_PB_incorrect      <- resultats$Base_PB_incorrect
    rv$pas_present            <- resultats$pas_present
    rv$pas_present_a_ajouter  <- resultats$pas_present_a_ajouter
    rv$Base_PB_total          <- resultats$Base_PB_total
    rv$Base_PB_aide_soignant  <- resultats$Base_PB_aide_soignant
    rv$Base_PB_ash            <- resultats$Base_PB_ash
    rv$Base_PB_medecin        <- resultats$Base_PB_medecin
    rv$Base_PB_arm            <- resultats$Base_PB_arm
    rv$Base_PB_cadre          <- resultats$Base_PB_cadre
    rv$Base_PB_chir           <- resultats$Base_PB_chir
    rv$Base_PB_pilot          <- resultats$Base_PB_pilot
    rv$Base_PB_secu           <- resultats$Base_PB_secu
    rv$Base_PB_ibode          <- resultats$Base_PB_ibode
    rv$Base_PB_ide            <- resultats$Base_PB_ide
    rv$Base_PB_autre          <- resultats$Base_PB_autre
    rv$nombre_personnel       <- resultats$nombre_personnel
    rv$nb_personne_total      <- resultats$nb_personne_total
    rv$nb_personne_incorrect  <- resultats$nb_personne_incorrect
    rv$pourcentage_reponse_total <- resultats$pourcentage_reponse_total
    rv$dispo_h_gap            <- resultats$dispo_h_gap
    rv$dispo_h_briancon       <- resultats$dispo_h_briancon
    rv$dispo_h_sisteron       <- resultats$dispo_h_sisteron
    rv$dispo_dans_h_total     <- resultats$dispo_dans_h_total
    rv$nb_personne_disponible <- resultats$nb_personne_disponible
    rv$nb_personne_30min      <- resultats$nb_personne_30min
    rv$nb_personne_1h         <- resultats$nb_personne_1h
    rv$nb_personne_3h         <- resultats$nb_personne_3h
    rv$nb_personne_6h         <- resultats$nb_personne_6h
    rv$nb_personne_12h        <- resultats$nb_personne_12h
    rv$nb_personne_no_reponse <- resultats$nb_personne_no_reponse
    rv$nb_personne_pas_present<- resultats$nb_personne_pas_present
  }
  
  # --- Initialisation automatique au démarrage ---
  charger_donnees()
  
  observeEvent(input$file_sms, {
    req(input$file_sms)
    fichier <- input$file_sms$name
    
    # Vérification stricte de l'extension
    ext <- tolower(tools::file_ext(fichier))
    if (ext != "xls") {
      showNotification(
        "Erreur : veuillez importer un fichier Excel au format .xls uniquement.",
        type = "error",
        duration = 5
      )
      log_import(fichier, "ÉCHEC", "Mauvais format (non .xls)")
      return(NULL)
    }
    
    # Vérification de la signature binaire du format .xls
    raw_header <- readBin(input$file_sms$datapath, what = "raw", n = 4)
    valid_xls_signature <- identical(raw_header, as.raw(c(0xD0, 0xCF, 0x11, 0xE0)))
    
    if (!valid_xls_signature) {
      showNotification(
        "Erreur : le fichier ne correspond pas à un vrai fichier Excel (.xls).",
        type = "error",
        duration = 6
      )
      log_import(fichier, "ÉCHEC", "Signature invalide")
      return(NULL)
    }
    
    # Test de lecture pour s'assurer que le contenu est exploitable
    tryCatch({
      df_test <- readxl::read_excel(input$file_sms$datapath, n_max = 1)
    }, error = function(e) {
      showNotification(
        paste("Erreur : le fichier Excel est illisible ou corrompu.", e$message),
        type = "error",
        duration = 6
      )
      log_import(fichier, "ÉCHEC", paste("Fichier illisible :", e$message))
      return(NULL)
    })
    
    # Copie du fichier validé
    dest_file <- file.path("..", "DB", "reponsesSMS.xls")
    file.copy(input$file_sms$datapath, dest_file, overwrite = TRUE)
    
    # Mise à jour de la date
    rv$last_modif <- file.info(dest_file)$mtime
    output$date_fichier <- renderText({
      format(rv$last_modif, "%d/%m/%Y %H:%M:%S")
    })
    
    # Rechargement des données
    charger_donnees()
    
    showNotification(
      "Nouvelles réponses SMS importées avec succès !",
      type = "message",
      duration = 5
    )
    log_import(fichier, "SUCCES", "Import réussi")
  })
  
  
  
  
  # --- Fonction utilitaire pour créer les tableaux ---
  render_dt_table <- function(data) {
    dt <- datatable(
      data,
      extensions = c('Buttons'),
      class = 'cell-border compact',
      selection = "none",
      options = list(
        columnDefs = list(list(visible = FALSE, targets = c(6))),
        language = fr,
        scrollX = TRUE,
        pageLength = -1,
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = FALSE,
        fixedHeader = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('excel', 'pdf')
      ),
      filter = list(position = 'top', clear = FALSE),
      editable = list(target = "column", disable = list(columns = c(0,1,2,3,4))),
      rownames = FALSE
    )
    
    # Appliquer le format de style seulement si la colonne existe
    if("Disponibilité" %in% colnames(data)) {
      dt <- dt %>% 
        formatStyle(
          'Disponibilité',
          target = 'row',
          backgroundColor = styleEqual(
            c("sur place","disponible en <30 min","disponible dans l'heure","disponible dans les 3h",
              "disponible dans les 6h","disponible dans les 12h","réponse incorrecte"),
            c("#FFFFFF","#F0EDCF","#FDE767","#F3B95F","#EE9322","#C08261","#D83F31")
          )
        )
    }
    
    return(dt)
  }
  
  
  render_dt_table_2 <- function(data) {
    req(!is.null(data))
    req("Disponibilité" %in% names(data))
    
    # Nettoyage éventuel des colonnes inutiles (Excel)
    data <- data[, !names(data) %in% c("row.names", "X", "X1")]
    
    datatable(
      data,
      rownames = FALSE,   # supprime la colonne indices
      extensions = 'Buttons',
      class = 'cell-border compact',
      selection = "none",
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        paging = TRUE,
        searching = TRUE,
        dom = 'Bfrtip',
        buttons = c('excel', 'pdf'),
        language = fr
      )
    ) %>% 
      formatStyle(
        'Disponibilité',
        target = 'row',
        backgroundColor = styleEqual(
          c("sur place","disponible en <30 min","disponible dans l'heure",
            "disponible dans les 3h","disponible dans les 6h",
            "disponible dans les 12h","réponse incorrecte"),
          c("#FFFFFF","#F0EDCF","#FDE767","#F3B95F","#EE9322","#C08261","#D83F31")
        )
      )
  }
  
  
  
  # --- Tous les outputs textuels (sidebar) ---
  observe({
    output$nb_personne_disponible   <- renderText({ rv$nb_personne_disponible })
    output$nb_personne_30min        <- renderText({ rv$nb_personne_30min })
    output$nb_personne_1h           <- renderText({ rv$nb_personne_1h })
    output$nb_personne_3h           <- renderText({ rv$nb_personne_3h })
    output$nb_personne_6h           <- renderText({ rv$nb_personne_6h })
    output$nb_personne_12h          <- renderText({ rv$nb_personne_12h })
    output$nb_personne_incorrect    <- renderText({ rv$nb_personne_incorrect })
    output$nb_personne_pas_present  <- renderText({ rv$nb_personne_pas_present })
    output$nb_personne_total        <- renderText({ rv$nb_personne_total })
    output$dispo_h_gap              <- renderText({ rv$dispo_h_gap })
    output$dispo_h_briancon         <- renderText({ rv$dispo_h_briancon })
    output$dispo_h_sisteron         <- renderText({ rv$dispo_h_sisteron })
    output$pourcentage_reponse_total      <- renderText({ rv$pourcentage_reponse_total })
    output$nb_annuaire              <- renderText({ rv$nombre_personnel })
  })
  
  # --- Tous les tableaux dynamiques ---
  observe({
    
    # print(rv$Base_PB_incorrect)
    
    output$table_total              <- renderDT({ render_dt_table(rv$Base_PB_total) })
    output$table_ash                <- renderDT({ render_dt_table(rv$Base_PB_ash) })
    output$table_aide               <- renderDT({ render_dt_table(rv$Base_PB_aide_soignant) })
    output$table_ide                <- renderDT({ render_dt_table(rv$Base_PB_ide) })
    output$table_medecin            <- renderDT({ render_dt_table(rv$Base_PB_medecin) })
    output$table_arm                <- renderDT({ render_dt_table(rv$Base_PB_arm) })
    output$table_cadre              <- renderDT({ render_dt_table(rv$Base_PB_cadre) })
    output$table_chirurgien         <- renderDT({ render_dt_table(rv$Base_PB_chir) })
    output$table_ibode              <- renderDT({ render_dt_table(rv$Base_PB_ibode) })
    output$table_securite           <- renderDT({ render_dt_table(rv$Base_PB_secu) })
    output$table_pilote             <- renderDT({ render_dt_table(rv$Base_PB_pilot) })
    output$table_autres             <- renderDT({ render_dt_table(rv$Base_PB_autre) })
    output$table_non_enregistre     <- renderDT({ render_dt_table_2(rv$pas_present) })
    output$table_reponse_incorrect  <- renderDT({ render_dt_table(rv$Base_PB_incorrect) })
  })
}
