server <- function(input, output, session) {
  
  rv <- reactiveValues()
  

  # --- Quand un fichier annuaire est importé ---
  observeEvent(input$file_annuaire, {
    req(input$file_annuaire)
    
    # Copier le fichier dans ton dossier de stockage
    dest_file <- file.path("..","DB","annuaire.xlsx")
    file.copy(input$file_annuaire$datapath, dest_file, overwrite = TRUE)
    
    # Relancer le script de traitement des données
    source("global.R", local = TRUE)
    
    # Recharger les données
    charger_donnees()
    
    # Afficher notification
    showNotification(
      "Nouvel annuaire importé avec succès !", 
      type = "message",  # peut être "message", "warning", "error"
      duration = 5       # durée en secondes
    )
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
    resultats <- traitement_donnees_complet()
    
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
    
    # Copier le fichier dans ton dossier de stockage
    dest_file <- file.path("..","DB","reponsesSMS.xls")
    file.copy(input$file_sms$datapath, dest_file, overwrite = TRUE)
    
    # Obtenir la date de dernière modification du fichier réel
    rv$last_modif <- file.info(dest_file)$mtime
    
    # Mettre à jour le texte dans l'UI
    output$date_fichier <- renderText({
      format(rv$last_modif, "%d/%m/%Y %H:%M:%S")
    })
    
    # Recharger les données
    charger_donnees()
    
    # Afficher notification
    showNotification(
      "Nouvelles réponses SMS importées avec succès !",
      type = "message",
      duration = 5
    )
  })
  
  
  
  # --- Fonction utilitaire pour créer les tableaux ---
  render_dt_table <- function(data) {
    datatable(
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
    ) %>% 
      formatStyle(
        'Disponibilité',
        target = 'row',
        backgroundColor = styleEqual(
          c("sur place","disponible en <30 min","disponible dans l'heure","disponible dans les 3h",
            "disponible dans les 6h", "disponible dans les 12h","réponse incorrecte"),
          c("#FFFFFF","#F0EDCF","#FDE767","#F3B95F","#EE9322","#C08261","#D83F31")
        )
      )
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
