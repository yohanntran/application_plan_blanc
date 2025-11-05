# global.R

# --- Configuration & Package Loading ---

# Set a global variable for simulation mode.
# If TRUE, the app will generate random data for testing purposes.
simulation <- F

# Define the list of required packages for the application.
packages <- c(
  "shiny", "shinyjs", "shinydashboard", "flexdashboard", # Shiny framework
  "tidyverse", "dplyr", "stringr", "lubridate", # Data manipulation & management
  "readxl", "rio", "openxlsx", # File import/export
  "DT", "plotly", "powerjoin", # Interactive UI elements
  "here", "devtools" # Development & path management
)

# Load all required packages.
# In a production environment, it is expected that these packages are pre-installed.
lapply(packages, library, character.only = TRUE)


# --- File Path Management ---

# Use an absolute path inside the container, as defined in docker-compose.yml.
# This is more robust than using relative paths like `..`.
db_dir <- "/srv/DB" 
# db_dir <- "//NETUSERS/Partages/Intra-Service/CHB/dim/Statistiques/APP PLAN BLANC/DB"
annuaire_path <- file.path(db_dir, "annuaire.xlsx")
reponsesSMS_path <- file.path(db_dir, "reponsesSMS.xls")

# Create the directory if it doesn't exist (useful for first-time setup)
if (!dir.exists(db_dir)) {
  dir.create(db_dir, recursive = TRUE)
}

# Check for the existence of the 'annuaire.xlsx' file and display a message if not found.
if (!file.exists(annuaire_path)) {
  message("⚠️ Fichier annuaire.xlsx introuvable — il sera créé lors du premier import.")
}

# Check for the existence of the 'reponsesSMS.xls' file and display a message if not found.
if (!file.exists(reponsesSMS_path)) {
  message("⚠️ Fichier reponsesSMS.xls introuvable — il sera créé lors du premier import.")
}

# --- Main Data Processing Function ---

# Calcul des disponibilités

# --- Fonction pour calculer la disponibilité ---
calcul_disponibilite <- function(df, dispo_levels) {
  df %>%
    mutate(
      Message_origine = ifelse(is.na(Message), NA_character_, paste0(Message, " (", format(Date.de.réponse, "%H:%M"), ")")),
      Message_std = case_when(
        str_detect(Message, "^[1]$") ~ "sur place",
        str_detect(Message, "^[2]$") ~ "disponible en <30 min",
        str_detect(Message, "^[3]$") ~ "disponible dans l'heure",
        str_detect(Message, "^[4]$") ~ "disponible dans les 3h",
        str_detect(Message, "^[5]$") ~ "disponible dans les 6h",
        str_detect(Message, "^[6]$") ~ "disponible dans les 12h",
        TRUE ~ "réponse incorrecte"  # plusieurs chiffres ou autre valeur
      ),
      `Disponibilité théorique` = case_when(
        Message_std == "sur place" ~ Date.de.réponse,
        Message_std == "disponible en <30 min" ~ Date.de.réponse + minutes(30),
        Message_std == "disponible dans l'heure" ~ Date.de.réponse + hours(1),
        Message_std == "disponible dans les 3h" ~ Date.de.réponse + hours(3),
        Message_std == "disponible dans les 6h" ~ Date.de.réponse + hours(6),
        Message_std == "disponible dans les 12h" ~ Date.de.réponse + hours(12),
        TRUE ~ as.POSIXct(NA)
      ),
      `Minute avant disponiblité` = as.numeric(difftime(`Disponibilité théorique`, now(), units = 'mins')),
      `Disponibilité` = case_when(
        Message_std == "réponse incorrecte" ~ "réponse incorrecte",
        is.na(`Minute avant disponiblité`) | `Minute avant disponiblité` > 720 ~ "indisponible",
        `Minute avant disponiblité` <= 0 ~ "sur place",
        `Minute avant disponiblité` <= 30 ~ "disponible en <30 min",
        `Minute avant disponiblité` <= 60 ~ "disponible dans l'heure",
        `Minute avant disponiblité` <= 180 ~ "disponible dans les 3h",
        `Minute avant disponiblité` <= 360 ~ "disponible dans les 6h",
        `Minute avant disponiblité` <= 720 ~ "disponible dans les 12h"
      ),
      `Disponibilité` = factor(`Disponibilité`, levels = dispo_levels),
      `Numéro de téléphone` = str_replace(`Numéro de téléphone`, "^\\+33", "0"),
      `Contact` = "_______"
    ) %>%
    arrange(`Disponibilité`, `Identité`)
}

#' traitement_donnees_complet
#'
#' This is the core function for data processing. It reads, cleans, merges, and analyzes
#' the 'annuaire' and 'reponsesSMS' data to generate the final datasets for the dashboard.
#'
#' @param simulation A logical value. If TRUE, simulates SMS responses for testing.
#' @return A list containing all the data frames and summary statistics for the UI.
traitement_donnees_complet <- function(simulation = FALSE) {

  
  # --- 1. Data Loading ---
  # Read the 'Contacts' sheet from the annuaire Excel file.
  annuaire <- readxl::read_excel(annuaire_path, sheet = "Contacts") %>% data.frame()
  # Read the SMS responses Excel file.
  reponseSMS <- readxl::read_excel(reponsesSMS_path) %>% data.frame()

  # --- 2. Initial Data Cleaning ---
  # Clean the 'Liste' column in the directory.
  annuaire <- annuaire %>%
    mutate(Liste = str_replace_all(Liste, "[§\r\n]", ""))

  # Select relevant columns from both data frames.
  annuaire <- annuaire %>%
    dplyr::select(
      `Prénom`, Nom, Fonction, `Téléphone.mobile.1`, Liste, Date.de.modification
    )
  reponseSMS <- reponseSMS %>%
    dplyr::select(`Date.de.réponse`, `Numéro`, `Réponse.reçue.de`, `Message`)

  # Convert date-time columns to the correct format.
  reponseSMS$`Date.de.réponse` <- as.POSIXct(reponseSMS$`Date.de.réponse`, format = "%d/%m/%Y %H:%M:%S")

  # --- 3. Pre-processing for Merging ---
  # Create a clean 'nom_prenom' key for joining.
  # This involves standardizing names by removing titles/prefixes and converting to lowercase.
  name_prefixes_to_remove <- "urgences médecin - |urgences IDE - |arm - |anesthésiste - |urgences as -|as\\. sociale - |bloc as - |bloc brancardier - |bloc ide - |ergothérapeute - |chirurgien - |ibode - |merm - |radio brancardier - |réa as/ash - |réa ide - |réa médecin - |urgences ash - |/"
  
  annuaire <- annuaire %>%
    mutate(
      Nom = tolower(Nom),
      `Prénom` = tolower(`Prénom`),
      Nom = str_replace_all(Nom, name_prefixes_to_remove, ""),
      nom_prenom = str_trim(paste(Nom, `Prénom`))
    ) %>%
    dplyr::select(-`Téléphone.mobile.1`) # Remove phone number to avoid conflicts after join

  # Robustly separate 'Personnel' from 'Fonction_sms' even with multiple commas.
  reponseSMS <- reponseSMS %>%
    mutate(
      `Réponse.reçue.de` = tolower(`Réponse.reçue.de`),
      `Réponse.reçue.de` = str_replace_all(`Réponse.reçue.de`, name_prefixes_to_remove, ""),
      # Extract the part before the last comma as the name
      Personnel = str_replace(`Réponse.reçue.de`, ",[^,]+$", ""),
      # Extract the part after the last comma as the function (if it exists)
      Fonction_sms = str_extract(`Réponse.reçue.de`, "[^,]+$")
    ) %>%
    mutate(nom_prenom = str_trim(tolower(Personnel))) %>%
    dplyr::select(-Personnel, -Fonction_sms, -`Réponse.reçue.de`)


  colnames(reponseSMS)[colnames(reponseSMS) == "Numéro"] <- "Numéro de téléphone"

  if (simulation) {
    Base_PB_total$Message <- as.character(sample(1:7, nrow(Base_PB_total), replace = TRUE))
    Base_PB_total$Date.de.réponse <- sample(seq(now() - hours(2), now(), by = "2 mins"), nrow(Base_PB_total), replace = TRUE)
  }
  
  # --- 4. Filter for Most Recent Records (Performance Optimization) ---
  # Instead of looping, use dplyr's group_by and slice_max to efficiently get the latest entry per person.
  
  # Get the most recent directory entry for each person.
  recent_annuaire <- annuaire %>%
    filter(!is.na(Date.de.modification)) %>%
    group_by(nom_prenom) %>%
    slice_max(order_by = Date.de.modification, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  nombre_personnel <- n_distinct(recent_annuaire$nom_prenom)

  # Get the last non-empty SMS response from each person.
  last_reponseSMS <- reponseSMS %>%
    filter(Message != "" & !is.na(Message)) %>%
    group_by(nom_prenom) %>%
    slice_max(order_by = Date.de.réponse, n = 1, with_ties = FALSE) %>%
    ungroup()

  # --- 5. Merge Data & Identify Unregistered Users ---
  # Left join SMS responses with the directory.
  Base_PB <- left_join(last_reponseSMS, recent_annuaire, by = "nom_prenom")

  # Identify users who sent an SMS but are not in the directory (or have mismatching names).
  pas_present <- anti_join(last_reponseSMS, recent_annuaire, by = "nom_prenom")

  # --- 6. Process Merged Data (Base_PB) ---
  Base_PB <- Base_PB %>%
    mutate(
      `Hôpital` = case_when(
        grepl("brian", Liste, ignore.case = TRUE) ~ "Briançon",
        grepl("sis", Liste, ignore.case = TRUE) ~ "Sisteron",
        grepl("gap", Liste, ignore.case = TRUE) ~ "Gap",
        grepl("emb", Liste, ignore.case = TRUE) ~ "Embrun",
        TRUE ~ "Inconnu"
      ),
      Nom = str_to_title(Nom),
      `Prénom` = str_to_title(`Prénom`),
      `Identité` = paste(Nom, `Prénom`),
      Fonction = tolower(str_replace_all(Fonction, "é", "e"))
    ) %>%
    dplyr::select(
      `Identité`, `Numéro de téléphone`, `Hôpital`, Fonction, Message, `Date.de.réponse`
    ) %>% 
    mutate(Identité = ifelse(`Identité` == "NA NA", "Inconnu", Identité)) %>% 
    mutate(Fonction = ifelse(is.na(`Fonction`), "Inconnu", Fonction))

  # --- 7. Process Unregistered Data (pas_present) ---
  # Create a clean data frame for users not found in the directory.
  if (nrow(pas_present) > 0) {
    pas_present <- pas_present %>%
      mutate(
        `Identité` = "Inconnu",
        `Hôpital` = "Inconnu",
        Fonction = "Inconnu"
      ) %>%
      dplyr::select(
        `Identité`, `Numéro de téléphone`, `Hôpital`, Fonction, Message, `Date.de.réponse`
      )
  }


  Base_PB_total <- Base_PB

  # --- 9. Calculate Availability Status ---
  # This section translates the raw SMS message (e.g., "1", "2") into human-readable availability.
  


  # Translate numeric codes to availability status.
  # Then, calculate the actual current availability based on the time elapsed since the response.
  dispo_levels <- c("sur place", "disponible en <30 min", "disponible dans l'heure", "disponible dans les 3h", "disponible dans les 6h", "disponible dans les 12h", "réponse incorrecte", "indisponible")

  dispo_levels <- c("sur place", "disponible en <30 min", "disponible dans l'heure",
                    "disponible dans les 3h", "disponible dans les 6h",
                    "disponible dans les 12h", "réponse incorrecte", "indisponible")
  
  Base_PB_total <- calcul_disponibilite(Base_PB_total, dispo_levels)
  
  pas_present <- calcul_disponibilite(pas_present, dispo_levels)
  
  # --- 10. Final Data Preparation ---
  # Select final columns for the main table.
  Base_PB_final <- Base_PB_total %>%
    dplyr::select(`Identité`, `Numéro de téléphone`, `Hôpital`, Fonction, `Message d'origine` = Message_origine, `Disponibilité`, `Contact`)

  # --- 11. Create Subsets for UI Tabs ---
  # Filter the main table by function for the different tabs in the UI.
  Base_PB_incorrect <- Base_PB_final %>% dplyr::filter(`Disponibilité` == "réponse incorrecte")
  Base_PB_aide_soignant <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^aide'))
  Base_PB_ash <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^ash|^as/ash'))
  Base_PB_medecin <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^medecin'))
  Base_PB_arm <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^arm'))
  # CORRECTED TYPO: str_detec -> str_detect
  Base_PB_cadre <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^cadre'))
  Base_PB_chir <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^chir'))
  Base_PB_pilot <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^pilot'))
  Base_PB_secu <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^secu'))
  Base_PB_ibode <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^ibode'))
  Base_PB_ide <- Base_PB_final %>% dplyr::filter(str_detect(Fonction, '^ide'))
  Base_PB_autre <- Base_PB_final %>% dplyr::filter(!str_detect(Fonction, '^aide|^ash|^as/ash|^medecin|^arm|^cadre|^chir|^pilot|^secu|^ibode|^ide'))
  
  # Prepare the table for unregistered users, selecting only relevant columns.
  pas_present_final <- pas_present %>% 
    mutate(
        `Date de réponse` = format(Date.de.réponse, "%d/%m/%Y"),
        `Message d'origine` = paste0(Message, " (", format(Date.de.réponse, "%H:%M"), ")"),
        # `Disponibilité` = "Inconnu" # Since they are not in the main flow
    ) %>%
    dplyr::select(`Date de réponse`, `Numéro de téléphone`, `Message d'origine`, `Disponibilité`)


  # --- 12. Calculate Summary Statistics for Sidebar ---
  nb_personne_total <- nrow(Base_PB_total)
  pourcentage_reponse_total <- paste0(nb_personne_total, "/", nombre_personnel, " (", round((nb_personne_total / nombre_personnel) * 100, 1), "%)")
  dispo_summary <- Base_PB_final %>%
    group_by(`Disponibilité`) %>%
    summarise(count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = `Disponibilité`, values_from = count, values_fill = 0)

  # Helper to safely get counts
  get_count <- function(df, col_name) { if (col_name %in% names(df)) df[[col_name]] else 0 }
  
  # Calculate hospital availability within the hour.
  dispo_h_total <- Base_PB_final %>%
    filter(`Disponibilité` %in% c("sur place", "disponible en <30 min", "disponible dans l'heure"))
    
  dispo_h_gap <- dispo_h_total %>% filter(`Hôpital` == "Gap") %>% nrow()
  dispo_h_briancon <- dispo_h_total %>% filter(`Hôpital` == "Briançon") %>% nrow()
  dispo_h_sisteron <- dispo_h_total %>% filter(`Hôpital` == "Sisteron") %>% nrow()
  dispo_dans_h_total <- dispo_h_gap + dispo_h_briancon + dispo_h_sisteron

  # --- 13. Return All Objects ---
  # Return a list containing every data frame and value needed by the server.
  return(list(
    Base_PB_total = Base_PB_final,
    Base_PB_incorrect = Base_PB_incorrect,
    pas_present = pas_present_final,
    Base_PB_aide_soignant = Base_PB_aide_soignant,
    Base_PB_ash = Base_PB_ash,
    Base_PB_medecin = Base_PB_medecin,
    Base_PB_arm = Base_PB_arm,
    Base_PB_cadre = Base_PB_cadre,
    Base_PB_chir = Base_PB_chir,
    Base_PB_pilot = Base_PB_pilot,
    Base_PB_secu = Base_PB_secu,
    Base_PB_ibode = Base_PB_ibode,
    Base_PB_ide = Base_PB_ide,
    Base_PB_autre = Base_PB_autre,
    nombre_personnel = nombre_personnel,
    nb_personne_total = nb_personne_total,
    pourcentage_reponse_total = pourcentage_reponse_total,
    dispo_h_gap = dispo_h_gap,
    dispo_h_briancon = dispo_h_briancon,
    dispo_h_sisteron = dispo_h_sisteron,
    dispo_dans_h_total = dispo_dans_h_total,
    nb_personne_disponible = get_count(dispo_summary, "sur place"),
    nb_personne_30min = get_count(dispo_summary, "disponible en <30 min"),
    nb_personne_1h = get_count(dispo_summary, "disponible dans l'heure"),
    nb_personne_3h = get_count(dispo_summary, "disponible dans les 3h"),
    nb_personne_6h = get_count(dispo_summary, "disponible dans les 6h"),
    nb_personne_12h = get_count(dispo_summary, "disponible dans les 12h"),
    nb_personne_incorrect = get_count(dispo_summary, "réponse incorrecte"),
    nb_personne_pas_present = nrow(pas_present)
  ))
}