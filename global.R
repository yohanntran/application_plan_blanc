simulation=F

# Fichier Excel contenant les métadonnées PDF
annuaire <- normalizePath(file.path("..", "DB", "annuaire.xlsx"), mustWork = TRUE)
reponsesSMS <- normalizePath(file.path("..", "DB", "reponsesSMS.xls"), mustWork = TRUE)

annuaire = readxl::read_excel(annuaire, sheet="Contacts")%>% data.frame
reponseSMS = readxl::read_excel(reponsesSMS) %>% data.frame


# Gestion des conflits
annuaire = annuaire %>% # on enlève dans l'annuaire (dans la colonne liste) les caractères qui sont inutiles
  mutate(Liste = str_replace_all(Liste, "§",""),
         Liste = str_replace_all(Liste, "\r",""),
         Liste = str_replace_all(Liste, "\n","")
  )

# Sélection des colonnes
annuaire = annuaire %>% # on sélectionne le nom, prénom, fonction, numéro de téléphone, dernière date de modification
  dplyr::select(`Prénom`, Nom, Fonction, `Téléphone.mobile.1`, Liste, Date.de.modification)
# dans la liste on récupère le nom de l'établissement + fonction
reponseSMS = reponseSMS %>% # on sélectionne la date de reponse, le numéro, la réponse, le message des agents hospitaliers
  dplyr::select(`Date.de.réponse`, `Numéro`,`Réponse.reçue.de`,`Message`)

# on met les bons format dates et heures
reponseSMS$`Date.de.réponse` <- as.POSIXct(reponseSMS$`Date.de.réponse`, format = "%d/%m/%Y %H:%M:%S")


# Fusion des tableaux
annuaire = annuaire %>% 
  mutate(Nom = tolower(Nom),
         `Prénom` = tolower(`Prénom`)) %>% 
  mutate(
    Nom = str_replace(Nom, "urgences médecin - ",""), # on supprime les informations inutiles dans le nom pour ne récupérer que le nom
    Nom = str_replace(Nom, "urgences IDE - ",""),
    Nom = str_replace(Nom, "arm - ",""),
    Nom = str_replace(Nom, "anesthésiste - ",""),
    Nom = str_replace(Nom, "urgences as -",""),
    Nom = str_replace(Nom, "as. sociale - ",""),
    Nom = str_replace(Nom, "bloc as - ",""),
    Nom = str_replace(Nom, "bloc brancardier - ",""),
    Nom = str_replace(Nom, "bloc brancardier - ",""),
    Nom = str_replace(Nom, "bloc ide - ",""),
    Nom = str_replace(Nom, "ergothérapeute - ",""),
    Nom = str_replace(Nom, "chirurgien - ",""),
    Nom = str_replace(Nom, "ibode - ",""),
    Nom = str_replace(Nom, "merm - ",""),
    Nom = str_replace(Nom, "radio brancardier - ",""),
    Nom = str_replace(Nom, "réa as/ash - ",""),
    Nom = str_replace(Nom, "réa ide - ",""),
    Nom = str_replace(Nom, "réa médecin - ",""),
    Nom = str_replace(Nom, "urgences ash - ",""),
    Nom = str_replace(Nom, "urgences ide - ",""),
    Nom = str_replace(Nom, "/",""))

reponseSMS = reponseSMS %>% 
  mutate(`Réponse.reçue.de` = tolower(`Réponse.reçue.de` ),
         `Réponse.reçue.de` = str_replace(`Réponse.reçue.de`, "urgences médecin - ",""), #on supprime les informations inutiles dans le nom
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "urgences IDE - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "arm - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "anesthésiste - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "urgences as -",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "as. sociale - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "bloc as - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "bloc brancardier - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "bloc brancardier - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "bloc ide - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "ergothérapeute - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "chirurgien - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "ibode - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "merm - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "radio brancardier - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "réa as/ash - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "réa ide - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "réa médecin - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "urgences ash - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "urgences ide - ",""),
         `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "/",""))

annuaire = annuaire %>% #dans l'annuaire on créé une colonne nom prénom
  mutate(nom_prenom = paste(Nom,Prénom)) %>% 
  dplyr::select(-Téléphone.mobile.1) # on enlève le numéro de mobile puisqu'on va joindre selon le nom et prénom

#certains medecin ont des espaces superflus devant le nom... on les enlève donc
annuaire = annuaire %>% 
  mutate(nom_prenom = gsub("^\\s+|\\s+$", "", nom_prenom))

reponseSMS = reponseSMS %>% 
  mutate(`Réponse.reçue.de` = tolower(Réponse.reçue.de)) # on prépare à la jointure ... on transforme le nom prénom en minuscule
reponseSMS[c('Personnel', 'Fonction')] <- str_split_fixed(reponseSMS$`Réponse.reçue.de`, ', ',  2) # on créé une colonne personnel, fonction
reponseSMS = reponseSMS %>% 
  dplyr::select(-`Réponse.reçue.de`, -Fonction)

colnames(reponseSMS)[colnames(reponseSMS) =="Personnel"] = "nom_prenom" # on renomme la colonne personnel par nom_prenom dans la table des réponses
colnames(reponseSMS)[colnames(reponseSMS) =="Numéro"] = "Numéro de téléphone" # on renomme le numéro de téléphone

nombre_personnel = annuaire$nom_prenom %>% unique %>% length 

# on parcourt l'annuaire pour trouver la dernière ligne
recent_annuaire=data.frame()
#Plusieurs lignes pour certains médecin
for(i in annuaire$nom_prenom %>% unique){
  tmp = annuaire %>% 
    filter(nom_prenom == i) %>% 
    arrange(Date.de.modification)
  tmp = tail(tmp,1) # on sélectionne la ligne de l'annuaire la plus recente
  recent_annuaire = rbind(recent_annuaire,tmp)
}

#On enlève les messages vides
reponseSMS = reponseSMS %>% 
  filter(Message != "")

reponseSMS = distinct(reponseSMS)

#on selectionne la derniere reponse de chaque praticien
last_reponseSMS=data.frame()
for(i in reponseSMS$nom_prenom %>% unique){
  tmp = reponseSMS %>% 
    filter(nom_prenom == i) %>% 
    arrange(Date.de.réponse)
  tmp = tail(tmp,1)
  last_reponseSMS = rbind(last_reponseSMS,tmp)
} 

#liste les numéro de réponses
last_reponseSMS$num_reponse = seq(1,dim(last_reponseSMS)[1])

#on merge les 2 bases
Base_PB = merge(last_reponseSMS, recent_annuaire, how="left",by = "nom_prenom")


#étude des médecins non/mal enregistrés dans l'annuaire
pas_present = last_reponseSMS %>% 
  filter(num_reponse%in% setdiff(last_reponseSMS$num_reponse,Base_PB$num_reponse)) %>% 
  dplyr::select(-num_reponse, -nom_prenom)
pas_present$`Date.de.réponse` <- as.POSIXct(pas_present$`Date.de.réponse`, format = "%d/%m/%Y %H:%M:%S")


pas_present = pas_present %>% 
  mutate(Message_origine = paste0(Message, " (",substr(Date.de.réponse, 12,16),")"))

pas_present = pas_present %>% 
  mutate(Message = ifelse(nchar(Message) > 1, NA, Message))


pas_present = pas_present %>% 
  mutate(Message = case_when(str_detect(Message, "1")~"sur place",
                             str_detect(Message, "2")~"disponible en <30 min",
                             str_detect(Message, "3")~"disponible dans l'heure",
                             str_detect(Message, "4")~"disponible dans les 3h",
                             str_detect(Message, "5")~"disponible dans les 6h",
                             str_detect(Message, "6")~"disponible dans les 12h"
  )) %>% 
  mutate(Message = ifelse(is.na(Message), "réponse incorrecte",Message))

# pas_present$Date.de.réponse = as.POSIXct(pas_present$`Date.de.réponse`, format = "%d/%m/%Y %H:%M:%S")
pas_present = pas_present %>% 
  mutate(`Disponibilité théorique` = case_when(
    Message == "sur place" ~ `Date.de.réponse`,
    Message == "disponible en <30 min" ~ `Date.de.réponse`,
    Message == "disponible dans l'heure" ~ `Date.de.réponse` + hours(1),
    Message == "disponible dans les 3h" ~ `Date.de.réponse`  + hours(3),
    Message == "disponible dans les 6h" ~ `Date.de.réponse`  + hours(6),
    Message == "disponible dans les 12h" ~ `Date.de.réponse`  + hours(12)
  ))

pas_present = pas_present %>% 
  mutate(heure = now()) %>% 
  mutate(`Minute avant disponiblité` = difftime(`Disponibilité théorique` %>% as.character,heure%>% as.character, units='mins'))


pas_present = pas_present %>% 
  mutate(`Disponibilité` = case_when(`Minute avant disponiblité` <= 0 ~ "sur place",
                                     `Minute avant disponiblité` > 0 & `Minute avant disponiblité` < 30 ~ "disponible en <30 min",
                                     `Minute avant disponiblité` > 30 &  `Minute avant disponiblité` <= 60 ~ "disponible dans l'heure",
                                     `Minute avant disponiblité` > 60 & `Minute avant disponiblité` <= 180 ~ "disponible dans les 3h",
                                     `Minute avant disponiblité` > 180 & `Minute avant disponiblité` <= 360 ~ "disponible dans les 6h",
                                     `Minute avant disponiblité` > 360 & `Minute avant disponiblité` <= 720 ~ "disponible dans les 12h",
                                     is.na(`Minute avant disponiblité`) ~ "réponse incorrecte"))

pas_present = pas_present %>% 
  mutate(`Date.de.réponse` = substr(`Date.de.réponse`,1,10),
         `Date.de.réponse` = paste0(substr(`Date.de.réponse`,9,10), "/",substr(`Date.de.réponse`, 6,7), "/",substr(`Date.de.réponse`, 1,4))) %>% 
  dplyr::select(`Date.de.réponse`, `Numéro de téléphone`, Message_origine, `Disponibilité`)

colnames(pas_present)=c("Date de réponse","Numéro de téléphone", "Message d'origine","Disponibilité")

Base_PB = Base_PB %>% 
  dplyr::select(-num_reponse)

Base_PB = Base_PB %>% 
  dplyr::select(-Date.de.modification, -nom_prenom)
head(Base_PB)

#on recherche l'hopital de chaque médecin
Base_PB = Base_PB %>%
  mutate(`Hôpital` = case_when(grepl("brian", Liste, ignore.case = TRUE) ~"Briançon",
                               grepl("sis", Liste, ignore.case = TRUE) ~"Sisteron",
                               grepl("gap", Liste, ignore.case = TRUE) ~"Gap",
                               grepl("emb", Liste, ignore.case = TRUE) ~"Embrun"))

#On met les nom, prénom au propre
Base_PB = Base_PB %>% 
  mutate(Nom=str_to_title(Nom),
         `Prénom`=str_to_title(`Prénom`)
  )

Base_PB = Base_PB %>% 
  mutate(Liste = str_replace(Liste, "Gap - ",""), #on supprime les informations inutiles dans le nom
         Liste = str_replace(Liste, "Briançon - ",""),
         Liste = str_replace(Liste, "Sisteron - ",""),
         Liste = str_replace(Liste, "Pré alerte Sisteron ",""))

Base_PB = Base_PB %>% 
  dplyr::select(-Liste)

#Simulation liste de réponse
if(simulation == T){
  Base_PB$Message = sample(1:5, length(Base_PB$Message), replace=T)
}



Base_PB = Base_PB %>% 
  mutate(Message_origine = paste0(Message, " (",substr(Date.de.réponse, 12,16),")"))

Base_PB = Base_PB %>% 
  mutate(Message = ifelse(nchar(Message) > 1, NA, Message))

Base_PB = Base_PB %>% 
  mutate(Message = case_when(str_detect(Message, "1")~"sur place",
                             str_detect(Message, "2")~"disponible en <30 min",
                             str_detect(Message, "3")~"disponible dans l'heure",
                             str_detect(Message, "4")~"disponible dans les 3h",
                             str_detect(Message, "5")~"disponible dans les 6h",
                             str_detect(Message, "6")~"disponible dans les 12h"
  )) %>% 
  mutate(Message = ifelse(is.na(Message), "réponse incorrecte",Message))

# on créé l'identité de la personne
Base_PB = Base_PB %>% 
  mutate(`Identité` = paste(Nom, `Prénom`)) %>% 
  dplyr::select(-Nom,-`Prénom`)


# on simule des dates aléatoires
x <- seq(now(), now()+60*60*2, by="2 mins")

if(simulation == T){
  Base_PB$Date.de.réponse = sample(x[hour(x) > "00:00" & hour(x) < "24:00"], dim(Base_PB)[1], replace=T)
}

# on calcul la date de disponibilité théorique

Base_PB = Base_PB %>% 
  mutate(`Disponibilité théorique` = case_when(
    Message == "sur place" ~ `Date.de.réponse`,
    Message == "disponible en <30 min" ~ `Date.de.réponse`,
    Message == "disponible dans l'heure" ~ `Date.de.réponse` + hours(1),
    Message == "disponible dans les 3h" ~ `Date.de.réponse`  + hours(3),
    Message == "disponible dans les 6h" ~ `Date.de.réponse`  + hours(6),
    Message == "disponible dans les 12h" ~ `Date.de.réponse`  + hours(12)
    
  ))
# pwet
Base_PB = Base_PB %>% 
  mutate(heure = now()) %>% 
  mutate(`Minute avant disponiblité` = difftime(`Disponibilité théorique` %>% as.character, heure%>% as.character, units='mins'))

# pwet

Base_PB = Base_PB %>% 
  mutate(`Disponibilité` = case_when(`Minute avant disponiblité` <= 0 ~ "sur place",
                                     `Minute avant disponiblité` > 0 & `Minute avant disponiblité` < 30 ~ "disponible en <30 min",
                                     `Minute avant disponiblité` > 30 &  `Minute avant disponiblité` <= 60 ~ "disponible dans l'heure",
                                     `Minute avant disponiblité` > 60 & `Minute avant disponiblité` <= 180 ~ "disponible dans les 3h",
                                     `Minute avant disponiblité` > 180 & `Minute avant disponiblité` <= 360 ~ "disponible dans les 6h",
                                     `Minute avant disponiblité` > 360 & `Minute avant disponiblité` <= 720 ~ "disponible dans les 12h",
                                     is.na(`Minute avant disponiblité`) ~ "réponse incorrecte"))
# pwet
Base_PB = Base_PB %>% 
  mutate(Fonction = str_replace_all(Fonction, "é","e"),
         Fonction = tolower(Fonction))


Base_PB = Base_PB %>% 
  dplyr::select(`Identité`,`Numéro de téléphone`, `Hôpital`, Fonction, Message_origine, `Disponibilité`)

Base_PB$`Disponibilité`[is.na(Base_PB$`Disponibilité`)] = "indisponible"


Base_PB$`Disponibilité` = factor(Base_PB$`Disponibilité` , levels=c("sur place",
                                                                    "disponible en <30 min",
                                                                    "disponible dans l'heure",
                                                                    "disponible dans les 3h",
                                                                    "disponible dans les 6h",
                                                                    "disponible dans les 12h",
                                                                    "réponse incorrecte"))
Base_PB = Base_PB %>% 
  arrange(`Disponibilité`) %>% 
  mutate(`Numéro de téléphone` = str_replace_all(`Numéro de téléphone`, "\\+33","0"))

Base_PB$`Contact` = "_______"

Base_PB <- data.frame(sapply(Base_PB, as.factor, simplify=FALSE))

Base_PB_incorrect = Base_PB %>% 
  filter(`Disponibilité` == "réponse incorrecte")

# pwet
colnames(Base_PB_incorrect)=c("Identité","Numéro de téléphone", "Hôpital", "Fonction", "Message d'origine","Disponibilité","Contact")

Base_PB = Base_PB %>% 
  filter(`Disponibilité` != "réponse incorrecte")

Base_PB_aide_soignant = Base_PB %>% 
  filter(str_detect(Fonction, '^aide'))
Base_PB_ash = Base_PB %>% 
  filter(str_detect(Fonction, '^ash|^as/ash'))
Base_PB_medecin = Base_PB %>% 
  filter(str_detect(Fonction, '^medecin'))
Base_PB_arm = Base_PB %>% 
  filter(str_detect(Fonction, '^arm'))
Base_PB_cadre = Base_PB %>% 
  filter(str_detect(Fonction, '^cadre'))
Base_PB_chir = Base_PB %>% 
  filter(str_detect(Fonction, '^chir'))
Base_PB_pilot = Base_PB %>% 
  filter(str_detect(Fonction, '^pilot'))
# pwet
Base_PB_secu = Base_PB %>% 
  filter(str_detect(Fonction, '^secu'))
Base_PB_ibode = Base_PB %>% 
  filter(str_detect(Fonction, '^ibode'))
Base_PB_ide = Base_PB %>% 
  filter(str_detect(Fonction, '^ide'))

Base_PB_autre = Base_PB %>% 
  filter(!str_detect(Fonction, '^aide|^ash|^as/ash|^medecin|^arm|^cadre|^chir|^pilot|^secu|^ibode|^ide|^secu'))

# Base_PB_total <- bind_rows(Base_PB, Base_PB_incorrect, pas_present %>% 
#                              rename(`Message d'origine` = `Message d'origine`)) 

###########################################
# Traduction de certains modules utilisés #
###########################################
fr <- list(
  sProcessing = "Traitement en cours...", sSearch = "Rechercher&nbsp;:", 
  sLengthMenu = "Afficher _MENU_ &eacute;l&eacute;ments", 
  sInfo = "Affichage de l'&eacute;l&eacute;ment _START_ &agrave; _END_ sur _TOTAL_ &eacute;l&eacute;ments", 
  sInfoEmpty = "Affichage de l'&eacute;l&eacute;ment 0 &agrave; 0 sur 0 &eacute;l&eacute;ment", 
  sInfoFiltered = "(filtr&eacute; de _MAX_ &eacute;l&eacute;ments au total)", 
  sInfoPostFix = "", sLoadingRecords = "Chargement en cours...", 
  sZeroRecords = "Aucun &eacute;l&eacute;ment &agrave; afficher", 
  sEmptyTable = "Aucune donn&eacute;e disponible dans le tableau", 
  oPaginate = list(
    sFirst = "Premier", sPrevious = "Pr&eacute;c&eacute;dent", 
    sNext = "Suivant", sLast = "Dernier"
  ), 
  oAria = list(
    sSortAscending = ": activer pour trier la colonne par ordre croissant", 
    sSortDescending = ": activer pour trier la colonne par ordre d&eacute;croissant"
  )
)

nb_personne_disponible = Base_PB$Disponibilité[Base_PB$Disponibilité == "sur place ou disponible en <30 min"] %>% length
nb_personne_1h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans l'heure"] %>% length
nb_personne_3h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 3h"] %>% length
nb_personne_6h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 6h"] %>% length
nb_personne_12h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 12h"] %>% length
nb_personne_pas_present = dim(pas_present)[1]
nb_personne_no_reponse = Base_PB$Disponibilité[Base_PB$Disponibilité == "réponse incorrecte"] %>% length

pourcentage_reponse = paste0(dim(Base_PB)[1],"/",nombre_personnel," (", round((dim(Base_PB)[1]/nombre_personnel)*100,1),"%)")

nb_annuaire=nombre_personnel

colnames(Base_PB)[colnames(Base_PB) == "Numéro.de.téléphone"] = "Numéro de téléphone"
colnames(Base_PB)[colnames(Base_PB) == "Message_origine"] = "Message d'origine"


dispo_h_gap = Base_PB %>% 
  filter(`Hôpital` == "Gap") %>% 
  filter(`Disponibilité` %in% c("sur place","disponible en <30 min","disponible dans l'heure")) %>% dim()
dispo_h_gap = dispo_h_gap[1]

dispo_h_briancon = Base_PB %>% 
  filter(`Hôpital` == "Briançon") %>% 
  filter(`Disponibilité` %in% c("sur place","disponible en <30 min","disponible dans l'heure")) %>% dim()
dispo_h_briancon = dispo_h_briancon[1]

dispo_h_sisteron = Base_PB %>% 
  filter(`Hôpital` == "Sisteron") %>% 
  filter(`Disponibilité` %in% c("sur place","disponible en <30 min","disponible dans l'heure")) %>% dim()
dispo_h_sisteron = dispo_h_sisteron[1]

dispo_dans_h_total=dispo_h_gap+dispo_h_briancon+dispo_h_sisteron

nb_personne_disponible = Base_PB$Disponibilité[Base_PB$Disponibilité == "sur place"] %>% length
nb_personne_30min = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible en <30 min"] %>% length
nb_personne_1h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans l'heure"] %>% length
nb_personne_3h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 3h"] %>% length
nb_personne_6h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 6h"] %>% length
nb_personne_12h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 12h"] %>% length
nb_personne_incorrect = Base_PB_total$Disponibilité[Base_PB_total$Disponibilité == "réponse incorrecte"] %>% length
# print(Base_PB_total)
# print(paste0("nb_personne_incorrect ",nb_personne_incorrect))

pas_present_a_ajouter <- pas_present %>% 
  mutate(
    Identité = "Inconnu",                    # ou créer à partir du numéro si tu veux
    Hôpital = NA,                             # on ne connaît pas l'hôpital
    Fonction = NA,                            # fonction inconnue
    Contact = "_______"                        # même valeur que Base_PB
  ) %>% 
  dplyr::select(
    Identité, `Numéro de téléphone`, Hôpital, Fonction, `Message d'origine`, Disponibilité, Contact
  )

Base_PB_total <- bind_rows(Base_PB, Base_PB_incorrect, pas_present_a_ajouter %>% 
                             rename(`Message d'origine` = `Message d'origine`)) 

nb_personne_total <- nrow(Base_PB_total)
pourcentage_reponse_total <- paste0(nb_personne_total, "/", nombre_personnel,
                                    " (", round((nb_personne_total/nombre_personnel)*100,1), "%)")


traitement_donnees_complet <- function(simulation = F){
  
  # --- Fichiers ---
  annuaire_path <- normalizePath(file.path("..", "DB", "annuaire.xlsx"), mustWork = TRUE)
  reponsesSMS_path <- normalizePath(file.path("..", "DB", "reponsesSMS.xls"), mustWork = TRUE)
  
  # Lire les fichiers
  annuaire <- readxl::read_excel(annuaire_path, sheet = "Contacts") %>% data.frame()
  reponseSMS <- readxl::read_excel(reponsesSMS_path) %>% data.frame()
  
  # Maintenant tu peux utiliser mutate()
  annuaire <- annuaire %>% 
    mutate(Liste = str_replace_all(Liste, "§",""),
           Liste = str_replace_all(Liste, "\r",""),
           Liste = str_replace_all(Liste, "\n",""))
  
  # Sélection des colonnes
  annuaire = annuaire %>% # on sélectionne le nom, prénom, fonction, numéro de téléphone, dernière date de modification
    dplyr::select(`Prénom`, Nom, Fonction, `Téléphone.mobile.1`, Liste, Date.de.modification)
  # dans la liste on récupère le nom de l'établissement + fonction
  reponseSMS = reponseSMS %>% # on sélectionne la date de reponse, le numéro, la réponse, le message des agents hospitaliers
    dplyr::select(`Date.de.réponse`, `Numéro`,`Réponse.reçue.de`,`Message`)
  
  # on met les bons format dates et heures
  reponseSMS$`Date.de.réponse` <- as.POSIXct(reponseSMS$`Date.de.réponse`, format = "%d/%m/%Y %H:%M:%S")
  
  
  # Fusion des tableaux
  annuaire = annuaire %>% 
    mutate(Nom = tolower(Nom),
           `Prénom` = tolower(`Prénom`)) %>% 
    mutate(
      Nom = str_replace(Nom, "urgences médecin - ",""), # on supprime les informations inutiles dans le nom pour ne récupérer que le nom
      Nom = str_replace(Nom, "urgences IDE - ",""),
      Nom = str_replace(Nom, "arm - ",""),
      Nom = str_replace(Nom, "anesthésiste - ",""),
      Nom = str_replace(Nom, "urgences as -",""),
      Nom = str_replace(Nom, "as. sociale - ",""),
      Nom = str_replace(Nom, "bloc as - ",""),
      Nom = str_replace(Nom, "bloc brancardier - ",""),
      Nom = str_replace(Nom, "bloc brancardier - ",""),
      Nom = str_replace(Nom, "bloc ide - ",""),
      Nom = str_replace(Nom, "ergothérapeute - ",""),
      Nom = str_replace(Nom, "chirurgien - ",""),
      Nom = str_replace(Nom, "ibode - ",""),
      Nom = str_replace(Nom, "merm - ",""),
      Nom = str_replace(Nom, "radio brancardier - ",""),
      Nom = str_replace(Nom, "réa as/ash - ",""),
      Nom = str_replace(Nom, "réa ide - ",""),
      Nom = str_replace(Nom, "réa médecin - ",""),
      Nom = str_replace(Nom, "urgences ash - ",""),
      Nom = str_replace(Nom, "urgences ide - ",""),
      Nom = str_replace(Nom, "/",""))
  
  reponseSMS = reponseSMS %>% 
    mutate(`Réponse.reçue.de` = tolower(`Réponse.reçue.de` ),
           `Réponse.reçue.de` = str_replace(`Réponse.reçue.de`, "urgences médecin - ",""), #on supprime les informations inutiles dans le nom
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "urgences IDE - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "arm - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "anesthésiste - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "urgences as -",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "as. sociale - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "bloc as - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "bloc brancardier - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "bloc brancardier - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "bloc ide - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "ergothérapeute - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "chirurgien - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "ibode - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "merm - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "radio brancardier - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "réa as/ash - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "réa ide - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "réa médecin - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "urgences ash - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "urgences ide - ",""),
           `Réponse.reçue.de`  = str_replace(`Réponse.reçue.de`, "/",""))
  
  annuaire = annuaire %>% #dans l'annuaire on créé une colonne nom prénom
    mutate(nom_prenom = paste(Nom,Prénom)) %>% 
    dplyr::select(-Téléphone.mobile.1) # on enlève le numéro de mobile puisqu'on va joindre selon le nom et prénom
  
  #certains medecin ont des espaces superflus devant le nom... on les enlève donc
  annuaire = annuaire %>% 
    mutate(nom_prenom = gsub("^\\s+|\\s+$", "", nom_prenom))
  
  reponseSMS = reponseSMS %>% 
    mutate(`Réponse.reçue.de` = tolower(Réponse.reçue.de)) # on prépare à la jointure ... on transforme le nom prénom en minuscule
  reponseSMS[c('Personnel', 'Fonction')] <- str_split_fixed(reponseSMS$`Réponse.reçue.de`, ', ',  2) # on créé une colonne personnel, fonction
  reponseSMS = reponseSMS %>% 
    dplyr::select(-`Réponse.reçue.de`, -Fonction)
  
  colnames(reponseSMS)[colnames(reponseSMS) =="Personnel"] = "nom_prenom" # on renomme la colonne personnel par nom_prenom dans la table des réponses
  colnames(reponseSMS)[colnames(reponseSMS) =="Numéro"] = "Numéro de téléphone" # on renomme le numéro de téléphone
  
  nombre_personnel = annuaire$nom_prenom %>% unique %>% length 
  
  # on parcourt l'annuaire pour trouver la dernière ligne
  recent_annuaire=data.frame()
  #Plusieurs lignes pour certains médecin
  for(i in annuaire$nom_prenom %>% unique){
    tmp = annuaire %>% 
      filter(nom_prenom == i) %>% 
      arrange(Date.de.modification)
    tmp = tail(tmp,1) # on sélectionne la ligne de l'annuaire la plus recente
    recent_annuaire = rbind(recent_annuaire,tmp)
  }
  
  #On enlève les messages vides
  reponseSMS = reponseSMS %>% 
    filter(Message != "")
  
  reponseSMS = distinct(reponseSMS)
  
  #on selectionne la derniere reponse de chaque praticien
  last_reponseSMS=data.frame()
  for(i in reponseSMS$nom_prenom %>% unique){
    tmp = reponseSMS %>% 
      filter(nom_prenom == i) %>% 
      arrange(Date.de.réponse)
    tmp = tail(tmp,1)
    last_reponseSMS = rbind(last_reponseSMS,tmp)
  } 
  
  #liste les numéro de réponses
  last_reponseSMS$num_reponse = seq(1,dim(last_reponseSMS)[1])
  
  #on merge les 2 bases
  Base_PB = merge(last_reponseSMS, recent_annuaire, how="left",by = "nom_prenom")
  
  
  #étude des médecins non/mal enregistrés dans l'annuaire
  pas_present = last_reponseSMS %>% 
    filter(num_reponse%in% setdiff(last_reponseSMS$num_reponse,Base_PB$num_reponse)) %>% 
    dplyr::select(-num_reponse, -nom_prenom)
  pas_present$`Date.de.réponse` <- as.POSIXct(pas_present$`Date.de.réponse`, format = "%d/%m/%Y %H:%M:%S")
  
  
  pas_present = pas_present %>% 
    mutate(Message_origine = paste0(Message, " (",substr(Date.de.réponse, 12,16),")"))
  
  pas_present = pas_present %>% 
    mutate(Message = ifelse(nchar(Message) > 1, NA, Message))
  
  
  pas_present = pas_present %>% 
    mutate(Message = case_when(str_detect(Message, "1")~"sur place",
                               str_detect(Message, "2")~"disponible en <30 min",
                               str_detect(Message, "3")~"disponible dans l'heure",
                               str_detect(Message, "4")~"disponible dans les 3h",
                               str_detect(Message, "5")~"disponible dans les 6h",
                               str_detect(Message, "6")~"disponible dans les 12h"
    )) %>% 
    mutate(Message = ifelse(is.na(Message), "réponse incorrecte",Message))
  
  # pas_present$Date.de.réponse = as.POSIXct(pas_present$`Date.de.réponse`, format = "%d/%m/%Y %H:%M:%S")
  pas_present = pas_present %>% 
    mutate(`Disponibilité théorique` = case_when(
      Message == "sur place" ~ `Date.de.réponse`,
      Message == "disponible en <30 min" ~ `Date.de.réponse`,
      Message == "disponible dans l'heure" ~ `Date.de.réponse` + hours(1),
      Message == "disponible dans les 3h" ~ `Date.de.réponse`  + hours(3),
      Message == "disponible dans les 6h" ~ `Date.de.réponse`  + hours(6),
      Message == "disponible dans les 12h" ~ `Date.de.réponse`  + hours(12)
    ))
  
  pas_present = pas_present %>% 
    mutate(heure = now()) %>% 
    mutate(`Minute avant disponiblité` = difftime(`Disponibilité théorique` %>% as.character,heure%>% as.character, units='mins'))
  
  
  pas_present = pas_present %>% 
    mutate(`Disponibilité` = case_when(`Minute avant disponiblité` <= 0 ~ "sur place",
                                       `Minute avant disponiblité` > 0 & `Minute avant disponiblité` < 30 ~ "disponible en <30 min",
                                       `Minute avant disponiblité` > 30 &  `Minute avant disponiblité` <= 60 ~ "disponible dans l'heure",
                                       `Minute avant disponiblité` > 60 & `Minute avant disponiblité` <= 180 ~ "disponible dans les 3h",
                                       `Minute avant disponiblité` > 180 & `Minute avant disponiblité` <= 360 ~ "disponible dans les 6h",
                                       `Minute avant disponiblité` > 360 & `Minute avant disponiblité` <= 720 ~ "disponible dans les 12h",
                                       is.na(`Minute avant disponiblité`) ~ "réponse incorrecte"))
  
  pas_present = pas_present %>% 
    mutate(`Date.de.réponse` = substr(`Date.de.réponse`,1,10),
           `Date.de.réponse` = paste0(substr(`Date.de.réponse`,9,10), "/",substr(`Date.de.réponse`, 6,7), "/",substr(`Date.de.réponse`, 1,4))) %>% 
    dplyr::select(`Date.de.réponse`, `Numéro de téléphone`, Message_origine, `Disponibilité`)
  
  colnames(pas_present)=c("Date de réponse","Numéro de téléphone", "Message d'origine","Disponibilité")
  
  Base_PB = Base_PB %>% 
    dplyr::select(-num_reponse)
  
  Base_PB = Base_PB %>% 
    dplyr::select(-Date.de.modification, -nom_prenom)
  head(Base_PB)
  
  #on recherche l'hopital de chaque médecin
  Base_PB = Base_PB %>%
    mutate(`Hôpital` = case_when(grepl("brian", Liste, ignore.case = TRUE) ~"Briançon",
                                 grepl("sis", Liste, ignore.case = TRUE) ~"Sisteron",
                                 grepl("gap", Liste, ignore.case = TRUE) ~"Gap",
                                 grepl("emb", Liste, ignore.case = TRUE) ~"Embrun"))
  
  #On met les nom, prénom au propre
  Base_PB = Base_PB %>% 
    mutate(Nom=str_to_title(Nom),
           `Prénom`=str_to_title(`Prénom`)
    )
  
  Base_PB = Base_PB %>% 
    mutate(Liste = str_replace(Liste, "Gap - ",""), #on supprime les informations inutiles dans le nom
           Liste = str_replace(Liste, "Briançon - ",""),
           Liste = str_replace(Liste, "Sisteron - ",""),
           Liste = str_replace(Liste, "Pré alerte Sisteron ",""))
  
  Base_PB = Base_PB %>% 
    dplyr::select(-Liste)
  
  #Simulation liste de réponse
  if(simulation == T){
    Base_PB$Message = sample(1:5, length(Base_PB$Message), replace=T)
  }
  
  
  
  Base_PB = Base_PB %>% 
    mutate(Message_origine = paste0(Message, " (",substr(Date.de.réponse, 12,16),")"))
  
  Base_PB = Base_PB %>% 
    mutate(Message = ifelse(nchar(Message) > 1, NA, Message))
  
  Base_PB = Base_PB %>% 
    mutate(Message = case_when(str_detect(Message, "1")~"sur place",
                               str_detect(Message, "2")~"disponible en <30 min",
                               str_detect(Message, "3")~"disponible dans l'heure",
                               str_detect(Message, "4")~"disponible dans les 3h",
                               str_detect(Message, "5")~"disponible dans les 6h",
                               str_detect(Message, "6")~"disponible dans les 12h"
    )) %>% 
    mutate(Message = ifelse(is.na(Message), "réponse incorrecte",Message))
  
  # on créé l'identité de la personne
  Base_PB = Base_PB %>% 
    mutate(`Identité` = paste(Nom, `Prénom`)) %>% 
    dplyr::select(-Nom,-`Prénom`)
  
  
  # on simule des dates aléatoires
  x <- seq(now(), now()+60*60*2, by="2 mins")
  
  if(simulation == T){
    Base_PB$Date.de.réponse = sample(x[hour(x) > "00:00" & hour(x) < "24:00"], dim(Base_PB)[1], replace=T)
  }
  
  # on calcul la date de disponibilité théorique
  
  Base_PB = Base_PB %>% 
    mutate(`Disponibilité théorique` = case_when(
      Message == "sur place" ~ `Date.de.réponse`,
      Message == "disponible en <30 min" ~ `Date.de.réponse`,
      Message == "disponible dans l'heure" ~ `Date.de.réponse` + hours(1),
      Message == "disponible dans les 3h" ~ `Date.de.réponse`  + hours(3),
      Message == "disponible dans les 6h" ~ `Date.de.réponse`  + hours(6),
      Message == "disponible dans les 12h" ~ `Date.de.réponse`  + hours(12)
      
    ))
  # pwet
  Base_PB = Base_PB %>% 
    mutate(heure = now()) %>% 
    mutate(`Minute avant disponiblité` = difftime(`Disponibilité théorique` %>% as.character, heure%>% as.character, units='mins'))
  
  # pwet
  
  Base_PB = Base_PB %>% 
    mutate(`Disponibilité` = case_when(`Minute avant disponiblité` <= 0 ~ "sur place",
                                       `Minute avant disponiblité` > 0 & `Minute avant disponiblité` < 30 ~ "disponible en <30 min",
                                       `Minute avant disponiblité` > 30 &  `Minute avant disponiblité` <= 60 ~ "disponible dans l'heure",
                                       `Minute avant disponiblité` > 60 & `Minute avant disponiblité` <= 180 ~ "disponible dans les 3h",
                                       `Minute avant disponiblité` > 180 & `Minute avant disponiblité` <= 360 ~ "disponible dans les 6h",
                                       `Minute avant disponiblité` > 360 & `Minute avant disponiblité` <= 720 ~ "disponible dans les 12h",
                                       is.na(`Minute avant disponiblité`) ~ "réponse incorrecte"))
  # pwet
  Base_PB = Base_PB %>% 
    mutate(Fonction = str_replace_all(Fonction, "é","e"),
           Fonction = tolower(Fonction))
  
  
  Base_PB = Base_PB %>% 
    dplyr::select(`Identité`,`Numéro de téléphone`, `Hôpital`, Fonction, Message_origine, `Disponibilité`)
  
  Base_PB$`Disponibilité`[is.na(Base_PB$`Disponibilité`)] = "indisponible"
  
  
  Base_PB$`Disponibilité` = factor(Base_PB$`Disponibilité` , levels=c("sur place",
                                                                      "disponible en <30 min",
                                                                      "disponible dans l'heure",
                                                                      "disponible dans les 3h",
                                                                      "disponible dans les 6h",
                                                                      "disponible dans les 12h",
                                                                      "réponse incorrecte"))
  Base_PB = Base_PB %>% 
    arrange(`Disponibilité`) %>% 
    mutate(`Numéro de téléphone` = str_replace_all(`Numéro de téléphone`, "\\+33","0"))
  
  Base_PB$`Contact` = "_______"
  
  Base_PB <- data.frame(sapply(Base_PB, as.factor, simplify=FALSE))
  
  Base_PB_incorrect = Base_PB %>% 
    filter(`Disponibilité` == "réponse incorrecte")
  
  # pwet
  colnames(Base_PB_incorrect)=c("Identité","Numéro de téléphone", "Hôpital", "Fonction", "Message d'origine","Disponibilité","Contact")
  

  
  Base_PB_aide_soignant = Base_PB %>% 
    filter(str_detect(Fonction, '^aide'))
  Base_PB_ash = Base_PB %>% 
    filter(str_detect(Fonction, '^ash|^as/ash'))
  Base_PB_medecin = Base_PB %>% 
    filter(str_detect(Fonction, '^medecin'))
  Base_PB_arm = Base_PB %>% 
    filter(str_detect(Fonction, '^arm'))
  Base_PB_cadre = Base_PB %>% 
    filter(str_detect(Fonction, '^cadre'))
  Base_PB_chir = Base_PB %>% 
    filter(str_detect(Fonction, '^chir'))
  Base_PB_pilot = Base_PB %>% 
    filter(str_detect(Fonction, '^pilot'))
  Base_PB_secu = Base_PB %>% 
    filter(str_detect(Fonction, '^secu'))
  Base_PB_ibode = Base_PB %>% 
    filter(str_detect(Fonction, '^ibode'))
  Base_PB_ide = Base_PB %>% 
    filter(str_detect(Fonction, '^ide'))
  
  Base_PB_autre = Base_PB %>% 
    filter(!str_detect(Fonction, '^aide|^ash|^as/ash|^medecin|^arm|^cadre|^chir|^pilot|^secu|^ibode|^ide|^secu'))
  
  Base_PB = Base_PB %>% 
    filter(`Disponibilité` != "réponse incorrecte")
  
  # Base_PB_total <- bind_rows(Base_PB, Base_PB_incorrect, pas_present %>% 
  #                              rename(`Message d'origine` = `Message d'origine`)) 
  
  ###########################################
  # Traduction de certains modules utilisés #
  ###########################################
  fr <- list(
    sProcessing = "Traitement en cours...", sSearch = "Rechercher&nbsp;:", 
    sLengthMenu = "Afficher _MENU_ &eacute;l&eacute;ments", 
    sInfo = "Affichage de l'&eacute;l&eacute;ment _START_ &agrave; _END_ sur _TOTAL_ &eacute;l&eacute;ments", 
    sInfoEmpty = "Affichage de l'&eacute;l&eacute;ment 0 &agrave; 0 sur 0 &eacute;l&eacute;ment", 
    sInfoFiltered = "(filtr&eacute; de _MAX_ &eacute;l&eacute;ments au total)", 
    sInfoPostFix = "", sLoadingRecords = "Chargement en cours...", 
    sZeroRecords = "Aucun &eacute;l&eacute;ment &agrave; afficher", 
    sEmptyTable = "Aucune donn&eacute;e disponible dans le tableau", 
    oPaginate = list(
      sFirst = "Premier", sPrevious = "Pr&eacute;c&eacute;dent", 
      sNext = "Suivant", sLast = "Dernier"
    ), 
    oAria = list(
      sSortAscending = ": activer pour trier la colonne par ordre croissant", 
      sSortDescending = ": activer pour trier la colonne par ordre d&eacute;croissant"
    )
  )
  
  nb_personne_disponible = Base_PB$Disponibilité[Base_PB$Disponibilité == "sur place ou disponible en <30 min"] %>% length
  nb_personne_1h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans l'heure"] %>% length
  nb_personne_3h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 3h"] %>% length
  nb_personne_6h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 6h"] %>% length
  nb_personne_12h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 12h"] %>% length
  nb_personne_pas_present = dim(pas_present)[1]
  nb_personne_no_reponse = Base_PB$Disponibilité[Base_PB$Disponibilité == "réponse incorrecte"] %>% length
  
  pourcentage_reponse = paste0(dim(Base_PB)[1],"/",nombre_personnel," (", round((dim(Base_PB)[1]/nombre_personnel)*100,1),"%)")
  
  nb_annuaire=nombre_personnel
  
  colnames(Base_PB)[colnames(Base_PB) == "Numéro.de.téléphone"] = "Numéro de téléphone"
  colnames(Base_PB)[colnames(Base_PB) == "Message_origine"] = "Message d'origine"
  
  
  dispo_h_gap = Base_PB %>% 
    filter(`Hôpital` == "Gap") %>% 
    filter(`Disponibilité` %in% c("sur place","disponible en <30 min","disponible dans l'heure")) %>% dim()
  dispo_h_gap = dispo_h_gap[1]
  
  dispo_h_briancon = Base_PB %>% 
    filter(`Hôpital` == "Briançon") %>% 
    filter(`Disponibilité` %in% c("sur place","disponible en <30 min","disponible dans l'heure")) %>% dim()
  dispo_h_briancon = dispo_h_briancon[1]
  
  dispo_h_sisteron = Base_PB %>% 
    filter(`Hôpital` == "Sisteron") %>% 
    filter(`Disponibilité` %in% c("sur place","disponible en <30 min","disponible dans l'heure")) %>% dim()
  dispo_h_sisteron = dispo_h_sisteron[1]
  
  dispo_dans_h_total=dispo_h_gap+dispo_h_briancon+dispo_h_sisteron
  
  nb_personne_disponible = Base_PB$Disponibilité[Base_PB$Disponibilité == "sur place"] %>% length
  nb_personne_30min = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible en <30 min"] %>% length
  nb_personne_1h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans l'heure"] %>% length
  nb_personne_3h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 3h"] %>% length
  nb_personne_6h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 6h"] %>% length
  nb_personne_12h = Base_PB$Disponibilité[Base_PB$Disponibilité == "disponible dans les 12h"] %>% length
  # nb_personne_incorrect = Base_PB$Disponibilité[Base_PB$Disponibilité == "réponse incorrecte"] %>% length
  
  
  pas_present_a_ajouter <- pas_present %>% 
    mutate(
      Identité = "Inconnu",                    # ou créer à partir du numéro si tu veux
      Hôpital = NA,                             # on ne connaît pas l'hôpital
      Fonction = NA,                            # fonction inconnue
      Contact = "_______"                        # même valeur que Base_PB
    ) %>% 
    dplyr::select(
      Identité, `Numéro de téléphone`, Hôpital, Fonction, `Message d'origine`, Disponibilité, Contact
    )
  
  Base_PB_total <- bind_rows(Base_PB, Base_PB_incorrect, pas_present_a_ajouter %>% 
                               rename(`Message d'origine` = `Message d'origine`)) 
  
  # Définir l'ordre des disponibilités
  dispo_levels <- c(
    "sur place", 
    "disponible en <30 min", 
    "disponible dans l'heure", 
    "disponible dans les 3h",
    "disponible dans les 6h",
    "disponible dans les 12h",
    "Inconnu",          # on gère Inconnu dans le tri par Identité aussi
    "réponse incorrecte" # toujours à la fin
  )
  
  Base_PB_total <- Base_PB_total %>%
    mutate(
      Disponibilité = factor(Disponibilité, levels = dispo_levels)
    ) %>%
    arrange(
      # Mettre "réponse incorrecte" à la fin
      Disponibilité == "réponse incorrecte",
      Disponibilité,               # ordre des disponibilités
      Identité == "Inconnu",       # mettre Inconnu à la fin dans chaque dispo
      Identité                     # tri normal des autres noms
    )
  
  nb_personne_total <- nrow(Base_PB_total)
  pourcentage_reponse_total <- paste0(nb_personne_total, "/", nombre_personnel,
                                      " (", round((nb_personne_total/nombre_personnel)*100,1), "%)")
  nb_personne_incorrect = Base_PB_total$Disponibilité[Base_PB_total$Disponibilité == "réponse incorrecte"] %>% length
  

  Base_PB_incorrect = Base_PB_total %>% 
    filter(`Disponibilité` == "réponse incorrecte")
  

  return(list(
    Base_PB = Base_PB,
    Base_PB_incorrect = Base_PB_incorrect,
    pas_present = pas_present,
    pas_present_a_ajouter = pas_present_a_ajouter,
    Base_PB_total = Base_PB_total,
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
    nb_personne_incorrect = nb_personne_incorrect,
    nb_personne_total = nb_personne_total,
    pourcentage_reponse_total = pourcentage_reponse_total,
    dispo_h_gap = dispo_h_gap,
    dispo_h_briancon = dispo_h_briancon,
    dispo_h_sisteron = dispo_h_sisteron,
    dispo_dans_h_total = dispo_dans_h_total,
    nb_personne_disponible = nb_personne_disponible,
    nb_personne_30min = nb_personne_30min,
    nb_personne_1h = nb_personne_1h,
    nb_personne_3h = nb_personne_3h,
    nb_personne_6h = nb_personne_6h,
    nb_personne_12h = nb_personne_12h,
    nb_personne_pas_present = nb_personne_pas_present
  ))
}