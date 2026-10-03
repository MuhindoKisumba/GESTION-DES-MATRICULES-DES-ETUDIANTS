####################################################################################################
# GESTION DES MATRICULES DES ÉTUDIANTS
# Langage : R
# Auteur   : MUHINDO KISUMBA Abdiel
# Description :
#   - Génération automatique des matricules
#   - Vérification de l'unicité
#   - Recherche d'un étudiant
#   - Ajout d'un étudiant
#   - Modification d'un étudiant
#   - Suppression d'un étudiant
#   - Exportation CSV
#   - Statistiques
####################################################################################################

# ================================================================================================
# Chargement des bibliothèques
# ================================================================================================

if (!require(dplyr)) install.packages("dplyr", repos="https://cloud.r-project.org")
library(dplyr)

# ================================================================================================
# Paramètres
# ================================================================================================

ANNEE <- 2026
CODE_ETABLISSEMENT <- "ISIG"

# ================================================================================================
# Base de données des étudiants (vide au départ)
# ================================================================================================

etudiants <- data.frame(
  Matricule = character(),
  Nom = character(),
  PostNom = character(),
  Prenom = character(),
  Sexe = character(),
  Faculté = character(),
  Département = character(),
  Promotion = character(),
  Téléphone = character(),
  Email = character(),
  Année = integer(),
  stringsAsFactors = FALSE
)

# ================================================================================================
# Fonction de génération du matricule
# Format :
# ISIG-2026-000001
# ================================================================================================

generer_matricule <- function(df){

  if(nrow(df)==0){

    numero <- 1

  }else{

    dernier <- tail(df$Matricule,1)

    numero <- as.numeric(substr(dernier,
                                nchar(dernier)-5,
                                nchar(dernier))) + 1
  }

  matricule <- paste0(
    CODE_ETABLISSEMENT,
    "-",
    ANNEE,
    "-",
    sprintf("%06d",numero)
  )

  return(matricule)

}

# ================================================================================================
# Ajouter un étudiant
# ================================================================================================

ajouter_etudiant <- function(df,
                             nom,
                             postnom,
                             prenom,
                             sexe,
                             faculte,
                             departement,
                             promotion,
                             telephone,
                             email){

  mat <- generer_matricule(df)

  nouveau <- data.frame(

    Matricule=mat,
    Nom=nom,
    PostNom=postnom,
    Prenom=prenom,
    Sexe=sexe,
    Faculté=faculte,
    Département=departement,
    Promotion=promotion,
    Téléphone=telephone,
    Email=email,
    Année=ANNEE,
    stringsAsFactors = FALSE

  )

  df <- rbind(df,nouveau)

  cat("\nEtudiant ajouté avec succès\n")
  cat("Matricule :",mat,"\n")

  return(df)

}

# ================================================================================================
# Rechercher un étudiant
# ================================================================================================

rechercher_etudiant <- function(df,matricule){

  resultat <- df[df$Matricule==matricule,]

  if(nrow(resultat)==0){

    cat("\nAucun étudiant trouvé.\n")

  }else{

    print(resultat)

  }

}

# ================================================================================================
# Modifier un étudiant
# ================================================================================================

modifier_etudiant <- function(df,
                              matricule,
                              telephone=NULL,
                              email=NULL,
                              promotion=NULL){

  indice <- which(df$Matricule==matricule)

  if(length(indice)==0){

    cat("\nMatricule introuvable.\n")
    return(df)

  }

  if(!is.null(telephone))
    df$Téléphone[indice] <- telephone

  if(!is.null(email))
    df$Email[indice] <- email

  if(!is.null(promotion))
    df$Promotion[indice] <- promotion

  cat("\nModification effectuée.\n")

  return(df)

}

# ================================================================================================
# Supprimer un étudiant
# ================================================================================================

supprimer_etudiant <- function(df,matricule){

  df <- df[df$Matricule!=matricule,]

  cat("\nSuppression effectuée.\n")

  return(df)

}

# ================================================================================================
# Vérification des doublons
# ================================================================================================

verifier_doublons <- function(df){

  doublons <- duplicated(df$Matricule)

  if(any(doublons)){

    cat("\nDes doublons existent.\n")

  }else{

    cat("\nAucun doublon trouvé.\n")

  }

}

# ================================================================================================
# Nombre total des étudiants
# ================================================================================================

nombre_etudiants <- function(df){

  cat("\nNombre total :",nrow(df),"\n")

}

# ================================================================================================
# Répartition par sexe
# ================================================================================================

repartition_sexe <- function(df){

  print(table(df$Sexe))

}

# ================================================================================================
# Répartition par faculté
# ================================================================================================

repartition_faculte <- function(df){

  print(table(df$Faculté))

}

# ================================================================================================
# Export CSV
# ================================================================================================

exporter_csv <- function(df,
                         fichier="etudiants.csv"){

  write.csv(df,
            fichier,
            row.names=FALSE,
            fileEncoding="UTF-8")

  cat("\nExportation terminée :",fichier,"\n")

}

# ================================================================================================
# Import CSV
# ================================================================================================

importer_csv <- function(fichier){

  read.csv(fichier,
           stringsAsFactors = FALSE)

}

# ================================================================================================
# Affichage complet
# ================================================================================================

afficher_etudiants <- function(df){

  print(df)

}

# ================================================================================================
# EXEMPLES D'UTILISATION
# ================================================================================================

etudiants <- ajouter_etudiant(
  etudiants,
  "KISUMBA",
  "MUHINDO",
  "ABDIEL",
  "M",
  "Sciences Informatiques",
  "Informatique de Gestion",
  "L3",
  "0999999999",
  "abdiel@gmail.com"
)

etudiants <- ajouter_etudiant(
  etudiants,
  "KAVIRA",
  "BANDU",
  "MARIE",
  "F",
  "Sciences Informatiques",
  "Réseaux",
  "L2",
  "0977777777",
  "marie@gmail.com"
)

etudiants <- ajouter_etudiant(
  etudiants,
  "KAMBALE",
  "VIGHENI",
  "JOSEPH",
  "M",
  "Economie",
  "Gestion",
  "G3",
  "0811111111",
  "joseph@gmail.com"
)

cat("\n================ LISTE DES ETUDIANTS ================\n")
afficher_etudiants(etudiants)

cat("\n================ RECHERCHE ===========================\n")
rechercher_etudiant(etudiants,"ISIG-2026-000001")

cat("\n================ MODIFICATION ========================\n")
etudiants <- modifier_etudiant(
  etudiants,
  "ISIG-2026-000001",
  telephone="0822222222"
)

cat("\n================ CONTROLE DES DOUBLONS ==============\n")
verifier_doublons(etudiants)

cat("\n================ STATISTIQUES ========================\n")
nombre_etudiants(etudiants)

cat("\nRépartition par sexe\n")
repartition_sexe(etudiants)

cat("\nRépartition par faculté\n")
repartition_faculte(etudiants)

cat("\n================ EXPORT CSV ==========================\n")
exporter_csv(etudiants)

cat("\n================ SUPPRESSION =========================\n")
etudiants <- supprimer_etudiant(
  etudiants,
  "ISIG-2026-000002"
)

cat("\n================ ETAT FINAL ==========================\n")
afficher_etudiants(etudiants)

####################################################################################################
# FIN DU SCRIPT
####################################################################################################