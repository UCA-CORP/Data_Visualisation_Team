####### IMPORT ######
# Installation des packages si nÃ©cessaire
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("purrr")) install.packages("purrr")
if (!require("ggplot2")) install.packages("ggplot2")

library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# Chargement des donnÃ©es

json_data <- rjson::fromJSON(file="/home/UCA/madusseaux/tetris_Project/database_ucacorp.json")

###### TRANSFORMATION EN DATAFRAME ######

full_df <- map_dfr(json_data, function(partie) {
  
  map_dfr(partie$Tours, function(tour) {
    
    map_dfr(tour$Actions, function(action) {
      
      # Gestion des positions (le fichier utilise 'pos' ou 'start'/'end')
      # On cherche d'abord 'pos', sinon 'start'
      current_pos <- if (!is.null(action$pos)) action$pos else action$start
      end_pos     <- if (!is.null(action$pos)) action$pos else action$end
      
      data.frame(
        Partie        = partie$Partie,
        utilisateur   = partie$utilisateur,
        Turn          = tour$Tour,
        Level         = tour$Level,
        Score_Total   = tour$Score,
        Full_lines    = tour$Full_lines,
        
        # Statistiques du tour
        Score_ration  = tour$Statistiques$Score_ration,
        Efficiency    = tour$Statistiques$Efficiency,
        
        # DÃ©tails de l'action
        Timestamp     = if (!is.null(action$timestamp)) action$timestamp else NA,
        Action        = action$type,
        Shape_ID      = if (!is.null(action$shape)) action$shape else NA,
        Start_Pos     = if (!is.null(current_pos)) current_pos else NA,
        End_Pos       = if (!is.null(end_pos)) end_pos else NA,
        
        stringsAsFactors = FALSE
      )
    })
  })
})

# Nettoyage des lignes sans timestamp (ex: key_pressed sans heure prÃ©cise)
full_df <- full_df %>% filter(!is.na(Timestamp))

###### ANALYSE ET CALCULS ######

# 1. Calcul de la vitesse de rÃ©action
full_df <- full_df %>%
  arrange(utilisateur, Partie, Timestamp) %>%
  group_by(utilisateur, Partie) %>%
  mutate(temps_reaction = Timestamp - lag(Timestamp, default = first(Timestamp))) %>%
  ungroup()

# 2. Rendement par tour
full_df <- full_df %>%
  mutate(rendement_action = Score_Total / (Turn + 1))

# 3. Traduction des formes (Shapes)
full_df <- full_df %>%
  mutate(
    forme = case_when(
      Shape_ID == 0 ~ "Z_gauche",
      Shape_ID == 1 ~ "Z_droite",
      Shape_ID == 2 ~ "T",
      Shape_ID == 3 ~ "Carre",
      Shape_ID == 4 ~ "L_droit",
      Shape_ID == 5 ~ "L_gauche",
      Shape_ID == 6 ~ "Barre",
      TRUE ~ "Inconnue"
    )
  )

# 4. Analyse des dÃ©placements (sÃ©paration des coordonnÃ©es X:Y)
full_df <- full_df %>%
  separate(Start_Pos, into = c("x_start", "y_start"), sep = ":", convert = TRUE, fill = "right") %>%
  separate(End_Pos, into = c("x_end", "y_end"), sep = ":", convert = TRUE, fill = "right") %>%
  mutate(
    distance_deplacement = abs(replace_na(x_end, 0) - replace_na(x_start, 0)) +
      abs(replace_na(y_end, 0) - replace_na(y_start, 0)),
    deplacement = ifelse(distance_deplacement == 0, "Aucun", "Oui")
  )

###### PRÃ‰PARATION POUR PRÃ‰DICTION ######

# AgrÃ©gation par tour pour le modÃ¨le
data_model <- full_df %>%
  group_by(Partie, utilisateur, Turn) %>%
  summarise(
    nb_actions   = n(),
    nb_drops     = sum(Action == "drop"),
    nb_rotations = sum(grepl("rotate", Action)),
    Level        = first(Level),
    Score        = first(Score_Total),
    Efficiency   = first(Efficiency),
    Reaction_moy = mean(temps_reaction, na.rm = TRUE),
    .groups      = "drop"
  )

# Ajout du score final de la partie (cible Ã  prÃ©dire)
data_model <- data_model %>%
  group_by(Partie, utilisateur) %>%
  mutate(score_final = max(Score)) %>%
  ungroup()

###### MODÃˆLE DE PRÃ‰DICTION (GLM) ######

# On utilise les variables numÃ©riques pour prÃ©dire le score final
res_glm <- glm(
  score_final ~ Level + Turn + nb_actions + Efficiency + Reaction_moy,
  data = data_model,
  family = poisson()
)

data_model$prediction <- predict(res_glm, type = "response")

# graphe Mouvements moyens par tour
names(full_df)
#J ai une colonne Turn qui correspond au tour dans lequel la personne est sur tetris. il y a une autre colonne Action dans lequelle il y a none pour aucun mouvement ou autre comme spwan left, right, rotation etc...Je veux que tu fasses un code mouvement moyen par tour avec le nombre de tours avec un jeu de couleurs sur le score max.
library(dplyr)
library(ggplot2)

# Vérifie que level_start est bien un facteur d'entiers
df_game <- df_game %>%
  mutate(level_start = factor(level_start))

# Crée une palette bleu → rouge selon le nombre de niveaux
levels_count <- length(levels(df_game$level_start))
palette_levels <- colorRampPalette(c("blue", "red"))(levels_count)

# Graphique
ggplot(df_game, aes(x = moves_per_turn, y = max_score, color = level_start)) +
  geom_point(size = 3) +
  scale_color_manual(values = palette_levels) +
  labs(
    title = "Mouvements moyens par tour vs score max",
    x = "Mouvements moyens par tour",
    y = "Score max",
    color = "Niveau choisi"
  ) +
  theme_minimal()

