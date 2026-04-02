if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("purrr")) install.packages("purrr")
if (!require("ggplot2")) install.packages("ggplot2")
install.packages("rjson")

library(rjson)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(ggimage)
library(viridis)
library(patchwork)
library(jsonlite)


# ---- DATAFRAME ----

json_data <- fromJSON(file="/home/UCA/gafournier4/testpython/database_ucacorp2.json")

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
        
        # Détails de l'action
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

# Nettoyage des lignes sans timestamp (ex: key_pressed sans heure précise)
full_df <- full_df %>% filter(!is.na(Timestamp))

###### ANALYSE ET CALCULS ######

# 1. Calcul de la vitesse de réaction
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

# 4. Analyse des déplacements (séparation des coordonnées X:Y)
full_df <- full_df %>%
  separate(Start_Pos, into = c("x_start", "y_start"), sep = ":", convert = TRUE, fill = "right") %>%
  separate(End_Pos, into = c("x_end", "y_end"), sep = ":", convert = TRUE, fill = "right") %>%
  mutate(
    distance_deplacement = abs(replace_na(x_end, 0) - replace_na(x_start, 0)) +
      abs(replace_na(y_end, 0) - replace_na(y_start, 0)),
    deplacement = ifelse(distance_deplacement == 0, "Aucun", "Oui")
  )

View(full_df)





# ---- 1. NOMBRE D'ACTIONS PAR TOUR ----
actions_per_turn <- full_df %>%
  group_by(Turn) %>%
  summarise(Num_Actions = n())

graph1 <- ggplot(actions_per_turn, aes(x = Turn, y = Num_Actions)) +
  geom_line(color = "darkseagreen") +
  geom_point(color = "darkseagreen") +
  ggtitle("Nombre d’actions par tour") +
  xlab("Tour") +
  ylab("Nombre d'actions")





# ---- 2. CLASSEMENT DES MEILLEURS UTILISATEURS ----
best_users <- full_df %>%
  group_by(utilisateur) %>%
  summarise(Max_Score = max(Score_Total, na.rm = TRUE)) %>%
  arrange(desc(Max_Score)) %>%
  slice_head(n = 10)  

graph2 <- ggplot(best_users, aes(x = reorder(utilisateur, Max_Score), y = Max_Score)) +
  geom_col(fill = "darkseagreen") +
  coord_flip() +
  labs(
    title = "Classement des meilleurs utilisateurs (score max)",
    x = "Utilisateur",
    y = "Score maximum"
  ) +
  theme_minimal()






# ---- 3. ACTIONS PAR TYPE DE PIÈCE ----
images <- data.frame(
  Shape_ID = c(1, 2, 3, 4, 5, 6, 7),
  img = c("/home/UCA/gafournier4/testpython/CAP_Z_Gauche.png",
          "/home/UCA/gafournier4/testpython/CAP_Z_droit.png",
          "/home/UCA/gafournier4/testpython/CAP_T2.png",
          "/home/UCA/gafournier4/testpython/CAP_carre.png",
          "/home/UCA/gafournier4/testpython/CAP_L_droit.png",
          "/home/UCA/gafournier4/testpython/CAP_L_gauche.png",
          "/home/UCA/gafournier4/testpython/CAP_barre1.png")
)
action_freq <- full_df %>%
  filter(Action != "None") %>%
  group_by(Shape_ID, Action) %>%
  summarise(count = n(), .groups = "drop")
y_max <- max(action_freq$count)

graph3 <- ggplot(action_freq, aes(x = factor(Shape_ID), y = count, fill = Action)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_viridis_d(option = "C") +
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.2))) +
  labs(title = "Actions par type de pièce",
       x = "",
       y = "Nombre d'actions",
       fill = "Action") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(t = 10, r = 20, b = 80, l = 10)
  ) +
  coord_cartesian(ylim = c(-0.2 * y_max, y_max), clip = "off")

# Ajout images
graph3 +
  geom_image(
    data = images,
    aes(x = factor(Shape_ID), y = -0.12 * y_max, image = img),
    inherit.aes = FALSE,
    size = 0.05,
    nudge_x = -1,
    asp = 1
  )







# ---- 4. TEMP DE RÉACTION PAR TOUR ----
reaction_by_turn <- full_df %>%
  group_by(Turn) %>%
  summarise(Avg_Reaction = mean(temps_reaction, na.rm = TRUE))

graph4 <- ggplot(reaction_by_turn, aes(x = Turn, y = Avg_Reaction)) +
  geom_line(color = "darkseagreen") +
  geom_point(color = "darkseagreen") +
  ggtitle("Temps de réaction moyen par tour") +
  xlab("Tour") +
  ylab("Temps de réaction moyen (ms)") +
  theme_minimal()





# ---- DASHBOARD PATCHWORK ----

graph3 <- graph3 +
  geom_image(
    data = images,
    aes(x = factor(Shape_ID), y = -0.12 * y_max, image = img),
    inherit.aes = FALSE,
    size = 0.10,
    nudge_x = -1,
    asp = 1
  )
(graph1 | graph2) / 
  (graph3 | graph4) 



(graph6) 
