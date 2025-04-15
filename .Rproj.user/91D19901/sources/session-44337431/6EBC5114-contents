# Charger les bibliothèques nécessaires

library(readxl)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyr)
library(wordcloud)
library(tidyverse)
library(tm)
library(SnowballC)
library(stringr)
library(udpipe) # Pour la lemmatisation


Enquête_dopinion_relative_à_la_journée_dintégration_ <- read_excel("Data/Enquête_dopinion_relative_à_la_journée_dintégration).xlsx")
View(Enquête_dopinion_relative_à_la_journée_dintégration_)
tweetsDF <- Enquête_dopinion_relative_à_la_journée_dintégration_

colnames(tweetsDF)

glimpse(tweetsDF)


nrow(tweetsDF) # check the # of rows

table(tweetsDF$id) # build a contingency table of counts

# Fonction de nettoyage du texte
clean_text <- function(text) {
  text <- tolower(text)           # Minuscule
  text <- removePunctuation(text) # Enlever la ponctuation
  text <- removeNumbers(text)     # Enlever les chiffres
  text <- stripWhitespace(text)   # Supprimer les espaces en trop
  return(text)
}

# Ajouter une nouvelle colonne "Texte_corrige"
data <- tweetsDF %>%
  mutate(Texte_corrige = sapply(Texte, clean_text))
# Vérifier l'ajout de la colonne corrigée


# Tokeniser la colonne Texte_corrige
text_df <- data %>%
  select(id, Texte_corrige) %>%
  unnest_tokens(word, Texte_corrige)


# Identifier les documents vides ou avec seulement NA
text_empty <- text_df %>%
  group_by(id) %>%
  summarise(nb_mots = sum(!is.na(word))) %>%
  filter(nb_mots == 0) %>%
  pull(id)

# Garde uniquement les documents non vides pour le LDA
text_filtered <- text_df %>% 
  filter(!(id %in% text_empty))

# Afficher les premières lignes
head(text_filtered)

# Nombre total de lignes après tokenisation
nrow(text_filtered)

# Charger les stop words en français
stop_words_fr <- tibble(word = stopwords("fr")) 


# Nombre total de lignes avant suppression
nrow(text_filtered)

# Supprimer les mots vides en français
text_filtered <- text_filtered %>% 
  anti_join(stop_words_fr, by = "word")

# Nombre total de lignes après suppression
nrow(text_filtered)


# unique count of words before stemming
text_filtered %>%
  count(word, sort = TRUE)%>%
  nrow()
# Le nombre de mots unique par tweet

# stemming
text_filtered = text_filtered %>%
  mutate(stem = wordStem(word))


# unique count of words after stemming
text_filtered %>%
  count(stem, sort = TRUE) %>%
  nrow()


# Fréquence des mots avant le stemming
text_filtered %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(10)

# Fréquence des mots après le stemming
text_filtered %>%
  count(stem) %>%
  arrange(desc(n)) %>%
  head(10)

#_______________________________________________________________________

# 1. Regrouper tous les mots par id pour reconstituer les textes

ud_model <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")

text_grouped <- text_filtered %>%
  group_by(id) %>%
  summarise(text = paste(word, collapse = " "), .groups = "drop")

# 2. Annoter avec le modèle UDPipe (on garde les ID d'origine)
anno <- udpipe_annotate(ud_model, x = text_grouped$text, doc_id = text_grouped$id)
anno_df <- as.data.frame(anno)

# 3. Nettoyer : supprimer ponctuation et stop words français
text_df1 <- anno_df %>%
  filter(!lemma %in% stop_words_fr$word, upos != "PUNCT") %>%
  select(id = doc_id, word = lemma)

text_df1 <- text_df1 %>%
  mutate(
    word = ifelse(word == "NA", NA, word)
  )
#_________________________________________________________________



# Top 10 des mots les plus fréquents
text_df1 %>%
  count(word) %>%  # Compter les occurrences des mots
  arrange(desc(n)) %>%  # Trier par ordre décroissant
  top_n(10, n) %>%  # Prendre les 10 mots les plus fréquents
  ggplot(aes(x = reorder(word, n), y = n)) +  # Trier les mots par fréquence
  geom_col(fill = "steelblue") +
  coord_flip() +  # Mettre l'axe des mots à l'horizontale
  labs(title = "Top 10 des mots les plus fréquents",
       x = "Mots",
       y = "Fréquence") +
  theme_minimal()


glimpse(text_df1)
n_distinct(text_df1$id)

# Après le pretraitement, on remarque la disparition de certains textes. On les identifie donc

# 1. Extraire les ID avant prétraitement (tweetsDF)
Id_initial_tweet <- unique(tweetsDF$id)
length(Id_initial_tweet)

# 2. Extraire les ID après prétraitement (text_df)
Id_final_tweet <- unique(text_df1$id)
length(Id_final_tweet)


#. Identifier les tweets manquants (présents avant, absents après)
Id_tweets_disparus <- setdiff(Id_initial_tweet, Id_final_tweet)


# Les tweets supprimés ne contenaient pas d'informations essentielles
# La majorité d'entre eux étaient composés de hashtags ou de mots servant principalement de liaison.


# Utilisez la colonne `word` ou `stem` selon votre préférence
df_dtm <- text_df1 %>%
  count(id, word) %>%              
  cast_dtm(id, word, n)  

# Étape 2 : Appliquer la LDA. Choix de k______________________________________


# Tester plusieurs valeurs de K (par exemple, de 2 à 15)
k_values <- 2:15
perplexities <- sapply(k_values, function(k) {
  lda_model <- LDA(df_dtm, k = k, control = list(seed = 1234))
  perplexity(lda_model)
})

# Afficher le graphique
ggplot(data.frame(K = k_values, Perplexity = perplexities), aes(x = K, y = Perplexity)) +
  geom_point() + geom_line() +
  labs(title = "Choix du K optimal avec la perplexité",
       x = "Nombre de thèmes (K)", y = "Perplexité")


# Autre méthode : score__________________________________________________________

library(topicmodels)
library(textmineR) # Pour la fonction coherence

# Définir les valeurs de K
k_values <- 2:15

# Initialiser un vecteur pour stocker les scores de cohérence
coherence_scores <- numeric(length(k_values))

# Calcul du Coherence Score pour chaque K
for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  # Ajuster le modèle LDA
  lda_model <- LDA(df_dtm, k = k, control = list(seed = 1234))
  
  # Extraire les mots-clés des thèmes
  topics <- posterior(lda_model)$terms
  
  # Calculer le Coherence Score (CV)
  coherence_scores[i] <- mean(Coherence(topics, dtm = df_dtm, metrics = "cv"))
}

# Afficher le graphique du Coherence Scorehttp://127.0.0.1:36567/graphics/f93472e0-ceb9-432b-ae5b-7645071d2405.png
library(ggplot2)

ggplot(data.frame(K = k_values, Coherence = coherence_scores), aes(x = K, y = Coherence)) +
  geom_point() + geom_line() +
  labs(title = "Choix du K optimal avec le Coherence Score",
       x = "Nombre de thèmes (K)", y = "Coherence Score")

#____________________________________________________________________________________________



# K elevé dès le début_________

#__1. Grouper les mots sémantiquement avant LDA___________________________

# But : Réduire la complexité lexicale avant d’appliquer LDA.


text_df2 <- text_df1 %>%
  mutate(text_semantic = word) %>%  # dupliquer la colonne lemmatisée
  mutate(
    text_semantic = str_replace_all(text_semantic, "\\b(manger|plat|mets|piment|restaurant)\\b", "alimentation"),
    text_semantic = str_replace_all(text_semantic, "\\b(sketch|micro|temps|chanter|court|courtmétrage|sketcheval|scène|culture|culturel|lapprentissage|salle|présentation|apprendre|discours|métrage|fun|espace|prestataire|marrant|comique|communauté|aménager)\\b", "animation"),
    text_semantic = str_replace_all(text_semantic, "\\b(audible|sonorisation|technique)\\b", "technique"),
    text_semantic = str_replace_all(text_semantic, "\\b(communication|organisation|créativité|organiser)\\b", "Organisation")
  )


# Top 10 des mots les plus fréquents
text_df2 %>%
  count(text_semantic) %>%  # Compter les occurrences des mots
  arrange(desc(n)) %>%  # Trier par ordre décroissant
  top_n(10, n) %>%  # Prendre les 10 mots les plus fréquents
  ggplot(aes(x = reorder(text_semantic, n), y = n)) +  # Trier les mots par fréquence
  geom_col(fill = "steelblue") +
  coord_flip() +  # Mettre l'axe des mots à l'horizontale
  labs(title = "Top 10 des mots les plus fréquents",
       x = "Mots",
       y = "Fréquence") +
  theme_minimal()


glimpse(text_df2)
n_distinct(text_df2$id)


# Les tweets supprimés ne contenaient pas d'informations essentielles
# La majorité d'entre eux étaient composés de hashtags ou de mots servant principalement de liaison.


# Utilisez la colonne `word` ou `stem` selon votre préférence
df_dtm1 <- text_df2 %>%
  count(id, text_semantic) %>%              
  cast_dtm(id, text_semantic, n)

# Étape 2 : Appliquer la LDA. Choix de k______________________________________


# Tester plusieurs valeurs de K (par exemple, de 2 à 10)
k_values <- 2:15
perplexities1 <- sapply(k_values, function(k) {
  lda_model1 <- LDA(df_dtm1, k = k, control = list(seed = 1234))
  perplexity(lda_model1)
})

# Afficher le graphique
ggplot(data.frame(K = k_values, Perplexity = perplexities1), aes(x = K, y = Perplexity)) +
  geom_point() + geom_line() +
  labs(title = "Choix du K optimal avec la perplexité",
       x = "Nombre de thèmes (K)", y = "Perplexité")


lda_model <- LDA(df_dtm1, k = 6, control = list(seed = 1234))


# Étape 3 : Explorer les résultats
# Termes par thème
terms_by_topic <- tidy(lda_model, matrix = "beta")


# Afficher les 10 termes les plus probables pour chaque thème
top_terms <- terms_by_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%  # Prend exactement 5 termes par thème
  ungroup() %>%
  arrange(topic, -beta)  # Trie par thème et par probabilité décroissante

# Vérification du nombre de termes sélectionnés par thème
top_terms %>%
  count(topic) 

# Visualisation des termes les plus probables par thème
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Termes les plus probables par thème",
       x = "Probabilité (beta)", y = "Terme")

# Étape 4 : Classification des tweets par thème
tweets_gamma <- tidy(lda_model, matrix = "gamma")


# Afficher les tweets avec leur thème dominant
tweets_classified <- tweets_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup()

tweets_classified %>%
  count(topic)

# Visualisation des thèmes dominants par tweet
tweets_classified %>%
  ggplot(aes(factor(topic), fill = factor(topic))) +
  geom_bar() +
  labs(title = "Distribution des thèmes dominants",
       x = "Thème", y = "Nombre de tweets")

# Labelliser les thèmes

# Ajouter des libellés aux thèmes en fonction des mots dominants
tweets_gamma <- tweets_gamma %>%
  mutate(topic_label = case_when(
    topic == 1 ~ "Animation & présentation",
    topic == 2 ~ "Culture & diversité",
    topic == 3 ~ "Interventions scéniques",
    topic == 4 ~ "Retours & impressions",
    topic == 5 ~ "Organisation logistique",
    topic == 6 ~ "Alimentation & prestations",
    TRUE ~ "Autre"
  ))


# Convertir "document" en entier pour correspondre à "X"
tweets_by_topic <- tweets_gamma %>%
  mutate(document = as.integer(document)) %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

glimpse(tweets_by_topic)

# 1. Créer un dataframe avec les nouveaux documents
doc_vides <- data.frame(
  document = Id_tweets_disparus
)
# 2. Identifier les noms des autres colonnes (sauf "document")
autres_colonnes <- setdiff(names(tweets_by_topic), "document")

# 3. Ajouter des NA pour les autres colonnes
doc_vides[autres_colonnes] <- NA

# 4. Fusionner avec la base existante
tweets_by_topic_complet <- rbind(tweets_by_topic, doc_vides)

# 5. Optionnel: trier par document si nécessaire
tweets_by_topic_complet <- tweets_by_topic_complet[order(tweets_by_topic_complet$document), ]

# Joindre les thèmes dominants avec les tweets originaux
tweets_classified <- tweetsDF %>%
  inner_join(tweets_by_topic_complet, by = c("id" = "document"))

# Afficher les 10 premiers tweets avec leur thème dominant et leur libellé
head(tweets_classified, 10)


View(tweets_classified)