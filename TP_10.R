
# Approche basée sur des mots-clés (Règles heuristiques)

library(stringr)

# Définition des catégories et mots-clés associés
categories <- list(
  "Éducation" = c("école", "université", "formation"),
  "Santé" = c("hôpital", "médecin", "maladie"),
  "Économie" = c("argent", "emploi", "commerce")
)


# Fonction pour classifier un texte
classify_text <- function(text, categories) {
  for (category in names(categories)) {
    if (any(str_detect(text, categories[[category]]))) {
      return(category)
    }
  }
  return("Autre")
}

# Exemple de classification
texte <- "J'ai étudié à l'université."
classify_text(texte, categories)




# Approche basée sur les fréquences de mots (TF-IDF + Machine Learning classique)

library(tm)
library(e1071)
library(caret)

# Exemple de corpus
documents <- c("L'université propose des formations en ligne.",
               "Les hôpitaux sont pleins en hiver.",
               "Le commerce en ligne se développe rapidement.")

# Labels associés (catégories connues)
labels <- c("Éducation", "Santé", "Économie")

# Création du corpus et de la matrice TF-IDF
corpus <- Corpus(VectorSource(documents))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("fr"))

dtm <- DocumentTermMatrix(corpus)
dtm <- as.matrix(dtm)

# Entraînement d'un modèle Naïve Bayes
model <- naiveBayes(dtm, labels)

# Test sur une nouvelle phrase
new_text <- "Les écoles développent de nouveaux programmes."
new_dtm <- DocumentTermMatrix(Corpus(VectorSource(new_text)))
new_dtm <- as.matrix(new_dtm)

predict(model, new_dtm)



#__________________________________________________________________________________________________________________


library(dplyr)      # Manipulation de données
library(tidytext)   # Pour la tokenisation
library(tm)         # Nettoyage de texte
library(stringi)    # Gestion des accents
library(dplyr)      # Utilisation des opérations pip
library(SnowballC)  # suprimer les mots lexicaux
library(ggplot2)    # Grphique
library(topicmodels) # Pour LDA

# Importer un fichier CSV
data <- read.csv("C:/Users/lenovo/Desktop/Semestre_6/Exposé/Traitement-de-questions-ouvertes-avec-R-Text-mining/data_text.csv", header = TRUE, sep = ",")

head(data)


# Fonction de nettoyage du texte
clean_text <- function(text) {
  text <- tolower(text)                        # Minuscule
  text <- removePunctuation(text)              # Enlever la ponctuation
  text <- removeNumbers(text)                  # Enlever les chiffres
  text <- stripWhitespace(text)                # Supprimer les espaces en trop
  text <- stri_trans_general(text, "Latin-ASCII") # Supprimer les accents
  text <- removeWords(text, stopwords("fr"))   # Enlever les stopwords français
  return(text)
}
View(data)
# Ajouter une nouvelle colonne "Texte_corrige"
data <- data %>%
  mutate(Texte_corrige = sapply(Texte, clean_text))

# Vérifier l'ajout de la colonne corrigée
head(data)


# Tokeniser la colonne Texte_corrige
text_df <- data %>%
  select(id, Texte_corrige) %>%
  unnest_tokens(word, Texte_corrige)

# Afficher les premières lignes
head(text_df)

# Nombre total de lignes après tokenisation
nrow(text_df)

# take a look at the stop words
stop_words_fr <- tibble(word = stopwords("fr")) 


View(stop_words_fr)
# Nombre total de lignes avant suppression
nrow(text_df)

nrow(text_df) # count before removal 

text_df <- text_df %>% 
  anti_join(stop_words[stop_words$lexicon == "snowball",], by = "word") %>%
  anti_join(stop_words[stop_words$lexicon == "SMART",], by = "word") %>%
  anti_join(stop_words[stop_words$lexicon == "onix",], by = "word")
nrow(text_df) # count after removal 


# unique count of words before stemming
text_df %>%
  count(word, sort = TRUE)%>%
  nrow()
# Le nombre de mots unique par tweet

# stemming
text_df = text_df %>%
  mutate(stem = wordStem(word))


# unique count of words after stemming
text_df %>%
  count(stem, sort = TRUE) %>%
  nrow()


# Fréquence des mots avant le stemming
text_df %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(10)

# Fréquence des mots après le stemming
text_df %>%
  count(stem) %>%
  arrange(desc(n)) %>%
  head(10)


# Top 10 des mots les plus fréquents
text_df %>%
  count(stem) %>%  # Compter les occurrences des mots
  arrange(desc(n)) %>%  # Trier par ordre décroissant
  top_n(10, n) %>%  # Prendre les 10 mots les plus fréquents
  ggplot(aes(x = reorder(stem, n), y = n)) +  # Trier les mots par fréquence
  geom_col(fill = "steelblue") +
  coord_flip() +  # Mettre l'axe des mots à l'horizontale
  labs(title = "Top 10 des mots les plus fréquents",
       x = "Mots",
       y = "Fréquence") +
  theme_minimal()


glimpse(text_df)
n_distinct(text_df$id)

# Après le pretraitement, on remarque la disparition de certains textes. On les identifie donc

# 1. Extraire les ID avant prétraitement (tweetsDF)
Id_initial_tweet <- unique(data$id)
length(Id_initial_tweet)

# 2. Extraire les ID après prétraitement (text_df)
Id_final_tweet <- unique(text_df$X)
length(Id_final_tweet)


#. Identifier les tweets manquants (présents avant, absents après)
Id_tweets_disparus <- setdiff(Id_initial_tweet, Id_final_tweet)

View(tweets_disparus)

# Les tweets supprimés ne contenaient pas d'informations essentielles
# La majorité d'entre eux étaient composés de hashtags ou de mots servant principalement de liaison.


# Utilisez la colonne `word` ou `stem` selon votre préférence
df_dtm <- text_df %>%
  count(id, stem) %>%              
  cast_dtm(id, stem, n)  

# Étape 2 : Appliquer la LDA. Choix de k______________________________________


# Tester plusieurs valeurs de K (par exemple, de 2 à 10)
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


# Autre méthode : score_____________________________________________________

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

# Afficher le graphique du Coherence Score
library(ggplot2)

ggplot(data.frame(K = k_values, Coherence = coherence_scores), aes(x = K, y = Coherence)) +
  geom_point() + geom_line() +
  labs(title = "Choix du K optimal avec le Coherence Score",
       x = "Nombre de thèmes (K)", y = "Coherence Score")

#________________________________________________________




# Entraîner le modèle LDA

lda_model <- LDA(df_dtm, k = 2, control = list(seed = 1234))


# Étape 3 : Explorer les résultats
# Termes par thème
terms_by_topic <- tidy(lda_model, matrix = "beta")


# Afficher les 10 termes les plus probables pour chaque thème
top_terms <- terms_by_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%  # Extraire les 10 termes les plus probables
  ungroup() %>%
  arrange(topic, -beta)   # Trier par thème et par probabilité

print(top_terms)


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

View(tweets_classified)

tweets_classified %>%
  count(topic)

# Visualisation des thèmes dominants par tweet
tweets_classified %>%
  ggplot(aes(factor(topic), fill = factor(topic))) +
  geom_bar() +
  labs(title = "Distribution des thèmes dominants",
       x = "Thème", y = "Nombre de tweets")

# Labelliser les thèmes

# Extraire la matrice gamma (probabilité des thèmes par tweet)
tweets_gamma <- tidy(lda_model, matrix = "gamma")

# Ajouter les libellés aux thèmes
tweets_gamma <- tweets_gamma %>%
  mutate(topic_label = case_when(
    topic == 1 ~ "Symptômes généraux",
    topic == 2 ~ "Symptômes de la Covid",
    TRUE ~ "Autre" # Si d'autres valeurs apparaissent
  ))

# Convertir "document" en entier pour correspondre à "X"
tweets_by_topic <- tweets_gamma %>%
  mutate(document = as.integer(document)) %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

# Joindre les thèmes dominants avec les tweets originaux
tweets_classified <- tweetsDF %>%
  inner_join(tweets_by_topic, by = c("X" = "document"))

# Afficher les 10 premiers tweets avec leur thème dominant et leur libellé
head(tweets_classified, 10)


View(tweets_classified)





