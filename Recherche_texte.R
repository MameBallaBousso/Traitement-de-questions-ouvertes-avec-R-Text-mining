library(stringr)
library(naivebayes)  # Charger le package
library(readr)
# a collection of package for data wrangling.
library(tidyverse)
library(glmnet)  # Charger le package
# package for text processing
library(tidytext)
# collection of packages for modeling and L 
library(tidymodels)
library(scales)
# R package for managing and analyzing textual data
library(quanteda)
# An R package with word stemming algorithm
# collapsing words to a common root to aid comparison of vocabular. 
library(SnowballC)
# library for topic models (LDA)
library(topicmodels)
# text recipe
library(textrecipes)
# dealing with imbalance data using `step_downsample or upsample`.
library(themis)
# https://github.com/tidymodels/discrim
library(discrim)
# framework for constructing variable importance plots from ML models
library(vip)



urlfile_class <- "https://raw.githubusercontent.com/tianyuan09/ai4ph2022/main/sampleTwitterDataForClassification.csv"
tweetsDF <- read.csv(url(urlfile_class), encoding = "UTF-8")
colnames(tweetsDF)
View(tweetsDF)
nrow(tweetsDF) # check the # of rows

table(tweetsDF$annotation) # build a contingency table of counts

# print the a few tweets
tweetsDF$tweet[1:3]


tweet <- "RT @PurpleForever19 @SciFiTV50 @ufpearth Cool! # #AI4PH #92. #whatif commentary 200 https://t.co/7Mde3"
tweet

# check if the text contains a pattern
str_detect(tweet,"@")

# extract the first match with the pattern

str_extract(tweet,"#[a-zA-Z0-9]*")

# extract all the matches to the pattern
str_extract_all(tweet,"#[a-zA-Z0-9]*")

# replace usernames (starts with @) with empty "". 
str_replace_all(tweet, "@[a-z,A-Z]*[0-9]*[a-z,A-Z]*", "USERNAME")


### UDF to remove the URLs from the tweets
removeURLs <- function(tweet) {
  return(gsub("http\\S+", "", tweet))
}
tweet # before removal

removeURLs(tweet) # after removal

### UDF to remove RT from the tweets

removeUsernamesWithRT <- function(tweet) {
  return(gsub("^RT @[a-z,A-Z]*[0-9]*[a-z,A-Z]*[0-9]*: ","", tweet))
}
removeUsernamesWithRT(tweet) # after removal

### UDF to remove the usernames or callouts from the tweets
removeUsernames <- function(tweet) {
  return(gsub("@[a-z,A-Z]*[0-9]*[a-z,A-Z]*[0-9]*", "", tweet))
}

removeUsernames(tweet)# after removal

removeHashtagSignOnly <- function(tweet) {
  return(gsub("#", "", tweet))
}
removeHashtagSignOnly(tweet)



### Remove the URLs and usernames from the tweets
tweetsDF$processed_tweet <- apply(tweetsDF['tweet'], 2, removeURLs) 
tweetsDF$processed_tweet <- apply(tweetsDF['processed_tweet'],2, removeUsernamesWithRT) 
tweetsDF$processed_tweet <- apply(tweetsDF['processed_tweet'],2, removeUsernames)
tweetsDF$processed_tweet <- apply(tweetsDF['processed_tweet'],2, removeHashtagSignOnly)


text_df <- tweetsDF %>% 
  select(X,annotation,processed_tweet) %>%
  unnest_tokens(word, processed_tweet)
nrow(text_df)


tweetsDF$processed_tweet[1]

head(text_df,100) # take a look at the first 10 rows. 



# take a look at the stop words
data(stop_words)
#_____________________________________________Cas français_________________
stop_words_fr <- get_stopwords(language = "fr")

# Afficher quelques stop words français
head(stop_words_fr, 15)

#____________________________________________________________________________

# different lexicons
unique(stop_words$lexicon)

# count of words for each lexicon
table(stop_words$lexicon)

# take a look at the stop words in snowball
head(stop_words[stop_words$lexicon == "snowball",],15)
#_____________________________________________________________En français_______________________________
# En français seul "snowball_fr" existe

#_____________________________________________________________________________



nrow(text_df) # count before removal 

text_df <- text_df %>% 
  anti_join(stop_words[stop_words$lexicon == "snowball",], by = "word")
nrow(text_df) # count after removal 


# unique count of words before stemming
text_df %>%
  count(word, sort = TRUE)%>%
  nrow()


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


# Nuage de mots
glimpse(text_df)



# Calculer la fréquence des mots pour chaque annotation
word_freq <- text_df %>%
  count(annotation, stem) %>%
  group_by(annotation) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()


word_freq_wide <- word_freq %>%
  pivot_wider(names_from = annotation, values_from = proportion, values_fill = list(proportion = 0))

word_freq_wide <- word_freq_wide %>%
mutate(across(where(is.numeric), ~ . + 1e-8))

View(word_freq_wide)
# Restructurer les données pour comparer les proportions entre catégories

ggplot(word_freq_wide, aes(x = `1`, y = `0`)) +  # Utilisation de X1 et X2 comme exemple
  geom_point(alpha = 0.3, color = "steelblue") +  # Points transparents
  geom_text(aes(label = stem), check_overlap = TRUE, size = 3, color = "gray30") +  # Ajouter les mots
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +  # Ajouter la ligne de régression y = x
  scale_x_log10(labels = scales::percent_format(accuracy = 0.01)) +  # Échelle log en x
  scale_y_log10(labels = scales::percent_format(accuracy = 0.01)) +  # Échelle log en y
  labs(x = "Proportion dans tweet 1", y = "Proportion dans tweet 2", title = "Comparaison des fréquences des mots") +
  theme_minimal()


#_____________________________________________________________________

frequency <- text_df %>% count(annotation, word) %>% group_by(annotation) %>%
  mutate(proportion = n / sum(n)) %>% select(-n) %>% 
  pivot_wider(names_from = annotation, values_from = proportion) %>%
  rename(No = `0`) %>%
  pivot_longer(3,names_to ="Yes",values_to = "proportion")
View(frequency)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = No,color = abs(No - proportion)))+
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position = "none") +
  labs(y = "No", x = "Yes", title = "Comparing the Word Frequencies of Different Annotation", subtitle = "Words far from the line are words found more in one set of texts than another")

#_________________________________________________________________________

cor.test(data = frequency[frequency$Yes == 1,], ~ proportion + `No`)

# Resolve conflicts between tidymodels packages and others
tidymodels_prefer()
tweetsDF2class <- tweetsDF %>% 
  mutate(outcome = factor(annotation)) %>% 
  arrange(desc(outcome))


# default 3/4 for training and 1/4 for testing
# use `strata` argument to specify stratified random sampling.
# use `set.seed()` to reproduce the results later. 
set.seed(123)
tweet_df_split <- tweetsDF2class %>% initial_split(0.7, strata = outcome)
# the training set
tweet_df_train <- training(tweet_df_split)
table(tweet_df_train$outcome)

# the test set
tweet_df_test <- testing(tweet_df_split)
table(tweet_df_test$outcome)
tweet_df_split


tweet_rec <-recipe(outcome ~ processed_tweet, data = tweet_df_train) %>%
  step_tokenize(processed_tweet) %>% # tokenization
  step_stopwords(processed_tweet)%>% # stopwords removal
  step_stem(processed_tweet) %>% #stem
  step_tokenfilter(processed_tweet, max_tokens = 1e3) %>% # select tokens
  step_tfidf(processed_tweet) # convert to tf-idf

tweet_rec # a recipe


# Naive Bayes model
# the engine usually refers a R model package
nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes") 
nb_spec


# Regularized linear models
# you can tune the parameters, but we will start from these values. 
lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
lasso_spec


nb_wf = workflow() %>% 
  add_recipe(tweet_rec)%>%
  add_model(nb_spec)
nb_wf


# workflow with upsampling pre-processing
lasso_wf = workflow() %>%
  add_recipe(tweet_rec)%>%
  add_model(lasso_spec) 
lasso_wf



# it takes a long time to finish
nb_fit <- workflow() %>% 
  add_recipe(tweet_rec)%>%
  add_model(nb_spec) %>%
  fit(data = tweet_df_train)

# predict() on the fitted workflow
predict(nb_fit, tweet_df_test %>% slice(1:100))

lasso_fit <- workflow() %>%
  add_recipe(tweet_rec)%>%
  add_model(lasso_spec) %>%
  fit(data = tweet_df_train)



# create a 3-fold cross-validation set.
set.seed(123)
tweet_folds <- vfold_cv(tweet_df_train, v = 3)
tweet_folds
# create two workflow with different recipes
lasso_wf <- workflow() %>%
  add_recipe(tweet_rec) %>%
  add_model(lasso_spec)
lasso_wf


##

#________________________________________________________________
tweet_upsample_rec <- recipe(annotation ~ text, data = tweet_df_train) %>%
  step_tokenize(text) %>%  # Tokenisation des tweets
  step_stopwords(text) %>%  # Suppression des mots vides
  step_stem(text) %>%  # Stemmatisation
  step_tokenfilter(text, max_tokens = 1000) %>%  # Filtrer les tokens les plus fréquents
  step_tfidf(text) %>%  # Calculer le TF-IDF
  step_upsample(annotation)  # Upsampling pour équilibrer les classes

# Maintenant ton workflow devrait fonctionner
lasso_upsample_wf <- workflow() %>%
  add_recipe(tweet_upsample_rec) %>%
  add_model(lasso_spec)
#_____________________________________________________


# without upsampling (imbalanced data)

set.seed(123)
lasso_rs <- fit_resamples(
  lasso_wf,
  tweet_folds,
  control = control_resamples(save_pred = TRUE))


lasso_rs_metrics <- collect_metrics(lasso_rs)
lasso_rs_predictions <- collect_predictions(lasso_rs)
lasso_rs_metrics


lasso_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = outcome, .pred_1, event_level = "second") %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC curve (lasso, no upsampling)",
    subtitle = "Each resample fold is shown in a different color"
  )


lasso_rs_predictions %>%
  group_by(id) %>%
  pr_curve(truth = outcome, .pred_1, event_level = "second") %>%
  autoplot() +
  labs(
    color = NULL,
    title = "Precision Recall curve (lasso, no upsampling)",
    subtitle = "Each resample fold is shown in a different color"
  )
































# Charger les bibliothèques nécessaires
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)


# Utilisez la colonne `word` ou `stem` selon votre préférence
df_dtm <- text_df %>%
  count(X, stem) %>%              
  cast_dtm(X, stem, n)  


# Étape 2 : Appliquer la LDA
# Choisissez un nombre de thèmes (k). Ici, nous utilisons 4 comme exemple.

library(topicmodels)
library(ggplot2)

# Tester plusieurs valeurs de K (par exemple, de 2 à 10)
k_values <- 2:10
perplexities <- sapply(k_values, function(k) {
  lda_model <- LDA(df_dtm, k = k, control = list(seed = 1234))
  perplexity(lda_model)
})

# Afficher le graphique
ggplot(data.frame(K = k_values, Perplexity = perplexities), aes(x = K, y = Perplexity)) +
  geom_point() + geom_line() +
  labs(title = "Choix du K optimal avec la perplexité",
       x = "Nombre de thèmes (K)", y = "Perplexité")


# Entrainer le modèle LDA

lda_model <- LDA(df_dtm, k = 2, control = list(seed = 1234))

# Étape 3 : Explorer les résultats
# Termes par thème
terms_by_topic <- tidy(lda_model, matrix = "beta")

# Afficher les 10 termes les plus probables pour chaque thème
top_terms <- terms_by_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%      # Extraire les 10 termes les plus probables
  ungroup() %>%
  arrange(topic, -beta)            # Trier par thème et par probabilité

print(top_terms)
View(top_terms)

# Visualisation des termes par thème
library(ggplot2)

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

print(tweets_classified)

tweets_classified %>%
  count(topic)


# Visualisation des thèmes dominants par tweet
tweets_classified %>%
  ggplot(aes(factor(topic), fill = factor(topic))) +
  geom_bar() +
  labs(title = "Distribution des thèmes dominants",
       x = "Thème", y = "Nombre de tweets")




# Charger les bibliothèques nécessaires
library(tidytext)
library(dplyr)

# Calculer TF-IDF
df_tf_idf <- text_df %>%
  count(X, stem) %>%                # Compter la fréquence des mots par document
  bind_tf_idf(stem, X, n)           # Calculer TF-IDF

# Afficher les résultats
print(df_tf_idf)



library(tm)

# Convertir en DTM
dtm_tf_idf <- df_tf_idf %>%
  cast_dtm(document = X, term = stem, value = tf_idf)  # Utiliser tf_idf comme valeur

# Afficher la DTM
print(dtm_tf_idf)




# Filtrer les termes avec un TF-IDF élevé
df_filtered <- df_tf_idf %>%
  filter(tf_idf > 0.02)  # Choisissez un seuil approprié

# Afficher les termes filtrés
print(df_filtered)


# Créer une DTM avec les termes filtrés
dtm_filtered <- df_filtered %>%
  cast_dtm(document = X, term = stem, value = n)  # Utiliser les fréquences brutes (n)

# Afficher la DTM
print(dtm_filtered)



# Appliquer LDA avec k = 4
lda_model <- LDA(dtm_filtered, k = 5, control = list(seed = 1234))

# Extraire les termes les plus probables par thème
top_terms <- terms(lda_model, 10)  # 10 termes par thème

# Afficher les termes par thème
print(top_terms)






