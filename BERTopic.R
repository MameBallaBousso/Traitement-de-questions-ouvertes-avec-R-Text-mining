library(reticulate)

# 📦 Imports des modules Python
sentence_transformers <- import("sentence_transformers")
hdbscan <- import("hdbscan")
bertopic <- import("bertopic")
umap <- import("umap")  # 👈 important pour fixer le random_state



# 🔤 Modèle d'embedding (changeable par d'autres plus bas)
embedding_model <- sentence_transformers$SentenceTransformer("paraphrase-MiniLM-L6-v2")


# Les variétés existents du modèle (plus puissants) :

# "paraphrase-distilbert-base-nli-stsb" : (taille : 768 | Bonne précision sémantique
# "bert-base-nli-mean-tokens" : (taille : 768 | Très bonne précision sémantique)
# "all-mpnet-base-v2" : (taille: 768 | Excellente précision sémantique)

# 🔧 Clustering HDBSCAN
hdbscan_model <- hdbscan$HDBSCAN(
  min_cluster_size = reticulate::r_to_py(3L),
  min_samples = reticulate::r_to_py(1L)
)

# 🎯 Réduction de dimension via UMAP avec seed fixée pour reproductibilité
umap_model <- umap$UMAP(
  n_neighbors = 15L,
  n_components = 5L,
  min_dist = 0.0,
  metric = "cosine",
  random_state = 42L  # ✅ Seed fixée ici
)

# 📚 Création du modèle BERTopic
topic_model <- bertopic$BERTopic(
  language = "french",
  embedding_model = embedding_model,
  hdbscan_model = hdbscan_model,
  umap_model = umap_model
  )



text_empty <- tweetsDF %>%
  group_by(id) %>%
  summarise(nb_mots = sum(!is.na(Texte))) %>%
  filter(nb_mots == 0) %>%
  pull(id)

# Garde uniquement les documents non vides pour le LDA
text_filtered <- tweetsDF %>% 
  filter(!(id %in% text_empty))
View(text_filtered)


# 📦 Préparation des données
docs <- text_filtered$Texte
ids <- text_filtered$id  # on garde l'id associé à chaque texte


# 🧠 Application du modèle
result <- topic_model$fit_transform(docs)


# 🎯 Extraction des résultats
topics <- result[[1]]
probs <- result[[2]]


# ➕ Ajout des résultats au data.frame d’origine
text_filtered$topic <- topics
text_filtered$topic_proba <- probs


# 🔁 Reconstruction du data.frame avec id + texte + classe
base_categorisee <- data.frame(
  id = ids,
  texte = docs,
  classe = topics,
  proba = probs
)

View(base_categorisee)


# 🔍 Affichage des infos sur les thèmes trouvés
topic_info <- topic_model$get_topic_info()
View(topic_info)

topic_info$label <- c(
  "Suggestions diverses",                                    # Topic -1
  "Sketchs et scènes courtes",                                                  # Topic 0
  "Elargir le restaurant Salle",                                  # Topic 1
  "Culture et traditions des pays",                           # Topic 2
  "Organisation et co-animation",                             # Topic 3
  "Aménagement de l’espace et durée",                         # Topic 4
  "Qualité du son"                                         # Topic 5
                                                   # Topic 8
)


base_categorisee <- merge(
  base_categorisee, 
  topic_info[, c("Topic", "label")], 
  by.x = "classe", 
  by.y = "Topic", 
  all.x = TRUE
)


base_categorisee <- subset(base_categorisee, select = -c(classe, proba))


doc_vides <- data.frame(
  id = text_empty
)
# 2. Identifier les noms des autres colonnes (sauf "document")
autres_colonnes <- setdiff(names(base_categorisee), "id")

# 3. Ajouter des NA pour les autres colonnes
doc_vides[autres_colonnes] <- NA

# 4. Fusionner avec la base existante
tweets_by_topic_complet <- rbind(base_categorisee, doc_vides)

# 5. Optionnel: trier par document si nécessaire
tweets_by_topic_complet <- tweets_by_topic_complet[order(tweets_by_topic_complet$id), ]

# Joindre les thèmes dominants avec les tweets originaux
tweets_classified <- tweetsDF %>%
  inner_join(tweets_by_topic_complet, by = c("id" = "id"))

# Afficher les 10 premiers tweets avec leur thème dominant et leur libellé
head(tweets_classified, 10)

View(tweets_classified)

