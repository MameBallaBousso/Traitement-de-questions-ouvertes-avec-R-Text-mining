Projet Statistique avec R — Traitement de questions ouvertes

📚 Contexte

Ce projet a été réalisé dans le cadre de la matière Projet statistique avec R conduite par Monsieur Aboubacar HEMA, ingénieur statisticein et analyste de Recherche à IFPRI.
Le thème assigné était “Traitement de questions ouvertes en R”.

Les objectifs du projet :
	•	Identifier les réponses ouvertes dans un questionnaire d’enquête et étudier leur contenu textuel.
	•	Appliquer des techniques de text mining pour extraire des informations pertinentes.
	•	Classer les réponses en thèmes de manière automatique ou semi-automatique.
	•	Évaluer différentes méthodes de regroupement thématique (modèles probabilistes et vectoriels).
	•	Fournir un résultat structuré, intelligible et prêt pour l’analyse statistique.

🛠 Technologies utilisées
	•	R pour le traitement de texte et l’analyse.
	•	Packages principaux :
	•	topicmodels pour les modèles LDA (Latent Dirichlet Allocation).
	•	BERTopic (via reticulate) pour la modélisation thématique basée sur des embeddings BERT.
	•	tidytext, tm, SnowballC pour le prétraitement, l’équilibrage et la vectorisation du texte.
	•	dplyr, ggplot2, stringr, tidyverse, purrr pour la manipulation et la visualisation.


 🧩 Détail des dossiers
01_pretraitement/
Nettoyage initial des textes : suppression ponctuation, accents, doublons, mots vides.

Script Recherche_texte.R pour visualiser les mots fréquents et guider les regroupements.

Script Traitement.R : fonction clé de validation manuelle permettant de décider du sort de certains mots ambigus ou fréquents notament pour les donnée du RGPH-5.
Egament, cette fonction prend en entrée la base déjà tokenisée.

02_modeles/
Application d’un modèle LDA avec topicmodels pour détecter des thèmes statistiques.

Utilisation de BERTopic (via reticulate) pour une approche par similarité sémantique (embeddings).

Script BERTopic.R : utilisation des modèles UMAP, HDBSCAN et intégration du modèle pré-entraîné multilingue.

03_regroupement_mots/
Regroupement manuel/semi-automatisé de mots proches en sens (voir Rgph.R)

Base restants1, restants2, … pour suivre chaque étape de simplification lexicale.

Utilisation progressive de filtres et remplacements (str_replace, case_when, etc.).


05_application_shiny/
Ici, il faudra charger et avoir accés à différentes étapes d'analyse et de traitement de données, notamment par l'application du modèle. L'application inclus également
la possibilité de faire de l'analyse de sentiment.
👨‍💻 Membres du groupe
	•	Mame Balla BOUSSO
	•	Paul BALAFAÏ

📄 Remarques supplémentaires
	•	Le projet explore à la fois des méthodes statistiques classiques (LDA) et des modèles modernes basés sur des embeddings (BERTopic).
	•	Une attention particulière a été portée au nettoyage du texte et à la réduction de la diversité lexicale par synonymisation.
	
