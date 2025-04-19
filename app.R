library(shiny)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(reshape2)
library(wordcloud)
library(igraph)
library(ggraph)
library(topicmodels)
library(tm)
library(textdata)

# UI
ui <- fluidPage(
  titlePanel("Text Mining in R - Application Interactive"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Options d'analyse"),
      
      # Options pour la visualisation des mots
      numericInput("word_freq_filter", 
                  "Filtrer les mots apparaissant plus de X fois:", 
                  value = 5, min = 1, max = 50),
      
      # Options pour le nuage de mots  
      sliderInput("max_words",
                 "Nombre max de mots dans le nuage:",
                 min = 5, max = 100, value = 50),
                 
      sliderInput("text_size",
                 "Taille maximale du texte:",
                 min = 5, max = 30, value = 15),
                 
      # Analyse de sentiment
      selectInput("sentiment_lexicon",
                 "Lexique pour l'analyse de sentiment:",
                 choices = c("afinn", "bing", "nrc"),
                 selected = "bing"),
      
      # TF-IDF options
      numericInput("tf_idf_tweets", 
                  "Nombre de tweets à analyser pour TF-IDF:", 
                  value = 6, min = 1, max = 50),
                  
      numericInput("tf_idf_words", 
                  "Nombre de mots par tweet pour TF-IDF:", 
                  value = 6, min = 1, max = 20),
      
      # Topic modeling options
      numericInput("num_topics", 
                  "Nombre de topics pour le modèle LDA:", 
                  value = 3, min = 2, max = 10),
                  
      numericInput("top_n_words", 
                  "Nombre de mots à afficher par topic:", 
                  value = 10, min = 5, max = 30),
                  
      # N-grams options
      numericInput("ngram_size", 
                  "Taille des n-grammes:", 
                  value = 2, min = 2, max = 3),
                  
      numericInput("ngram_min_count", 
                  "Fréquence minimale des n-grammes:", 
                  value = 2, min = 1, max = 20)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction", 
                 h3("Présentation de l'analyse de texte"),
                 p("Cette application est basée sur le text mining avec R."),
                 p("Utilisez les options du panneau de gauche pour personnaliser l'analyse."),
                 h4("Jeu de données"),
                 p("Les tweets analysés proviennent du compte Twitter CCIDM."),
                 dataTableOutput("tweets_preview")
        ),
        
        tabPanel("Tokenization", 
                 h3("Tokenization des tweets"),
                 p("Le processus de tokenization consiste à diviser le texte en unités significatives (tokens)."),
                 p("Un token est défini comme une unité significative de texte comme un mot, une phrase, un paragraphe ou un n-gramme."),
                 h4("Aperçu des tokens"),
                 dataTableOutput("tokenized_preview"),
                 h4("Fréquence des mots"),
                 plotOutput("word_freq_plot"),
                 p(HTML("Comme vous pouvez le constater, certains mots apparaissent plus fréquemment que d'autres sans pour autant apporter un plus a notre analsye.Ces mots sont appelés <b>Mots vides</b> (ou stop words)")),
                 p("Dans le processus de traitement , on doit les supprimer"),
                 h4("Sans les mots vides (stop words)"),
                 plotOutput("word_freq_no_stop")
        ),
        
        tabPanel("Nuage de mots", 
                 h3("Visualisation sous forme de nuage de mots"),
                 p("Les nuages de mots permettent de visualiser la fréquence des mots de manière intuitive."),
                 plotOutput("wordcloud_plot", height = "500px")
        ),
        
        tabPanel("Analyse de sentiment", 
                 h3("Analyse de sentiment"),
                 p("L'analyse de sentiment tente d'extraire les émotions véhiculées par le texte."),
                 h4("Distribution du sentiment par tweet"),
                 plotOutput("sentiment_dist_plot"),
                 h4("Nuage de mots par sentiment"),
                 plotOutput("sentiment_cloud_plot")
        ),
        
        tabPanel("TF-IDF", 
                 h3("Term Frequency - Inverse Document Frequency"),
                 p("TF-IDF est une mesure statistique qui évalue l'importance d'un mot dans un document par rapport à une collection de documents."),
                 h4("Top mots par fréquence"),
                 plotOutput("tfidf_freq_plot"),
                 h4("Top mots par TF-IDF"),
                 plotOutput("tfidf_importance_plot")
        ),
        
        tabPanel("N-grammes", 
                 h3("Analyse des n-grammes"),
                 p("Les n-grammes permettent d'analyser les séquences de mots qui apparaissent ensemble."),
                 h4("Top n-grammes"),
                 dataTableOutput("ngrams_table"),
                 h4("Réseau de relations entre mots"),
                 plotOutput("ngrams_network", height = "600px")
        ),
        
        tabPanel("Topic Modeling", 
                 h3("Modélisation de sujets (LDA)"),
                 p("La modélisation de sujets permet d'identifier automatiquement les sujets abordés dans un corpus de textes."),
                 h4("Mots les plus probables par sujet"),
                 plotOutput("topics_plot", height = "600px")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Charger les données
  tweets_data <- reactive({
    tweets <- read_csv("tweets.csv", col_names = TRUE)
    tweets <- tweets %>%
      mutate(tweet_id = row_number())
    return(tweets)
  })
  
  # Aperçu des tweets
  output$tweets_preview <- renderDataTable({
    head(tweets_data(), 10)
  })
  
  # Tokenization
  tokenized_tweets <- reactive({
    unnest_tokens(tweets_data(), input = 'tweet', output = 'word')
  })
  
  # Aperçu des tokens
  output$tokenized_preview <- renderDataTable({
    head(tokenized_tweets(), 20)
  })
  
  # Fréquence des mots
  output$word_freq_plot <- renderPlot({
    tokenized_tweets() %>%
      count(word, sort = TRUE) %>%
      rename(count = n) %>%
      filter(count > input$word_freq_filter) %>%
      mutate(word = reorder(word, count)) %>%
      ggplot(aes(x = count, y = word)) + 
        geom_col() + 
        labs(title = "Fréquence des mots dans les tweets") + 
        scale_x_continuous(breaks = seq(0, 50, 5))
  })
  
  # Fréquence des mots (sans stop words)
  output$word_freq_no_stop <- renderPlot({
    tokenized_tweets() %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      rename(count = n) %>%
      filter(count > input$word_freq_filter) %>%
      mutate(word = reorder(word, count)) %>%
      ggplot(aes(x = count, y = word)) + 
        geom_col() + 
        labs(title = "Fréquence des mots (sans mots vides)") + 
        scale_x_continuous(breaks = seq(0, 50, 5))
  })
  
  # Nuage de mots
  output$wordcloud_plot <- renderPlot({
    tokenized_tweets() %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      filter(n > input$word_freq_filter) %>%
      slice_head(n = input$max_words) %>%
      ggplot(aes(label = word, size = n, color = n)) +
        geom_text_wordcloud() + 
        scale_size_area(max_size = input$text_size) +
        labs(title = "Nuage de mots")
  })
  
  # Analyse de sentiment
  sentiment_data <- reactive({
    tokenized_tweets() %>%
      inner_join(get_sentiments(input$sentiment_lexicon))
  })
  
  # Distribution du sentiment par tweet
  output$sentiment_dist_plot <- renderPlot({
    if (input$sentiment_lexicon == "afinn") {
      tokenized_tweets() %>%
        group_by(tweet_id) %>%
        inner_join(get_sentiments("afinn")) %>%
        summarise(mean_sentiment = mean(value)) %>%
        ggplot(aes(x = tweet_id, y = mean_sentiment)) + 
          geom_col() + 
          labs(title = 'Sentiment moyen par tweet - Lexique Afinn', 
               x = "Tweet ID", y = 'Sentiment moyen') + 
          scale_x_continuous(breaks = seq(1, nrow(tweets_data()))) +
          scale_y_continuous(breaks = seq(-3, 3, 0.5))
    } else if (input$sentiment_lexicon == "bing") {
      tokenized_tweets() %>%
        inner_join(get_sentiments("bing")) %>%
        count(tweet_id, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment_score = positive - negative) %>%
        ggplot(aes(x = tweet_id, y = sentiment_score)) + 
          geom_col(aes(fill = sentiment_score > 0)) + 
          scale_fill_manual(values = c("red", "green"), guide = "none") +
          labs(title = 'Score de sentiment par tweet - Lexique Bing', 
               x = "Tweet ID", y = 'Score') +
          scale_x_continuous(breaks = seq(1, nrow(tweets_data())))
    } else {
      tokenized_tweets() %>%
        inner_join(get_sentiments("nrc")) %>%
        filter(!sentiment %in% c("positive", "negative")) %>%
        count(sentiment) %>%
        mutate(sentiment = reorder(sentiment, n)) %>%
        ggplot(aes(x = n, y = sentiment)) + 
          geom_col() + 
          labs(title = 'Distribution des émotions - Lexique NRC', 
               x = "Fréquence", y = 'Émotion')
    }
  })
  
  # Nuage de mots par sentiment
  output$sentiment_cloud_plot <- renderPlot({
    if (input$sentiment_lexicon == "bing") {
      tokenized_tweets() %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("red", "green"),
                         max.words = input$max_words)
    } else if (input$sentiment_lexicon == "nrc") {
      tokenized_tweets() %>%
        inner_join(get_sentiments("nrc")) %>%
        filter(!sentiment %in% c("positive", "negative")) %>%
        count(word, sentiment, sort = TRUE) %>%
        group_by(sentiment) %>%
        slice_head(n = 10) %>%
        ggplot(aes(label = word, size = n, color = sentiment)) +
          geom_text_wordcloud_area() +
          scale_size_area(max_size = input$text_size) +
          facet_wrap(~sentiment) +
          theme_minimal()
    } else {
      tokenized_tweets() %>%
        inner_join(get_sentiments("afinn")) %>%
        mutate(sentiment = ifelse(value > 0, "positive", "negative")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("red", "green"),
                         max.words = input$max_words)
    }
  })
  
  # TF-IDF
  tf_idf_data <- reactive({
    tokenized_tweets() %>%
      count(word, tweet_id, sort = TRUE) %>%
      rename(count = n) %>%
      bind_tf_idf(word, tweet_id, count)
  })
  
  # TF-IDF - Mots les plus fréquents
  output$tfidf_freq_plot <- renderPlot({
    tf_idf_data() %>%
      select(word, tweet_id, tf_idf, count) %>%
      group_by(tweet_id) %>%
      slice_max(order_by = count, n = input$tf_idf_words, with_ties = FALSE) %>%
      filter(tweet_id < input$tf_idf_tweets) %>%
      ggplot(aes(label = word)) + 
        geom_text_wordcloud() + 
        facet_grid(rows = vars(tweet_id)) +
        labs(title = "Top mots par fréquence brute")
  })
  
  # TF-IDF - Mots les plus importants
  output$tfidf_importance_plot <- renderPlot({
    tf_idf_data() %>%
      select(word, tweet_id, tf_idf) %>%
      group_by(tweet_id) %>%
      slice_max(order_by = tf_idf, n = input$tf_idf_words, with_ties = FALSE) %>%
      filter(tweet_id < input$tf_idf_tweets) %>%
      ggplot(aes(label = word)) + 
        geom_text_wordcloud() + 
        facet_grid(rows = vars(tweet_id)) +
        labs(title = "Top mots par importance TF-IDF")
  })
  
  # N-grammes
  ngrams_data <- reactive({
    if (input$ngram_size == 2) {
      tweets_data() %>%
        unnest_tokens(ngram, tweet, token = 'ngrams', n = 2) %>%
        separate(ngram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        unite(ngram, word1, word2, sep = " ")
    } else {
      tweets_data() %>%
        unnest_tokens(ngram, tweet, token = 'ngrams', n = 3) %>%
        separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(!word3 %in% stop_words$word) %>%
        unite(ngram, word1, word2, word3, sep = " ")
    }
  })
  
  # Table des n-grammes
  output$ngrams_table <- renderDataTable({
    ngrams_data() %>%
      count(ngram, sort = TRUE) %>%
      filter(n >= input$ngram_min_count) %>%
      rename(fréquence = n)
  })
  
  # Réseau de n-grammes
  output$ngrams_network <- renderPlot({
    if (input$ngram_size == 2) {
      ngrams_data() %>%
        separate(ngram, c("word1", "word2"), sep = " ") %>%
        count(word1, word2, sort = TRUE) %>%
        filter(n >= input$ngram_min_count) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
          geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = FALSE) +
          geom_node_point(color = "lightblue", size = 5) +
          geom_node_text(aes(label = name), vjust = 1.8) +
          labs(title = "Réseau de relations entre mots (Bigrammes)")
    } else {
      ngrams_data() %>%
        separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
        count(word1, word2, word3, sort = TRUE) %>%
        filter(n >= input$ngram_min_count) %>%
        unite(bigram1, word1, word2, sep = " ") %>%
        unite(bigram2, word2, word3, sep = " ") %>%
        count(bigram1, bigram2, sort = TRUE) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
          geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = FALSE) +
          geom_node_point(color = "lightblue", size = 5) +
          geom_node_text(aes(label = name), vjust = 1.8) +
          labs(title = "Réseau de relations entre mots (Trigrammes)")
    }
  })
  
  # Topic Modeling
  topics_model <- reactive({
    tf_idf_data() %>%
      anti_join(stop_words) %>%
      cast_dtm(document = tweet_id, term = word, value = count) %>%
      LDA(k = input$num_topics)
  })
  
  topics_data <- reactive({
    topics_model() %>%
      tidy() %>%
      group_by(topic) %>%
      slice_max(beta, n = input$top_n_words) %>%
      ungroup() %>%
      arrange(topic, -beta)
  })
  
  # Visualisation des topics
  output$topics_plot <- renderPlot({
    topics_data() %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(beta, term, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        scale_y_reordered() +
        labs(title = "Mots les plus probables par sujet",
             x = "Probabilité (beta)",
             y = NULL)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)