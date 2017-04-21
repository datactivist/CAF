library(tidytext)
library(tidyverse)
library(wordcloud)
library(magrittr)
library(stringr)
library(igraph)
library(ggraph)
library(hrbrthemes)
library(topicmodels)


forum <- read_csv("./data/forums.csv")
forum <- forum %>% 
  fill(Sujet)

stopmots <- data_frame(word = c(tokenizers::stopwords("fr"), "a", "c'est", "plus", "si", "ça", "tout", "à", "afficher", "message", "de", "la", "cité", 1:9))

pdf("./livrables/wordcloud.pdf", width = 10, height = 10)
png("./livrables/wordcloud.png", width = 1000, height = 1000, res = 150)
forum %>% 
  rename(Date = `Date & heure`) %>% 
  unnest_tokens(word, Message) %>% 
  anti_join(stopmots) %>% 
  anti_join(forum %>% select(Auteur) %>% distinct(), by = c("word" = "Auteur")) %>% 
  count(word, sort = TRUE) %>% 
  slice(1:100) %$%
  wordcloud(word, n, colors = brewer.pal(8, "Dark2"), rot.per = 0.2)
dev.off()  
dev.off()

forum %>% 
  rename(Date = `Date & heure`) %>% 
  unnest_tokens(bigram, Message, token = "ngrams", n = 2) %>% 
  separate(bigram, c("mot1", "mot2"), sep = " ", remove = FALSE) %>% 
  filter(!mot1 %in% stopmots$word) %>% 
  filter(!mot2 %in% stopmots$word) %>%
  count(bigram, sort = TRUE) %>% 
  slice(1:100) %$%
  wordcloud(bigram, n, colors = brewer.pal(8, "Dark2"), rot.per = 0.2)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

png("./livrables/harcelement.png", width = 1000, height = 1000, res = 150)
pdf("./livrables/harcelement.pdf", width = 10, height = 10)

forum %>% 
  rename(Date = `Date & heure`) %>% 
  unnest_tokens(bigram, Message, token = "ngrams", n = 2) %>% 
  separate(bigram, c("mot1", "mot2"), sep = " ", remove = TRUE) %>% 
  filter(!mot1 %in% stopmots$word) %>% 
  filter(!mot2 %in% stopmots$word) %>%
  count(mot1, mot2, sort = TRUE) %>% 
  filter(mot1 == "harcèlement" | mot2 == "harcèlement") %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.5) +
  theme_ipsum(grid = "") +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
dev.off()
dev.off()

png("./livrables/bigrams.png", width = 1000, height = 1000, res = 150)
pdf("./livrables/bigrams.pdf", width = 10, height = 10)
forum %>% 
  rename(Date = `Date & heure`) %>% 
  unnest_tokens(bigram, Message, token = "ngrams", n = 2) %>% 
  separate(bigram, c("mot1", "mot2"), sep = " ", remove = TRUE) %>% 
  filter(!mot1 %in% stopmots$word) %>% 
  filter(!mot2 %in% stopmots$word) %>%
  count(mot1, mot2, sort = TRUE) %>% 
  filter(n > 15) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = .5) +
  theme_ipsum(grid = "") +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
dev.off()
dev.off()

## topic clustering


lda <- forum %>% 
  rename(Date = `Date & heure`) %>% 
  unnest_tokens(word, Message) %>% 
  anti_join(stopmots) %>% 
  anti_join(forum %>% select(Auteur) %>% distinct(), by = c("word" = "Auteur")) %>% 
  count(Sujet, word, sort = TRUE) %>% 
  ungroup() %>% 
  cast_dtm(Sujet, word, n) %>% 
  LDA(k = 10)

lda %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>% 
  top_n(5, beta) %>% 
  ungroup %>% 
  arrange(topic, - beta) %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
