library(udpipe)
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(tidyr)
library(stringr)

clean_dir <- "Files/clean_text/"
results_dir <- "results/"
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# объединяем всё в один корпус
clean_files <- list.files(clean_dir, pattern = "_clean\\.txt$", full.names = TRUE)
corpus <- "" 
for (file in clean_files) {
  text <- readLines(file, encoding = "UTF-8")
  corpus <- paste(corpus, paste(text, collapse = " "))
}
cat("Корпус готов. Символов:", nchar(corpus), "\n\n")


model_path <- "english-ewt-ud-2.5-191206.udpipe"
ud_model <- udpipe_load_model(model_path)

# лемматизация + POS
annotated <- udpipe_annotate(ud_model, x = corpus)
annotated <- as.data.frame(annotated)

# Частотный словарь лемм (NOUN, ADJ, VERB)
cat("Словарь\n")
freq_dict <- annotated |>
  filter(upos %in% c("NOUN", "ADJ", "VERB")) |>
  count(lemma, sort = TRUE) |>
  filter(n >= 5)

write.csv(freq_dict, file.path(results_dir, "frequency_dictionary.csv"), row.names = FALSE)
cat("Частотный словарь сохранён\n")

# Топ-30 лемм
top30_df <- head(freq_dict, 30)

ggplot(top30_df, aes(x = reorder(lemma, n), y = n)) +
  geom_col(fill = "darkred", width = 0.7) +
  coord_flip() +
  labs(title = "Топ-30 лемм в корпусе Эдгара Аллана По",
       x = "Лемма", y = "Частота") +
  theme_minimal(base_size = 12)

ggsave(file.path(results_dir, "top30_lemmas.png"), width = 12, height = 9, dpi = 300)

cat("Извлекаем содержательные коллокации (ADJ+NOUN, NOUN+NOUN и т.д.)\n")

colloc <- keywords_collocation(annotated,
                               term = "lemma",             
                               group = c("doc_id", "sentence_id"),
                               ngram_max = 3,               
                               n_min = 5)                   

colloc <- colloc |>
  arrange(desc(pmi)) |>                  
  head(50)                                 

# Сохраняем полный список
write.csv(colloc, file.path(results_dir, "meaningful_collocations.csv"), row.names = FALSE)

# Топ-20 для барчарта
top20_colloc <- head(colloc, 20)
top20_colloc$collocation <- factor(top20_colloc$keyword, levels = rev(top20_colloc$keyword))

ggplot(top20_colloc, aes(x = collocation, y = freq)) +
  geom_col(fill = "darkred", width = 0.7) +
  coord_flip() +
  labs(title = "Топ-20 содержательных коллокаций в корпусе По",
       subtitle = "(прилагательное + существительное и др., без служебных слов)",
       x = "Коллокация", y = "Частота") +
  theme_minimal(base_size = 12)

ggsave(file.path(results_dir, "top20_meaningful_collocations_bar.png"), width = 12, height = 8, dpi = 300)

# Сетевой график (топ-30 коллокаций)
top30_colloc_net <- head(colloc, 30)

graph_df <- top30_colloc_net |>
  separate(keyword, into = c("word1", "word2"), sep = " ", extra = "merge") |> 
  mutate(word2 = str_trim(word2)) |>
  select(word1, word2, weight = freq)

graph <- graph_from_data_frame(graph_df, directed = FALSE)

ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = weight, edge_width = weight),
                 colour = "black", show.legend = FALSE) +
  scale_edge_width(range = c(0.5, 4)) +
  geom_node_point(color = "darkred", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, colour = "black", size = 4) +
  labs(title = "Сеть топ-30 содержательных коллокаций в корпусе Эдгара По") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

ggsave(file.path(results_dir, "meaningful_collocations_network.png"), width = 12, height = 10, dpi = 300, bg = "white")

cat("Готово!\n")