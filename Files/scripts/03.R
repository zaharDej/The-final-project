# Лемматизация (udpipe), частотный словарь, коллокации и визуализация

library(udpipe)
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(tidyr)
library(stringr)

clean_dir   <- "Files/clean_text/"
results_dir <- "results/"

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# объединяем всё в один корпус
clean_files <- list.files(clean_dir, pattern = "_clean\\.txt$", full.names = TRUE)

corpus <- ""   # сюда собираем весь текст
for (file in clean_files) {
  text <- readLines(file, encoding = "UTF-8")
  corpus <- paste(corpus, paste(text, collapse = " "))
}
cat("Корпус готов. Символов:", nchar(corpus), "\n\n")

# Загружаем модель udpipe
model_path <- "english-ewt-ud-2.5-191206.udpipe"
ud_model <- udpipe_load_model(model_path)

# лемматизация
annotated <- udpipe_annotate(ud_model, x = corpus)
annotated <- as.data.frame(annotated)

# Частотный словарь лемм (существительные, прилагательные, глаголы)
cat("Словарь\n")
freq_dict <- annotated[annotated$upos %in% c("NOUN", "ADJ", "VERB"), ]
freq_dict <- table(freq_dict$lemma)
freq_dict <- sort(freq_dict, decreasing = TRUE)
freq_dict <- freq_dict[freq_dict >= 5]   # частота слов >= 5

# Сохраняем словарь в файл
write.csv(as.data.frame(freq_dict), file.path(results_dir, "frequency_dictionary.csv"), row.names = TRUE)
cat("Частотный словарь сохранён\n")

# Берём топ лемм
top_n <- min(30, length(freq_dict))

top30_names <- names(freq_dict)[1:top_n]
top30_counts <- as.numeric(freq_dict[1:top_n])

# Создаём датафрейм
top30_df <- data.frame(
  lemma = top30_names,
  count = top30_counts,
  stringsAsFactors = FALSE
)

# График
ggplot(top30_df, aes(x = reorder(lemma, count), y = count)) +
  geom_col(fill = "darkred", width = 0.7) +
  coord_flip() +
  labs(title = "Топ-30 лемм в корпусе Эдгара Аллана По",
       x = "Лемма", y = "Частота") +
  theme_minimal(base_size = 12)

ggsave(file.path(results_dir, "top30_lemmas.png"), width = 12, height = 9, dpi = 300)

# Коллокации
toks <- tokens(corpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)

colloc <- textstat_collocations(toks, size = 2, min_count = 10)
colloc <- colloc[order(colloc$count, decreasing = TRUE), ]

# Берём топ-30
top_colloc <- colloc[1:30, ]

write.csv(top_colloc, file.path(results_dir, "collocations.csv"), row.names = FALSE)

top20_colloc <- top_colloc[1:20, ]
ggplot(top20_colloc, aes(x = reorder(collocation, count), y = count)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Топ-20 коллокаций в корпусе По", x = "Коллокация", y = "Частота") +
  theme_minimal()
ggsave(file.path(results_dir, "top20_collocations_bar.png"), width = 12, height = 8)


# Сетевой график коллокаций
words1 <- character(nrow(top_colloc))
words2 <- character(nrow(top_colloc))
for (i in 1:nrow(top_colloc)) {
  parts <- str_split(top_colloc$collocation[i], " ")[[1]]
  words1[i] <- parts[1]
  words2[i] <- parts[2]
}
graph_df <- data.frame(word1 = words1, word2 = words2, weight = top_colloc$count)

graph <- graph_from_data_frame(graph_df, directed = FALSE)

ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = weight, edge_width = weight), colour = "lightgray") +
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, colour = "white") +
  labs(title = "Сеть топ-30 коллокаций", colour = "white") +
  theme_void()
ggsave(file.path(results_dir, "collocations_network.png"), width = 12, height = 10)
cat("Готово\n")