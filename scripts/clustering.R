# ładowanie bibliotek
library(dendextend)
library(corrplot)
library(proxy)
library(flexclust)

# zmiana katalogu roboczego
work_dir <- "L:\\lato21na22\\PJNS11"
setwd(work_dir)

# lokalizacja katalogu ze skryptami
scripts_dir <- ".\\scripts"

# zdefiniowanie funkcji do tworzenia ścieżek dostępu
create_path <- function(master, slave)
  paste(master,slave,sep = "\\")

# zdefiniowanie lokalizacji katalogów na wyniki
topics_dir <- create_path(
  output_dir,
  "topics"
)
dir.create(topics_dir, showWarnings = F)

#wczytanie i wykonanie skryptu frequency_matrix.R
source_file <- create_path(
  scripts_dir,
  "frequency_matrix.R"
)
# source(source_file)
eval(
  parse(
    source_file,
    encoding = "UTF-8"
  )
)

# analiza skupień
# metoda hierarchiczna
# parametry metody:
# 1. macierz częstości
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. miara odległości (euclidean, jaccard, cosine)
# 3. sposób wyznaczania odległości skupień (single, complete, ward.D2)

# przygotowanie 
doc_names <- rownames(dtm_tf_all)
doc_count <- length(doc_names)
legend <- paste(
  paste(
    "d",
    1:length(doc_names),
    sep = ""
  ),
  doc_names,
  sep = " - "
)
rownames(dtm_tf_all_matrix) <- paste(
  "d",
  1:length(doc_names),
  sep = ""
)
rownames(dtm_tfidf_2_16_matrix) <- paste(
  "d",
  1:length(doc_names),
  sep = ""
)
rownames(dtm_bin_all_matrix) <- paste(
  "d",
  1:length(doc_names),
  sep = ""
)
clusters_pattern <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
cols <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen")
cols_pattern <- c()
for (i in 1:doc_count) {
  cols_pattern[i] <- cols[clusters_pattern[i]]
}
names(clusters_pattern) <- paste(
  "d",
  1:length(doc_names),
  sep = ""
)
names(cols_pattern) <- paste(
  "d",
  1:length(doc_names),
  sep = ""
)

# eksperyment 1
dist_matrix <- dist(dtm_tf_all_matrix, method = "euclidean")
h_clust <- hclust(dist_matrix, method = "complete")
plot(h_clust)
barplot(h_clust$height, names.arg = (doc_count-1):1)
dendrogram_1 <- as.dendrogram(h_clust)
clust_count <- find_k(dendrogram_1)$k
colored_dendrogram <- color_branches(
  dendrogram_1, 
  k = clust_count
)
plot(colored_dendrogram)
colored_dendrogram <- color_branches(
  dendrogram_1, 
  col = cols_pattern[dendrogram_1 %>% labels]
)
plot(colored_dendrogram)
legend(
  "topright",
  legend,
  cex = 0.4
)
clusters_1 <- cutree(h_clust, k = clust_count)
clusters_matrix <- matrix(0, doc_count, clust_count)
rownames(clusters_matrix) <- doc_names
for (i in 1:doc_count) {
  clusters_matrix[i, clusters_1[i]] <- 1
}
corrplot(clusters_matrix)

# eksperyment 2
dist_matrix <- dist(dtm_tfidf_2_16_matrix, method = "cosine")
h_clust <- hclust(dist_matrix, method = "ward.D2")
plot(h_clust)
barplot(h_clust$height, names.arg = (doc_count-1):1)
dendrogram_2 <- as.dendrogram(h_clust)
clust_count <- find_k(dendrogram_2)$k
colored_dendrogram <- color_branches(
  dendrogram_2, 
  k = clust_count
)
plot(colored_dendrogram)
colored_dendrogram <- color_branches(
  dendrogram_2, 
  col = cols_pattern[dendrogram_2 %>% labels]
)
plot(colored_dendrogram)
legend(
  "topright",
  legend,
  cex = 0.4
)
clusters_2 <- cutree(h_clust, k = clust_count)
clusters_matrix <- matrix(0, doc_count, clust_count)
rownames(clusters_matrix) <- doc_names
for (i in 1:doc_count) {
  clusters_matrix[i, clusters_2[i]] <- 1
}
corrplot(clusters_matrix)

# eksperyment 3
dist_matrix <- dist(dtm_bin_all_matrix, method = "jaccard")
h_clust <- hclust(dist_matrix, method = "single")
plot(h_clust)
barplot(h_clust$height, names.arg = (doc_count-1):1)
dendrogram <- as.dendrogram(h_clust)
clust_count <- find_k(dendrogram)$k
colored_dendrogram <- color_branches(
  dendrogram, 
  k = clust_count
)
plot(colored_dendrogram)
colored_dendrogram <- color_branches(
  dendrogram, 
  col = cols_pattern[dendrogram %>% labels]
)
plot(colored_dendrogram)
legend(
  "topright",
  legend,
  cex = 0.4
)
clusters <- cutree(h_clust, k = clust_count)
clusters_matrix <- matrix(0, doc_count, clust_count)
rownames(clusters_matrix) <- doc_names
for (i in 1:doc_count) {
  clusters_matrix[i, clusters[i]] <- 1
}
corrplot(clusters_matrix)

# metoda niehierarchiczna
# parametry metody:
# 1. macierz częstości
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. liczba skupień
clust_count <- 4
k_means <- kmeans(dtm_tfidf_2_16, centers = clust_count)
clusters <- k_means$cluster
clusters_matrix <- matrix(0, doc_count, clust_count)
rownames(clusters_matrix) <- doc_names
for (i in 1:doc_count) {
  clusters_matrix[i, clusters[i]] <- 1
}
corrplot(clusters_matrix)


# porównanie wyników eksperymentów
rand_exp4_pattern <- comPart(clusters, clusters_pattern)
rand_exp1_pattern <- comPart(clusters_1, clusters_pattern)
rand_exp2_pattern <- comPart(clusters_2, clusters_pattern)
rand_exp1_exp2 <- comPart(clusters_1, clusters_2)
rand_exp1_exp4 <- comPart(clusters_1, clusters)

Bk_plot(
  dendrogram_2,
  dendrogram,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Indeks Fawlkes'a Mallows'a",
  ylab = "Indeks Fawlkes'a Mallows'a"
)
