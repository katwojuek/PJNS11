# ładowanie bibliotek
library(wordcloud)

# zmiana katalogu roboczego
work_dir <- "D:\\KW\\PJNS11"
setwd(work_dir)

# lokalizacja katalogu ze skryptami
scripts_dir <- ".\\scripts"

# zdefiniowanie funkcji do tworzenia ścieżek dostępu
create_path <- function(master, slave)
  paste(master,slave,sep = "\\")

# zdefiniowanie lokalizacji katalogów na wyniki
tagclouds_dir <- create_path(
  output_dir,
  "clouds"
)
dir.create(tagclouds_dir, showWarnings = F)

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

# dla pierwszego dokumentu
# waga tf jako miara ważności słów
keywords_tf_1 <- head(sort(dtm_tf_all_matrix[1,], decreasing = T))
keywords_tf_1

# waga tfidf jako miara ważności słów
for (doc_no in 1:length(corpus)) {
  print(rownames(dtm_tf_2_16_matrix)[doc_no])
  print(head(sort(dtm_tfidf_2_16_matrix[doc_no,], decreasing = T)))
}

# prawdopodobieństwo w LDA jako miara ważności słów
for (doc_no in 1:length(corpus)){
  terms_importance <- c(results$topics[doc_no,]%*%results$terms)
  names(terms_importance) <- colnames(results$terms)
  print(rownames(results$topics)[doc_no])
  print(head(sort(terms_importance, decreasing = T)))
}

# chmury tagów
for (doc_no in 1:length(corpus)){
  cloud_file <- create_path(
    tagclouds_dir,
    paste(rownames(results$topics)[doc_no],".png")
  )
  png(cloud_file)
  wordcloud(
    corpus[doc_no],
    max.words = 200,
    colors = brewer.pal(8, "RdPu")
  )
  dev.off()
}
