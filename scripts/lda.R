# ładowanie bibliotek
library(topicmodels)

# zmiana katalogu roboczego
work_dir <- "D:\\KW\\PJNS11"
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

# analiza ukrytej alokacji Dirichlet'a
terms_count <- ncol(dtm_tf_all)
topics_count <- 4
lda <- LDA(
  dtm_tf_all,
  topics_count,
  method = "Gibbs",
  control = list(
    burnin = 2000,
    thin = 100,
    iter = 3000
  )
)
results <- posterior(lda)

cols <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen")

# prezentacja tematów
for (topic_no in 1:topics_count) {
  topic_file <- create_path(
    topics_dir,
    paste("Temat",topic_no,".png")
  )
  png(topic_file)
  par(mai = c(1,2,1,1))
  topic <- tail(sort(results$terms[topic_no,]),20)
  barplot(
    topic,
    horiz =TRUE,
    las = 1,
    main = paste("Temat", topic_no),
    xlab = "Prawdopodobieństwo",
    col = cols[topic_no]
  )
  dev.off()
}

#prezentacja dokumentów
for (doc_no in 1:length(lda@documents)) {
  doc_file <- create_path(
    topics_dir,
    paste(rownames(results$topics)[doc_no],".png")
  )
  png(doc_file)
  par(mai = c(1,2,1,1))
  document <- results$topics[doc_no,]
  barplot(
    document,
    horiz =TRUE,
    las = 1,
    main = rownames(results$topics)[doc_no],
    xlab = "Prawdopodobieństwo",
    col = cols
  )
  dev.off()
}

#udział tematów w słowach
words_1 <- c("czarodziej", "czarownica", "wampir")
round(results$terms[,words_1],2)

words_2 <- c("albus", "harry", "łucja", "bell")
round(results$terms[,words_2],2)
