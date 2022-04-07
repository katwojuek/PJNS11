# ładowanie bibliotek
library(topicmodels)

# zmiana katalogu roboczego
work_dir <- "L:\\lato21na22\\PJNS11"
setwd(work_dir)

# lokalizacja katalogu ze skryptami
scripts_dir <- ".\\scripts"

# zdefiniowanie funkcji do tworzenia ścieżek dostępu
create_path <- function(master, slave)
  paste(master,slave,sep = "\\")

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
topics_count <- 5
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
par(mai = c(1,2,1,1))
topic_no <- 1
topic <- tail(sort(results$terms[topic_no,]),20)
barplot(
  topic,
  horiz =TRUE,
  las = 1,
  main = paste("Temat", topic_no),
  xlab = "Prawdopodobieństwo",
  col = cols[topic_no]
)

topic_no <- 2
topic <- tail(sort(results$terms[topic_no,]),20)
barplot(
  topic,
  horiz =TRUE,
  las = 1,
  main = paste("Temat", topic_no),
  xlab = "Prawdopodobieństwo",
  col = cols[topic_no]
)

topic_no <- 3
topic <- tail(sort(results$terms[topic_no,]),20)
barplot(
  topic,
  horiz =TRUE,
  las = 1,
  main = paste("Temat", topic_no),
  xlab = "Prawdopodobieństwo",
  col = cols[topic_no]
)

topic_no <- 4
topic <- tail(sort(results$terms[topic_no,]),20)
barplot(
  topic,
  horiz =TRUE,
  las = 1,
  main = paste("Temat", topic_no),
  xlab = "Prawdopodobieństwo",
  col = cols[topic_no]
)

topic_no <- 5
topic <- tail(sort(results$terms[topic_no,]),20)
barplot(
  topic,
  horiz =TRUE,
  las = 1,
  main = paste("Temat", topic_no),
  xlab = "Prawdopodobieństwo",
  col = cols[topic_no]
)