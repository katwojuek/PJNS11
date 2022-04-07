# ładowanie bibliotek
library(lsa)

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

# analiza ukrytych wymiarów semantycznych
# (dekompozycja wg wartości osobliwych)
lsa <- lsa(tdm_tf_2_16_matrix)

# przygotowanie danych do wykresu
legend <- paste(
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16_matrix)),
    sep = ""
  ),
  colnames(tdm_tf_2_16_matrix),
  sep = " - "
)
coord_docs <- lsa$dk%*%diag(lsa$sk)
coord_terms <- lsa$tk%*%diag(lsa$sk)

terms_importance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
important_terms <- names(tail(sort(terms_importance),30))
coord_important_terms <- coord_terms[important_terms,]

own_terms <- c("eustachy", "harry", "ron", "hermiona", "dumbledore", 
               "umbridge", "syriusz", "łucja", "zuzanna", "piotr", 
               "edmund", "aslana", "narnii", "bell", "edward", 
               "jacob", "wampir", "czarownica", "czarodziej")
coord_own_terms <- coord_terms[own_terms,]

coord_plot_terms <- coord_own_terms

x1 <- coord_docs[,1]
y1 <- coord_docs[,2]

x2 <- coord_plot_terms[,1]
y2 <- coord_plot_terms[,2]

# wykres w przestrzeni dwuwymiarowej
options(scipen = 5)
plot(
  x1,
  y1,
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
  col = "purple"
)
text(
  x1,
  y1, 
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16_matrix)),
    sep = ""
  ),
  pos = 4,
  col = "purple"
)
legend(
  "topleft",
  legend,
  cex = 0.5,
  text.col = "purple"
)
points(
  x2, 
  y2, 
  col = "darkslateblue",
  pch = 2
)
text(
  x2,
  y2, 
  rownames(coord_plot_terms),
  pos = 4,
  col = "darkslateblue"
)

# zapis wykresu do pliku .png
lsa_file <- create_path(
  output_dir,
  "lsa.png"
)
png(lsa_file)
options(scipen = 5)
plot(
  x1,
  y1,
  main = "Analiza ukrytych wymiarów semantycznych",
  xlab = "SD1",
  ylab = "SD2",
  col = "purple"
)
text(
  x1,
  y1, 
  paste(
    "d",
    1:length(colnames(tdm_tf_2_16_matrix)),
    sep = ""
  ),
  pos = 4,
  col = "purple"
)
legend(
  "topleft",
  legend,
  cex = 0.5,
  text.col = "purple"
)
points(
  x2, 
  y2, 
  col = "darkslateblue",
  pch = 2
)
text(
  x2,
  y2, 
  rownames(coord_plot_terms),
  pos = 4,
  col = "darkslateblue"
)
dev.off()
