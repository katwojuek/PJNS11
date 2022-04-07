# ładowanie bibliotek

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

# analiza głównych składowych
pca <- prcomp(dtm_tfidf_2_16)

# przygotowanie danych do wykresu
legend <- paste(
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  rownames(dtm_tfidf_2_16),
  sep = " - "
)
x <- pca$x[,1]
y <- pca$x[,2]

# wykres w przestrzeni dwuwymiarowej
options(scipen = 5)
plot(
  x,
  y,
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
  col = "purple"
)
text(
  x,
  y, 
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  pos = 4,
  col = "purple"
)
legend(
  "bottom",
  legend,
  cex = 0.5,
  text.col = "purple"
)

# zapis wykresu do pliku .png
pca_file <- create_path(
  output_dir,
  "pca.png"
)
png(pca_file)
options(scipen = 5)
plot(
  x,
  y,
  main = "Analiza głównych składowych",
  xlab = "PC1",
  ylab = "PC2",
  col = "purple"
)
text(
  x,
  y, 
  paste(
    "d",
    1:length(rownames(dtm_tfidf_2_16)),
    sep = ""
  ),
  pos = 4,
  col = "purple"
)
legend(
  "bottom",
  legend,
  cex = 0.7,
  text.col = "purple"
)
dev.off()
