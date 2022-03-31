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

pca <- prcomp(dtm_tfidf_2_16)
