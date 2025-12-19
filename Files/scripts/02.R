library(stringr)

raw_dir <- "Files/raw_text/"
clean_dir <- "Files/clean_text/"

dir.create(clean_dir, showWarnings = FALSE)

raw_files <- list.files(raw_dir, pattern = "_raw.txt$", full.names = TRUE)

cat("Найдено сырых текстов:", length(raw_files), "\n\n")

for (file in raw_files) {
  name <- tools::file_path_sans_ext(basename(file))
  cat("Очищаю:", name, "\n")
  
  text <- readLines(file, encoding = "UTF-8")
  text <- paste(text, collapse = "\n")
  
  clean_text <- text |>
    str_to_lower() |>                                  
    str_replace_all("[^a-z\\s]", " ") |>               
    str_replace_all("-\\s+", "") |>                 
    str_replace_all("\\b\\w{1,2}\\b", " ")|>          
    str_replace_all("\\s+", " ") |>        
    str_trim()
  
  writeLines(clean_text, file.path(clean_dir, paste0(name, "_clean.txt")))
  cat("Сохранён _clean.txt\n\n")
}

cat("Готово\n")