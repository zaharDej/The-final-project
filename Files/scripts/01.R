library(pdftools)  
library(magick)    

source_dir <- "Files/source_docs/"
raw_dir <- "Files/raw_text/"
examples_dir <- "results/ocr_examples/"

dir.create(raw_dir, showWarnings = FALSE)
dir.create("Files/results", showWarnings = FALSE)
dir.create(examples_dir, showWarnings = FALSE)

pdf_files <- list.files(source_dir, pattern = ".pdf$", full.names = TRUE, ignore.case = TRUE)

cat("Найдено файлов:", length(pdf_files), "\n\n")

for (pdf_path in pdf_files) {
  name <- tools::file_path_sans_ext(basename(pdf_path))
  cat("Обрабатываю:", name, ".pdf\n")
  
  # Извлекаем текст напрямую
  pages_text <- pdf_text(pdf_path)
  full_text <- paste(pages_text, collapse = "\n")
  
  # Сохраняем сырой текст
  writeLines(full_text, file.path(raw_dir, paste0(name, "_raw.txt")))
  cat("  Сохранён _raw.txt\n")
  
  # Демонстрация предобработки: только для первых 3 файлов, первая страница, для отчёта
  if (pdf_path %in% pdf_files[1:3]) {
    img_file <- pdf_convert(pdf_path, format = "png", dpi = 300, pages = 1)[1]
    img <- image_read(img_file) |>
      image_convert(type = "Grayscale") |>
      image_deskew() |>
      image_modulate(brightness = 120) |>
      image_contrast(sharpen = 1) |>
      image_normalize()
    
    image_write(img, file.path(examples_dir, paste0(name, "_example_page.png")))
    file.remove(img_file)
    cat("Сохранён пример\n")
  }
}

cat("Готово", length(pdf_files), "PDF.\n")