library(tesseract)
library(pdftools)
library(magick)

# Пути к папкам
source_dir   <- "source_docs/"
raw_dir      <- "raw_text/"
ocr_examples_dir <- "results/ocr_examples/"

# Создаём папки, если их нет
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create(ocr_examples_dir, showWarnings = FALSE, recursive = TRUE)

# Английская модель Tesseract
eng <- tesseract("eng")

# Находим все PDF
pdf_files <- list.files(source_dir, pattern = "\\.pdf$", full.names = TRUE)

cat("Найдено PDF-файлов:", length(pdf_files), "\n")

if (length(pdf_files) == 0) {
  stop("Не найдено ни одного PDF в папке source_docs/. Положи туда файлы.")
}

# Цикл по всем PDF
for (pdf in pdf_files) {
  name <- tools::file_path_sans_ext(basename(pdf))
  cat("Обрабатываем:", name, "\n")
  
  # Конвертируем PDF в изображения с высоким DPI
  images <- pdf_convert(pdf, format = "png", dpi = 400)
  
  # OCR по каждой странице
  ocr_pages <- sapply(images, function(img_path) {
    img <- image_read(img_path) %>%
      image_convert(type = "Grayscale") %>%     # Ч/б
      image_deskew() %>%                        # Выравнивание
      image_modulate(brightness = 120, saturation = 120) %>%
      image_contrast(sharpen = 2) %>%
      image_normalize()
    
    # Сохраняем первую страницу как пример (для отчёта)
    if (grepl("_1.png$", img_path)) {  # только первая страница каждого файла
      example_name <- paste0(name, "_page1_processed.png")
      image_write(img, path = file.path(ocr_examples_dir, example_name))
      cat("  Сохранён пример страницы:", example_name, "\n")
    }
    
    ocr(img, engine = eng)
  })
  
  # Склеиваем текст всех страниц
  full_text <- paste(ocr_pages, collapse = "\n")
  
  # Сохраняем сырой текст
  writeLines(full_text, file.path(raw_dir, paste0(name, "_raw.txt")))
  
  # Удаляем временные PNG
  file.remove(images)
  
  cat("  Готово, сохранён:", paste0(name, "_raw.txt"), "\n")
}

cat("\nПункт 1 завершён!\n")
cat("Сырые тексты сохранены в папке:", raw_dir, "\n")
cat("Примеры обработанных страниц — в:", ocr_examples_dir, "\n")