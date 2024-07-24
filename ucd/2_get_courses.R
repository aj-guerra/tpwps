# Load required packages
library(pdftools)
library(stringr)
library(dplyr)
library(readr)

# Main function to automate the entire process for a folder of PDFs
automate_pdf_processing <- function(input_folder, output_folder) {
   pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)
   for (pdf_path in pdf_files) {
      text <- paste(pdf_text(pdf_path), collapse = " ")
      
      major_name <- tools::file_path_sans_ext(basename(pdf_path))
      
      pattern <- "\\b[A-Z]{3}\\s[0-9]{3}[A-Z]?\\b"
      courses <- gsub(" ", "", str_extract_all(text, pattern)[[1]])
      
      cat(major_name, "\n\n")
      cat(courses, '\n\n')
      
      output_rds_path <- file.path(output_folder, 
                                   paste0(tools::file_path_sans_ext(basename(pdf_path)), ".rds"))
      saveRDS(courses, output_rds_path)
   }
}

# Example usage
input_folder <- "ucdavis_pdfs"
output_folder <- "ucdavis_courses"

automate_pdf_processing(input_folder, output_folder)

