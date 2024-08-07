library(rvest)
library(httr)
library(purrr)
library(pdftools)

# Define the main URL
main_url <- "https://catalog.ucdavis.edu/departments-programs-degrees"

# Read the HTML content from the main URL
webpage <- read_html(main_url)

# Extract all department and major subpage links
subpage_links <- webpage %>%
   html_nodes('.az_sitemap ul li a') %>%
   html_attr('href') %>%
   keep(~ grepl("^/departments-programs-degrees/", .)) %>%
   keep(~ grepl("-bs|-ab", .)) %>%
   # keep(~ !grepl("-minor|-me|-ms|-ma|-jd|-phd|graduate-group", .)) %>%
   map_chr(~ paste0("https://catalog.ucdavis.edu", .))

# Function to find PDF links on a given subpage
get_pdf_links <- function(subpage_url) {
   pdf_links <- c()
   try({
      subpage <- read_html(subpage_url)
      pdf_links <- subpage %>%
         html_nodes("a") %>%
         html_attr("href") %>%
         na.omit() %>%
         .[grepl(".pdf$", .)] %>% 
         map_chr(~ ifelse(grepl("^http", .), ., paste0("https://catalog.ucdavis.edu", .)))
   }, silent = FALSE)
   return(pdf_links)
}

# Collect all PDF links from each subpage
all_pdf_links <- map(subpage_links, get_pdf_links) %>%
   unlist() 

all_pdf_links <- setdiff(all_pdf_links, 
                         "https://catalog.ucdavis.edu/pdf/GenCat20242025.pdf")

# Ensure the directory for storing PDFs exists
dir.create("ucdavis_pdfs", showWarnings = FALSE)

# Download all the PDFs
for (link in all_pdf_links) {
   try({
   pdf_url <- link
   pdf_name <- basename(pdf_url)
   pdf_content <- GET(pdf_url)
   writeBin(content(pdf_content, "raw"), file.path("ucdavis_pdfs", pdf_name))
   }, silent = FALSE)
}

input_folder <- "ucd/major_pdfs"
output_folder <- "ucd/major_courses"

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

