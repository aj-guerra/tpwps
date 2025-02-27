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

individual_majors <- all_pdf_links[str_detect(all_pdf_links, 'individual-major')]
master_majors <- all_pdf_links[str_detect(all_pdf_links, '-ms')]
EHUF_extra <- all_pdf_links[str_detect(all_pdf_links, 'EHUF')]
removes <- c(individual_majors, 
             master_majors, 
             EHUF_extra, 
             "https://catalog.ucdavis.edu/pdf/GenCat20242025.pdf" 
             )

all_pdf_links <- setdiff(all_pdf_links,
                         removes)

# Ensure the directory for storing PDFs exists
dir.create("data/major_pdfs", showWarnings = FALSE)

# Download all the PDFs
for (link in all_pdf_links) {
   try({
   pdf_url <- link
   pdf_name <- basename(pdf_url)
   pdf_content <- GET(pdf_url)
   writeBin(content(pdf_content, "raw"), file.path("data/major_pdfs", pdf_name))
   }, silent = FALSE)
}

input_folder <- "data/major_pdfs"
output_folder <- "data/major_courses"

major_metadata <- data.frame('major' = character(), 
                             'mean_units' = numeric(),
                             'college' = factor())

pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)

for (pdf_path in pdf_files) {
   text <- paste(pdf_text(pdf_path), collapse = " ")
   
   major_name <- tools::file_path_sans_ext(basename(pdf_path))
   
   # Extract course codes
   pattern <- "\\b[A-Z]{3}\\s[0-9]{3}[A-Z]?\\b"
   courses <- gsub(" ", "", str_extract_all(text, pattern)[[1]])
   
   # get total units
   unit_ranges_raw <- str_match_all(text, "Total Units\\s*:?\\s*(\\S+)")[[1]][,2]

   average_range <- function(x) {
      # Check if the input is a valid numeric range (e.g., "24-32") or single number (e.g., "22")
      if (grepl("^\\d+-\\d+$", x)) {  
         numbers <- as.numeric(str_split(x, "-", simplify = TRUE))  # Split and convert to numeric
         return(mean(numbers, na.rm=TRUE))  # Compute the average
      } else if (grepl("^\\d+$", x)) {  
         return(as.numeric(x))  # If it's a single number, return as numeric
      } else {
         return(NA)  # Return NA for non-numeric values
      }
   }
   
   # line is drawn at SD of 10, between music 
   ## (11.7, which uses total units to describe subtotals for tracks)
   # and theater and dance 
   # (9.89, which uses total units to describe total units)
   
   sd_total_units <- sd(sapply(unit_ranges_raw, average_range), na.rm = TRUE)
   sd_total_units <- ifelse(is.na(sd_total_units), 0, sd_total_units)
   
   if (sd_total_units > 10){
      mean_total_units <- max(sapply(unit_ranges_raw, average_range), na.rm = TRUE)
   } else{
      mean_total_units <- mean(sapply(unit_ranges_raw, average_range), na.rm = TRUE)
   }
   

   # Define the regex pattern
   
   # Function to extract first 30 words
   extract_first_n_words <- function(text, n = 30) {
      words <- unlist(strsplit(text, "\\s+"))  # Split text into words
      first_n_words <- paste(words[1:min(n, length(words))], collapse = " ")  # Get first n words
      return(first_n_words)
   }
   
   # Extract first 30 words
   short_text <- extract_first_n_words(text, 100)
   
   # Define the regex pattern
   college_pattern <- "College of (Letters & Science|Biological Sciences|Engineering|Agricultural & Environmental Sciences)"
   
   college <- regmatches(short_text, 
                         gregexpr(college_pattern, short_text, ignore.case = TRUE))[[1]][1]
   
   # Replace full names with abbreviations
   abbreviations <- c(
      "College of Letters & Science" = "CLS",
      "College of Biological Sciences" = "CBS",
      "College of Engineering" = "CE",
      "College of Agricultural & Environmental Sciences" = "CAES"
   )
   
   college_abbreviation <- if (!is.na(college)) unname(abbreviations[college]) else NA
   
   print(college_abbreviation)
   
   
   major_metadata <- rbind(major_metadata, 
                           data.frame('major' = major_name, 
                                      'mean_units' = mean_total_units,
                                      'college' = college_abbreviation))
   
   cat(major_name, "\n\n")
   
   output_rds_path <- file.path(output_folder, 
                                paste0(tools::file_path_sans_ext(basename(pdf_path)), ".rds"))
   saveRDS(courses, output_rds_path)
}

saveRDS(major_metadata, "data/major_metadata.rds")

