library(rvest)
library(httr)
library(purrr)
library(stringr)

# Define the main URL
main_url <- "https://catalog.ucsc.edu/en/current/general-catalog/academic-programs/bachelors-degrees/"
output_folder <- "ucsc/major_courses"
nodelist <- readRDS("ucsc/nodelist.rds")

# Read the HTML content from the main URL

webpage <- read_html(main_url)

major_links <- webpage %>%
   html_nodes('ul li a') %>% 
   html_attr('href') %>%
   keep(~ grepl("/academic-units/", .)) %>%
   map_chr(~ paste0("https://catalog.ucsc.edu", ., "/#-degree-req-2"))


# Extract the major name from each URL
major_names <- major_links %>%
   map_chr(~ str_split_i(., "/", -2))


add_courses_manually <- function(dept_in, min_number, max_number = Inf) {
   filtered_courses <- nodelist %>%
      filter(dept == dept_in) %>%
      filter(as.numeric(str_extract(course_code, "\\d+")) >= min_number) %>%
      filter(as.numeric(str_extract(course_code, "\\d+")) <= max_number)
   return(filtered_courses$course_code)
}

addl_page <- function(page) {
   courses <- read_html(page) %>%
      html_nodes('.sc-coursenumber .sc-courselink') %>%
      html_text(trim = TRUE) %>% 
      map_chr(~ str_squish(.)) %>%
      map_chr(~ str_replace_all(., "\\s+", ""))  
   return(courses)
}

languages6 <- c('CHIN 6',
               'FREN 6',
               'GERM 6',
               'ITAL 6',
               'JAPN 6',
               'SPAN 6',
               'SPHS 6')

languages5 <- c('CHIN 5',
               'FREN 5',
               'GERM 5',
               'ITAL 5',
               'JAPN 5',
               'SPAN 5',
               'SPHS 5')

languages3 <- c('HEBR 3',
                'PERS 3',
                'PORT 3',
                'PUNJ 3',
                'YIDD 3')
   

# Loop through each major link to read the HTML and extract courses
for (i in seq_along(major_links)) {
   major_page <- read_html(major_links[[i]])
   major_name <- major_names[[i]]
   
   courses <- major_page %>%
      html_nodes('.sc-coursenumber .sc-courselink') %>%
      html_text(trim = TRUE) %>% 
      map_chr(~ str_squish(.)) %>%
      map_chr(~ str_replace_all(., "\\s+", "")) 
   
   manual_courses <- c()
   
   if (major_name == "agroecology-ba") {
   } else if (major_name == "anthropology-ba") {
      manual_courses <- c(add_courses_manually('ANTH', 100, 199), manual_courses)
      manual_courses <- c(addl_page('https://catalog.ucsc.edu/en/current/general-catalog/academic-units/social-sciences-division/anthropology/anthropology-course-list'), manual_courses)
   } else if (major_name == "applied-linguistics-and-multilingualism-ba") {
   } else if (major_name == "applied-mathematics-bs") {
      manual_courses <- c(add_courses_manually('AM', 100, 299), manual_courses)
   } else if (major_name == "applied-physics-bs") {
   } else if (major_name == "art-design-games-playable-media-ba") {
      manual_courses <- c(add_courses_manually('HAVC', 0, 199), manual_courses)
      manual_courses <- c(add_courses_manually('ARTG', 100, 189), manual_courses)
   } else if (major_name == "art-ba") {
      manual_courses <- c(add_courses_manually('HAVC', 30, 80), manual_courses)
      manual_courses <- c(add_courses_manually('HAVC', 110, 179), manual_courses)
      manual_courses <- c(add_courses_manually('ART', 101, 199), manual_courses)
   } else if (major_name == "biochemistry-and-molecular-biology-bs") {
   } else if (major_name == "biology-ba") {
      manual_courses <- c(add_courses_manually('BIOE', 100, 199), manual_courses)
   } else if (major_name == "biology-bs") {
   } else if (major_name == "biomolecular-engineering-and-bioinformatics-bs") {
   } else if (major_name == "biotechnology-ba") {
   } else if (major_name == "business-management-economics-ba") {
   } else if (major_name == "chemistry-ba") {
   } else if (major_name == "chemistry-bs") {
   } else if (major_name == "classical-studies-ba") {
   } else if (major_name == "cognitive-science-bs") {
      manual_courses <- c(c('PSYC 119', 'PSYC 139', 'PSYC 159', 'PSYC 179'), manual_courses)
   } else if (major_name == "community-studies-ba") {
   } else if (major_name == "computer-engineering-bs") {
      manual_courses <- c(addl_page('https://catalog.ucsc.edu/en/current/general-catalog/academic-units/baskin-engineering/computer-science-and-engineering/computer-engineering-bs-elective-course-list/'), manual_courses)
   } else if (major_name == "computer-science-ba") {
      manual_courses <- c(add_courses_manually('CSE', 100, 189), manual_courses)
   } else if (major_name == "computer-science-bs") {
      manual_courses <- c(add_courses_manually('CSE', 100, 189), manual_courses)
   } else if (major_name == "computer-science-computer-game-design-bs") {
   } else if (major_name == "creative-technologies-ba") {
      manual_courses <- c(c('CT 100', 'CT 101', 'CT 110', 'CT 120', 'CT 125', 'CT195'), manual_courses)
      manual_courses <- c(add_courses_manually('CT', 160, 163), manual_courses)
   } else if (major_name == "critical-race-and-ethnic-studies-ba") {
   } else if (major_name == "earth-sciences-bs") {
      manual_courses <- c(add_courses_manually('EART', 100, 199), manual_courses)
   } else if (major_name == "earth-sciencesanthropology-combined-major-ba") {
      manual_courses <- c(add_courses_manually('EART', 100, 199), manual_courses)
      manual_courses <- c(addl_page('https://catalog.ucsc.edu/current/general-catalog/academic-units/social-sciences-division/anthropology/anthropology-course-list/'), manual_courses)
      manual_courses <- c(c('EART 189A', 'EART 189B', 'EART 191', 'EART 191D', 'EART 195', 'EART 198'), manual_courses)
   } else if (major_name == "ecology-and-evolution-bs") {
   } else if (major_name == "economics-ba") {
   } else if (major_name == "economicsmathematics-combined-ba") {
   } else if (major_name == "education-democracy-and-justice-ba") {
      manual_courses <- c(add_courses_manually('EDUC', 102, 187), manual_courses)
   } else if (major_name == "electrical-engineering-bs") {
   } else if (major_name == "environmental-sciences-bs") {
      manual_courses <- c(add_courses_manually('EART', 100, 199), manual_courses)
      manual_courses <- c(add_courses_manually('OCEA', 100, 199), manual_courses)
      manual_courses <- c(add_courses_manually('ESCI', 100, 189), manual_courses)
   } else if (major_name == "environmental-studies-ba") {
      manual_courses <- c(add_courses_manually('ENVS', 100, 179), manual_courses)
   } else if (major_name == "environmental-studiesbiology-combined-major-ba") {
      manual_courses <- c(add_courses_manually('ENVS', 101, 179), manual_courses)
   } else if (major_name == "environmental-studiesearth-sciences-combined-major-ba") {
   } else if (major_name == "environmental-studieseconomics-combined-major-ba") {
      manual_courses <- c(add_courses_manually('ENVS', 101, 179), manual_courses)
   } else if (major_name == "feminist-studies-ba") {
      manual_courses <- c(add_courses_manually('FMST', 100, 199), manual_courses)
   } else if (major_name == "film-and-digital-media-ba") {
   } else if (major_name == "global-and-community-health-ba") {
   } else if (major_name == "global-and-community-health-bs") {
   } else if (major_name == "global-economics-ba") {
      manual_courses <- c(languages6, manual_courses)
   } else if (major_name == "history-ba") {
      manual_courses <- c(addl_page('https://catalog.ucsc.edu/en/current/general-catalog/academic-units/humanities-division/history/history-course-list/'), manual_courses)
   } else if (major_name == "history-of-art-and-visual-culture-ba") {
      manual_courses <- c(add_courses_manually('HAVC', 10, 80), manual_courses)
      manual_courses <- c(add_courses_manually('HAVC', 110, 191), manual_courses)
   } else if (major_name == "jewish-studies-ba") {
   } else if (major_name == "language-studies-ba") {
      manual_courses <- c(add_courses_manually('CHIN', 100, 199), manual_courses)
      manual_courses <- c(add_courses_manually('FREN', 100, 199), manual_courses)
      manual_courses <- c(add_courses_manually('GERM', 100, 199), manual_courses)
      manual_courses <- c(add_courses_manually('ITAL', 100, 199), manual_courses)
      manual_courses <- c(add_courses_manually('JAPN', 100, 199), manual_courses)
      manual_courses <- c(add_courses_manually('SPAN', 100, 199), manual_courses)
      manual_courses <- c(add_courses_manually('SPHS', 100, 199), manual_courses)
      manual_courses <- c(add_courses_manually('LIT', 182, 189), manual_courses)
      manual_courses <- c(add_courses_manually('LING', 102, 189), manual_courses)
      manual_courses <- c(addl_page('https://catalog.ucsc.edu/en/current/general-catalog/academic-units/humanities-division/linguistics/language-studies-cultural-context-electives-course-list/'), manual_courses)
   } else if (major_name == "latin-american-and-latino-studies-ba") {
      manual_courses <- c(add_courses_manually('LALS', 1, 194), manual_courses)
   } else if (major_name == "latin-american-and-latino-studieseducation-democracy-and-justice-ba") {
      manual_courses <- c(add_courses_manually('LALS', 101, 190), manual_courses)
      manual_courses <- c(add_courses_manually('EDUC', 102, 187), manual_courses)
   } else if (major_name == "latin-american-and-latino-studiespolitics-combined-ba") {
      manual_courses <- c(add_courses_manually('LALS', 101, 194), manual_courses)
      manual_courses <- c(add_courses_manually('POLI', 100, 190), manual_courses)
   } else if (major_name == "latin-american-and-latino-studiessociology-combined-ba") {
      manual_courses <- c(add_courses_manually('LALS', 101, 194), manual_courses)
      manual_courses <- c(add_courses_manually('SOCY', 110, 189), manual_courses)
   } else if (major_name == "legal-studies-ba") {
   } else if (major_name == "linguistics-ba") {
      manual_courses <- c(languages5, manual_courses)
      manual_courses <- c(languages3, manual_courses)
      manual_courses <- c(add_courses_manually('LING', 102, 189), manual_courses)
   } else if (major_name == "literature-ba") {
      manual_courses <- c(add_courses_manually('LIT', 60, 61), manual_courses)
      manual_courses <- c(add_courses_manually('LIT', 80, 81), manual_courses)
      manual_courses <- c(add_courses_manually('LIT', 109, 189), manual_courses)
   } else if (major_name == "marine-biology-bs") {
      manual_courses <- c(add_courses_manually('BIOE', 100, 179), manual_courses)
   } else if (major_name == "mathematics-ba") {
      manual_courses <- c(add_courses_manually('MATH', 101, 190), manual_courses)
      manual_courses <- c(add_courses_manually('AM', 101, 190), manual_courses)
      manual_courses <- c(add_courses_manually('STAT', 101, 190), manual_courses)
   } else if (major_name == "mathematics-bs") {
      manual_courses <- c(add_courses_manually('MATH', 101, 190), manual_courses)
      manual_courses <- c(add_courses_manually('AM', 101, 190), manual_courses)
      manual_courses <- c(add_courses_manually('STAT', 101, 190), manual_courses)
   } else if (major_name == "mathematics-education-ba") {
   } else if (major_name == "mathematics-theory-and-computation-bs") {
   } else if (major_name == "microbiology-bs") {
   } else if (major_name == "molecular-cell-and-developmental-biology-bs") {
   } else if (major_name == "music-ba") {
      manual_courses <- c(addl_page('https://catalog.ucsc.edu/en/current/general-catalog/academic-units/arts-division/music/contemporary-practices-concentration-module-course-list/'), manual_courses)
   } else if (major_name == "music-bm") {
   } else if (major_name == "network-and-digital-technology-ba") {
      manual_courses <- c(addl_page('https://catalog.ucsc.edu/en/current/general-catalog/academic-units/baskin-engineering/computer-science-and-engineering/network-and-digital-technology-electives-course-list/'), manual_courses)
   } else if (major_name == "neuroscience-bs") {
   } else if (major_name == "philosophy-ba") {
      manual_courses <- c(add_courses_manually('PHIL', 1, 198), manual_courses)
   } else if (major_name == "physics-astrophysics-bs") {
   } else if (major_name == "physics-bs") {
      manual_courses <- c(add_courses_manually('PHYS', 100, 180), manual_courses)
      manual_courses <- c(add_courses_manually('ASTR', 111, 118), manual_courses)
   } else if (major_name == "plant-sciences-bs") {
      manual_courses <- c(add_courses_manually('BIOE', 100, 179), manual_courses)
   } else if (major_name == "politics-ba") {
      manual_courses <- c(add_courses_manually('POLI', 1, 70), manual_courses)
      manual_courses <- c(add_courses_manually('POLI', 100, 195), manual_courses)
   } else if (major_name == "psychology-ba") {
      manual_courses <- c(add_courses_manually('PSYC', 100, 189), manual_courses)
      #technically this is missing one elective in one of a dozen different depts...
   } else if (major_name == "robotics-engineering-bs") {
   } else if (major_name == "science-education-bs") {
      manual_courses <- c(add_courses_manually('CHEM', 100, 180), manual_courses)
      manual_courses <- c(add_courses_manually('EART', 100, 189), manual_courses)
   } else if (major_name == "sociology-ba") {
      manual_courses <- c(add_courses_manually('SOCY', 110, 189), manual_courses)
      manual_courses <- c(addl_page('https://catalog.ucsc.edu/en/current/general-catalog/academic-units/social-sciences-division/sociology/sociology-general-major-course-list/'), manual_courses)
   } else if (major_name == "spanish-studies-ba") {
   } else if (major_name == "technology-and-information-management-bs") {
      manual_courses <- c(add_courses_manually('ECON', 100, 189), manual_courses)
      manual_courses <- c(add_courses_manually('AM', 100, 189), manual_courses)
      manual_courses <- c(add_courses_manually('BME', 100, 189), manual_courses)
      manual_courses <- c(add_courses_manually('CMPM', 100, 189), manual_courses)
      manual_courses <- c(add_courses_manually('CSE', 100, 189), manual_courses)
      manual_courses <- c(add_courses_manually('ECE', 100, 189), manual_courses)
      manual_courses <- c(add_courses_manually('TIM', 100, 189), manual_courses)
   } else if (major_name == "theater-arts-ba") {
      manual_courses <- c(add_courses_manually('THEA', 1, 99), manual_courses)
   }
   
   all_courses <- unique(c(courses, manual_courses))
   
   output_rds_path <- file.path(output_folder, paste0(major_name, ".rds"))
   saveRDS(all_courses, output_rds_path)
   
   cat(major_name, "\n\n")
   cat(all_courses, '\n\n')
   
}



# # Function to prompt user for input
# prompt_user <- function() {
#    response <- readline(prompt = "Enter 'y' to open the next URL or any other key to exit: ")
#    return(response)
# }
# 
# # Loop through each major link
# for (i in seq_along(major_links[41:76])) {
#    # Open the current URL in the default web browser
#    browseURL(major_links[i+41])
#    
#    # Print the current major name
#    cat("Opening URL for:", major_names[i], "\n")
#    
#    # Prompt user to proceed
#    response <- prompt_user()
#    
#    # Check the user's response
#    if (tolower(response) != 'y') {
#       cat("Exiting...\n")
#       break
#    }
# }
# 
# 
