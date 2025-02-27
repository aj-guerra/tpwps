library(tidyverse)
library(rvest)
library(igraph)
library(ggraph)

courses_url <- "https://catalog.ucsc.edu/en/current/general-catalog/courses/"

urls <- read_html(courses_url) %>% 
   html_nodes('.sc-child-item-links a') %>% 
   html_attr('href') %>% 
   keep(~ grepl("^/en/current/general-catalog/courses/", .)) %>% 
   map_chr(~ paste0("https://catalog.ucsc.edu", .))

# Initialize nodelist and edgelist
nodelist <- data.frame(course_code = character(),
                       course_title = character(),
                       course_units = integer(),
                       dept = factor(),
                       stringsAsFactors = FALSE)

edgelist <- data.frame(prerequisites = character(),
                       course = character(),
                       stringsAsFactors = FALSE)

# Iterate over each URL
for (url in urls) {
   # Load courses from the page
   page <- read_html(url)
   course_elements <- page %>% html_nodes('.courselist > *')
   
   # Save department abbreviation
   dept_code <- str_split_i(url, '/', -1)
   
   course_info <- list()
   
   for (element in course_elements) {
      class_name <- element %>% html_attr('class')
      
      if (!is.null(class_name) && class_name == 'course-name') {
         # Save previous course information if exists
         if (length(course_info) > 0) {
            # Extract course code and title
            course_parts <- str_match(course_info[['name']], "^([A-Z]{1,4} \\d{1,4}[A-Z]?)\\s+(.*)$")            
            code <- course_parts[, 2]
            title <- course_parts[, 3]
            
            # Extract course units
            units <- as.integer(str_extract(course_info[['units']], "\\d+"))
            
            # Add course details to nodelist
            nodelist <- rbind(nodelist, data.frame(course_code = code,
                                                   course_title = title,
                                                   course_units = units,
                                                   dept = as.factor(dept_code),
                                                   stringsAsFactors = FALSE))
            
            # Add prerequisites to edgelist if exists
            if (!is.null(course_info[['prerequisites']])) {
               prerequisites_list <- course_info[['prerequisites']] %>% html_nodes('a') %>% html_text(trim = TRUE)
               prerequisites_text <- paste(prerequisites_list, collapse = ", ")
               prerequisites_text <- gsub('"', '', prerequisites_text)
               
               edgelist <- rbind(edgelist, data.frame(prerequisites = prerequisites_text,
                                                      course = code,
                                                      stringsAsFactors = FALSE))
            }
         }
         
         # Start new course information
         course_info <- list()
         course_info[['name']] <- element %>% html_text(trim = TRUE)
      } else if (!is.null(class_name) && class_name == 'sc-credithours') {
         course_info[['units']] <- element %>% html_text(trim = TRUE)
      } else if (!is.null(class_name) && class_name == 'extraFields') {
         course_info[['prerequisites']] <- element
      }
   }
   
   # Handle the last course in the list
   if (length(course_info) > 0) {
      course_parts <- str_match(course_info[['name']], "^([A-Z]{1,4} \\d{1,4}[A-Z]?)\\s+(.*)$")            
      code <- course_parts[, 2]
      title <- course_parts[, 3]
      
      units <- as.integer(str_extract(course_info[['units']], "\\d+"))
      
      nodelist <- rbind(nodelist, data.frame(course_code = code,
                                             course_title = title,
                                             course_units = units,
                                             dept = as.factor(dept_code),
                                             stringsAsFactors = FALSE))
      
      if (!is.null(course_info[['prerequisites']])) {
         prerequisites_list <- course_info[['prerequisites']] %>% html_nodes('a') %>% html_text(trim = TRUE)
         prerequisites_text <- paste(prerequisites_list, collapse = ", ")
         prerequisites_text <- gsub('"', '', prerequisites_text)
         
         edgelist <- rbind(edgelist, data.frame(prerequisites = prerequisites_text,
                                                course = code,
                                                stringsAsFactors = FALSE))
      }
   }
}

nodelist <- tibble(nodelist)
edgelist <- tibble(edgelist)

# Print results
print(nodelist)
print(edgelist)


edgelist <- edgelist %>%
   drop_na() %>% 
   mutate(prerequisites = gsub('[“”"]', '', prerequisites), # Remove potential quotation marks
          prerequisites = gsub('\\s*,\\s*', ', ', prerequisites), # Standardize separators
          prerequisites = gsub('[^a-zA-Z0-9, _]', '', prerequisites)) %>% # Remove unexpected characters
   separate_rows(prerequisites, sep = ",\\s*") %>%
   mutate(course = toupper(gsub(" ", "", course)),
          prerequisites = toupper(gsub(" ", "", prerequisites)))

nodelist <- nodelist %>% 
   drop_na(course_code) %>% 
   mutate(course_code = gsub('[“”"]', '', course_code), # Remove potential quotation marks
          course_code = gsub('\\s*,\\s*', ', ', course_code), # Standardize separators
          course_code = gsub('[^a-zA-Z0-9, _]', ' ', course_code),
          course_code = toupper(gsub(" ", "", course_code)),
          dept_long = dept,
          dept = toupper(str_extract(dept, "^[^-]+"))) %>% 
   distinct(course_code, .keep_all = TRUE)

removes <- setdiff(edgelist$prerequisites, nodelist$course_code)

edgelist <- edgelist %>% 
   filter(!prerequisites %in% removes)

saveRDS(nodelist, file = 'ucsc/nodelist.rds')
saveRDS(edgelist, file = 'ucsc/edgelist.rds')

coursenet <- graph_from_data_frame(edgelist, vertices = nodelist)

saveRDS(coursenet, file = 'ucsc/coursenet.rds')



