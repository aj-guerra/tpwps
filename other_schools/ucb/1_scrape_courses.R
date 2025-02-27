library(tidyverse)
library(rvest)
library(igraph)
library(ggraph)

courses_url <- "https://guide.berkeley.edu/courses/"
ucb_url <- "https://guide.berkeley.edu/"


# process list of urls from list of depts
urls <- read_html(courses_url) %>% 
   html_nodes('ul li a') %>% 
   html_attr('href') %>% 
   keep(~ grepl("^/courses/", .)) %>% 
   map_chr(~ paste0(ucb_url, .))

# init nodelist and edgelist
nodelist <- data.frame(course_code = character(),
                       course_title = character(),
                       course_units = integer(),
                       dept = factor(),
                       stringsAsFactors = FALSE)

edgelist <- data.frame(prerequisites = character(),
                       course = character())

for (url in urls){
   #load courses from page
   page <- read_html(url)
   courses <- page %>% html_nodes('.courseblock')
   #save dept abbreviation
   dept_code <- str_split_i(url, '/', -2)
   
   for (i in seq_along(courses)) {
      # course name and features for nodelist
      course <- courses[i] %>%
         html_node('h3') %>%
         html_text(trim = TRUE)
      course_parts <- str_match(course, "^(.*?)\\s*—\\s*(.*?)\\s*\\((.*?)(?: unit| units)\\)$")
      code <- courses[i] %>% html_node('.code') %>% html_text(trim = TRUE)
      title <- courses[i] %>% html_node('.title') %>% html_text(trim = TRUE)
      units <- courses[i] %>% html_node('.hours') %>% html_text(trim = TRUE)
      nodelist <- rbind(nodelist,data.frame(course_code = code,
                                            course_title = title,
                                            course_units = units,
                                            dept = as.factor(dept_code),
                                            stringsAsFactors = FALSE))
      
      # add prereqs if they exist to edgelist
      prerequisites <- courses[i] %>%
         html_nodes('.course-section p') %>%
         .[grepl("Prerequisites", html_text(., trim = TRUE), ignore.case = TRUE)]

      if (length(prerequisites) > 0) {
         prerequisites_list <- courses[i] %>%
            html_nodes('.course-section p') %>%
            .[grepl("Prerequisites", html_text(., trim = TRUE), ignore.case = TRUE)] %>% 
            html_text(trim = TRUE)
         
         print(course)
         print(prerequisites_list)

         prerequisites_text <- paste(prerequisites_list, collapse = ", ")
         prerequisites_text <- gsub('"', '', prerequisites_text)
         # add prereqs to edgelist
         edgelist <- rbind(edgelist, 
                           data.frame(prerequisites = prerequisites_text,
                                      course = code))
      }
   }
}

### EDGELIST IS BROKEN - MOST DEPARTMENTS (E.G. PHYSICS) DO NOT TAG PREREQS WITH LINKS, 
### INSTEAD WITH VAGUE TEXT REFERENCES

nodelist2 <- tibble(nodelist)
edgelist2 <- tibble(edgelist)

edgelist2 <- edgelist2 %>%
   mutate(prerequisites = gsub('[“”"]', '', prerequisites), # Remove potential quotation marks
          prerequisites = gsub('\\s*,\\s*', ', ', prerequisites), # Standardize separators
          prerequisites = gsub('[^a-zA-Z0-9, _]', '', prerequisites),
          course = gsub('[“”"]', '', course), # Remove potential quotation marks
          course = gsub('\\s*,\\s*', ', ', course), # Standardize separators
          course = gsub('[^a-zA-Z0-9, _]', '', course)) %>% # Remove unexpected characters
   separate_rows(prerequisites, sep = ",\\s*") %>%
   mutate(course = toupper(gsub(" ", "", course)),
          prerequisites = toupper(gsub(" ", "", prerequisites)),
          prerequisites = gsub('[“”""]', '', prerequisites),
          prerequisites = ifelse(prerequisites == '', NA, prerequisites)) %>% 
   drop_na()

nodelist2 <- nodelist2 %>% 
   drop_na(course_code) %>% 
   mutate(course_code = gsub('[“”"]', '', course_code), # Remove potential quotation marks
          course_code = gsub('\\s*,\\s*', ', ', course_code), # Standardize separators
          course_code = gsub('[^a-zA-Z0-9, _]', ' ', course_code),
          course_code = toupper(gsub(" ", "", course_code))) %>% 
   distinct(course_code, .keep_all = TRUE)

removes <- setdiff(edgelist2$prerequisites, nodelist2$course_code)

edgelist2 <- edgelist2 %>% 
   filter(!prerequisites %in% removes)

coursenet <- graph_from_data_frame(edgelist2, vertices = nodelist2)

saveRDS(coursenet, file = 'ucb/coursenet.rds')
