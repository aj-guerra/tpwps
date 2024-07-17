library(tidyverse)
library(rvest)
library(igraph)
library(ggraph)

courses_url <- "https://catalog.ucdavis.edu/courses-subject-code/"
ucd_url <- "https://catalog.ucdavis.edu"

# process list of urls from list of depts
urls <- read_html(courses_url) %>% 
  html_nodes('.az_sitemap ul li a') %>% 
  html_attr('href') %>% 
  keep(~ grepl("^/courses-subject-code/", .)) %>% 
  map_chr(~ paste0(ucd_url, .))

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
    code <- course_parts[, 2]
    title <- course_parts[, 3]
    units <- as.integer(course_parts[, 4])
    nodelist <- rbind(nodelist,data.frame(course_code = code,
                                          course_title = title,
                                          course_units = units, 
                                          dept = as.factor(dept_code),
                                          stringsAsFactors = FALSE))
    
    # add prereqs if they exist to edgelist
    prerequisites_exists <- courses[i] %>% 
      html_nodes('p.text.courseblockdetail.detail-prerequisite') %>% 
      html_text(trim = TRUE)
    
    if (length(prerequisites_exists) > 0) {
      prerequisites_list <- courses[i] %>% 
        html_nodes('p.text.courseblockdetail.detail-prerequisite a') %>% 
        html_text(trim = TRUE)
      prerequisites_text <- paste(prerequisites_list, collapse = ", ")
      prerequisites_text <- gsub('"', '', prerequisites_text)
      # add prereqs to edgelist
      edgelist <- rbind(edgelist, data.frame(prerequisites = prerequisites_text,
                                             course = code))
    }
  }
}

nodelist <- tibble(nodelist)
edgelist <- tibble(edgelist)

saveRDS(nodelist, file = 'nodelist.rds')
saveRDS(edgelist, file = 'edgelist.rds')

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
         course_code = toupper(gsub(" ", "", course_code))) %>% 
  distinct(course_code, .keep_all = TRUE)

removes <- setdiff(edgelist$prerequisites, nodelist$course_code)

edgelist <- edgelist %>% 
  filter(!prerequisites %in% removes)

coursenet <- graph_from_data_frame(edgelist, vertices = nodelist)

saveRDS(coursenet, file = 'coursenet.rds')



