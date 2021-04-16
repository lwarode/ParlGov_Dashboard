library(tidyverse)

# ParlGov logo and colors -------------------------------------------------
if (! "pg_logo.svg" %in% list.files()) {
  
  pg_logo_url <- "http://www.parlgov.org/static/images/parlgov-logo.svg"
  
  download.file(pg_logo_url, "pg_logo.svg")
  
  pg_color_1 <- colorfindr::get_colors("pg_logo.svg", 
                                      exclude_col = "white", 
                                      top_n = 5) %>% 
    pull(col_hex) %>% 
    pluck(1) # "#474749"
  
  pg_color_2 <- colorfindr::get_colors("pg_logo.svg", 
                                       exclude_col = "white", 
                                       top_n = 5) %>% 
    pull(col_hex) %>% 
    pluck(2) # "#2076B6"
  
} else {
  
  pg_color_1 <- colorfindr::get_colors("pg_logo.svg", 
                                       exclude_col = "white", 
                                       top_n = 5) %>% 
    pull(col_hex) %>% 
    pluck(1) # "#474749"
  
  pg_color_2 <- colorfindr::get_colors("pg_logo.svg", 
                                       exclude_col = "white", 
                                       top_n = 5) %>% 
    pull(col_hex) %>% 
    pluck(2) # "#2076B6"
  
}




# data section ------------------------------------------------------------



