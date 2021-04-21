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

pg_url <- "http://www.parlgov.org/static/data/experimental-cp1252/"

party_main <- read_csv(paste0(pg_url, "view_party.csv"), locale = locale(encoding = "Latin1"))
elec_main <- read_csv(paste0(pg_url, "view_election.csv"), locale = locale(encoding = "Latin1"))
cab_main <- read_csv(paste0(pg_url, "view_cabinet.csv"), locale = locale(encoding = "Latin1"))

country_list <- elec_main %>% 
  distinct(country_name, country_name_short) %>% 
  mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
  arrange(country_both) %>% 
  add_row(country_both = "All", .before = 1) %>% 
  pull(country_both)

party_list <- party_main %>% 
  mutate(party_both = paste0(party_name_english, 
                             " | ",
                             party_name,
                             " (", 
                             party_name_short, 
                             ")")) %>% 
  arrange(party_both) %>% 
  mutate(party_both = str_remove(party_both, "\"")) %>% 
  pull(party_both)

party_y_value <- c("state_market", 
                   "liberty_authority", 
                   "eu_anti_pro")

names(party_y_value) <- c("State/Market (Regulation of the Economy)", 
                          "Libertarian/Authoritarian",
                          "Position towards EU Integration")



# cab_raw <- read_csv(paste0(pg_url, "view_cabinet.csv"))
# elec_raw <- read_csv(paste0(pg_url, "view_election.csv"))
# party_raw <- read_csv(paste0(pg_url, "view_party.csv"))
