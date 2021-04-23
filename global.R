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

elec_type <- c("all",
               "parliament",
               "ep")
names(elec_type) <- c("All Elections",
                      "Parliamentary Elections",
                      "EP Elections")

# party color
pg_party_color_raw <- read.csv(here::here("pg_party_color.csv"))

pg_party_color <- pg_party_color_raw %>% 
  rename(party_id = dataset_party_id) %>% 
  # mutate(color = case_when(
  #   pg_party_id %in% c(808, 501, 791, …) ~ color_2,
  #   TRUE ~ color_1
  # ))
  mutate(color = case_when(
    # CDU
    party_id == 808 ~ color_2,
    # N-VA
    party_id == 501 ~ color_2,
    # DIE LINKE
    party_id == 791 ~ color_2,
    TRUE ~ color_1
  )) %>% 
  right_join(party_main) %>% 
  select(color, party_id) %>% 
  mutate(color = if_else(is.na(color), "grey", color))




# cab_raw <- read_csv(paste0(pg_url, "view_cabinet.csv"))
# elec_raw <- read_csv(paste0(pg_url, "view_election.csv"))
# party_raw <- read_csv(paste0(pg_url, "view_party.csv"))
