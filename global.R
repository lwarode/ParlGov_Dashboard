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


# Data Section ------------------------------------------------------------

# Create "Data subfolder" if not existing
if (! dir.exists(here::here("Data"))) {

  dir.create(here::here("Data"))

}

# custom date for update requirement
custom_date <- Sys.time() - 60 * 60 * 24 * 30 # 30 days prior Sys.time()

# date of last change
last_changed <-
  fs::dir_info(path = here::here("Data"), regexp = "view") %>%
  pull(modification_time) %>%
  min()

# (Re-)load data if csv is older than custom date or is not existent
if (last_changed < custom_date | is.infinite(last_changed)) {

  # file prefix
  file_prefix <- "view"
  
  # URL of data
  pg_url <- "http://www.parlgov.org/static/data/experimental-cp1252/"
  
  # Download ParlGov main ("view_") data
  map(c("party", "election", "cabinet"), 
      ~ download.file(paste0(pg_url, file_prefix, "_", .x, ".csv"), 
                      here::here("Data", paste0(file_prefix, "_", .x, ".csv")))
  )
  
    
}

# Add data to environment
party_main <- read_csv(here::here("Data", "view_party.csv"), locale = locale(encoding = "Latin1"))
election_main <- read_csv(here::here("Data", "view_election.csv"), locale = locale(encoding = "Latin1"))
cabinet_main <- read_csv(here::here("Data", "view_cabinet.csv"), locale = locale(encoding = "Latin1"))

# Country Search UI Input
country_list <- election_main %>% 
  distinct(country_name, country_name_short) %>% 
  mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
  arrange(country_both) %>% 
  add_row(country_both = "All", .before = 1) %>% 
  pull(country_both)


# Party -------------------------------------------------------------------
# Party Search UI Input
party_list <- party_main %>% 
  mutate(party_both = paste0(party_name_english, 
                             "/",
                             party_name,
                             " | ",
                             country_name_short,
                             " (", 
                             party_name_short, 
                             ")")) %>% 
  arrange(party_both) %>% 
  mutate(party_both = str_remove(party_both, "\"")) %>% 
  pull(party_both)

# Party LR Plot Y Axis
party_y_value <- c("state_market", 
                   "liberty_authority", 
                   "eu_anti_pro")
names(party_y_value) <- c("State/Market (Regulation of the Economy)", 
                          "Libertarian/Authoritarian",
                          "Position towards EU Integration")

# Party VS Plot Election Type
elec_type <- c("all",
               "parliament",
               "ep")
names(elec_type) <- c("All Elections",
                      "Parliamentary Elections",
                      "EP Elections")


# Election ----------------------------------------------------------------
# Election Search UI Input
election_list <- election_main %>% 
  mutate(election_date_both = paste0(election_date,
                                     " | ",
                                     country_name)) %>%
  mutate(election_date_both = if_else(election_type == "ep",
                                      paste0(election_date_both, " (EP)"),
                                      election_date_both)) %>% 
  arrange(desc(election_date)) %>% 
  pull(election_date_both)


# Cabinet -----------------------------------------------------------------
# Cabinet Search UI Input
cabinet_list <- cabinet_main %>% 
  mutate(cabinet_name_year = paste0(cabinet_name,
                                    " (",
                                    lubridate::year(election_date),
                                    ")")) %>% 
  arrange(desc(election_date)) %>%
  distinct(cabinet_name_year) %>% 
  pull(cabinet_name_year)
  

# Color (Party/All) -------------------------------------------------------
# Party Color (All)
pg_party_color_raw <- read.csv(here::here("pg_party_color.csv"))

pg_party_color <- pg_party_color_raw %>% 
  rename(party_id = dataset_party_id) %>% 
  mutate(color_1 = str_trim(color_1)) %>% 
  # mutate(color = case_when(
  #   pg_party_id %in% c(808, 501, 791, …) ~ color_2,
  #   TRUE ~ color_1
  # ))
  mutate(color = case_when(
    # CDU
    party_id == 808 ~ color_2,
    # DIE LINKE
    party_id == 791 ~ color_2,
    # N-VA
    party_id == 501 ~ color_2,
    # VB
    party_id == 993 ~ "#F2BD2E",
    TRUE ~ color_1
  )) %>% 
  right_join(party_main) %>% 
  select(color, party_id) %>% 
  mutate(color = if_else(is.na(color), "grey", color))

