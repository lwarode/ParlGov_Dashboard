library(shiny)
library(shinydashboard)
library(tidyverse)

source(here::here("global.R"))
source(here::here("theme_pg.R"))


# data section ------------------------------------------------------------
party_main <- read_csv(paste0(pg_url, "view_party.csv"))
elec_main <- read_csv(paste0(pg_url, "view_election.csv"))
cab_main <- read_csv(paste0(pg_url, "view_cabinet.csv"))

# pg_data <- c("view_party.csv", 
#              "view_election.csv",
#              "view_cabinet.csv")
# 
# pg_data %>% 
#   # map(~ paste0(pg_url, .x)) %>% 
#   purrr::map(function(file_name) { 
#     assign(x = str_remove(file_name, ".csv") %>% str_remove("view_"), 
#            value = read_csv(paste0(pg_url, file_name)),
#            envir = .GlobalEnv)
#   })
  

# server ------------------------------------------------------------------
function(input, output) {
  
  
  
}