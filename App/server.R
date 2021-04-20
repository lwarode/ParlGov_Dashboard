library(shiny)
library(shinydashboard)
library(tidyverse)

source(here::here("global.R"))
source(here::here("theme_pg.R"))


# data section ------------------------------------------------------------


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
function(input, output, session) {
  
  party_list_reactive <- reactive({
    
    if (input$country_search == "All") {
      
      # party_main %>% 
      #   mutate(party_both = paste0(party_name_english, " (", party_name_short, ")")) %>% 
      #   arrange(party_both) %>% 
      #   mutate(party_both = str_remove(party_both, "\"")) %>%
      #   pull(party_both)
      
      party_list

    } else {
      
      party_main %>% 
        mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
        filter(country_both %in% input$country_search) %>% 
        mutate(party_both = paste0(party_name_english, " (", party_name_short, ")")) %>% 
        arrange(party_both) %>% 
        pull(party_both)
      
    }
    
  })
  
  observeEvent(input$country_search, {

    # if (!is.null(input$country_search) && input$country_search != "All") {

      updateSelectInput(session, 
                        "party_search", 
                        choices = party_list_reactive(),
                        selected = input$party_search)

    # }
    
  })

    
  
}