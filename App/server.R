library(shiny)
library(shinydashboard)
library(tidyverse)

# source(here::here("global.R"))
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
  
  party_df <- reactive({
    
    party_main %>% 
      mutate(party_both = paste0(party_name_english, 
                                 " | ",
                                 party_name,
                                 " (", 
                                 party_name_short, 
                                 ")"),
             party_both_short = paste0(party_name_english, 
                                       " (", 
                                       party_name_short, 
                                       ")")) %>% 
      filter(party_both %in% input$party_search)
      # distinct(party_both, .keep_all = TRUE) %>% 
      # select(- party_both)
      
  })
  
  elec_df <- reactive({
    
    elec_main %>% 
      mutate(party_both = paste0(party_name_english, " (", party_name_short, ")")) %>% 
      filter(party_both %in% input$party_search) 
      # distinct(party_both, .keep_all = TRUE) %>% 
      # select(- party_both)
    
  })
  
  party_family <- reactive({
    
    party_df() %>% 
      pull(family_name) %>% 
      unique() %>% 
      toString() 
    
  })
  
  # output$party_family <- renderPrint(party_family())
  
  output$party_family <- renderInfoBox({

    infoBox(
      "Party Families", party_family(), icon = icon("flag"), color = "blue", width = 6
    )

  })
  
  output$party_lr <- renderPlot({
    
    y_axis_var <- input$y_axis_id %>% unname()
    y_axis_label <- input$y_axis_id %>% names()
    
    ggplot(party_df(), aes_string(x = "left_right", y = y_axis_var)) + 
      geom_point(size = 2.5, alpha = 0.75) + 
      ggrepel::geom_text_repel(aes(label = party_both_short), point.size = 2.5, nudge_y = 0.25) +
      scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) + 
      scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
      geom_vline(xintercept = 5, alpha = 0.5) +
      geom_hline(yintercept = 5, alpha = 0.5) +
      theme_pg() +
      labs(x = "Left-Right", y = y_axis_label)
    
  })

  output$party_table <- DT::renderDataTable({
    
    party_df() %>% 
      select(- party_both) %>% 
      DT::datatable(options = list(dom = "t",
                                   # autoWidth = TRUE,
                                   scrollX = TRUE))
                                   # columnDefs = list(list(width = "100px", targets = "_all"))))
  
  })
  
}