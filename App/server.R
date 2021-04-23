library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)

# source(here::here("global.R"))
source(here::here("theme_pg.R"))
# source(here::here("party_color.R"))



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
    
      party_list

    } else {
      
      party_main %>% 
        mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
        filter(country_both %in% input$country_search) %>% 
        mutate(party_both = paste0(party_name_english, 
                                   "/",
                                   party_name,
                                   " | ",
                                   country_name_short,
                                   " (", 
                                   party_name_short, 
                                   ")")) %>% 
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
  
  # Party L-R Section -------------------------------------------------------
  party_df <- reactive({
    
    party_main %>% 
      mutate(party_both = paste0(party_name_english, 
                                 "/",
                                 party_name,
                                 " | ",
                                 country_name_short,
                                 " (", 
                                 party_name_short, 
                                 ")"),
             party_both_short = paste0(party_name_english, 
                                       " (", 
                                       party_name_short, 
                                       ")")) %>% 
      filter(party_both %in% input$party_search)
    
  })
  
  # Party Family Character
  party_family <- reactive({
    
    party_df() %>% 
      pull(family_name) %>% 
      unique() %>% 
      toString() 
    
  })
  
  # UI InfoBox Party Family
  output$party_family <- renderInfoBox({
    
    infoBox(
      "Party Families", 
      value = tags$p(party_family(), style = "font-size: 75%;"), 
      icon = icon("flag"), 
      color = "blue"
    )

  })
  
  party_lr <- reactive({
    
    y_axis_var <- input$y_axis_id %>% unname()
    y_axis_label <- which(party_y_value == input$y_axis_id) %>% names()
    
    ggplot(party_df(), aes_string(x = "left_right", y = y_axis_var)) + 
      geom_point(size = 2.5, alpha = 0.75) + 
      ggrepel::geom_text_repel(aes(label = party_both_short), point.size = 2.5, nudge_y = 0.25) +
      scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) + 
      scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
      geom_vline(xintercept = 5, alpha = 0.5) +
      geom_hline(yintercept = 5, alpha = 0.5) +
      theme_pg() +
      labs(x = "Left-Right", 
           y = y_axis_label)
    
  })
  
  output$party_lr_plot <- renderPlot({
    
    party_lr()
    
  })
  
  output$party_lr_download <- downloadHandler(
    
    filename = "plot_party_lr.png",
    
    content = function(file) {
      
      ggsave(party_lr(), filename = file, device = "png", width = 8, height = 8)
      
    }
    
  )
  

  # Party (Elec) Vote Share Section -----------------------------------------
  elec_df <- reactive({
    
    if (input$elec_type_vs == "all") {
      
      elec_main %>% 
        mutate(party_both = paste0(party_name_english, 
                                           "/",
                                           party_name,
                                           " | ",
                                           country_name_short,
                                           " (", 
                                           party_name_short, 
                                           ")"), 
               party_both_short = paste0(party_name_english, 
                                         " (", 
                                         party_name_short, 
                                         ")")) %>% 
        filter(party_both %in% input$party_search) %>% 
        mutate(election_year = lubridate::year(election_date)) %>% 
        group_by(party_id) %>% 
        mutate(max_elec_date = max(election_date)) %>% 
        ungroup
        # mutate(
        #   party_name_short = paste0(party_name_short,
        #                             " (",
        #                             ifelse(election_type == "ep",
        #                                    "EP",
        #                                    "Parl."),
        #                             ")")
        # )

    } else {
      
      elec_main %>% 
        mutate(party_both = paste0(party_name_english, 
                                   "/",
                                   party_name,
                                   " | ",
                                   country_name_short,
                                   " (", 
                                   party_name_short, 
                                   ")"),
               party_both_short = paste0(party_name_english, 
                                         " (", 
                                         party_name_short, 
                                         ")")) %>% 
        filter(party_both %in% input$party_search,
               election_type %in% input$elec_type_vs) %>% 
        mutate(election_year = lubridate::year(election_date))
      
    }
    
  })
  
  # Max Vote Share Character
  elec_max_vs <- reactive({
    
    elec_df() %>% 
        group_by(party_id) %>% 
        mutate(max_vs = max(vote_share)) %>% 
        distinct(max_vs, party_name_short, .keep_all = TRUE) %>% 
        ungroup %>% 
        mutate(max_vs_party = paste0(max_vs, "%", " | ", party_name_short, " (", election_year, ")")) %>% 
        pull(max_vs_party) %>% 
        toString()
    
  })
  
  # UI InfoBox Vote Share
  output$vs_max <- renderInfoBox({
    
    infoBox(
      "Highest Vote Shares", 
      value = tags$p(elec_max_vs(), style = "font-size: 50%;"), 
      icon = icon("percent"), 
      color = "blue"
    )
    
  })
 
  party_vs <- reactive({
    
    color_vec <- elec_df() %>% 
      left_join(pg_party_color, by = "party_id") %>% 
      distinct(party_id, color) %>% 
      pull(color)
    
    names(color_vec) <- elec_df() %>% 
      left_join(pg_party_color, by = "party_id") %>% 
      distinct(party_id, color, party_name_short) %>% 
      pull(party_name_short)
    
    # Most recent Election date for highlighting in plot
    # max_elec_date_df <- elec_df() %>% 
    #   group_by(party_id) %>% 
    #   summarise(max_elec_date = max(election_date), party_name_short) 
    
    ggplot(elec_df(), aes(x = election_date, y = vote_share, color = party_name_short)) +
      geom_line(size = 1.5, alpha = 0.75) +
      # geom_path() +
      geom_point(size = 2.5) +
      ggrepel::geom_text_repel(aes(label = if_else(election_date == max_elec_date, 
                                                   party_name_short,
                                                   "")),
                               show.legend = TRUE,
                               nudge_x = -1,
                               size = 6) +
      theme_pg() +
      theme(legend.position = "none") + 
      scale_x_date(date_labels = "%Y") + 
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_color_manual(values = color_vec) +
      labs(x = "Election Year", 
           y = "Vote Share")
    
  })
  
  output$party_vs_plot <- renderPlot({
    
    party_vs()
    
  })
  
  output$party_vs_download <- downloadHandler(
    
    filename = "plot_party_vs.png",
    
    content = function(file) {
      
      ggsave(party_vs(), filename = file, device = "png", width = 10, height = 5)
      
    }
    
  )

  
  
   
  

  # Party Table -------------------------------------------------------------
  output$party_table <- DT::renderDataTable({
    
    party_df() %>% 
      select(- party_both) %>% 
      DT::datatable(options = list(dom = "t",
                                   # autoWidth = TRUE,
                                   scrollX = TRUE))
                                   # columnDefs = list(list(width = "100px", targets = "_all"))))
  
  })
  
  
  
  # enable/disable plot downloads
  observe({
    
    if (is.null(input$party_search)) {
      
      disable("party_lr_download")
      disable("party_vs_download")
      
    } else {
      
      enable("party_lr_download")
      enable("party_vs_download")
      
    }
    
  })
  
  
  
}