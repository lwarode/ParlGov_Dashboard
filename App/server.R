library(shiny)
library(shinydashboard) # library(bs4Dash)
library(shinyjs)
library(tidyverse)

# source(here::here("global.R"))
source(here::here("theme_pg.R"))
# source(here::here("party_color.R"))


# server ------------------------------------------------------------------
function(input, output, session) {
  
  # UI update ---------------------------------------------------------------
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
  
  election_list_reactive <- reactive({
    
    if (input$country_search == "All") {
      
      party_list
      
    } else {
      
      election_main %>% 
        mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
        filter(country_both %in% input$country_search) %>% 
        mutate(election_date_both = paste0(election_date,
                                           " | ",
                                           country_name)) %>%
        mutate(election_date_both = if_else(election_type == "ep",
                                            paste0(election_date_both, " (EP)"),
                                            election_date_both)) %>% 
        arrange(desc(election_date)) %>% 
        pull(election_date_both)
      
    }
    
  })
  
  observeEvent(input$country_search, {
    
    updateSelectInput(session, 
                      "party_search", 
                      choices = party_list_reactive(),
                      selected = input$party_search)
    
    updateSelectInput(session,
                      "election_search",
                      choices = election_list_reactive(),
                      selected = input$election_search)
    
  })
  

  # Party df ----------------------------------------------------------------
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
  
  # Party L-R Section -------------------------------------------------------
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
  elec_party_df <- reactive({
    
    if (input$elec_type_vs == "all") {
      
      election_main %>% 
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
      
      election_main %>% 
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
        mutate(election_year = lubridate::year(election_date)) %>% 
        group_by(party_id) %>% 
        mutate(max_elec_date = max(election_date)) %>% 
        ungroup
      
    }
    
  })
  
  # Max Vote Share Character
  elec_max_vs <- reactive({
    
    elec_party_df() %>% 
        group_by(party_id) %>% 
        mutate(max_vs = max(vote_share, na.rm = TRUE)) %>% 
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
    
    color_vec <- elec_party_df() %>% 
      left_join(pg_party_color, by = "party_id") %>% 
      distinct(party_id, color) %>% 
      pull(color)
    
    names(color_vec) <- elec_party_df() %>% 
      left_join(pg_party_color, by = "party_id") %>% 
      distinct(party_id, color, party_name_short) %>% 
      pull(party_name_short)
    
    # Most recent Election date for highlighting in plot
    # max_elec_date_df <- elec_party_df() %>% 
    #   group_by(party_id) %>% 
    #   summarise(max_elec_date = max(election_date), party_name_short) 
    
    ggplot(elec_party_df(), aes(x = election_date, y = vote_share, color = party_name_short)) +
      geom_line(size = 1.5, alpha = 0.75) +
      # geom_path() +
      geom_point(size = 2.5) +
      ggrepel::geom_text_repel(aes(label = if_else(election_date == max_elec_date, 
                                                   party_name_short,
                                                   "")),
                               show.legend = TRUE,
                               nudge_x = -1,
                               size = 4) +
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
  

  # Party Downloads ---------------------------------------------------------
  # All Party Data
  output$party_df_all_download <- downloadHandler(
    
    filename = "party_all_data.xlsx",
    
    content = function(file) {
      
     xlsx::write.xlsx(party_main, file = file)
      
    }
    
  )
  
  # Data Frame of select Countries
  party_country_df <- reactive({
    
    if (! input$country_search == "All") {
    
      party_main %>% 
        mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
        filter(country_both %in% input$country_search) %>% 
        select(- country_both)
      
    }
    
  })
  
  # Chracter of selected Countries
  party_country_chr <- reactive({
    
    party_country_df() %>% 
      pull(country_name_short) %>% 
      unique() %>% 
      toString() %>% 
      stringr::str_replace_all(", ", "_") 
    
  })
  
  
  # Country Party Data
  output$party_df_country_download <- downloadHandler(

    filename = function() {
      
      paste0(party_country_chr(), ".xlsx")
      
    }, 

    content = function(file) {

      xlsx::write.xlsx(party_country_df(), file = file)

    }

  )
  
  # Character of selected Parties
  party_party_chr <- reactive({
    
    party_df() %>% 
      pull(party_name_short) %>% 
      toString() %>% 
      stringr::str_replace_all(", ", "_") 
    
  })
  
  # (Selected) Party Data
  output$party_df_party_download <- downloadHandler(
    
    filename = function() {
      
      paste0(party_party_chr(), ".xlsx")
      
    }, 
    
    content = function(file) {
      
      xlsx::write.xlsx(party_df(), file = file)
      
    }
    
  )
  
  # enable/disable downloads
  ## party_search
  observe({
    
    if (is.null(input$party_search)) {
      
      # Plots
      disable("party_lr_download")
      disable("party_vs_download")
      
      # Datasets
      disable("party_df_party_download")
      
    } else {
      
      # Plots
      enable("party_lr_download")
      enable("party_vs_download")
      
      # Datasets
      enable("party_df_party_download")
      
    }
    
  })
  
  ## country_search
  observe({
    
    if (is.null(input$country_search) || input$country_search == "All") {
      
      # Datasets
      disable("party_df_country_download")
      
    } else {
      
      # Datasets
      enable("party_df_country_download")
      
    } 
    
    
  })
  

  # Election df -------------------------------------------------------------
  election_df <- reactive({
    
    election_main %>% 
      mutate(election_date_both = paste0(election_date,
                                         " | ",
                                         country_name)) %>%
      mutate(election_date_both = if_else(election_type == "ep",
                                          paste0(election_date_both, " (EP)"),
                                          election_date_both)) %>% 
      filter(election_date_both %in% input$election_search)
    
  }) 
  
  election_color <- reactive({
    
    color_vec <- election_df() %>% 
      left_join(pg_party_color, by = "party_id") %>% 
      distinct(party_id, color) %>% 
      pull(color)
    
    names(color_vec) <- election_df() %>% 
      left_join(pg_party_color, by = "party_id") %>% 
      distinct(party_id, color, party_name_short) %>% 
      pull(party_name_short)
    
    return(color_vec)
    
  })

  # Election Votes Section --------------------------------------------------
  election_votes <- reactive({

    if (input$election_search != "") {
      
      plot_title <- election_df() %>% 
        mutate(plot_title = paste0("Election Date: ", 
                                   election_date,
                                   " (",
                                   country_name,
                                   ")")) %>% 
        distinct(plot_title) %>% 
        pull() 
      
    } else {
      
      plot_title <- ""
      
    }
        
    election_df() %>% 
      mutate(vote_share_label = paste0(vote_share, "%")) %>% 
      ggplot(
           aes(x = fct_reorder(party_name_short, - vote_share), 
               y = vote_share)) +
      geom_col(aes(fill = party_name_short)) +
      geom_text(aes(label = vote_share_label), nudge_y = 1) +
      theme_pg() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_fill_manual(values = election_color()) +
      labs(x = "Party",
           y = "Vote Share",
           title = plot_title)
      
    

  })

  output$election_votes_plot <- renderPlot({

    election_votes()

  })

  output$election_votes_download <- downloadHandler(

    filename = "plot_election_votes.png",

    content = function(file) {

      ggsave(election_votes(), filename = file, device = "png", width = 8, height = 5)

    }

  )
  
  # Election Table ----------------------------------------------------------
  output$election_table <- DT::renderDataTable({
    
    election_df() %>% 
      select(- election_date_both) %>% 
      DT::datatable(options = list(dom = "t",
                                   # autoWidth = TRUE,
                                   scrollX = TRUE))
    # columnDefs = list(list(width = "100px", targets = "_all"))))
    
  })
  
  # enable/disable downloads
  ## election_search
  observe({
    
    if (input$election_search == "") {
      
      # Plots
      disable("election_votes_download")
      disable("election_seats_download")
      
      # Datasets
      # disable("party_df_party_download")
      
    } else {
      
      # Plots
      enable("election_votes_download")
      enable("election_seats_download")
      
      # Datasets
      # enable("party_df_party_download")
      
    }
    
  })
  
}