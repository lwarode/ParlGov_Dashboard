library(shiny)
library(shinydashboard)
# library(bs4Dash)
library(shinyjs)
library(tidyverse)
library(ggparliament)
library(patchwork)

# source(here::here("global.R"))
source("theme_pg.R")
# source(here::here("party_color.R"))


# server ------------------------------------------------------------------
function(input, output, session) {
  
  # UI update ---------------------------------------------------------------
  # Party
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
  
  # Election
  election_list_reactive <- reactive({
    
    if (input$country_search == "All") {
      
      election_list
      
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
  
  # Cabinet
  cabinet_list_reactive <- reactive({
    
    if (input$country_search == "All") {
      
      cabinet_list
      
    } else {
      
      cabinet_main %>% 
        mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
        filter(country_both %in% input$country_search) %>% 
        mutate(cabinet_name_year = paste0(cabinet_name,
                                          " (",
                                          lubridate::year(start_date),
                                          ")")) %>% 
        arrange(desc(election_date)) %>%
        distinct(cabinet_name_year) %>% 
        pull(cabinet_name_year)
      
    }
    
  })
  
  observeEvent(input$country_search, {
    
    # Party Search
    updateSelectInput(session, 
                      "party_search", 
                      choices = party_list_reactive(),
                      selected = input$party_search)
    
    # Election Search
    updateSelectInput(session,
                      "election_search",
                      choices = election_list_reactive(),
                      selected = input$election_search)
    
    # Cabinet Search
    updateSelectInput(session,
                      "cabinet_search",
                      choices = cabinet_list_reactive(),
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
    
    y_axis_var <- input$y_axis_id_party %>% unname()
    y_axis_label <- which(party_y_value == input$y_axis_id_party) %>% names()
    
    ggplot(party_df(), aes(x = left_right, y = !! sym(y_axis_var))) + 
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
    
    validate(
      need(input$party_search, "\n\n\n\nSelect at least 1 Party")
    )
    
    party_lr()
    
  })

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
        filter(vote_share == max(vote_share, na.rm = TRUE)) %>% 
        ungroup %>% 
        mutate(max_vs_party = paste0(vote_share, "%", " | ", party_name_short, " (", election_year, ")")) %>% 
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
    
    validate(
      need(input$party_search, "\n\n\n\nSelect at least 1 Party")
    )
    
    party_vs()
    
  })
  


  
  
   
  

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
  # Plot L-R
  party_lr_pg <- reactive({
    
    party_lr() +
      labs(caption = "Data obtained from: parlgov.org (Döring and Manow). 
           \nParlGov Dashboard is a project from Lukas Warode.")
    
  })
  
  output$party_lr_download <- downloadHandler(
    
    filename = "plot_party_lr.png",
    
    content = function(file) {
      
      ggsave(party_lr_pg(), filename = file, device = "png", width = 8, height = 8)
      
    }
    
  )
  
  # Plot Vote Share
  party_vs_pg <- reactive({
    
    party_vs() +
      labs(caption = "Data obtained from: parlgov.org (Döring and Manow). 
           \nParlGov Dashboard is a project from Lukas Warode.")
    
  })
  
  output$party_vs_download <- downloadHandler(
    
    filename = "plot_party_vs.png",
    
    content = function(file) {
      
      ggsave(party_vs_pg(), filename = file, device = "png", width = 10, height = 5)
      
    }
    
  )
  
  # All Party Data
  output$party_df_all_download <- downloadHandler(
    
    filename = "party_all_data.xlsx",
    
    content = function(file) {
      
     xlsx::write.xlsx(party_main, file = file)
      
    }
    
  )
  
  # Data Frame of selected Country
  party_country_df <- reactive({
    
    if (! input$country_search == "All") {
    
      party_main %>% 
        mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
        filter(country_both %in% input$country_search) %>% 
        select(- country_both)
      
    }
    
  })
  
  # Character of selected Countries
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
      
      paste0(party_country_chr(), "_party_data", ".xlsx")
      
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
      mutate(vote_share_label = paste0(vote_share %>% round(1), "%")) %>% 
      ggplot(
           aes(x = fct_reorder(party_name_short, - vote_share), 
               y = vote_share)) +
      geom_col(aes(fill = party_name_short)) +
      geom_text(aes(label = vote_share_label), nudge_y = 1, size = 3) +
      theme_pg() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_fill_manual(values = election_color()) +
      labs(x = "Party",
           y = "Vote Share",
           title = plot_title)

  })
  
  output$election_votes_plot <- renderPlot({
    
    validate(
      need(input$election_search, "\n\n\n\nSelect at least 1 Election")
    )
    
    election_votes()
    
  })


  # Election Seats section --------------------------------------------------
  election_seats <- reactive({
    
    if (input$election_search != "") {
    
      # Number of Rows for Seat Plot
      seats_election <- election_df() %>% 
        distinct(seats_total) %>% 
        pull()
      
      parl_rows_df <- tibble(
        seats = seq(0, 700, 100),
        parl_rows = seq(5, 12, 1)
      )
      
      parl_rows_nr <- parl_rows_df %>% 
        filter(seats < seats_election) %>% 
        filter(parl_rows == max(parl_rows)) %>% 
        pull(parl_rows) 
    
      # Arranged (L-R) df
      election_lr_arranged <- election_df() %>% 
        filter(seats > 0) %>% 
        arrange(left_right) %>% 
        mutate(cum_seats = cumsum(seats),
               seat_share = seats / seats_total * 100,
               seat_share_label = seat_share %>% round(1) %>% as.character() %>% paste0("%"),
               cum_seats_position = case_when(
                 cum_seats == min(cum_seats) ~ cum_seats / 2,
                 TRUE ~ lag(cum_seats) + (seats / 2)
               ),
               full_label = paste0(seats, " Seats", " (", seat_share_label, ")"))
      
      election_lr_arranged_no_na <- election_df() %>% 
        mutate(seat_share = seats / seats_total * 100,
               seat_share_label = seat_share %>% round(1) %>% as.character() %>% paste0("%")) %>% 
        filter(seats > 0,
               seat_share > 2.5,
               ! is.na(left_right)) %>% 
        arrange(left_right) %>% 
        mutate(cum_seats = cumsum(seats),
               # seat_share = seats / seats_total * 100,
               # seat_share_label = seat_share %>% round(1) %>% as.character() %>% paste0("%"),
               cum_seats_position = case_when(
                 cum_seats == min(cum_seats) ~ cum_seats / 2,
                 TRUE ~ lag(cum_seats) + (seats / 2)
               ),
               full_label = paste0(seats, " Seats", " (", seat_share_label, ")"))

      # Color for all Seats
      color_seats <- election_color() %>% 
        data.frame() %>% 
        rownames_to_column("colour") %>%
        rename(party_name_short = 1, colour = 2) 
      
      election_parliament <- parliament_data(
        election_data = election_lr_arranged,  
        parl_rows = parl_rows_nr,
        type = 'semicircle',
        party_seats = election_lr_arranged$seats
      ) %>% 
        left_join(color_seats, by = "party_name_short")
      
      seats_parl <- ggplot(election_parliament, 
                           aes(x, y, color = party_name_short)) +
        geom_parliament_seats() +
        theme_ggparliament(legend = TRUE) +
        theme(legend.position = "top") +
        guides(color = guide_legend(nrow = 2)) +
        scale_color_manual("Party", values = election_color())
      
      seats_share <- ggplot(election_lr_arranged_no_na,
                            aes(x = seats, 
                                y = "", 
                                fill = fct_reorder(party_name_short, - left_right))) +
        geom_col(color = "black", alpha = 0.75) +
        geom_text(aes(x = cum_seats_position, label = full_label), 
                  size = 3, 
                  angle = 90) +
        geom_text(aes(x = cum_seats_position, label = party_name_short), 
                  nudge_y = -0.5, 
                  size = 3) +
        scale_fill_manual("Party", values = election_color()) +
        theme_void() +
        theme(legend.position = "none") 
      
      # Final plot
      final_plot <- seats_parl / seats_share + plot_layout(heights = c(1.5, 1))
      
      return(final_plot)
    
    }

  })
  
  output$election_seats_plot <- renderPlot({
    
    validate(
      need(input$election_search, "\n\n\n\nSelect at least 1 Election")
    )
    
    election_seats()
    
  })
  
  # Election Table ----------------------------------------------------------
  output$election_table <- DT::renderDataTable({
    
    election_df() %>% 
      select(- election_date_both) %>% 
      DT::datatable(options = list(dom = "t",
                                   # autoWidth = TRUE,
                                   scrollX = TRUE))
    # columnDefs = list(list(width = "100px", targets = "_all"))))
    
  })
  

  # Election Downloads ------------------------------------------------------
  # Election Votes
  election_votes_pg <- reactive({
    
    election_votes() +
      labs(caption = "Data obtained from: parlgov.org (Döring and Manow). 
           \nParlGov Dashboard is a project from Lukas Warode.")
    
  })
  
  output$election_votes_download <- downloadHandler(
    
    filename = "plot_election_votes.png",
    
    content = function(file) {
      
      ggsave(election_votes_pg(), filename = file, device = "png", width = 8, height = 5)
      
    }
    
  )
  
  # Election Seats
  election_seats_pg <- reactive({
    
    election_seats() +
      labs(caption = "Data obtained from: parlgov.org (Döring and Manow). 
           \nParlGov Dashboard is a project from Lukas Warode.")
    
  })
  
  output$election_seats_download <- downloadHandler(
    
    filename = "plot_election_seats.png",
    
    content = function(file) {
      
      ggsave(election_seats_pg(), filename = file, device = "png", width = 10, height = 6)
      
    }
    
  )
  
  # All Election Data
  output$election_df_all_download <- downloadHandler(
    
    filename = "election_all_data.xlsx",
    
    content = function(file) {
      
      xlsx::write.xlsx(election_main, file = file)
      
    }
    
  )
  
  # Data Frame of selected Country
  election_country_df <- reactive({
    
    if (! input$country_search == "All") {
      
      election_main %>% 
        mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
        filter(country_both %in% input$country_search) %>% 
        select(- country_both)
      
    }
    
  })
  
  # Character of selected Country
  election_country_chr <- reactive({
    
    election_country_df() %>%
      pull(country_name_short) %>% 
      unique() %>% 
      toString() %>% 
      stringr::str_replace_all(", ", "_") 
    
  })
  
  # Country Election Data
  output$election_df_country_download <- downloadHandler(
    
    filename = function() {
      
      paste0(election_country_chr(), "_election_data", ".xlsx")
      
    }, 
    
    content = function(file) {
      
      xlsx::write.xlsx(election_country_df(), file = file)
      
    }
    
  )
  
  # Character of selected Election
  election_election_chr <- reactive({
    
    election_df() %>% 
      mutate(country_election = paste(country_name_short, 
                                      election_date %>% as.character(),
                                      election_type,
                                      sep = "_")) %>% 
      # distinct(country_election) %>% 
      pull(country_election) %>% 
      unique() %>% 
      toString() 
    
  })
  
  # (Selected) Election Data
  output$election_df_election_download <- downloadHandler(
    
    filename = function() {
      
      paste0(election_election_chr(), ".xlsx")
      
    }, 
    
    content = function(file) {
      
      xlsx::write.xlsx(election_df(), file = file)
      
    }
    
  )
  
  
  
  
  # enable/disable downloads
  ## election_search
  observe({
    
    if (input$election_search == "") {
      
      # Plots
      disable("election_votes_download")
      disable("election_seats_download")
      
      # Datasets
      disable("election_df_election_download")
      
    } else {
      
      # Plots
      enable("election_votes_download")
      enable("election_seats_download")
      
      # Datasets
      enable("election_df_election_download")
      
    }
    
  })
  
  ## country_search
  observe({
    
    if (is.null(input$country_search) || input$country_search == "All") {
      
      # Datasets
      disable("election_df_country_download")
      
    } else {
      
      # Datasets
      enable("election_df_country_download")
      
    } 
    
  })
  

  # Cabinet df --------------------------------------------------------------
  cabinet_df <- reactive({
    
    cabinet_main %>% 
      mutate(cabinet_name_year = paste0(cabinet_name,
                                        " (",
                                        lubridate::year(start_date),
                                        ")")) %>% 
      filter(cabinet_name_year %in% input$cabinet_search,
             cabinet_party == 1) %>% 
      group_by(cabinet_name_year) %>% 
      mutate(cabinet_seats_total = sum(seats)) %>% 
      ungroup
    
  }) 

  cabinet_color <- reactive({
    
    color_vec <- cabinet_df() %>% 
      left_join(pg_party_color, by = "party_id") %>% 
      distinct(party_id, color) %>% 
      pull(color)
    
    names(color_vec) <- cabinet_df() %>% 
      left_join(pg_party_color, by = "party_id") %>% 
      distinct(party_id, color, party_name_short) %>% 
      pull(party_name_short)
    
    return(color_vec)
    
  })

  # Cabinet UI Boxes --------------------------------------------------------
  # Cabinet Prime Minister
  cabinet_pm <- reactive({
    
    cabinet_df() %>% 
      filter(prime_minister == 1) %>% 
      mutate(cabinet_pm_label = paste0(party_name_short, " | ", cabinet_name_year)) %>%  
      pull(cabinet_pm_label) %>% 
      toString()
    
  })
  
  # UI InfoBox Prime Minister
  output$cabinet_pm <- renderInfoBox({

    infoBox(
      "Party of Prime Minister",
      value = tags$p(cabinet_pm(), style = "font-size: 75%;"),
      icon = icon("user-tie"),
      color = "blue"
    )

  })
   
  # Cabinet Status in Legislature
  cabinet_seats_status <- reactive({

    cabinet_df() %>%
      group_by(cabinet_name_year) %>% 
      summarise(election_seats = first(election_seats_total),
                cabinet_seats = sum(seats, na.rm = TRUE),
                seats_min = min(seats, na.rm = TRUE),
                seats_max = max(seats, na.rm = TRUE)) %>% 
      mutate(
        cabinet_type = case_when(
          cabinet_seats <= election_seats / 2 ~ "Minority",
          cabinet_seats - seats_min > election_seats / 2 ~ "Surplus",
          TRUE ~ "Minimum Winning"),
        cabinet_type_label = paste0(cabinet_type, " | ", cabinet_name_year)
      ) %>% 
      pull(cabinet_type_label) %>% 
      toString()

  })
  
  # UI InfoBox Cabinet Status
  output$cabinet_seats_status <- renderInfoBox({
    
    infoBox(
      "Cabinet Status", 
      value = tags$p(cabinet_seats_status(), style = "font-size: 75%;"), 
      icon = icon("pie-chart"), 
      color = "blue"
    )
    
  })
  
  # Cabinet Term Dates
  cabinet_term <- reactive({
    
    cabinet_main %>% 
      distinct(cabinet_id, .keep_all = TRUE) %>% 
      group_by(country_name) %>% 
      arrange(start_date) %>%  
      mutate(end_date = if_else(! is.na(lead(start_date)),
                                lead(start_date),
                                Sys.Date())) %>% 
      ungroup %>% 
      mutate(cabinet_name_year = paste0(cabinet_name,
                                        " (",
                                        lubridate::year(start_date),
                                        ")")) %>% 
      filter(cabinet_name_year %in% input$cabinet_search) %>% 
      mutate(cab_term = difftime(end_date, start_date),
             cab_term_label = paste0(start_date, " - ", end_date,
                                     " (", cab_term, " days)",
                                     " | ", cabinet_name_year)) %>% 
      pull(cab_term_label) %>% 
      toString()
      
    
  })
  
  # UI InfoBox Cabinet Term 
  output$cabinet_term <- renderInfoBox({
    
    infoBox(
      "Cabinet Term", 
      value = tags$p(cabinet_term(), style = "font-size: 50%;"), 
      icon = icon("calendar"), 
      color = "blue"
    )
    
  })
  

  # Cabinet L-R -------------------------------------------------------------
  cabinet_lr <- reactive({

    # Ideological Y Axis Values
    y_axis_var <- input$y_axis_id_cabinet %>% unname() %>% sym()
    y_axis_label <- which(party_y_value == input$y_axis_id_cabinet) %>% names()

    # Compute L-R Ranges of Cabinets
    cabinet_lr_data <- cabinet_df() %>%
      left_join(party_main %>% select(state_market:eu_anti_pro, party_id), by = "party_id") %>%
      filter(! is.na(left_right), ! is.na(!! y_axis_var)) %>% 
      mutate(lr_share = seats/cabinet_seats_total * left_right,
             y_share = seats/cabinet_seats_total * !! y_axis_var) %>%
      group_by(cabinet_id) %>%
      summarise(max_lr = max(left_right, na.rm = TRUE),
                min_lr = min(left_right, na.rm = TRUE),
                mean_lr = sum(lr_share, na.rm = TRUE),
                max_y = max(!! y_axis_var, na.rm = TRUE),
                min_y = min(!! y_axis_var, na.rm = TRUE),
                mean_y = sum(y_share, na.rm = TRUE),
                cabinet_name_year) %>%
      distinct(cabinet_name_year, .keep_all = T)

    # Final Plot with Range Lines / Intervals
    if (input$plot_cabinet_lr_choice == FALSE) {
      
      final_plot <- ggplot(cabinet_lr_data) +
        geom_point(aes(x = mean_lr, y = mean_y)) +
        ggrepel::geom_text_repel(aes(x = mean_lr, y = mean_y, label = cabinet_name_year)) +
        scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) + 
        scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
        geom_vline(xintercept = 5, alpha = 0.5) +
        geom_hline(yintercept = 5, alpha = 0.5) +
        theme_pg() +
        labs(x = "Left-Right",
             y = y_axis_label,
             title = "Ideological Position of Cabinets")
   
      
    } else {
      
      final_plot <- ggplot(cabinet_lr_data) +
        geom_pointrange(aes(xmin = min_lr, xmax = max_lr, y = mean_y, x = mean_lr), alpha = 0.3) +
        geom_errorbar(aes(xmin = min_lr, xmax = max_lr, y = mean_y, x = mean_lr), alpha = 0.3, width = 0.6) +
        geom_pointrange(aes(ymin = min_y, ymax = max_y, x = mean_lr, y = mean_y), alpha = 0.3) + 
        geom_errorbar(aes(ymin = min_y, ymax = max_y, x = mean_lr, y = mean_y), alpha = 0.3, width = 0.6) + 
        ggrepel::geom_text_repel(aes(x = mean_lr, y = mean_y, label = cabinet_name_year)) +
        scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) + 
        scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
        geom_vline(xintercept = 5, alpha = 0.5) +
        geom_hline(yintercept = 5, alpha = 0.5) +
        theme_pg() +
        labs(x = "Left-Right",
             y = y_axis_label,
             title = "Ideological Range of Cabinets")

    }
    
    return(final_plot)

  })

  output$cabinet_lr_plot <- renderPlot({

    validate(
      need(input$cabinet_search, "\n\n\n\nSelect at least 1 Cabinet")
    )
    
    cabinet_lr()

  })
  

  # Cabinet Seats -----------------------------------------------------------
  cabinet_seats <- reactive({
    
    cabinet_lr_arranged <- cabinet_df() %>% 
      group_by(cabinet_id) %>% 
      mutate(seat_share = seats / election_seats_total,
             seat_share_label = (seat_share * 100) %>% round(1) %>% as.character() %>% paste0("%")) %>% 
      filter(seats > 0) %>% 
      arrange(left_right) %>% 
      mutate(cum_seats = cumsum(seat_share),
             # seat_share = seats / seats_total * 100,
             # seat_share_label = seat_share %>% round(1) %>% as.character() %>% paste0("%"),
             cum_seats_position = case_when(
               cum_seats == min(cum_seats, na.rm = TRUE) ~ cum_seats / 2,
               TRUE ~ lag(cum_seats) + (seat_share / 2)
             ),
             full_label = paste0(seats, " Seats", " (", seat_share_label, ")"))
    
    cabinet_seats_share <- ggplot(cabinet_lr_arranged,
                                  aes(x = seat_share, 
                                      y = cabinet_name, 
                                      fill = fct_reorder(party_name_short, - left_right))) +
      geom_col(color = "black", alpha = 0.75) +
      geom_text(aes(x = cum_seats_position, label = full_label), 
                size = 3, 
                angle = 90) +
      geom_text(aes(x = cum_seats_position, label = party_name_short), 
                nudge_y = -0.5, 
                size = 3) +
      geom_vline(xintercept = 0.5, linetype = 2, alpha = 0.75) +
      # annotate("text", label = "50%", x = 50, y = 0.5) +
      scale_fill_manual("Party", values = cabinet_color()) +
      scale_x_continuous(labels = scales::percent_format(1), breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
      theme_pg() +
      theme(legend.position = "none",
            axis.title.y = element_blank()) + 
      labs(x = "Seat Share")
    
    return(cabinet_seats_share)
    
  })
  
  
  output$cabinet_seats_plot <- renderPlot({
    
    validate(
      need(input$cabinet_search, "\n\n\n\nSelect at least 1 Cabinet")
    )
    
    cabinet_seats()
    
  })
  
  
  # Cabinet Table -----------------------------------------------------------
  output$cabinet_table <- DT::renderDataTable({
    
    cabinet_main %>% 
      mutate(cabinet_name_year = paste0(cabinet_name,
                                        " (",
                                        lubridate::year(start_date),
                                        ")")) %>% 
      filter(cabinet_name_year %in% input$cabinet_search) %>%
      select(- cabinet_name_year) %>% 
      DT::datatable(options = list(dom = "t",
                                   # autoWidth = TRUE,
                                   scrollX = TRUE))
    # columnDefs = list(list(width = "100px", targets = "_all"))))
    
  })
  
  
  # Cabinet Downloads -------------------------------------------------------
  # Cabinet L-R
  cabinet_lr_pg <- reactive({
    
    cabinet_lr() +
      labs(caption = "Data obtained from: parlgov.org (Döring and Manow). 
           \nParlGov Dashboard is a project from Lukas Warode.")
    
  })
  
  output$cabinet_lr_download <- downloadHandler(
    
    filename = "plot_cabinet_lr.png",
    
    content = function(file) {
      
      ggsave(cabinet_lr_pg(), filename = file, device = "png", width = 8, height = 8)
      
    }
    
  )
  
  # Cabinet Seats
  cabinet_seats_pg <- reactive({
    
    cabinet_seats() +
      labs(caption = "Data obtained from: parlgov.org (Döring and Manow). 
           \nParlGov Dashboard is a project from Lukas Warode.")
    
  })
  
  output$cabinet_seats_download <- downloadHandler(
    
    filename = "plot_cabinet_seats.png",
    
    content = function(file) {
      
      ggsave(cabinet_seats_pg(), filename = file, device = "png", width = 10, height = 6)
      
    }
    
  )
  
  # All Cabinet Data
  output$cabinet_df_all_download <- downloadHandler(
    
    filename = "cabinet_all_data.xlsx",
    
    content = function(file) {
      
      xlsx::write.xlsx(cabinet_main, file = file)
      
    }
    
  )
  
  # Data Frame of selected Country
  cabinet_country_df <- reactive({
    
    if (! input$country_search == "All") {
      
      cabinet_main %>% 
        mutate(country_both = paste0(country_name, " (", country_name_short, ")")) %>% 
        filter(country_both %in% input$country_search) %>% 
        select(- country_both)
      
    }
    
  })
  
  # Character of selected Country
  cabinet_country_chr <- reactive({
    
    cabinet_country_df() %>%
      pull(country_name_short) %>% 
      unique() %>% 
      toString() %>% 
      stringr::str_replace_all(", ", "_") 
    
  })

  # Country Cabinet Data
  output$cabinet_df_country_download <- downloadHandler(
    
    filename = function() {
      
      paste0(cabinet_country_chr(),  "_cabinet_data", ".xlsx")
      
    }, 
    
    content = function(file) {
      
      xlsx::write.xlsx(cabinet_country_df(), file = file)
      
    }
    
  )
  
  # Character of selected Cabinets
  cabinet_cabinet_chr <- reactive({
    
    cabinet_df() %>% 
      distinct(cabinet_name_year) %>% 
      pull(cabinet_name_year) %>% 
      toString() %>% 
      stringr::str_replace_all(", ", "_") 
    
  })
  
  # (Selected) Cabinet Data
  output$cabinet_df_cabinet_download <- downloadHandler(
    
    filename = function() {
      
      paste0(cabinet_cabinet_chr(), ".xlsx")
      
    }, 
    
    content = function(file) {
      
      xlsx::write.xlsx(cabinet_df(), file = file)
      
    }
    
  )
  
  # enable/disable downloads
  ## cabinet_search
  observe({

    if (is.null(input$cabinet_search)) {

      # Plots
      disable("cabinet_lr_download")
      disable("cabinet_seats_download")

      # Datasets
      disable("cabinet_df_cabinet_download")

    } else {

      # Plots
      enable("cabinet_lr_download")
      enable("cabinet_seats_download")

      # Datasets
      enable("cabinet_df_cabinet_download")

    }

  })
  
  ## country_search
  observe({

    if (is.null(input$country_search) || input$country_search == "All") {

      # Datasets
      disable("cabinet_df_country_download")

    } else {

      # Datasets
      enable("cabinet_df_country_download")

    }

  })
  
  
}