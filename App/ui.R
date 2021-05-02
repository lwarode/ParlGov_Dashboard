library(shiny)
library(shinydashboard) # library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)

source(here::here("App/global.R"))


# custom download buttons (varying icons) ---------------------------------
plotDownloadButton <- function(outputId, label = "Download"){
    tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
           target = "_blank", download = NA, icon("chart-bar"), label)
}

excelDownloadButton <- function(outputId, label = "Download"){
    tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
           target = "_blank", download = NA, icon("file-excel"), label)
}


dashboardPage(
 
    dashboardHeader(title = tags$a(href = "http://www.parlgov.org/",
                                  tags$img(src = "http://www.parlgov.org/static/images/parlgov-logo.svg",
                                           title = "ParlGov Webpage")),
                    
                    # Link to GitHub Repository
                    tags$li(a(title = "Link to GitHub Repository", 
                              href = "https://github.com/lwarode/ParlGov_Dashboard",
                              icon("github")),
                            class = "dropdown"),
                    
                    # ParlGov Documentation
                    tags$li(a(title = "ParlGov Documentation", 
                              href = "http://www.parlgov.org/#documentation",
                              icon("align-left")),
                            class = "dropdown")),
    
    dashboardSidebar(
        
        useShinyjs(),
        
        sidebarMenu(
            
            id = "sidebarid",

            menuItem("Parties", tabName = "party_section", icon = icon("users")),
            menuItem("Elections", tabName = "election_section", icon = icon("person-booth")),
            menuItem("Cabinets", tabName = "cabinet_section", icon = icon("landmark")),
            
            hr(),
        
            # Manual search engine
            selectInput("country_search", 
                        "Search Countries", 
                        choices = country_list, 
                        multiple = TRUE),
            

            # Conditional Party Panel -------------------------------------------------
            conditionalPanel(

                condition = "input.sidebarid == 'party_section'",
                
                # Manual party selection
                selectInput("party_search", 
                            "Search Parties", 
                            choices = party_list, 
                            multiple = TRUE),
                
                hr(),
                
                # Download Section
                h4("Download Section", align = "center"),
                
                h5("Download Plots", align = "center"),
                
                div(style = "text-align: center;", 
                    plotDownloadButton("party_lr_download", label = "Party L-R Plot")
                ),
                
                br(),
                
                div(style = "text-align: center;", 
                    plotDownloadButton("party_vs_download", label = "Party Vote Share Plot")
                ),
                
                br(),
                
                h5("Download Datasets", align = "center"),
                
                div(style = "text-align: center;",
                    excelDownloadButton("party_df_all_download", label = "All Party Data")
                ),

                br(),

                div(style = "text-align: center;",
                    excelDownloadButton("party_df_country_download", label = "Country Party Data")
                ),

                br(),

                div(style = "text-align: center;",
                    excelDownloadButton("party_df_party_download", label = "Selected Party Data")
                ),

            ),
            

            # Conditional Election Panel ----------------------------------------------
            conditionalPanel(
                
                condition = "input.sidebarid == 'election_section'",
                
                # Manual party selection
                selectInput("election_search", 
                            "Search Elections", 
                            choices = election_list),
                
                hr(),
                
                # Download Section
                h4("Download Section", align = "center"),
                
                h5("Download Plots", align = "center"),
                
                div(style = "text-align: center;", 
                    plotDownloadButton("election_votes_download", label = "Election Votes Plot")
                ),
                
                br(),
                
                div(style = "text-align: center;", 
                    plotDownloadButton("election_seats_download", label = "Election Seats Plot")
                ),
                
                br(),
                
                h5("Download Datasets", align = "center"),
                
                div(style = "text-align: center;",
                    excelDownloadButton("election_df_all_download", label = "All Election Data")
                ),
                
                br(),
                
                div(style = "text-align: center;",
                    excelDownloadButton("election_df_country_download", label = "Country Election Data")
                ),
                
                br(),
                
                div(style = "text-align: center;",
                    excelDownloadButton("election_df_election_download", label = "Selected Election Data")
                )
                
            ),
            

            # Conditional Cabinet Panel -----------------------------------------------
            conditionalPanel(
                
                condition = "input.sidebarid == 'cabinet_section'",
                
                # Manual cabinet selection
                selectInput("cabinet_search", 
                            "Search Cabinet", 
                            choices = cabinet_list,
                            multiple = TRUE
                            # selected = NULL
                            ),
                
                hr(),
                
                # Download Section
                h4("Download Section", align = "center"),
                
                h5("Download Plots", align = "center"),
                
                div(style = "text-align: center;",
                    plotDownloadButton("cabinet_lr_download", label = "Cabinet L-R Plot")
                ),

                br(),

                div(style = "text-align: center;",
                    plotDownloadButton("cabinet_seats_download", label = "Cabinet Seats Plot")
                ),

                br(),
                
                h5("Download Datasets", align = "center"),
                
                div(style = "text-align: center;",
                    excelDownloadButton("cabinet_df_all_download", label = "All Cabinet Data")
                ),
                
                br(),
                
                div(style = "text-align: center;",
                    excelDownloadButton("cabinet_df_country_download", label = "Country Cabinet Data")
                ),
                
                br(),
                
                div(style = "text-align: center;",
                    excelDownloadButton("cabinet_df_cabinet_download", label = "Selected Cabinet Data")
                )
                
            )
            
        )
    ),
    
    dashboardBody(
    
        tabItems(

            # Parties -----------------------------------------------------------------
            tabItem(
                tabName = "party_section",
                
                h2("Parties", align = "center"),
                
                hr(),
                
                # Party UI elements
                fluidRow(
                    
                    # Left side
                    column(width = 6,
                        fluidRow(
                            column(width = 6,
                                   selectInput("y_axis_id_party", 
                                               "Select Y-Axis Score", 
                                               choices = party_y_value
                                   )
                            ),
                            column(width = 6,
                                   uiOutput("party_family")
                            )
                        )
                    ),
                    
                    # Right side
                    column(width = 6,
                           fluidRow(
                               column(width = 6,
                                      selectInput("elec_type_vs", 
                                                  "Select Election Type", 
                                                  choices = elec_type
                                      )
                               ),
                                column(width = 6,
                                       uiOutput("vs_max")
                                )
                            )
                    )
                    
                ),
                
                # Plots
                fluidRow(
                    
                    column(width = 6,
                           plotOutput("party_lr_plot") %>% 
                               withSpinner(color = "#2076B6")
                    ),
                    
                    column(width = 6,
                           plotOutput("party_vs_plot") %>% 
                               withSpinner(color = "#2076B6")
                    )
                    
                ),
                
                hr(),
                
                # tableOutput("party_table")
                h4("Browse full Parties data", align = "center"),
                fluidRow(
                    column(
                           DT::dataTableOutput("party_table"), width = 12
                    )
                )
                
                
                ),
            
            # Elections ---------------------------------------------------------------
            tabItem(
                tabName = "election_section",
                
                h2("Elections", align = "center"),
                
                hr(),
                
                # Plots
                fluidRow(
                    
                    column(width = 6,
                           plotOutput("election_votes_plot") %>% 
                               withSpinner(color = "#2076B6")
                    ),
                    
                    column(width = 6,
                           plotOutput("election_seats_plot") %>% 
                               withSpinner(color = "#2076B6")
                    )
                    
                ),
                
                hr(),
                
                h4("Browse full Elections data", align = "center"),
                fluidRow(
                    column(
                        DT::dataTableOutput("election_table"), width = 12
                    )
                )
                
            ),
            
            # Cabinets ----------------------------------------------------------------
            tabItem(
                tabName = "cabinet_section",
                
                h2("Cabinets", align = "center"),
                
                hr(),
                
                # Cabinet UI Elements
                fluidRow(
                    
                    # Left side
                    column(width = 6,
                           fluidRow(
                               column(width = 6,
                                      selectInput("y_axis_id_cabinet", 
                                                  "Select Y-Axis Score", 
                                                  choices = party_y_value
                                      ),
                                      awesomeCheckbox("plot_cabinet_lr_choice",
                                                      "Show Ideological Range",
                                                      value = TRUE)
                               ),
                               column(width = 6,
                                      uiOutput("cabinet_pm")
                               )
                           )
                    ),
                    
                    # Right side
                    column(width = 6,
                           fluidRow(
                               column(width = 6,
                                      uiOutput("cabinet_seats_status")
                               ),
                               column(width = 6,
                                      uiOutput("cabinet_term")
                               )
                           )
                    )
      
                ),
                
                # Plots
                fluidRow(
                    
                    column(width = 6,
                           plotOutput("cabinet_lr_plot") %>% 
                               withSpinner(color = "#2076B6")
                    ),
                    
                    column(width = 6,
                           plotOutput("cabinet_seats_plot") %>%
                               withSpinner(color = "#2076B6")
                    )
                    
                ),
                  
                hr(),
                
                h4("Browse full Cabinets data", align = "center"),
                fluidRow(
                    column(
                        DT::dataTableOutput("cabinet_table"), width = 12
                    )
                )
                
            )
            
        ),
        
        # Color properties
        tags$head(tags$style(HTML('
        
                                /* button center alignment */
                                .btn { vertical-align: middle; width: 84%;}
                                
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #F5F5F5;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #FFFFFF;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #2076B6;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #474749;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #474749;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #474749;
                                color: #FFFFFF;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #2076B6;
                                }
                                
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #474749;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #F5F5F5;
                                }

                                ')))
    )

)


