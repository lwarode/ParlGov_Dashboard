library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)

source(here::here("global.R"))


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
                                  tags$img(src = "http://www.parlgov.org/static/images/parlgov-logo.svg"))),
    
    dashboardSidebar(
        
        useShinyjs(),
        
        sidebarMenu(id = "sidebarid",

            menuItem("Parties", tabName = "party_section", icon = icon("users")),
            menuItem("Elections", tabName = "elec_section", icon = icon("person-booth")),
            menuItem("Cabinets", tabName = "cab_section", icon = icon("landmark")),
            
            hr(),
            
            # verbatimTextOutput("party_family"),
            
            # Manual search engine
            selectInput("country_search", "Search Countries", choices = country_list, multiple = TRUE),
            
            # Conditional Party Panel
            conditionalPanel(
                
                condition = "input.sidebarid == 'party_section'",
                
                # Manual party selection
                selectInput("party_search", "Search Parties", choices = party_list, multiple = TRUE),
                
                hr(),
                
                h5("Download Section", align = "center"),
                
                div(style = "text-align: center;", 
                    plotDownloadButton("party_lr_download", label = "Party L-R Plot"),
                ),
                
                br(),
                
                div(style = "text-align: center;", 
                    plotDownloadButton("party_vs_download", label = "Party Vote Share Plot"),
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
                                   uiOutput("party_family")
                            ),
                            column(width = 6,
                                   selectInput("y_axis_id", 
                                               "Select Y-Axis Score", 
                                               choices = party_y_value
                                   )
                            )
                        )
                    ),
                    
                    # Right side
                    column(width = 6,
                           fluidRow(
                                column(width = 6,
                                       uiOutput("vs_max")
                                ),
                                column(width = 6,
                                       selectInput("elec_type_vs", 
                                                   "Select Election Type", 
                                                   choices = elec_type
                                       )
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
            
            # Elections
            tabItem(
                tabName = "elec_section",
                h2("Elections")
                
            ),
            
            # Cabinets
            tabItem(
                tabName = "cab_section",
                h2("Cabinets")
                
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


