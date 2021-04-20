library(shiny)
library(shinydashboard)
library(shinyWidgets)

source(here::here("global.R"))

dashboardPage(
    
    dashboardHeader(title = tags$a(href = "http://www.parlgov.org/",
                                  tags$img(src = "http://www.parlgov.org/static/images/parlgov-logo.svg"))),
    dashboardSidebar(
        
        sidebarMenu(

            menuItem("Parties", tabName = "party_section", icon = icon("users")),
            menuItem("Elections", tabName = "elec_section", icon = icon("person-booth")),
            menuItem("Cabinets", tabName = "cab_section", icon = icon("landmark")),
            
            hr(),
            
            # Manual search engine
            selectInput("country_search", "Search Countries", choices = country_list, multiple = TRUE),
            
            # Manual party selection
            selectInput("party_search", "Search Parties", choices = party_list, multiple = TRUE)
            
            
            
        )
    ),
    
    dashboardBody(
    
        tabItems(
            
            # Parties
            tabItem(
                tabName = "party_section",
                h2("Parties"),
                
                # Debug
                verbatimTextOutput("debug_pl")
                
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


