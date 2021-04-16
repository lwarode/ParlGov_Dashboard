library(shiny)
library(shinydashboard)
library(shinyWidgets)

source(here::here("global.R"))

dashboardPage(
    
    dashboardHeader(title = tags$a(href = "http://www.parlgov.org/",
                                  tags$img(src = "http://www.parlgov.org/static/images/parlgov-logo.svg"))),
    dashboardSidebar(),
    dashboardBody(
        
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
                                background-color: #2076B6;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #FFFFFF;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #F5F5F5;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #474749;
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


