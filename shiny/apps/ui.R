library(shiny)
library(shinydashboard)
# header part
header <- dashboardHeader(title = "Bioinformagician Center")

# sidebar
sidebar <- dashboardSidebar(
    # stats components
    sidebarMenu(
        id = 'side_menu_navbar',
        #menuItem(text = "Home",tabName = "home",icon = icon('home')),
        menuItem(
            text="Stats", tabName = "stats_navbar",icon = icon("bar-chart"),
            # >> pairwise comparison
            ui_list$ui_sidebar_pairwise_comparison
        )
    )
)

# body
body <-  dashboardBody(
    tabItems(
        #tabItem(tabName = "home",h2("Home page")),
        ui_list$ui_body_pairwise_comparison
    )
)

shinydashboard::dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body,
    tags$head(tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon.png"))
)