shiny::shinyUI(
  shiny::tagList(
    shinydashboard::dashboardPage(
      skin = "black",

      # Application title
      shinydashboard::dashboardHeader(title = "tRomics"),

      shinydashboard::dashboardSidebar(

        # Include shinyjs in the UI Sidebar
        shinyjs::useShinyjs(),

        #Sidebar Panel
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem(
            "Data import",
            tabName = "dataimport",
            icon = icon("file")
          ),
          shinydashboard::sidebarMenuOutput("menu"),
          shiny::tags$hr(),
          shiny::actionButton("reset", "Reset")
        ),
        shiny::div(
          class = "sidebar-menu",
          style = paste0("position:fixed; bottom:0; ",
                         "left:0; white-space: normal;",
                         "text-align:left;",
                         "padding: 9.5px 9.5px 9.5px 9.5px;",
                         "margin: 6px 10px 6px 10px;",
                         "box-sizing:border-box;",
                         "heigth: auto; width: 230px;"),
          shiny::HTML("\u00A9 Universitätsklinikum Erlangen</i>"))
      ),

      shinydashboard::dashboardBody(

        # Include shinyjs in the UI Body
        shinyjs::useShinyjs(),

        # js reset function
        # https://stackoverflow.com/questions/25062422/restart-shiny-session
        # Add the js code to the page
        shinyjs::extendShinyjs(script = "reset.js",
                               functions = "reset"),

        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "dataimport",
                                  module_dataimport_ui("moduleDataimport")
          ),


          shinydashboard::tabItem(tabName = "batch_hist",
                                  module_visualization_ui("moduleVisualization")
          ),

          shinydashboard::tabItem(tabName = "tab_deg",
                                  module_deg_ui("moduleDEG")
          )
        )
      )
    )))
