shiny::shinyServer(function(input, output, session) {
    # define reactive values here
    rv <- shiny::reactiveValues(
        csvdir = paste0(dir, "/csv/"),
        plotdir = paste0(dir, "/plots/"),
        datadir = paste0(dir, "/datadir/"),
        deg = list()
    )

    observe({
        if (is.null(rv$deg$results)) {
            # init results list here
            rv$deg$results <- list()
        }
    })

    # handle reset
    shiny::observeEvent(input$reset, {
        shinyjs::js$reset()
    })

    input_reactive <- reactive({
        input
    })

    ######################
    ## Dataimport Tab
    ######################
    shiny::callModule(
        module_dataimport_server,
        "moduleDataimport",
        rv = rv,
        input_re = input_reactive
    )

    shiny::observe({
        shiny::req(rv$finished_plotting)

        output$menu <- shinydashboard::renderMenu({
            shinydashboard::sidebarMenu(
                shinydashboard::menuItem(
                    "Visualizations",
                    tabName = "tab_viz"
                ),
                shinydashboard::menuItem(
                    "DEG analysis",
                    tabName = "tab_deg"
                )
            )
        })
        shinydashboard::updateTabItems(
            session = session,
            inputId = "tabs",
            selected = "tab_viz"
        )
    })


    shiny::callModule(
        module_visualization_server,
        "moduleVisualization",
        rv = rv,
        input_re = input_reactive
    )

    shiny::callModule(
        module_deg_server,
        "moduleDEG",
        rv = rv,
        input_re = input_reactive
    )
})
