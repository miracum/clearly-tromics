shiny::shinyServer(function(input, output, session) {
    # define reactive values here
    rv <- shiny::reactiveValues(
        csvdir = paste0(dir, "/csv/"),
        plotdir = paste0(dir, "/plots/"),
        datadir = paste0(dir, "/datadir/")
    )

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

    observe({
        req(rv$load_flag)
        shinyjs::disable("moduleDataimport-geo_load")
    })

    shiny::observe({
        shiny::req(rv$import_finished)

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
