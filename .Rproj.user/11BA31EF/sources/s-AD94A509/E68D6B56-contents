#
# Ships Data that shows distance traveled at different times
#

# libraries
library(shiny)
library(shinythemes)
library(readr)
library(tidyverse)
library(leaflet)
library(geodist)

# reading in csv file
data <- read_csv("data/ships_04112020.zip")
# calculate distance in meters between observations
#ship_data <- data %>%
#    group_by(SHIP_ID) %>%
#    arrange(DATETIME, .by_group = T) %>%
#    mutate(obs_dist = geodist(cbind(LON,LAT), sequential = T, pad = T, measure = "geodesic"))
#ship_data <- read_csv("data/ship_data_distance.zip")

# Define UI for application
ui <- fluidPage(
    theme = shinytheme("cerulean"),

    # Application title
    titlePanel("Marine Data Analysis"),

    # Row with two sections
    fluidRow(
        column(5,
               tabsetPanel(
                   tabPanel(h5(strong("Summary")),
                            br(),
                            selectInput("select_vessel", label = "Select Vessel Type:", choices = unique(data$ship_type), width = "80%"),
                            br(),
                            selectInput("select_ship", label = "Select Ship:", choices = "", width = "80%"),
                            br(),
                            div(
                                textOutput("dist_sailed"), br(),
                                textOutput("destination"), br(),
                                textOutput("speed")
                            )
                            ),
                   tabPanel(h5(strong("Graphs")),
                            br(),
                            radioButtons("stats", label = "Select Option to Display Summary Statistics:", choices = c("Speed", "Distance", "Status","Destination"))

                   )

               )
               ),
        column(7,
               tabsetPanel(
                   tabPanel(h5(strong("Map")),
                            leafletOutput("marine_map", height = "590px")),
                   tabPanel(h5(strong("Data")),
                            h4("Data for Selected Ship"),
                            div(DT::dataTableOutput("data_table"), style = "font-size:80%")
                            )
               )
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # dropdown change
    observeEvent(input$select_vessel, {
        vessel_names = data$SHIPNAME[data$ship_type %in% input$select_vessel]
        updateSelectInput(session, "select_ship", choices = unique(vessel_names))
    })

    # map output
    output$marine_map <- renderLeaflet({
        leaflet(data) %>%
            addTiles(group = "OSM", options = tileOptions(minZoom = 1, maxZoom = 16)) %>%
            fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
    })
    # data
    observeEvent(input$select_ship,{
        output$data_table = DT::renderDataTable({
            ship_data()[ship_data()$SHIPNAME %in% input$select_ship,]
        })
    })
    # functions
    ship_data <- reactive({
        # calculate distance in meters between observations
        data %>%
            filter(SHIPNAME %in% input$select_ship) %>%
            arrange(DATETIME, .by_group = T) %>%
            mutate(obs_dist = geodist(cbind(LON,LAT), sequential = T, pad = T, measure = "geodesic"))
    })
    ship_filter <- reactive({
        sel_ship <- ship_data() %>%
            filter(SHIPNAME %in% input$select_ship) %>%
            arrange(DATETIME)

        end_position <- sel_ship %>%
            filter(obs_dist == max(obs_dist, na.rm = T)) %>%
            arrange(desc(DATETIME)) %>%
            filter(row_number() == 1) %>%
            mutate(popup = "Terminal")

        start_position <- sel_ship[which(sel_ship$DATETIME == end_position$DATETIME) - 1,]
        start_position$popup <- "Initial"
        return(rbind(start_position,end_position))
    })
    map_render <- reactive({
        leafletProxy("marine_map", data = ship_filter()) %>%
            clearMarkers() %>%
            addMarkers(lng = ~LON, lat = ~LAT, label = ~as.character(DATETIME)) %>%
            fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
    })
    info_render <- reactive({
        print(as.character(ship_filter()$DATETIME))

        info_txt <- data.frame(
            dist_sailed = paste0("The most recent longest distance sailed by ",input$select_ship," in two minutes is ",formatC(round(ship_filter()[2,"obs_dist"],2), format="d", big.mark=","), " metres.
                                 This happened between ", as.character(ship_filter()$DATETIME[1]), " and ", as.character(ship_filter()$DATETIME[2]), " UTC."),
            destination = paste0("The ship's destination is  ",ship_filter()[2,"DESTINATION"], "."),
            speed = paste0("The ship's speed was ",ship_filter()[2,"SPEED"], " knots.")
        )
        return(info_txt)
    })
    # filter the ships by selected and get most recent longest distance
    observeEvent(input$select_ship,{
        req(input$select_ship)
        #filter by ship
        ship_filter()
        #re-render map
        map_render()
        #change information
        output$dist_sailed <- renderText({info_render()$dist_sailed})
        output$destination <- renderText({info_render()$destination})
        output$speed <- renderText({info_render()$speed})
    })

}

# Run the application
shinyApp(ui = ui, server = server)
