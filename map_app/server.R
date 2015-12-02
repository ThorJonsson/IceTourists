library(zoo)
library(jsonlite)
library(leaflet)
library(shiny)
library(RColorBrewer)

# --- Preparing for shiny --- 

# read in the geoJSON file
geojson <- readLines("iceland_regions.geojson", warn = FALSE) %>%
paste(collapse = "\n") %>%
fromJSON(simplifyVector = FALSE)

# Default styles for all features
geojson$style = list(
		     weight = 0.5,
		     color = "#555555",
		     opacity = 0.5,
		     fillOpacity = 0.8
		     )
function(input, output) {
	# First we define the geojson data as reactive values
	# Reactive values are values that will change depending on the user input
	reacVals <- reactiveValues()
	reacVals$geo <- geojson

	#colorPal <- reactiveValues()
	#PalValues <- reactiveValues()
	# Create the colour palette
	get_palette <- function(data) {
		colorNumeric(palette=input$colors, domain=data$Log)
		}
	
	
#	PalVal <- reactive({
#		tourist_data[(tourist_data$Nationality == input$nat)]$Log
	#})
	Pal <- reactive({
		get_palette(tourist_data[(tourist_data$Nationality == input$nat) & (tourist_data$date == time_range[[input$time]]),])
	})

	get_color <- function(data, time, region_name, pal) {
		colorPal = get_palette(data)

		return(colorPal(data[(data$Region == region_name) & (data$Date == time),]$Log))
	}
	# Add a properties$style list to each feature (each region)
	observe(priority=1, {
		reacVals$geo$features <- lapply(reacVals$geo$features, function(region) {
						region_name = region$properties$Name
						if (region_name == "Vesturland" | region_name == "Vestfirðir") {
							region_name = "Vesturland, Vestfirðir"
						} else if (region_name == "Höfuðborgarsvæðið" | region_name == "Reykjanes") {
							region_name = "Höfuðborgarsvæði"
						}
						PalValues = tourist_data[(tourist_data$Nationality == input$nat),]
						region$properties$style <- list(
										fillColor = get_color(PalValues, time_range[[input$time]], region_name),
										popup = region_name
										)
						return(region)
			      })
		output$my_map <- renderLeaflet({
			m <- leaflet("my_map") %>% setView(lng = -19.000, lat = 65.000, zoom = 6.20) # zoom = 6.20
			m %>% addTiles() %>%
		    addLegend("bottomleft",pal =Pal(), values = tourist_data[(tourist_data$Nationality == input$nat)& (tourist_data$Date == time_range[[input$time]]),]$Number)
		})
		output$timeValue <- renderText({
			format(time_range[[input$time]],"%b, %Y")
		})
		output$natPlot <- renderPlot({
barplot(tourist_data$Number[tourist_data$Date==time_range[[input$time]]&tourist_data$Nationality!="Alls"&tourist_data$Region=="Allt landið"&tourist_data$Nationality!="Útlendingar"],names.arg = tourist_data$Nationality[tourist_data$Date==time_range[[input$time]]&tourist_data$Nationality!="Alls"&tourist_data$Region=="Allt landið"&tourist_data$Nationality!="Útlendingar"],las=2)	
		})
	})
	observeEvent(input$colors, {
		     leafletProxy("my_map") %>% clearGeoJSON() %>% addGeoJSON(reacVals$geo)
	})

	observeEvent(input$time, {
		     leafletProxy("my_map") %>% clearGeoJSON() %>% addGeoJSON(reacVals$geo)
	})

	observeEvent(input$nat, {
		     leafletProxy("my_map") %>% clearGeoJSON() %>% addGeoJSON(reacVals$geo)
	})

}

