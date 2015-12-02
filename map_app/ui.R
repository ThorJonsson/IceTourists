library(zoo)
library(jsonlite)
library(leaflet)
library(shiny)
library(RColorBrewer)

# --- Read in the data and prepare the dataframe ---

# read in the data
tourist_data = read.csv("px.hotelnights.csv", header=TRUE, sep=",", na.strings="..")

# filter out the unnecessary rows where Mánuður = "Allir"
tourist_data <- tourist_data[tourist_data$Mánuður != "Allir",]


# change the month names to english
month_names_isl <- c("janúar", "febrúar", "mars", "apríl", "maí", "júní", "júlí",
		    "ágúst", "september", "október", "nóvember", "desember")
month_names_en <- c("January", "February", "March", "April", "May", "June", "July",
		    "August", "September", "October", "November", "December")

translate <- function(from_lang, to_lang){
	function(from_name){
		return (to_lang[match(tolower(from_name), from_lang)])
	}
}
# returns a function to translate between two languages
month_names_to_en <- translate(month_names_isl, month_names_en)


# append combined year/month column to the right
tourist_data <- cbind(Dagsetning = as.yearmon(
					      paste(
						    tourist_data$Ár, 
						    month_names_to_en(tourist_data$Mánuður), 
						    sep = ","),
					      format = "%Y,%B"),
		      tourist_data)

# remove the unnecessary "Ár" and "Mánuður" columns
tourist_data[c("Ár", "Mánuður")] <- list(NULL)

# rename the headers to english
names(tourist_data)[names(tourist_data) == 'Dagsetning'] <- 'Date'
names(tourist_data)[names(tourist_data) == 'Þjóðerni'] <- 'Nationality'
names(tourist_data)[names(tourist_data) == 'Landsvæði'] <- 'Region'
names(tourist_data)[names(tourist_data) == 'Value0'] <- 'Number'

# order by date
tourist_data <- tourist_data[order(tourist_data$Date),]


# --- Processing ---

# add logarithm of number of tourists
tourist_data$Log <- log(tourist_data$Number)

# List of nationalities
nationalities <- unique(tourist_data$Nationality)

# List of time values
time_range <- unique(tourist_data$Date)
time_min <- tourist_data$Date[[1]]
time_max <- tail(tourist_data$Date, 1)



# Choices for drop-downs
# Shiny processing, we specify the UI and the server.
bootstrapPage(

#		 tabPanel("Interactive map",
#			  div(class="outer",

#			      tags$head(
					# Include our custom CSS
#					includeCSS("styles.css"),
#					includeScript("gomap.js")
#					),
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
			      leafletOutput("my_map", width="100%", height="100%"),

			      # Shiny versions prior to 0.11 should use class="modal" instead.
			      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
					    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
					    width = 410, height = "auto",

					    h2("Distribution of Tourists"),

					   sliderInput(inputId="time", label="Time", min = 1, max = length(time_range), value=1, step=1),
					   textOutput("timeValue"),
					   selectInput(inputId="nat", label="Nationality",choices= as.character(nationalities)),
					   selectInput("colors", "Color Scheme",rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
              				   plotOutput("natPlot", height = 400, width = 400))
              	)
