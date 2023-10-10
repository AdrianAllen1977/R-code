# Load libraries.
library(shiny)
library(leaflet)
library(shinyjs)
library(dplyr)
library(DT)

### Import distance metric and location data files.

snp_distance_matrix <- read.csv("snp_distance_matrix.csv", header = TRUE, stringsAsFactors = FALSE)

location_data <- read.csv("location_data.csv", header = TRUE,  stringsAsFactors = FALSE)


## Make sure Lat and Lon data are numeric
location_data$Lat<-as.numeric(location_data$Lat)
location_data$Lon<-as.numeric(location_data$Lon)

#### Set up the user interface (ui) for the shiny app

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tabsetPanel(
    ## Tab for the title page
    tabPanel("i-MAGE",
             titlePanel("i-MAGE: Interactive MycobActerium bovis Genome Epidemiology"),
             mainPanel(
               img(src="logo.png", align="centre", width="95%"), ## Your image must be in a subfolder called 'www' or else it won't display
             )
    ),
    
    ## Tab for instructions page
    tabPanel("User guide",
             titlePanel("Instructions"),
             mainPanel(code("i-MAGE is a genome epidemiological tool for helping trace sources of bovine TB infection."),
                       p("1. First, go to the Map screen, key in or select from the drop down box the name of the M. bovis isolate you wish to compare to the rest of the dataset."),
                       p("2. Then, use the slider to select the maximum number of SNP differences you want to set to find related M. bovis isolates."),
                       p("3. The lower the number of SNPs different, the more related isolates are."),
                       p("4. There is no 'right' cutoff to find related isolates, but using a cutoff of 5 or lower can find isolates more likely to be linked by recent transmission."),
                       p("5. The map will display the locations of linked isolates."),
                       p("6. Then, go to the 'Follow up Investigation' tab where the closest relatives of your isolate will be listed, and the herd in which they have been found."),
                       p("7. The format of the isolate name will tell you the host species (bov=bovine, bad=badger) and the year of isolation"),
                       p("8. If some of the closest isolates are form wildlife sources, there will be no herd ID entry"),
                       strong("CAVEAT! M. bovis does not mutate very frequently, so in outbreak areas there may be little diversity - a close match to another herd does NOT necessarily mean that is where infection came from, indeed many isolates could have a similar level of relatedness or be identical to the one you are interested in. Or the true source may not have been sampled, while others nearby have. BUT, given how the pathogen clusters in the landscape, the geographic location of related isolates can give you a clue as to where infection may have come from."),
                       p(code("In your outbreak investigations, check for animal movement data which link your isolate back to the area where the most closely related isolates are found.")),
                       p(code("Search land parcel data to assess if the animal your isolate was found in may have been in a contiguous herd location next to a herd with a closely related isolate."))
             )),
    
    # Tab for the map
    tabPanel("Map",
             titlePanel("i-MAGE: Interactive MycobActerium bovis Genome Epidemiology"),
             mainPanel(
               selectInput("sample", "Select Sample Name:", choices = colnames(snp_distance_matrix)),
               sliderInput("snp_distance_threshold", "Select SNP Distance Threshold - samples differ by x SNPs or fewer:", min = 0, max = 50, value = 5),
               leafletOutput("map")
             )
    ),
    # Tab for the derived information
    tabPanel("Follow up investigation",
             titlePanel("Check movement data to these locations"),
             mainPanel(
               # Add UI elements for displaying information derived from other_samples
               dataTableOutput("derived_table")
             )
    )
  )
)


### Set the server up to run the scripts that make your app tick.

server <- function(input, output) {
  
  # First filter the SNP distance matrix by the column name for your chosen sample and the chosen SNP distance
  
  filtered_matrix<-reactive({
    sample_ID <- input$sample
    filter_value <- input$snp_distance_threshold
    filtered_data <- snp_distance_matrix[, c("Sample_Name", sample_ID)]
   # filtered_data[filtered_data <= filter_value]
    filtered_data <- filtered_data[filtered_data[, 2] <= filter_value, ]
  })
  
  ## Then extract the rownames from the filter_data to get the closest matching sample names
  
other_samples<-reactive({
filtered_matrix()[,1]
  
})
  

  
  ## Parse locations file by these extracted names
  
  other_sample_locs<-reactive({
    
  location_data[location_data[,1] %in% other_samples(), ]
    
  })
    
## Render the map

# Render the map with filtered locations.
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -6.9, lat = 54.6, zoom = 8) %>%
      addMarkers(
        data = other_sample_locs(),
        lng = ~Lon,
        lat = ~Lat,
        label = ~paste("Label:",ID),  # Customize the label text here
        labelOptions = labelOptions(
          direction = "auto",                      # Automatically choose label direction
          noHide = F,                           # Keep the label open on hover
          style = list("border-color" = "white", "background-color" = "white")  # Style for the label
        ),
        clusterOptions = markerClusterOptions()  # Optional: Add marker clustering
      )
  })

  # Render the derived information on the second page/tab
  output$derived_table <- renderDataTable({
    selected_data <-other_sample_locs()[,c(1,4)]
    datatable(selected_data)
  })
  
  
  
observe({
  cat("Sample names selected:", input$sample, "\n")
  cat("SNP Distance Threshold:", input$snp_distance_threshold, "\n")
  print(filtered_matrix())       # Call reactive expression with parentheses
  print(other_samples())         # Call reactive expression with parentheses
  print(other_sample_locs())     # Call reactive expression with parentheses
})

}


shinyApp(ui = ui, server = server)
