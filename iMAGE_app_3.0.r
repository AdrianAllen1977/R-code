# Load libraries.
library(shiny)
library(leaflet)
library(shinyjs)
library(dplyr)
library(DT)
library(ape)
library(ggplot2)
library(ggdensity)
library(rgdal)


### Import distance matrix, phylogeny and location data files.

snp_distance_matrix <- read.csv("All_bovis_Aug24_snp_distance_matrix.csv", header = TRUE, stringsAsFactors = FALSE)

location_data <- read.csv("All_bovis_Aug24_locations.csv", header = TRUE,  stringsAsFactors = FALSE)

tree<-read.tree("All_bovis_Aug24_max_pars.tree")

# Make branches with lengths of 0 very very small - RaxML trees can appear very different, ie not sitting flat to node, when zoomed in on samples that are identical - feature of substitutions per site being the metric used.
tree$edge.length[tree$edge.length <= 0] <- 0.000001

## Make sure Lat and Lon data are numeric
location_data$Lat<-as.numeric(location_data$Lat)
location_data$Lon<-as.numeric(location_data$Lon)


## Fix any polytomies in the tree so that there are more nodes than tips or else the display of the sub tree phylogeny won't work
tree <- multi2di(tree)


#### Set up the user interface (ui) for the shiny app

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tabsetPanel(
    ## Tab for the title page
    tabPanel("i-MAGE",
             titlePanel("i-MAGE: Interactive MycobActerium bovis Genome Epidemiology"),
             mainPanel(
               img(src="iMage_logo_2.png", align="centre", width="100%"), ## Your image must be in a subfolder called 'www' or else it won't display
               img(src="AFBI_EMbI_logo.png", align="right", width="50%"),
               img(src="sigil.png", align="left", width="1%")
               )
    ),
    
    ## Tab for instructions page
    tabPanel("User guide",
             titlePanel("Instructions"),
             mainPanel(code(strong("i-MAGE is a genome epidemiological tool for helping trace sources of bovine TB (bTB) infection.")),
                       p(strong("The 'Herd Selection Tool' Tab")),
                       p("1. First, enter in the search box provided the six digit herd number from your patch you want to find associated genome data for."),
                       p("2. Note down the first three components of the genome sequence file name from the left nost column - example 'BTB_Prov_982'."),
                       p("3. Go to the 'Map' tab and enter one of these genome names into the search bar there, then follow the instructions."),
                       p("4. If you herd of choice is affected by multiple different lineages of M. bovis, you will need to check out the entries for all available genomes that come from different lineages."),
                       p("5. Lineages can be determined by the 6th component in the file name that follows the four digit year date - example 1.140, 2.142 etc - lineage is detailed as vntr and spoligotype numbers separated by a full-stop."),
                       p(strong("The 'Map' Tab")),
                       p("6. First, go to the Map screen, key in or select from the drop down box the name of the M. bovis isolate you wish to compare to the rest of the dataset."),
                       p("7. Then, use the slider to select the maximum number of SNP differences you want to set to find related M. bovis isolates."),
                       p("8. The lower the number of SNPs different, the more related isolates are."),
                       p("9. There is no 'right' cutoff to find related isolates, but using a cutoff of 5 or lower can find isolates more likely to be linked by recent transmission."),
                       p("10. The map will display the locations of linked isolates. Zoom in to see how clusters breakdown, and hover over the location points to see the sample ID details."),
                       p(strong("The 'Phylogeny' Tab")),
                       p("11. You can then go to the Phylogeny tab, to view the highlighted chosen sample's phylogenetic relationship to the closest relatives defined by the chosen SNP distance from the Map tab."),
                       p("12. If the SNP threshold is high, it may pull in so many samples that the phylogeny plot resolution is poor."),
                       p("13. Use the slider to dynamically adjust the height of the phylogeny plot accordingly."),
                       p(strong("The 'Home/Core Range Finder' Tab")),
                       p("14. You can go to the Home / Core range which displays the 50% and 95% kernel density estimate for the subset of samples chosen by the applied SNP cutoff"),
                       p("15. The 95% Home Range is minimum area over which 95% of the most densely clustered isolates are found and is coloured grey."),
                       p("16. The 50% Core Range is the minimum area over which 50% of the most densely clustered isolates are found and is coloured dark navy"),
                       p("17. The locations of the subset of isolates chosen by SNP cutoff are displayed as teal dots, except for the chosen input sample from the map tab, which will be coloured red."),
                       p("18.The 50% Core Range is the densest area of the chosen sample subset and likely corresponds to the historical origin of the selected M. bovis lineage."),
                       p("19. Isolates that are placed outside this core range should have their movement records checked to determine if they have a link back to the core area."),
                       p(strong("The 'Follow Up Investigation' Tab")),
                       p("20. Then, go to the 'Follow Up Investigation' tab where the closest relatives of your isolate will be listed, and the herd in which they have been found."),
                       p("21. The format of the isolate name will tell you the host species (bov=bovine, bad=badger) and the year of isolation"),
                       p("22. If some of the closest isolates are from wildlife sources, there will be no herd ID entry"),
                       p(strong("CAVEAT! M. bovis does not mutate very frequently, so in outbreak areas there may be little diversity.")),
                       p(strong("A close match to another herd does NOT necessarily mean that is where infection came from, indeed many isolates could have a similar level of relatedness or be identical to the one you are interested in.")),
                       p(strong("OR the true source may not have been sampled, while others nearby have.")),
                       p(strong("BUT, given how the pathogen clusters in the landscape, the geographic location of related isolates can give you a clue as to where infection may have come from.")),
                       p(code("In your outbreak investigations, check for animal movement data which link your isolate back to the area where the most closely related isolates are found.")),
                       p(code("Search land parcel data to assess if the animal your isolate was found in may have been in a contiguous herd location next to a herd with a closely related isolate."))
                       
             )
    ),
    
    # Tab for herd selection
    tabPanel("Herd Selection Tool",
             titlePanel("Enter herd ID to find genome data associated with it."),
             sidebarLayout(
               sidebarPanel(
                 textInput("herd_id", "Enter Herd ID:", value = ""),
                 actionButton("submit", "Submit")
               ),
               mainPanel(
                 # Add UI elements for displaying information derived from other_samples
                 dataTableOutput("derived_table_A")
               )
             )
    ),
    
    
    
    # Tab for the map
    tabPanel("Map",
             titlePanel("i-MAGE: Genetic relatedness search tool"),
             mainPanel(
               selectInput("sample", "Select Sample Name:", choices = colnames(snp_distance_matrix)),
               sliderInput("snp_distance_threshold", "Select SNP Distance Threshold - samples differ by x SNPs or fewer:", min = 0, max = 50, value = 25),
               leafletOutput("map")
             )
    ),
    
    
    # Tab for the phylogeny 
    
    tabPanel("Phylogeny",
             titlePanel("Phylogenetic context of selected sample: selected SNP cut off relatives displayed"),
             mainPanel(
                sliderInput("phylogeny_height", "Adjust Phylogeny Resolution (height of plot in pixels):", min = 200, max = 5000, value = 2000),
                plotOutput("phylogeny_plot")
                )
    ),
    
    # Add the new tab for the Core Range viewer
    tabPanel("Home / Core Range Finder",
             titlePanel("Core range of selected SNP distance isolates"),
             mainPanel(
               plotOutput("shapefile_map")
             )
    ), 
    # Tab for the derived information
    tabPanel("Follow Up Investigation",
             titlePanel("Check movement & land parcel data for these locations"),
             mainPanel(
               # Add UI elements for displaying information derived from other_samples
               dataTableOutput("derived_table")
             )
    ),
    

    
    ## Tab for the acknowledgements panel
    tabPanel("Acknowledgements", 
             titlePanel("i-MAGE development and curation"),
              mainPanel(p(strong("i-MAGE is a product of the AgriFood and Biosciences Institute (AFBI) Epidemiology, Molecular Biology and Immunology (EMbI) R&D cluster.")),
                        p("Team members who wrote the application, generate and curate the data that drive it are:"),
                        p(strong("TB molecular biology and genomics")),
                        p("Adrian Allen, Eleanor Presho, Carl McCormick, Robin Skuce, Ryan Devaney, Tara Ardis & Purnika Ranasinghe"), 
                        p(strong("TB culture and diagnostics")),
                        p("Tom Ford & Suzan Thompson."),
                        p("Development of i-MAGE is funded by AFBI and the Department of Agriculture, Environment and Rural Affairs (DAERA) for Northern Ireland."),
                        img(src="AFBI_EMbI_logo.png", align="centre", width="70%"),
                        img(src="DAERA_logo.png", align="centre", width="70%")
                        )
                    )               
                  )
                )


### Set the server up to run the scripts that make your app tick.

server <- function(input, output) {
  filtered_location_data <- reactive({
    req(input$herd_id)  
    location_data %>%
      filter(Herd == input$herd_id)
  })
  
  # Render the filtered data in the data table
  output$derived_table_A <- renderDataTable({
    req(filtered_location_data())  
    datatable(filtered_location_data())
  })
  
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
      setView(lng = -6.9, lat = 54.6, zoom = 6) %>%
      addMarkers(
        data = other_sample_locs(),
        lng = ~Lon,
        lat = ~Lat,
        label = ~paste("Label:",ID),  # Customize the label text here
        labelOptions = labelOptions(
          direction = "auto",                      # Automatically choose label direction
          noHide = F,                           # Keep the label open on hover
          style = list("border-color" = "blue", "background-color" = "white")  # Style for the label
        ),
        clusterOptions = markerClusterOptions(),  # Optional: Add marker clustering
      
      )
    
  })
  


  ## Render the phylogeny of samples from the tree within the snp cutoff
  # Create a reactive expression for the phylogeny plot
  phylogeny_plot <- reactive({
    phylogeny_height <- input$phylogeny_height  # Get the height from the slider input
    
    subtree_samples <- other_samples()  # Extract the relevant sample names
    subtree <- keep.tip(tree, subtree_samples)
    
    # Create a vector of colors based on whether each tip label matches the selected sample
    tip_colors <- ifelse(subtree$tip.label == input$sample, "red", "black")
    
    # Create the phylogeny plot
    plot(subtree, type = "phylogram", cex = 1.0, tip.color = tip_colors)
    
    # Get the size of the plot window in centimetres
    plot_size <- dev.size("cm")
    
    # Set the x and y coordinates for adding a scale bar
    xscale <- 0.3 * plot_size[1]
    
    add.scale.bar(x=xscale, y=-1, lcol="blue", lwd=3, length=2)
    
  })
  
  # Render the phylogeny plot
  output$phylogeny_plot <- renderPlot({
    # Set margins to ensure the scale bar is not cut off
    par(mar = c(5, 4, 4, 2) + 1)  # Adjust these values as needed
    
    # Plot the phylogeny
    phylogeny_plot()
  
  }, height = function() {
    input$phylogeny_height
  })
 
  # Load the core range shapefile
  NI <- readOGR(dsn = ".", layer = "NI2_outline_small")
  NI_df <- fortify(NI)
  
  ## Render the map with filtered locations and kernel density plot
  output$shapefile_map <- renderPlot({
    # print(NI_df)  # Check if NI_df contains data
    #print(other_sample_locs())  # Check if other_sample_locs() contains data
    
  
    
    ggplot() +
      geom_polygon(data = NI_df, aes(x = long, y = lat, group = group), colour = "black", fill = "white") +
      geom_hdr(data = other_sample_locs(), aes(x = Lon, y = Lat), method="kde", probs=c(0.95, 0.50), fill="#0F1E64") +
      geom_point(data = other_sample_locs(), aes(x = Lon, y = Lat, color = factor(ID == input$sample)), size = 1.3) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "#53DFD1"), guide = FALSE) +
      #geom_point(data = other_sample_locs(), aes(x = Lon, y = Lat), color="#53DFD1", size = 1.3) +
      theme_bw()
  })  
  
  
  
  # Render the derived information on the fourth page/tab
  output$derived_table <- renderDataTable({
    selected_data <-other_sample_locs()[,c(1,4,5,6)]
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
