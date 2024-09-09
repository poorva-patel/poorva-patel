#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Core
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyquant)
library(lubridate)
library(shiny)
library(shinydashboard)
library(glue)
library(scales)
library(lubridate)
library(rsconnect)
library(countrycode)
library(rnaturalearthdata)
library(ggplot2)
library(sf)
library(leaflet)
library(rnaturalearth)
library(gifski)
library(gganimate)




# Interactive Visualizations
library(plotly)
library(shinyWidgets)

#### Load and combine data sets
### Load the data 
# Energy use data set
energy_use_data = read.csv("energy_use.csv")

# Household income data
entire_household_income_data = read.csv("mhhinc.csv")

### Combine the data 
# Function to remove "k" and multiply by 100
convert_numbers = function(value) {
  if (any(grepl("k", value, fixed = TRUE))) {
    value = as.character(value)
    numeric_value = as.numeric(gsub("k", "", value))
    return(numeric_value * 1000)
  } else {
    # Corrected to as.integer
    return(as.integer(value))  
  }
}

## This function will reshape the data so it makes it easier to merge
## the two datasets.
# Reshapes the energy data 
reshape_data_frame_energy = function(data) {
  final_data = data.frame()
  
  # Loop through each column (starting from the second column)
  for (col in names(data)[-1]) {
    # Extract the year from the column name
    year = as.integer(gsub("X", "", col))
    
    # Create a temporary data frame for the current year
    temp_data = data.frame(
      country = data$country,
      year = rep(year, nrow(data)),
      emission = data[, col]
    )
    
    # Append the temporary data frame to the final data frame
    final_data = rbind(final_data, temp_data)
  }
  
  return(final_data)
}

# Reshapes the income data
reshape_data_frame_income = function(data) {
  final_data = data.frame()
  
  # Loop through each column (starting from the second column)
  for (col in names(data)[-1]) {
    # Extract the year from the column name
    year = as.integer(gsub("X", "", col))
    
    # Create a temporary data frame for the current year
    temp_data = data.frame(
      country = data$country,
      year = rep(year, nrow(data)),
      income = data[, col]
    )
    
    # Append the temporary data frame to the final data frame
    final_data = rbind(final_data, temp_data)
  }
  
  return(final_data)
}


# Function to generate the decade
get_decade = function(year) {
  third_letter_year <- substr(as.character(year), 3, 3)
  
  # Check if the year starts with "19" or "20" and adjust the decade_start accordingly
  if (startsWith(as.character(year), "19")) {
    decade_start <- as.numeric(paste0("19", third_letter_year, "0"))
  } else {
    decade_start <- as.numeric(paste0("20", third_letter_year, "0"))
  }
  
  return(paste0(decade_start, "s"))
}

# Function to add continent information to a data frame
add_continent_column <- function(data_frame) {
  # Check if the "Country" column exists
  if ("country" %in% colnames(data_frame)) {
    # Create a new column for continent
    data_frame$continent <- NA
    
    # Assign continents based on countries
    for (i in 1:nrow(data_frame)) {
      country <- data_frame$country[i]
      
      if (!is.na(country)) {
        if (country %in% c("Belarus", "Bosnia and Herzegovina", "Malta", "Moldova", "Montenegro", "Netherlands", "North Macedonia", "Russia", "Serbia", "Ukraine","Albania", "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "UK")) {
          data_frame$continent[i] <- "Europe"
        } else if (country %in% c("Cape Verde", "Cote d'Ivoire", "Lesotho", "Sudan", "Togo", "Tunisia","Algeria", "Angola", "Benin", "Botswana", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo, Dem. Rep.", "Congo, Rep.", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")) {
          data_frame$continent[i] <- "Africa"
        } else if (country %in% c("Jamaica", "St. Kitts and Nevis", "St. Lucia", "St. Vincent and the Grenadines", "Trinidad and Tobago","Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Canada", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "El Salvador", "Grenada", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago", "USA")) {
          data_frame$continent[i] <- "North America"
        } else if (country %in% c("Suriname","Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")) {
          data_frame$continent[i] <- "South America"
        } else if (country %in% c("Kuwait", "North Korea", "Oman", "Qatar", "Syria","Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "Hong Kong, China", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyz Republic", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", "Turkmenistan", "UAE", "Uzbekistan", "Vietnam", "Yemen")) {
          data_frame$continent[i] <- "Asia"
        } else if (country %in% c("Kiribati", "Palau", "Samoa", "Tonga","Australia", "Fiji", "New Zealand", "Papua New Guinea", "Solomon Islands", "Vanuatu")) {
          data_frame$continent[i] <- "Oceania"
        }
      }
    }
    
    return(data_frame)
  } else {
    # Print a message if the "Country" column is not found
    cat("Error: 'Country' column not found in the data frame.\n")
    return(NULL)
  }
}

calculate_average_carbon_intensity <- function(data) {
  # Group by country, continent, and decade, calculate average emission and income
  grouped_data <- aggregate(cbind(emission, income) ~ country + continent + decade, 
                            data = data,
                            FUN = mean,
                            na.rm = TRUE)
  
  # Calculate emissions per income
  grouped_data$emissions_per_income <- with(grouped_data, emission / income)
  
  return(grouped_data)
}


## Removes the country Column
# Energy use 
columns_to_process_energy = colnames(energy_use_data)[-1]
# Monthly Income 
columns_to_process_income = colnames(entire_household_income_data)[-1]

# Apply's the function to the columns to process and then puts replaces it in the data frame 
# Energy use 
for (col in columns_to_process_energy) {
  energy_use_data[, col] <- sapply(energy_use_data[, col], convert_numbers)
}
# Monthly Income 
for (col in columns_to_process_income) {
  entire_household_income_data[, col] = sapply(entire_household_income_data[, col], convert_numbers) }

#Subset the data so that it only covers years 1960 - 2015
subset_household_income_data = entire_household_income_data[, c("country", paste0("X", 1960:2014))]

## Merge both data sets together 
# Applies reshape function to Energy use 
energy_use_data = reshape_data_frame_energy(energy_use_data)

# Applies the function to the Monthly Income
subset_household_income_data = reshape_data_frame_income(subset_household_income_data)

# Combines the data into one data frame for analysis 
climate_change_data = merge(subset_household_income_data, energy_use_data, by = c("country", "year"))

# Adding a column that represents the ratio of emissions to income
climate_change_data$emissions_per_income <- with(climate_change_data, emission / income)

# Create a column for the decade
climate_change_data$decade = sapply(climate_change_data$year, get_decade)

# Add continent information to the climate_change_data
climate_change_data <- add_continent_column(climate_change_data)

# Calculate the average carbon intensity
average_carbon_intensity <- calculate_average_carbon_intensity(climate_change_data)

# Merge the calculated average carbon intensity back into the original data frame
climate_change_data <- merge(climate_change_data, average_carbon_intensity[, c("country", "continent", "decade", "emissions_per_income")], 
                             by = c("country", "continent", "decade"))

# Rename the column to match the original data frame
names(climate_change_data)[names(climate_change_data) == "emissions_per_income.y"] <- "average_carbon_intensity"

################## New Stuff #################################################

# Analysis

# Change the decade to numeric
climate_change_data$numeric_decade <- as.numeric(sub("\\D", "", climate_change_data$decade))


# Makes a new data set
min_max_by_continent = summarise(group_by(climate_change_data, numeric_decade, continent),
                                 min_income = min(income),
                                 max_income = max(income),
                                 min_emission = min(emission),
                                 max_emission = max(emission),
                                 min_average_carbon_intensity = min(average_carbon_intensity),
                                 max_average_carbon_intensity = max(average_carbon_intensity),
)

# Find a summary of the min and max by continent
#summary(min_max_by_continent)

# Find the average of each countries income and emisions
totals_by_country = summarise(group_by(average_carbon_intensity, country), total_emissions_per_income = sum(emissions_per_income), income=  sum(income), emissions = sum(emission))

# Add the Continent 
totals_by_country = add_continent_column(totals_by_country)

# Arrange by least to greatest 
totals_by_country = arrange(totals_by_country, continent, income, emissions, total_emissions_per_income)

# Summarizes all the data into one column 
summary_all_continents = summarise(
  group_by(totals_by_country, continent),
  total_emissions_per_income = sum(total_emissions_per_income),
  total_income = sum(income),
  total_emissions = sum(emissions)
)
# summary of all the eontinent data
#summary(summary_all_continents)

# Add a column for continent using country code

climate_change_data$continent <- countrycode(
  sourcevar = climate_change_data$country,
  origin = "country.name",
  destination = "continent"
)

north_america_countries <- c("Antigua and Barbuda","Bahamas","Barbados","Belize","Canada","Costa Rica","Cuba","Dominica",
                             "Dominican Republic","El Salvador","Grenada","Guatemala"," Haiti","Honduras","Jamaica",
                             "St. Kitts and Nevis","St. Lucia","St. Vincent and the Grenadines","Trinidad and Tobago",
                             "USA","Nicaragua","Mexico","Panama")
south_america_countries <- c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Guayana","Panama",
                             "Paraguay","Peru","Suriname","Uruguay","Venezuela")

climate_change_data <- climate_change_data %>%
  mutate(continent = case_when(
    country %in% north_america_countries ~ "North America",
    country %in% south_america_countries ~ "South America",
    TRUE ~ continent
  )
  )
# Create a list of data frames, each representing a decade
decades_data_list <- split(climate_change_data, climate_change_data$decade)



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Power and Prosperity"),
  dashboardSidebar(
    br(),
    # Widget for the decade  
    radioGroupButtons(
      inputId = "decade",
      label = "Change Decade Widget",
      choices = unique(climate_change_data$decade)[order(unique(climate_change_data$decade))],
    ),
    # Widget for the continent 
    pickerInput(
      inputId = "continent",
      label = "Metrics By Country Widget",
      choices = setdiff(unique(climate_change_data$continent), "Americas"),
    ),
    # Widget for the exploration
    pickerInput(
      inputId = "stats_widget",
      label = "Exploration Widget",
      choices = c("Total Income", "Total Emissions", "Total Emissions/Income")
    )
  ),
  dashboardBody(
    fluidRow(
      tabsetPanel(
        id = "tabs",
        # Creates the tab for the world map
        tabPanel("World Map",
                 plotlyOutput("continentMap")),
        # Creates the tab for teh metrics by country
        tabPanel("Metrics By Country",
                 plotlyOutput("bar_country")),
        # Creates the tab for exploration
        tabPanel("Exploration",
                 plotlyOutput("bar_stats")),
        # Creates the tab for animation
        tabPanel("Data in Motion", 
                 imageOutput("animation"))
      )
    )
  ),
  skin = "purple"
)

server <- function(input, output, session) {
  
  # Plotly outputs
  output$continentMap <- renderPlotly({
    # Reactive data based on input
    filtered_data_for_continent_map <- reactive({
      selected_decade <- input$decade
      selected_data <- decades_data_list[[selected_decade]]
      
      if (!is.null(input$continent)) {
        selected_data <- subset(selected_data, continent %in% input$continent)
      }
      
      return(selected_data)
    })
    
    # Get the world map data
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Data by continent
    decade_selected <- input$decade
    aggregated_data <- climate_change_data %>%
      filter(decade %in% decade_selected) %>%
      group_by(continent, decade) %>%
      summarize(emission = mean(emission, na.rm = TRUE),
                income = mean(income, na.rm = TRUE))
    
    # Merge the world map data with your aggregated data
    world_data <- merge(world, aggregated_data, by.x = "continent", by.y = "continent", all.x = TRUE)
    
    # Create the Choropleth Map
    p <- ggplot(data = world_data) +
      geom_sf(aes(fill = emission), color = NA) + 
      scale_fill_gradientn(colors = c("lavender", "#4B0082"), na.value = "grey") +
      theme_void() +
      labs(fill = "Emissions/Income", title = "Emissions/Income Over the Decades", subtitle = paste("Decade:", decade_selected))
    
    ggplotly(p)  
  })
  
  output$bar_country = renderPlotly({
    # Reactive data based on input
    filtered_data_for_scatter_plot <- reactive({
      selected_decade <- input$decade
      selected_data <- decades_data_list[[selected_decade]]
      
      if (!is.null(input$continent)) {
        selected_data <- subset(selected_data, continent %in% input$continent)
      }
      
      return(selected_data)
    })
    
    # Creates the ggplot and and places it into a variable 
    gg <- ggplot(filtered_data_for_scatter_plot(), aes(x = reorder(country, average_carbon_intensity) , y = average_carbon_intensity, fill = country)) +
      # Changes the label 
      labs(title = 'Emission/Income by Country',
           x = 'Year',
           y = 'Emissions/Income') +
      # Sets the theme to minimal
      theme_minimal() +
      # Change other aspects of the graph 
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.line = element_blank(),    
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(margin = margin(b = 20)),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      # Creates the bar graph 
      geom_col()
    
    # Convert ggplot to plotly directly
    plotly_conversion <- ggplotly(gg)
    
    # Additional layout customization for the plotly plot
    layout_options = list(
      title = 'Emissions/Income by Region',
      xaxis = list(title = 'Year'),
      yaxis = list(title = 'Emissions/Income'),
      showlegend = FALSE  # You can customize other layout options here
    )
    
    # Assign modified layout to the plotly object
    plotly_conversion = layout(plotly_conversion, layout_options)
    
    if (nrow(filtered_data_for_scatter_plot()) > 0) {
      plotly_conversion
      # If there is no data it will show the following message
    } else {
      plotly_conversion$layout$title = "No Data Available"
      plotly_conversion
    }
  })
  
  
  output$bar_stats = renderPlotly({
    # Creates the switch for the three bar graphs
    ggplot_data <- switch(input$stats_widget,
                          "Total Income" = aes(x = total_income, y = reorder(continent, total_income), fill = total_income),
                          "Total Emissions" = aes(x = total_emissions, y = reorder(continent, total_emissions), fill = total_emissions),
                          "Total Emissions/Income" = aes(x = total_emissions_per_income, y = reorder(continent, total_emissions_per_income), fill = total_emissions_per_income))
    # Creates the plot for the ggplot 
    ggplot(summary_all_continents, ggplot_data) +
      theme_minimal() +
      scale_fill_gradient(low = "lavender", high = "purple") +
      # Change the title and y-axis label to "Total Income" based on the selected input
      labs(title = switch(input$stats_widget,
                          "Total Income" = "Total Income Across the Countries",
                          "Total Emissions" = " Total Emissions Across the Countries",
                          "Total Emissions/Income" = "Total Emissions/Income Across the Countries"),
           x = "Continent",
           # Changes the y-axis based on the input recieved from the switch  
           y = switch(input$stats_widget,
                      "Total Income" = "Total Income",
                      "Total Emissions" = "Total Emissions",
                      "Total Emissions/Income" = "Total Emissions/Income")) +
      # removes the label
      guides(fill = FALSE) +
      # Creates the bar graph 
      geom_col()
  })
  
  #Displays the animation
  output$animation <- renderImage({list(src = "income_vs_energy.gif",
                                        contentType = "image/gif",
                                        alt = "Mean Household Income vs Energy Use per Person")},
                                  deleteFile = FALSE)
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
