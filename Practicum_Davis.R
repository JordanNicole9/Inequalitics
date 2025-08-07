# Jordan Davis - Inequalitics Practicum 2025
# Full R script: data preparation, clustering, and Shiny app

# Loads libraries for reading, processing, modeling, and visualizing data
# Reads in Excel files (used for Freedom House data).
library(readxl)
# Provides functions for filtering, transforming, and summarizing data.
library(dplyr)
# Handles reshaping and missing values in data.
library(tidyr)
# Creates all visualizations and plots.
library(ggplot2)
# Performs silhouette analysis to evaluate clustering.
library(cluster)
# Used to visualize clustering performance.
library(factoextra)
# Helps with axis formatting (like percentages).
library(scales)
# Builds the interactive Shiny web app.
library(shiny)
# Loads core tidyverse.
library(tidyverse)

# Read in the raw data files from their local paths.
vdem <- read_csv("V-Dem-CY-Full+Others-v15.csv")
# Loads democracy indicators from the V-Dem data set.
freedom <- read_excel("All_data_FIW_2013-2024.xlsx", sheet = 2)
# Loads Freedom House scores from the Excel sheet.
income <- read_csv("income.highest.10.csv")
# Loads the income share data for the top 10% from WID.
# Cleans the income data set by renaming and removing missing rows.
income_clean <- income %>%
  rename(country_name = country, year = year, income_top10_share = value) %>%
  filter(!is.na(income_top10_share))
# Renames columns for clarity and filters out rows with missing income data.

# Cleans and renames the Freedom House data set columns and filters
# by the relevant years.
freedom_clean <- freedom %>%
  rename(
    country_name = `Country/Territory`,
    year = Year,
    freedom_status = Status,
    civil_liberties_score = `CL Score`,
    freedom_total_score = `Total Score`
  ) %>%
  filter(year >= 2012 & year <= 2023)
# Standardizes column names and keeps only project years from 2012 to 2023.

# Selects relevant columns from V-Dem and filter by project years.
vdem_clean <- vdem %>%
  rename(country_name = country_name, year = year) %>%
  select(country_name, year, v2x_polyarchy, v2x_freexp_altinf, v2x_partip) %>%
  filter(year >= 2012 & year <= 2023)
# Filters out extra columns and keeps only needed democracy indicators.

# Merges the three cleaned data sets into a single data set using left joins.
merged <- income_clean %>%
  left_join(vdem_clean, by = c("country_name", "year")) %>%
  left_join(freedom_clean, by = c("country_name", "year"))
# Combines all cleaned data sets into one based on country and year.

# Defines the list of countries and years.
countries <- c("Sweden", "Colombia", "United Kingdom", "Uruguay", "Austria", "Belgium",
               "Finland", "France", "Israel", "Paraguay", "Argentina", "Brazil", "Canada",
               "Germany", "Greece", "Ireland", "Italy", "Portugal", "Spain")
# Sets the specific countries to include in the analysis.
years <- 2012:2023
# Defines the time range for the project.

# Filters the merged data set to only include selected countries and years.
filtered <- merged %>%
  filter(country_name %in% countries, year %in% years)
# Filters the full data set down to just the selected scope.

# Selects only the variables needed for clustering from the filtered data set.
vars <- filtered %>%
  select(income_top10_share, v2x_polyarchy, v2x_freexp_altinf, v2x_partip)
# Pulls just the indicators used in the clustering model.

# Drops rows that have any missing values in the selected variables.
filtered <- filtered[complete.cases(vars), ]
vars <- vars[complete.cases(vars), ]
# Removes incomplete data rows to avoid issues in clustering.

# Standardize the clustering variables using z-score normalization.
vars_scaled <- scale(vars)
# Scales each column so they are centered and have the same unit.

# Run k-means clustering with 3 clusters and 25 random starts.
set.seed(123)
# Sets seed to make results reproducible.
km <- kmeans(vars_scaled, centers = 3, nstart = 25)
# Performs the actual k-means clustering.

# Runs silhouette analysis to evaluate clustering performance.
sil <- silhouette(km$cluster, dist(vars_scaled))
# Measures how well each country fits in its assigned cluster.
summary(sil)
# Outputs a summary of silhouette scores to the console.

# Assigns cluster names to make interpretation easier.
filtered$Cluster <- factor(km$cluster, labels = c("Stable", "At Risk", "Backsliding"))
# Replaces cluster numbers with readable category names.

# Saves the clustered data set for use in external analysis.
write.csv(filtered, "clustered_output.csv", row.names = FALSE)
# Exports final data set with cluster assignments to CSV.

# Loads Shiny and define user interface layout for visual exploration.
ui <- fluidPage(
  # Add light styling to the app.
  tags$head(tags$style(HTML("body { background-color: #f8f9fa; } h2, label { color: #2c3e50; } .title-row { text-align: center; margin-bottom: 20px; } .btn-danger { margin-top: 10px; }"))),
  
  # Defines the top row with title and disclaimer.
  fluidRow(class = "title-row",
           column(12,
                  h2("Inequalitics"),
                  p("Note: All data used in this application are derived from publicly available sources including the Varieties of Democracy (V-Dem) dataset, the Freedom House Index (2013–2024), and the World Inequality Database (Top 10% Income Share). This Shiny app is designed for educational purposes only."),
                  actionButton("reset", "Reset", class = "btn btn-danger"))),
  
  # Defines top-left and top-right plots with selectors.
  fluidRow(
    column(6,
           selectInput("income_country", "Select Country (Income):",
                       choices = unique(filtered$country_name),
                       selected = unique(filtered$country_name)[1]),
           plotOutput("incomePlot")),
    
    column(6,
           selectInput("vdem_var", "Select V-Dem Variable:",
                       choices = c("v2x_polyarchy" = "Electoral", "v2x_freexp_altinf" = "Expression", "v2x_partip" = "Participation"),
                       selected = "v2x_polyarchy"),
           plotOutput("vdemPlot"))
  ),
  
  # Defines bottom-left and bottom-right plots with selectors.
  fluidRow(
    column(6,
           selectInput("freedom_var", "Select Freedom House Variable:",
                       choices = c("freedom_status" = "Status", "civil_liberties_score" = "Liberties", "freedom_total_score" = "TotalScore"),
                       selected = "freedom_total_score"),
           plotOutput("freedomPlot")),
    
    column(6,
           selectInput("overlay_country", "Select Countries for Overlay:",
                       choices = unique(filtered$country_name),
                       selected = unique(filtered$country_name)[1:2],
                       multiple = TRUE),
           checkboxInput("overlay", "Overlay Graphs", value = TRUE),
           plotOutput("overlayPlot"))
  )
)

# Defines server logic that controls interactive behavior and plotting.
server <- function(input, output, session) {
  
  # Reset all inputs to default when reset button is clicked.
  observeEvent(input$reset, {
    updateSelectInput(session, "income_country", selected = unique(filtered$country_name)[1])
    updateSelectInput(session, "vdem_var", selected = "v2x_polyarchy")
    updateSelectInput(session, "freedom_var", selected = "freedom_total_score")
    updateSelectInput(session, "overlay_country", selected = unique(filtered$country_name)[1:2])
    updateCheckboxInput(session, "overlay", value = TRUE)
  })
  
  # Generates a line plot showing top 10% income share over time for the 
  # selected country.
  output$incomePlot <- renderPlot({
    filtered %>%
      filter(country_name == input$income_country) %>%
      ggplot(aes(x = year, y = income_top10_share)) +
      geom_line(color = "#0072B2", size = 1.2) +
      labs(title = "Economic Concentration Among the Wealthiest", x = "Year", y = "% Share") +
      theme_minimal()
  })
  
  # Generates a bar chart showing average V-Dem score by country.
  output$vdemPlot <- renderPlot({
    var <- input$vdem_var
    filtered %>%
      filter(!is.na(.data[[var]])) %>%
      group_by(country_name) %>%
      summarize(avg = mean(.data[[var]], na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(country_name, -avg), y = avg, fill = country_name)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "How Countries Differ on Variables from V-Dem", x = "Country", y = "Score") +
      scale_fill_manual(values = scales::hue_pal()(length(unique(filtered$country_name)))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Generates either a bar chart or boxplot based on the selected Freedom House 
  # variable.
  output$freedomPlot <- renderPlot({
    var <- names(c("freedom_status" = "Status", "civil_liberties_score" = "Liberties", "freedom_total_score" = "TotalScore")[c("Status", "Liberties", "TotalScore") == input$freedom_var])[1]
    if (var == "freedom_status") {
      filtered %>%
        ggplot(aes(x = country_name, fill = freedom_status)) +
        geom_bar() +
        labs(title = "Freedom House Classification of Countries", x = "Country") +
        scale_fill_manual(values = scales::hue_pal()(length(unique(filtered$freedom_status)))) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      filtered %>%
        ggplot(aes_string(x = "country_name", y = var, fill = "country_name")) +
        geom_boxplot(show.legend = FALSE) +
        labs(title = "Distribution of Liberties/Total Score from Freedom House Rankings", x = "Country") +
        scale_fill_manual(values = scales::hue_pal()(length(unique(filtered$country_name)))) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Generates either an overlay area plot or 
  # a line plot for selected countries’ income share.
  output$overlayPlot <- renderPlot({
    df <- filtered %>% filter(country_name %in% input$overlay_country)
    p <- ggplot(df, aes(x = year, y = income_top10_share, group = country_name))
    if (input$overlay) {
      p + geom_area(aes(fill = country_name), alpha = 0.5, position = 'identity') +
        labs(title = "Comparing Income Inequality Across Countries", y = "% Share", x = "Year") +
        scale_fill_manual(values = scales::hue_pal()(length(input$overlay_country))) +
        theme_minimal()
    } else {
      p + geom_line(aes(color = country_name), size = 1.2) +
        labs(title = "Comparing Income Inequality Across Countries", y = "% Share", x = "Year") +
        scale_color_manual(values = scales::hue_pal()(length(input$overlay_country))) +
        theme_minimal()
    }
  })
}

# Launches the Shiny app using the defined UI and server components.
shinyApp(ui, server)
