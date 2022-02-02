library(tidyverse)
library(shiny)
library(sf) # spatial data package
library(tigris) # shapefile package
library(leaflet) # interactive map package
library(RColorBrewer)
library(shinythemes)

# Download Coronavirus data in California by County from the LA Times
ca_vaccine <- read_csv("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/cdph-vaccination-county-totals.csv")
ca_cases <- read_csv("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv")

# Join both data sets and calculate new variables
cases_vaccines <- inner_join(ca_vaccine, ca_cases, by = c("date", "fips", "county")) %>%
  mutate(
    case_rate = (confirmed_cases / population),
    death_rate = (deaths / confirmed_cases)
  )

# Import California county lines "shapefile"
counties <- counties(state = "California", cb = TRUE) %>%
  st_transform("+proj=longlat +datum=WGS84")

# Join shapefile with data set
counties_merged <- geo_join(counties, cases_vaccines, "COUNTYFP", "fips", how = "inner")

# Create Shiny App
ui <- fluidPage(
  theme = shinythemes::shinytheme('superhero'),
  titlePanel("California Covid-19 Trends by County"),
  
  sidebarPanel(
       helpText(h5("The Los Angeles Times made coronavirus data publicly available online. The data files come from a continual Times survey of California's 58 county health agencies and three city agencies. Updated numbers are published throughout the day at latimes.com/coronavirustracker."),
                tags$a(href = "https://github.com/datadesk/california-coronavirus-data", "LA Times Data Repository"),
),
       selectInput("day",
                "Cummulative Data from (date):",
                choices = unique(counties_merged$date)
    ),

  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Case Rate",
        leafletOutput("cases")
      ),
      tabPanel(
        "Vaccine Rate",
        leafletOutput("vaccines")
      ),
      tabPanel(
        "Death Rate",
        leafletOutput("deaths")
      )
    )
  )
)

server <- function(input, output) {
  reactive_data <- reactive({
    d <- counties_merged %>%
      filter(date == input$day)
    return(d)
  })
  
  output$cases <- renderLeaflet({
    palette_cases <- colorBin(palette = "Oranges", 6, domain = counties_merged$case_rate)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>case rate: %g <br/>total cases: %g",
      reactive_data()$county,
      round(reactive_data()$case_rate, 2),
      reactive_data()$confirmed_cases
    ) %>%
      lapply(HTML)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = reactive_data(),
        label = labels,
        fillColor = ~ palette_cases(reactive_data()$case_rate),
        fillOpacity = 0.7,
        weight = 0.2,
        smoothFactor = 0.2,
        highlightOptions = highlightOptions(
          weight = 2,
          fillOpacity = 1,
          color = "gray",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = palette_cases,
        values = reactive_data()$case_rate,
        position = "bottomright",
        title = "Case Rate",
        opacity = 0.7
      ) %>%
      setView(lat = 36.778259, lng = -119.417931, zoom = 5)
  })
  
  output$vaccines <- renderLeaflet({
    palette_vaccine <- colorBin(palette = "BuPu", 6, domain = counties_merged$fully_vaccinated_percent)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>vaccine rate: %g <br/>total vaccined: %g",
      reactive_data()$county,
      round(reactive_data()$fully_vaccinated_percent, 2),
      reactive_data()$fully_vaccinated
    ) %>%
      lapply(HTML)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = reactive_data(),
        label = labels,
        fillColor = ~ palette_vaccine(reactive_data()$fully_vaccinated_percent),
        fillOpacity = 0.7,
        weight = 0.2,
        smoothFactor = 0.2,
        highlightOptions = highlightOptions(
          weight = 2,
          fillOpacity = 1,
          color = "gray",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = palette_vaccine,
        values = reactive_data()$fully_vaccinated_percent,
        position = "bottomright",
        title = "Vaccine Rate",
        opacity = 0.7
      ) %>%
      setView(lat = 36.778259, lng = -119.417931, zoom = 5)
  })
  
  output$deaths <- renderLeaflet({
    palette_deaths <- colorBin(palette = "Reds", 6, domain = counties_merged$death_rate)
    
    
    labels <- sprintf(
      "<strong>%s</strong><br/>death rate: %g <br/>total deaths: %g",
      reactive_data()$county,
      round(reactive_data()$death_rate, 2),
      reactive_data()$deaths
    ) %>%
      lapply(HTML)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = reactive_data(),
        label = labels,
        fillColor = ~ palette_deaths(reactive_data()$death_rate),
        fillOpacity = 0.7,
        weight = 0.2,
        smoothFactor = 0.2,
        highlightOptions = highlightOptions(
          weight = 2,
          fillOpacity = 1,
          color = "gray",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = palette_deaths,
        values = reactive_data()$death_rate,
        position = "bottomright",
        title = "Death Rate",
        opacity = 0.7
      ) %>%
      setView(lat = 36.778259, lng = -119.417931, zoom = 5)
  })
  
 
}

shinyApp(ui = ui, server = server)
