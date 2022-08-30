#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(glue)




county_data <- read_csv("https://raw.githubusercontent.com/kayshavprakash/censusdatabycounty/main/county_complete.csv")
election_data <- read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv") %>%
  filter(state_name == "Wisconsin") %>%
  mutate(per_gop = per_gop * 100) %>%
  mutate(per_dem = per_dem * 100)
covid_data <- read_csv("https://uwmadison.box.com/shared/static/liwyil13uxzuzk6dafus1csqbv2gu4pa.csv")
county_data <- county_data %>%
  filter(state == "Wisconsin") %>%
  select(name, contains("2019"))
wisconsin <- read_sf("https://raw.githubusercontent.com/cs320-wisc/f21/main/p6/counties.geojson")
cities <- read_sf("https://uwmadison.box.com/shared/static/obmuiyxr9fhaip6y70z5aam6fd5qpuxj.geojson")
colleges <- read_sf("https://uwmadison.box.com/shared/static/tz78ndsy4e4guo6pg5lv3snzf42yxdlj.geojson")




wisconsin_joined <- wisconsin %>%
  full_join(election_data, by = c("NAME" = "county_name")) %>%
  mutate(per_point_diff = per_point_diff * -100) %>%
  full_join(covid_data, by = c("NAME" = "NAME")) %>%
  full_join(county_data, by = c("NAME" = "name"))
colleges <- colleges %>%
  filter(STATE == "WI") %>%
  filter(grepl("UNIVERSITY OF WISCONSIN", NAME)) %>%
  filter(NAME != "UNIVERSITY OF WISCONSIN COLLEGES FLEX") %>%
  filter(NAME != "UNIVERSITY OF WISCONSIN-SYSTEM ADMINISTRATION")


locations <- c("College Campuses", "Major Cities")
colors <- c("2020 Election Results", "Fully Vaccinated")

p_popup <- glue("<strong>{wisconsin_joined$NAME}</strong><br />
                    Percent GOP: {scales::comma(wisconsin_joined$per_gop,accuracy=1)}%  Percent DEM: {scales::comma(wisconsin_joined$per_dem,accuracy=1)} %<br />
                    Percent 1st Dose: {wisconsin_joined$DOSE_ONE_PERC}%  Percent Both Doses: {wisconsin_joined$DOSE_COMPLETE_PERC}%<br />
                    Age over 65: {wisconsin_joined$age_over_65_2019}% <br />
                    Age under 5: {wisconsin_joined$age_under_5_2019}% <br />
                    Median Age: {wisconsin_joined$median_age_2019} <br />
                    African-American: {wisconsin_joined$black_2019}% <br />
                    Bachelors Degree: {wisconsin_joined$bachelors_2019}%  High School Diploma: {wisconsin_joined$hs_grad_2019}% <br />
                    Uninsured: {wisconsin_joined$uninsured_2019}% <br />
                    Unemployed: {wisconsin_joined$unemployment_rate_2019}% <br />") %>% lapply(htmltools::HTML)


pal_election <- colorNumeric(
  palette = "RdBu",
  domain = c(-65, 65)
)





pal_fullvax <- colorNumeric(
  palette = "RdYlGn",
  domain = wisconsin_joined$DOSE_COMPLETE_PERC
)




# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Visualizing Possible Factors for Vaccine Hesitancy in Wisconsin"),
  selectInput("color", "Select A Color Scheme ", colors),
  selectInput("locations", "Select Locations to label", locations),
  mainPanel(
    h3("Data as of April 2022"),
    h4("Click on the map for more demographic information about each county"),
    leafletOutput("map")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  map <- reactive({
    if (input$color == "2020 Election Results") {
      if (input$locations == "Major Cities") {
        leaflet(wisconsin_joined) %>%
          addPolygons(
            color = "#444444", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.5, popup = p_popup, fillColor = ~ pal_election(wisconsin_joined$per_point_diff), highlightOptions = highlightOptions(
              color = "white", weight = 2,
              bringToFront = TRUE
            )
          ) %>%
          addMarkers(data = cities, popup = cities$name) %>%
          addLegend("bottomright",
            pal = pal_election, values = ~wisconsin_joined$per_point_diff,
            title = "Margin of Victory (2020)",
            opacity = 1, labFormat = labelFormat(suffix = "%",transform=function(x){abs(x)})
          ) %>%addLegend(position="topright", colors=c( '#8b0000','#00008B'), labels=c("Republican","Democrat"))

      } else {
        leaflet(wisconsin_joined) %>%
          addPolygons(
            color = "#444444", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.5, popup = p_popup, fillColor = ~ pal_election(wisconsin_joined$per_point_diff), highlightOptions = highlightOptions(
              color = "white", weight = 2,
              bringToFront = TRUE
            )
          ) %>%
          addMarkers(data = colleges, popup = colleges$NAME) %>%
          addLegend("bottomright",
            pal = pal_election, values = ~ wisconsin_joined$per_point_diff,
            title = "Margin of Victory (2020)",
            opacity = 1, labFormat = labelFormat(suffix = "%",transform=function(x){abs(x)})
          )%>%addLegend(position="topright", colors=c( '#8b0000','#00008B'), labels=c("Republican","Democrat"))
      }
    } else if (input$color == "Fully Vaccinated") {
      if (input$locations == "Major Cities") {
        leaflet(wisconsin_joined) %>%
          addPolygons(
            color = "#444444", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.5, popup = p_popup, fillColor = ~ pal_fullvax(wisconsin_joined$DOSE_COMPLETE_PERC), highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
          ) %>%
          addMarkers(data = cities, popup = cities$name) %>%
          addLegend("bottomright",
            pal = pal_fullvax, values = ~ wisconsin_joined$DOSE_COMPLETE_PERC,
            title = "Percent Fully Vaccinated (April 2022)",
            opacity = 1, labFormat = labelFormat(suffix = "%")
          )
      } else {
        leaflet(wisconsin_joined) %>%
          addPolygons(
            color = "#444444", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.5, popup = p_popup, fillColor = ~ pal_fullvax(wisconsin_joined$DOSE_COMPLETE_PERC), highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
          ) %>%
          addMarkers(data = colleges, popup = colleges$NAME) %>%
          addLegend("bottomright",
            pal = pal_fullvax, values = ~ wisconsin_joined$DOSE_COMPLETE_PERC,
            title = "Percent Fully Vaccinated (April 2022)",
            opacity = 1, labFormat = labelFormat(suffix = "%")
          )
      }
    }
  })

  output$map <- renderLeaflet({
    map()
  })
}
# Run the application
shinyApp(ui = ui, server = server)
