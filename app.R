
# setup

library(bslib)
library(DT)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(openxlsx)
library(sf)
library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# import data

load("ice_data.RData")

# define UI     

ui <- navbarPage(
  title = paste0("ICE in Pennsylvania, 2026 - Updated ", 
                 format(datecheck, "%m/%d/%Y"),
                 " - FOR INTERNAL USE ONLY - DO NOT SHARE"),
  # set theme
  theme = shinytheme("cosmo"),
  # set HTML tags style
  tags$style(type = "text/css",
             # for moving "No Data" box to bottom of legend
             "div.info.legend.leaflet-control br {clear: both;}",
             "#leafletMap {height: calc(87vh) !important;}"),
  # law enforcement activity map with coalition resource overlays
  tabPanel("Election Threats Dashboard",
           # map sidebar
           sidebarLayout(
             sidebarPanel(
               bsCollapse(id = "sidebar",
                          bsCollapsePanel(title = "Map Controls",
                                          style = "primary",
                                          pickerInput(
                                            inputId = "ice",
                                            label = "Law Enforcement Activity",
                                            choices = c("ICE Field Offices",
                                                        "ICE Detention Facilities",
                                                        "287(g) Agreements",
                                                        "FBI Offices"),
                                            selected = c("ICE Field Offices",
                                                         "ICE Detention Facilities",
                                                         "287(g) Agreements",
                                                         "FBI Offices"),
                                            multiple = TRUE,
                                            options = pickerOptions(actionsBox = TRUE,
                                                                    selectAllText = NULL,
                                                                    selectedTextFormat = "count > 2")
                                          ),
                                          div(style = "margin: auto; width: 90%;",
                                              sliderInput(
                                                inputId = "time_287g",
                                                label = "287(g) Dates Signed",
                                                min = min(data_ice_point$signed, na.rm = TRUE),
                                                max = max(data_ice_point$signed, na.rm = TRUE),
                                                value = c(min(data_ice_point$signed, na.rm = TRUE),
                                                          max(data_ice_point$signed, na.rm = TRUE)),
                                                ticks = FALSE,
                                                timeFormat = "%D"
                                              ),
                                              ifelse("max_daily_population_last_year" %in% colnames(data_ice_point),
                                                     sliderInput(
                                                       inputId = "capacity_detention",
                                                       label = "Detention Facility Max Recorded Capacity, Past 365 Days",
                                                       min = 0,
                                                       max = max(data_ice_point$max_daily_population_last_year, na.rm = TRUE),
                                                       value = max(data_ice_point$max_daily_population_last_year, na.rm = TRUE),
                                                       ticks = FALSE
                                                     ),
                                                     "")
                                          ),
                                          pickerInput(
                                            inputId = "demographic",
                                            label = "Background Demographic",
                                            choices = c(# "Total ICE (??) Arrests",       ##### EDIT
                                              "Electoral Races",
                                              "Total Population",
                                              "Total Population Non-White",
                                              "Total Population Hispanic or Latino",
                                              #            "Proportion Non-White (%)",
                                              #            "Proportion Hispanic or Latino (%)",
                                              "None"),
                                            selected = "Total Population",
                                            multiple = FALSE
                                          ),
                                          #pickerInput(
                                          #  inputId = "elections",
                                          #  label = "Ballot Return Outlets",
                                          #  choices = c("County Election Offices",
                                          #              "Satellite Election Offices",
                                          #              "Drop Boxes"),
                                          #  selected = c("County Election Offices",
                                          #               "Satellite Election Offices",
                                          #               "Drop Boxes"),
                                          #  multiple = TRUE,
                                          #  options = pickerOptions(actionsBox = TRUE,
                                          #                          selectAllText = NULL,
                                          #                          selectedTextFormat = "count > 2")
                                          #),
                                          pickerInput(
                                            inputId = "geography",
                                            label = "Geography",
                                            choices = c("Counties",
                                                        "Municipalities",
                                                        "US House Districts",
                                                        "PA Senate Districts",
                                                        "PA House Districts", 
                                                        "None"),
                                            selected = "Counties",
                                            multiple = FALSE
                                          ),
                                          actionButton(
                                            inputId = "popup_clear",
                                            label = "Clear All Popups",
                                            width = "100%"
                                          )
                          ),
                          bsCollapsePanel(title = "Summary Readout",
                                          style = "info",
                                          htmlOutput("summary")),
                          multiple = TRUE,
                          open = c("Map Controls", "Summary Readout")
               ),
               width = 3
             ),
             mainPanel = card(
               tabsetPanel(
                 tabPanel("Map",
                          leafletOutput(outputId = "leafletMap",
                                        width = "98.5%"),
                 ),
                 tabPanel("Law Enforcement Activity Table",
                          br(),
                          downloadBttn(outputId = "csv_ice_point",
                                       label = "Export as .CSV",
                                       style = "bordered",
                                       color = "primary",
                                       size = "sm"),
                          br(),
                          br(),
                          DTOutput("table_ice_point",
                                   width = "98.5%"),
                          br()
                 ),
                 tabPanel("Background Demographics Table",
                          br(),
                          downloadBttn(outputId = "csv_polygons",
                                       label = "Export as .CSV",
                                       style = "bordered",
                                       color = "primary",
                                       size = "sm"),
                          br(),
                          br(),
                          DTOutput("table_polygons",
                                   width = "98.5%"),
                          br()
                 ),
                 selected = "Map",
                 type = "pills"
               ),
               full_screen = TRUE
             ),
             fluid = TRUE
           )
  ),
  # mail ballot aggregator
  tabPanel("Mail Ballot Aggregator"),           ######### EDIT
  # about page
  tabPanel("About",
           htmlOutput("about")
  )
)

server <- function(input, output) {
  
  # user selects forms of ICE activity
  data_ice_point_filtered <- reactive({
    data_ice_point %>%                      ##### REVISIT AFTER NEW ICE DATA
      filter(icetype %in% input$ice) %>%
      { if(any("287(g) Agreements" %in% .$icetype)) 
        filter(., (.$signed >= input$time_287g[1] & .$signed <= input$time_287g[2]) | 
                 is.na(.$signed)) 
        else
          . } %>%
      { if(any("ICE Detention Facilities" %in% .$icetype) & "capacity_detention" %in% names(input)) 
        filter(., .$max_daily_population_last_year <= input$capacity_detention | 
                 is.na(.$max_daily_population_last_year)) 
        else
          . } %>%
      mutate(icetype_color = case_when(icetype == "ICE Field Offices" ~ "#eb301e",
                                       icetype == "ICE Detention Facilities" ~ "#9c4deb",
                                       icetype == "287(g) Agreements" ~ "orange",
                                       icetype == "FBI Offices" ~ "#95b9ed",
                                       .default = "black")) %>%
      arrange(icetype) %>%
      return()
  }) %>%
    bindEvent(input$ice, 
              input$time_287g,
              input$capacity_detention,
              ignoreNULL = FALSE)
  
  # create HTML tags for ICE labels and popups
  tags_ice <- reactive({
    # ICE field office tags
    case_when(data_ice_point_filtered()$icetype == "ICE Field Offices" ~                 
                paste0(ifelse(is.na(data_ice_point_filtered()$supervising_office),
                              paste0("<b>ICE Field Office:</b> ", data_ice_point_filtered()$name),
                              paste0("<b>ICE Field Office:</b> ", data_ice_point_filtered()$name, " ", data_ice_point_filtered()$type,
                                     "<br>
                                     <b>Supervising Office:</b> ", data_ice_point_filtered()$supervising_office)),
                       "<br>
                       <b>Agency:</b> ", data_ice_point_filtered()$agency),
              # ICE detention facility tags
              data_ice_point_filtered()$icetype == "ICE Detention Facilities" ~ 
                paste0("<b>Detention Facility:</b> ", data_ice_point_filtered()$name,
                       "<br>
                       <b>Facility Code:</b> ", data_ice_point_filtered()$detention_facility_code,
                       "<br>
                       <b>County:</b> ", data_ice_point_filtered()$county,
                       "<br>
                       <b>Detention Stats, Past 365 Days:</b>
                       <p style = 'margin-left: 3px;'>
                       - Days with At Least One Detention: ", ifelse("days_with_detentions_daily_last_year" %in% colnames(data_ice_point_filtered()),
                                                                     ifelse(is.na(data_ice_point_filtered()$days_with_detentions_daily_last_year),
                                                                            "No Data",
                                                                            data_ice_point_filtered()$days_with_detentions_daily_last_year),
                                                                     "No Data"),
                       "<br>
                       - Average Daily Detention Population: ", ifelse("average_daily_population_last_year" %in% colnames(data_ice_point_filtered()),
                                                                       case_when(round(data_ice_point_filtered()$average_daily_population_last_year) >= 1 ~
                                                                                   format(round(data_ice_point_filtered()$average_daily_population_last_year), big.mark = ","),
                                                                                 round(data_ice_point_filtered()$average_daily_population_last_year) < 1 ~
                                                                                   " < 1",
                                                                                 .default = "No Data"),
                                                                       "No Data"),
                       "<br>
                       - Max Daily Detention Population: ", ifelse("max_daily_population_last_year" %in% colnames(data_ice_point_filtered()),
                                                                   ifelse(is.na(data_ice_point_filtered()$max_daily_population_last_year),
                                                                          "No Data",
                                                                          format(data_ice_point_filtered()$max_daily_population_last_year, big.mark = ",")),
                                                                   "No Data"),
                       "</p>"),
              # 287(g) agreement tags
              data_ice_point_filtered()$icetype == "287(g) Agreements" ~ 
                paste0("<b>287(g) Agreement:</b> ", data_ice_point_filtered()$name,
                       "<br>
                       <b>County:</b> ", data_ice_point_filtered()$county,
                       "<br>
                       <b>Partnership Model:</b> ", data_ice_point_filtered()$type,
                       "<br>
                       <b>Date Signed:</b> ", data_ice_point_filtered()$signed_formatted,
                       "<br>
                       <b>MOA:</b> ", case_when(data_ice_point_filtered()$moa == "link" ~ paste0("<a href = ", data_ice_point_filtered()$moa_link, ">Linked</a>"),
                                                data_ice_point_filtered()$moa == "link pending" ~ "Link Pending",
                                                is.na(data_ice_point_filtered()$moa) ~ "None Listed",
                                                .default = data_ice_point_filtered()$moa),
                       "<br>
                       <b>Addendum:</b> ", case_when(data_ice_point_filtered()$addendum == "link" ~ paste0("<a href = ", data_ice_point_filtered()$addendum_link, ">Linked</a>"),
                                                     data_ice_point_filtered()$addendum == "link pending" ~ "Link Pending",
                                                     is.na(data_ice_point_filtered()$addendum) ~ "None Listed",
                                                     .default = data_ice_point_filtered()$addendum)),
              # FBI office tags
              data_ice_point_filtered()$icetype == "FBI Offices" ~ 
                paste0("<b>FBI Office:</b> ", data_ice_point_filtered()$name,
                       "<br>
                       <b>Office Type:</b> ", data_ice_point_filtered()$type,
                       ifelse(data_ice_point_filtered()$type == "Resident Agency",
                              paste0("<br>
                                     <b>Supervising Office:</b> ", data_ice_point_filtered()$supervising_office,
                                     "<br>"),
                              "<br>"),
                       "<b>Coverage Area:</b> ", data_ice_point_filtered()$coverage_area),
              .default = "") %>%
      # ensure output always has length > 0 even if no ice data selected for display 
      { if(length(.) == 0) paste0("No Data") else . } %>%
      # render text as HTML
      lapply(HTML)
  }) %>%
    bindEvent(data_ice_point_filtered())
  
  # user selects geography
  data_polygons_filtered <- reactive({
    data_polygons %>%
      filter(geotype %in% input$geography) %>%
      { if(input$demographic == "None")
        select(., name, geometry)
        else 
          select(., name, input$demographic, geometry) %>%
          arrange(input$demographic) } %>%
      return()
  }) %>%
    bindEvent(input$geography,
              input$demographic)
  
  # update geography color scheme with demographic selection
  census_pal <- reactive({
    if(input$demographic == "Electoral Races") {
      colorFactor(palette = c("salmon", "lightgray"),
                  levels = sort(unique(data_polygons_filtered()[[2]])),
                  na.color = "lightgray")
    } else if(input$demographic == "ACLU-PA Priority/Risk") {
      colorFactor(palette = c("purple", "orange", "beige"),
                  levels = unique(data_polygons_filtered())[[2]],
                  na.color = "lightgray")
    } else if(grepl("Population|Proportion", input$demographic) &
              !all(is.na(data_polygons_filtered()[[2]]))) {
      colorNumeric(palette = "Blues",
                   domain = data_polygons_filtered()[[2]],
                   na.color = "lightgray") 
    } else if(input$demographic == "None") {
      "lightgray"
    } else {
      "lightgray"
    }
  }) %>%
    bindEvent(data_polygons_filtered())
  
  # create HTML tags for geography labels and popups
  tags_geo <- reactive({
    paste0("<b>Geography:</b> ", data_polygons_filtered()$name,
           "<br>
           <b>", case_when(input$demographic == "Electoral Races" ~ "Contest Type: ",
                           input$demographic == "None" ~ "",
                           .default = paste0(input$demographic, ": ")), "</b>",
           if(input$demographic == "None") { "" }
           else if(is.numeric(data_polygons_filtered()[[2]])) { ifelse(is.na(data_polygons_filtered()[[2]]),
                                                                       "No Data",
                                                                       format(data_polygons_filtered()[[2]], big.mark = ",")) }
           else if(is.character(data_polygons_filtered()[[2]])) { ifelse(is.na(data_polygons_filtered()[[2]]),
                                                                         "No Data",
                                                                         data_polygons_filtered()[[2]]) }) %>%      ##### FIX DEFUNCT COLUMNS
      # render text as HTML
      lapply(HTML)
  }) %>%
    bindEvent(data_polygons_filtered())
  
  # generate leaflet map
  output$leafletMap <- renderLeaflet({
    # base map includes only aspects that don't change dynamically
    leaflet(data = basemap) %>%
      setView(lng = basemap$pa_center_lng,
              lat = basemap$pa_center_lat, 
              zoom = 8) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addTiles(urlTemplate = paste0("https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png?key=", carto_api),
               attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, 
                              &copy; <a href="https://carto.com/attributions">CARTO</a>',
               group = "backgroundMap") %>%
      addMapPane("polygons", zIndex = 410) %>%
      addMapPane("polylines", zIndex = 415) %>%
      addMapPane("markers", zIndex = 420) %>%
      addPolylines(color = "black",
                   weight = 2,
                   options = pathOptions(pane = "polylines"),
                   group = "state_border")               ### Add functionality to hide this? And to hide background map?
  })
  
  proxy <- leafletProxy("leafletMap")
  
  # generate ice markers
  observe({
    proxy %>%
      clearGroup("icemarkers") %>%
      removeControl("icelegend") %>%
      addCircleMarkers(data = data_ice_point_filtered() %>%
                         pull(geometry),
                       radius = 5,
                       stroke = TRUE,
                       color = "black",
                       weight = 3,
                       opacity = 0.8,
                       fill = TRUE,             
                       fillColor = data_ice_point_filtered()$icetype_color,
                       fillOpacity = 0.8,
                       label = tags_ice(),
                       labelOptions = labelOptions(direction = "left"),
                       popup = tags_ice(),
                       popupOptions = popupOptions(autoClose = FALSE,
                                                   direction = "left", ##### EDIT - CURRENTLY NOT DOING ANYTHING
                                                   closeOnClick = FALSE),
                       group = "icemarkers",
                       options = pathOptions(pane = "markers")) %>%
      { if(length(input$ice) > 0)
        addLegend(map = .,
                  data = data_ice_point_filtered(),
                  position = "bottomright",
                  title = "Law Enforcement Type",
                  pal = colorFactor(palette = unique(data_ice_point_filtered()$icetype_color),     #### FIX ORDER OF LEGEND OPTIONS
                                    domain = data_ice_point_filtered()$icetype),
                  values = ~ icetype,
                  layerId = "icelegend") }
  }) %>%
    bindEvent(data_ice_point_filtered(), input$popup_clear)  ## debug first time clicking clear all popups, then integrate other data
  
  # generate background geography
  observe({
    # define palette
    pal <- census_pal()
    # include exception for background demographic "None"
    pal_full <- if(class(pal) == "function") {
      # if census_pal() returns a function,
      # input census data to generate palette for fillColor
      pal(data_polygons_filtered()[[2]])
      # if census_pal() returns single-value character vector,
      # return single value for fillColor
    } else {
      pal
    }
    # build background map
    proxy %>%
      clearGroup(., "geoshapes") %>%
      removeControl("geolegend") %>%
      addPolygons(data = data_polygons_filtered(),    
                  stroke = TRUE,
                  color = "black",
                  weight = 1,
                  fill = TRUE,          
                  fillColor = pal_full,
                  fillOpacity = 0.7,
                  label = tags_geo(),
                  labelOptions = labelOptions(direction = "top"),
                  popup = tags_geo(),
                  popupOptions = popupOptions(autoClose = FALSE,
                                              closeOnClick = FALSE),
                  group = "geoshapes",
                  options = pathOptions(pane = "polygons")) # %>%
    # { if(input$geography != "None") 
    #    addLegend(map = .,
    #             data = data_polygons_filtered(),
    #              position = "bottomright",
    #             title = "Contest Type",
    #              pal = colorFactor(palette = unique(data_polygons_filtered()$contest_color),
    #                               domain = data_polygons_filtered()$contest),
    #              values = ~ contest,
    #              layerId = "geolegend") }
  }) %>%
    bindEvent(data_polygons_filtered(), input$popup_clear)
  
  # clear popups on click
  observe({
    proxy %>%
      clearPopups()
  }) %>%
    bindEvent(input$popup_clear)
  
  # create table of select ice activity
  data_ice_point_filtered_export <- reactive({
    data_ice_point_filtered() %>%
      mutate(MOA = ifelse(!is.na(moa_link),
                          paste0("<a href=", moa_link, ">", str_to_title(moa), "</a>"),
                          str_to_title(moa))) %>%
      mutate(Addendum = ifelse(!is.na(addendum_link),
                               paste0("<a href=", addendum_link, ">", str_to_title(addendum), "</a>"),
                               str_to_title(addendum))) %>%
      select(-c("county_fips_code",
                "moa",
                "moa_link",
                "addendum",
                "addendum_link",
                "icetype_color",
                "signed_formatted")) %>%
      rename(any_of(c("Type" = "icetype",
                      "Model or Subtype" = "type",          ###### COMBINE AGENCY WITH UNIT PARTNER TYPE
                      "County" = "county",
                      "Unit Name" = "name",
                      "Unit Partner Type" = "lea_type",
                      "ICE Agency" = "agency",
                      "Supervising Office" = "supervising_office",
                      "Address" = "address",
                      "City" = "city",
                      "State" = "state",
                      "ZIP" = "zip",
                      "Detention Facility Code" = "detention_facility_code",
                      "Days with Detentions, Past 365 Days" = "days_with_detentions_daily_last_year",
                      "Average Daily Detention Population, Past 365 Days" = "average_daily_population_last_year",
                      "Max Daily Detention Population, Past 365 Days" = "max_daily_population_last_year",
                      "Date Signed" = "signed",
                      "Coverage Type" = "coverage_geotype",
                      "Coverage Area" = "coverage_area"))) %>%
      st_drop_geometry()
  })
  
  # render table of select ice activity
  output$table_ice_point <- renderDT({
    datatable(data_ice_point_filtered_export(),         
              rownames = FALSE,
              options = list(pageLength = 50,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'white'});",
                               "}")
              ),
              # read HTML instead of escaping to normal string
              escape = FALSE
    ) %>%
      formatRound(columns = names(which(sapply(data_ice_point_filtered_export(), is.numeric))),
                  digits = 0)
  })
  
  # download handler for select ice activity
  output$csv_ice_point <- downloadHandler(
    filename = function() {
      paste0("lea-activity-", datecheck, ".csv")
    },
    content = function(file) {
      write.csv(data_ice_point_filtered_export() %>%
                  # remove HTML tags from columns with hyperlinks in export file
                  mutate(MOA = str_remove_all(MOA, "^\\<a href=|\\>.*$")) %>%
                  mutate(Addendum = str_remove_all(Addendum, "^\\<a href=|\\>.*$")),
                file,
                row.names = FALSE)
    }
  )
  
  # create table of select background demographics
  data_polygons_filtered_export <- reactive({
    data_polygons %>%
      filter(geotype %in% input$geography) %>%
      select(-c(geotype,
                estimate_B01001H_001,
                starts_with("moe_"),             ##### REVISIT
                county)) %>%
      rename(Geography = name) %>%
      st_drop_geometry()
  })
  
  # render table of select background demographics
  output$table_polygons <- renderDT({
    datatable(data_polygons_filtered_export(),
              rownames = FALSE,
              options = list(pageLength = 50,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'white'});",
                               "}")
              ),
              # read HTML instead of escaping to normal string
              escape = FALSE
    ) %>%
      formatRound(columns = 3:5,
                  digits = 0,
                  mark = ",")
  })
  
  # download handler for select background demographics
  output$csv_polygons <- downloadHandler(
    filename = function() {
      paste0("background-", datecheck, ".csv")
    },
    content = function(file) {
      write.csv(data_polygons_filtered_export(),
                file,
                row.names = FALSE)
    }
  )
  
  # generate summary readout tab
  output$summary <- renderText({
    paste0("<b>ICE Field Offices Displayed:</b> ", nrow(filter(data_ice_point_filtered(), icetype == "ICE Field Offices")),
           "<br>
            <b>ICE Detention Facilities Displayed:</b> ", nrow(filter(data_ice_point_filtered(), icetype == "ICE Detention Facilities")),
           "<br>
            <b>287(g) Agreements Displayed:</b> ", nrow(filter(data_ice_point_filtered(), icetype == "287(g) Agreements")),
           "<br>
            <b>FBI Offices Displayed:</b> ", nrow(filter(data_ice_point_filtered(), icetype == "FBI Offices")))
  })
  
  output$about <- renderText({
    paste0("<style>
           .title {
              font-size: 20px;
              margin-bottom: 0;
           }
           </style>
           
          <p class = title><b>CONCEPT</b></p>
          This app visualizes ICE activity and other potential election threats across Pennsylvania, 
          overlaid on background demographics of community racial composition and political competitiveness.
          <br>
          
          <br><p class = title><b>CONTENT</b></p>
          This app contains two dashboards, one for ICE activity and election threats and the other for
          ACLU-PA volunteer infrastructure.
          <p></p>
          <b>1. ICE Dashboard</b>
          <br>The map visualization shows one dot for each ballot cast but uncounted in the 2024 Pennsylvania general election. Each dot is located at the
          mailing address at which the voter requested the ballot. For this reason, some dots appear outside of Pennsylvania. These dots represent ballots
          requested by Pennsylvanians residing out of state around the time of the election.
          <p></p>
          <p style = 'margin-left: 15px;'>
            <b> — <i>Law Enforcement Activity</i></b> selects the kind of law enforcement activity to visualize in points on the map.
            Options include ICE field offices (in red), ICE detention centers (in purple), 287(g) agreements between ICE and local
            law enforcement (in orange), and FBI offices (in blue).
            <br><b> — <i>Background Demographic</i></b> selects the demographic to display. Users can turn off the demographic display by deselecting 'Show
            Demographic' in the 'Map Aesthetics' menu. The app takes its racial demographic categories from the American Community Survey, adminstered
            by the US Census Bureau.
            <br><b> — <i>287(g) Dates Signed</i></b> subsets all 287(g) agreements by the date each participating local law
            enforcement agency signed its agreement with ICE. Available 287(g) agreements span dates from ____ to ____.
            <br><b> — <i>Background Demographic</i></b> selects the demographic to display for the selected background geography. 
            Options include a geography's electoral race characterization (Standard, Competitive, or No Election), total population,
            total non-white population, and total Hispanic or Latino population.
            <br><b> — <i>Geography</i></b> selects which background geography to map. Options include Pennsylvania counties, 
            municipalities, US House Districts, General Assembly Senate Districts, and General Assembly House Districts.
            
            NOTE ON HAND GEOCODING FROM GOOGLE MAPS
          <br>
          <p></p>
            <p style = 'margin-left: 15px;'>
            <b>MAP CLICKABILITY</b>
            <br>Users can access quick geographically-specific demographic statistics by clicking on geographies of interest. To close a popup, click the
            selected geography again. To close all popups, click the 'Clear All Popups' button in the control panel.
            </p>
          <b>2. MAPPED VOTER CONTACTS</b>
          <p style = 'margin-left: 15px;'>
            This widget produces a table with the name, mailing address, contact information, and county of registration of each voter in the current selection
            of ballots. Users can search for specific names and addresses in the search bar at the widget's upper right-hand corner.
          <br>
          </p>
          <b>3. COUNTY-LEVEL READOUT</b>
          <p style = 'margin-left: 15px;'>
            This widget produces a summary table with the county-level totals of ballots in the current selection.
          <br>
          </p>
          <b>4. STATEWIDE READOUT</b>
          <p style = 'margin-left: 15px; margin-bottom: 0;'>
            This widget produces several statewide summary statistics of ballots in the current selection, including the raw number of ballots in the current selection
            and the percentage of selected ballots in the total set of uncounted mail ballots (excluding those marked 'PEND - NOT YET RETURNED' and 'CANC - LABEL
            CANCELLED'). The widget also breaks down the total set of uncounted mail ballots into three broad categories: all those canceled ('CANC - '), pending
            ('PEND - '), and other ('NO SURE CODE - ').
          </p>
          
          <br><p class = title><b>ATTRIBUTION</b></p>
          This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.
          <br>ICE Field Offices, Sub-Offices, Detention Centers: 'government data published by ICE, collated by the Deportation Data Project, and analyzed by [your organization].'
          <br>
          
          <br><p class = title><b>AUTHOR</b></p>
          Jack Starobin, Voting Rights Litigation Associate, ACLU-PA 2026
          <br>
          <br>")
  })
  
}

shinyApp(ui = ui, server = server)