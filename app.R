
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

load("ice_data_287g_only_public.RData")

# define UI     

ui <- navbarPage(
  title = paste0("287(g) Agreements in Pennsylvania - Updated ", format(datecheck, "%m/%d/%Y")),
  # set theme
  theme = shinytheme("cosmo"),
  # set HTML tags style
  tags$style(type = "text/css",
             # for moving "No Data" box to bottom of legend
             "div.info.legend.leaflet-control br {clear: both;}",
             "#leafletMap {height: calc(87vh) !important;}"),
  # law enforcement activity map with coalition resource overlays
  tabPanel("Dashboard",
           # map sidebar
           sidebarLayout(
             sidebarPanel(
               bsCollapse(id = "sidebar",
                          bsCollapsePanel(title = "Map Controls",
                                          style = "primary",
                                          pickerInput(
                                            inputId = "ice",
                                            label = "Partner Type",
                                            choices = unique(data_ice_point$lea_type),
                                            selected = unique(data_ice_point$lea_type),
                                            multiple = TRUE,
                                            options = pickerOptions(actionsBox = TRUE,
                                                                    selectAllText = NULL,
                                                                    selectedTextFormat = "count > 1")
                                          ),
                                          pickerInput(
                                            inputId = "model",
                                            label = "Partnership Model Type",
                                            choices = unique(data_ice_point$type),
                                            selected = unique(data_ice_point$type),
                                            multiple = TRUE,
                                            options = pickerOptions(actionsBox = TRUE,
                                                                    selectAllText = NULL,
                                                                    selectedTextFormat = "count > 1")
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
                                              )
                                          ),
                                          pickerInput(
                                            inputId = "demographic",
                                            label = "Background Demographic",
                                            choices = c("Total Population",
                                                        "Total Population Non-White",
                                                        "Total Population Hispanic or Latino",
                                                        "None"),
                                            selected = "Total Population",
                                            multiple = FALSE
                                          ),
                                          pickerInput(
                                            inputId = "geography",
                                            label = "Geography",
                                            choices = c("Counties",
                                                        "Municipalities",
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
                          bsCollapsePanel(title = "Total Displayed 287(g) Agreements",
                                          style = "info",
                                          htmlOutput("summary")),
                          multiple = TRUE,
                          open = c("Map Controls", "Total Displayed 287(g) Agreements")
               ),
               width = 3
             ),
             mainPanel = card(
               tabsetPanel(
                 tabPanel("Map",
                          leafletOutput(outputId = "leafletMap",
                                        width = "98.5%"),
                 ),
                 tabPanel("287(g) Selection Table",
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
  # about page
  tabPanel("About",
           htmlOutput("about")
  )
)

server <- function(input, output) {
  
  # user selects types of ICE partner agencies
  data_ice_point_filtered <- reactive({
    data_ice_point %>%                      
      filter(lea_type %in% input$ice) %>%
      filter(type %in% input$model) %>%
      filter(., (.$signed >= input$time_287g[1] & .$signed <= input$time_287g[2]) | 
                 is.na(.$signed)) %>%
      mutate(lea_color = case_when(lea_type == "Police" ~ "#ffdc3b",
                                   lea_type == "Sheriff" ~ "orange",
                                   lea_type == "Constable" ~ "#eb301e",
                                   lea_type == "District Attorney" ~ "maroon",
                                   lea_type == "Jail or Prison" ~ "#9c4deb",
                                   .default = "gray")) %>%
      arrange(lea_type) %>%
      return()
  }) %>%
    bindEvent(input$ice, 
              input$model,
              input$time_287g,
              ignoreNULL = FALSE)
  
  # create HTML tags for ICE labels and popups
  tags_ice <- reactive({
    # 287(g) agreement tags
    case_when(data_ice_point_filtered()$icetype == "287(g) Agreements" ~ 
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
    if(grepl("Population|Proportion", input$demographic) &
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
           <b>", case_when(input$demographic == "None" ~ "",
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
                       fillColor = data_ice_point_filtered()$lea_color,
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
                  title = "Partner Type",
                  pal = colorFactor(palette = unique(data_ice_point_filtered()$lea_color),     #### FIX ORDER OF LEGEND OPTIONS
                                    domain = data_ice_point_filtered()$lea_type),
                  values = ~ lea_type,
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
                  options = pathOptions(pane = "polygons")) %>%
      { if (input$geography == "Counties" & input$demographic != "None") 
        addLegend(map = .,
                  data = data_polygons_filtered(),
                  position = "bottomright",
                  title = input$demographic,
                  pal = pal,
                  values = ~ data_polygons_filtered()[[2]],
                  na.label = "No Data",
                  bins = 5,
                  layerId = "geolegend")
        else
          . }
  }) %>%
    bindEvent(data_polygons_filtered(),
              input$popup_clear)
  
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
      select(-c("icetype",
                "moa",
                "moa_link",
                "addendum",
                "addendum_link",
                "lea_color",
                "signed_formatted",
                "geometry")) %>%
      arrange(county,
              lea_type) %>%
      rename(any_of(c("Partnership Model" = "type",          ###### COMBINE AGENCY WITH UNIT PARTNER TYPE
                      "County" = "county",
                      "Partner Type" = "lea_type",
                      "Partner Name" = "name",
                      "Date Signed" = "signed",
                      "Coverage Type" = "coverage_geotype"))) %>%
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
      arrange(name) %>%
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
      formatRound(columns = 2:4,
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
    paste0("<b>All:</b> ", nrow(data_ice_point_filtered()),
           "<br>
            <b>Police Departments:</b> ", nrow(filter(data_ice_point_filtered(), lea_type == "Police")),
           "<br>
            <b>Sheriff's Offices:</b> ", nrow(filter(data_ice_point_filtered(), lea_type == "Sheriff")),
           "<br>
            <b>Constable's Offices:</b> ", nrow(filter(data_ice_point_filtered(), lea_type == "Constable")),
           "<br>
            <b>District Attorneys:</b> ", nrow(filter(data_ice_point_filtered(), lea_type == "District Attorney")),
           "<br>
            <b>Jails and Prisons:</b> ", nrow(filter(data_ice_point_filtered(), lea_type == "Jail or Prison")))
  })
  
  output$about <- renderText({
    paste0("<style>
           .title {
              font-size: 20px;
              margin-bottom: 0;
           }
           </style>
           
          <p class = title><b>CONCEPT</b></p>
          This app visualizes ICE 287(g) agreements with local law enforcement across Pennsylvania, overlaid on background population demographics.
          <br>
          
          <br><p class = title><b>CONTENT</b></p>
          The app contains three widgets on the Dashboard.
          <p></p>
          <b>1. Map</b>
          <br>The map visualization shows one dot for each 287(g) agreement in Pennsylvania, color coded by partner agency type.
          <p></p>
          <p style = 'margin-left: 15px;'>
            <b> — <i>Partner Type</i></b> selects the kinds of law enforcement agency partnered with ICE to display.
            <br><b> — <i>Partnership Model Type</i></b> selects the kinds of 287(g) partnership model to display.
            <br><b> — <i>287(g) Dates Signed</i></b> subsets all 287(g) agreements by the date each partner agency signed its agreement with ICE. 
            Available 287(g) agreements span dates from ", min(data_ice_point$signed, na.rm = TRUE), " to ", max(data_ice_point$signed, na.rm = TRUE), ".
            <br><b> — <i>Background Demographic</i></b> selects the demographic to display at the given geography level (e.g., county or municipality).
            Demographics are pulled from the US Census Bureau and are available only at the county level for this app. Available demographics include 
            a geography's total population, total non-white population, and total Hispanic or Latino population.
            <br><b> — <i>Geography</i></b> selects which background geography to map. Options include Pennsylvania counties and municipalities.
          <br>
          </p>
          <b>2. 287(g) Selection Table</b>
          <p style = 'margin-left: 15px;'>
            This widget produces a table with the partner agency, partnership model type, and other background information for each 287(g) agreement currently selected.
            Users can download this table as a .CSV file with the button at the widget's upper left-hand corner. Users can search for specific counties, municipalities,
            or other identifying information with the search bar at the widget's upper right-hand corner.
          <br>
          </p>
          <b>3. Background Demographics Table</b>
          <p style = 'margin-left: 15px;'>
            This widget produces a table with the available background demographics for all geographies of the type currently selected.
          </p>
          
          <br><p class = title><b>METHODOLOGY</b></p>
          This app involves two significant manipulations of source data: geocoding 287(g) agreements and computing certain background demographics.
          
          <p></p>
            <p style = 'margin-left: 15px;'>
            <b>Geocoding</b>
            <br>To geocode ICE records of 287(g) agreements with local law enforcement, ICE records were matched with Pennsylvania Department of Transportation 
            records of municipality boundaries and US Census Bureau county boundaries. Where the jurisdiction of an ICE record matched a PA DOT municipality record 
            exactly, the associated 287(g) agreement was geocoded at that municipality's centroid. Where an ICE record named county-wide or intra-county regional 
            jurisdiction, the record's 287(g) agreements was geocoded at the county's centroid or, in cases where the ICE record referenced a compass direction (e.g.,
            'northwest,' 'eastern,' etc.), at a point slightly offset from the centroid in the specificed direction. ICE records which did not match any PA DOT records 
            but which specified jurisdictions at the municipal level were handcoded using coordinates from Google Maps. In total, ", (stats_handcoded$total - stats_handcoded$count),
            " ICE records (", scales::label_percent()(1 - stats_handcoded$prop), ") were handcoded algorithmically and the remaining ", stats_handcoded$count, " records (",
            scales::label_percent()(stats_handcoded$prop), ") were handcoded.
            </p>
            
          <p></p>
            <p style = 'margin-left: 15px;'>
            <b>Demographic Computation</b>
            <br>County-level background demographics for total population and total Hispanic or Latino population were pulled directly from the US Census 
            Bureau's American Community Survey 5-Year Data. County-level background demographics for total non-white population were estimated by subtracting
            the 5-year ACS estimates of 'White Alone, Not Hispanic or Latino' populations from total population estimates for each county.
            </p>
          
          <br><p class = title><b>ATTRIBUTION</b></p>
          Data on 287(g) agreements in Pennsylvania are provided by U.S. Immigrations and Customs Enforcement. For the source file, see the link below:
          <br>
          <a href=https://www.ice.gov/identify-and-arrest/287g>ICE 287(g) Participating Agencies</a>
          <br>
          <p></p>
          This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau. In particular, the app uses
          Census Bureau data for county-level demographics and geometry.
          <br>
          <p></p>
          Data on Pennsylvania municipal boundaries and county FIPS codes, used to identify each municipality's encompassing county or counties, are provided by the 
          Pennsylvania Department of Transportation and accessed through the Commonwealth of Pennsylvania Open Data Portal API. For the source files, see the links below:
          <br>
          <a href=https://data.pa.gov/Geospatial-Data/PA-Municipality-Boundaries/ednf-5bi6/about_data>PA Municipality Boundaries</a>
          <br>
          <a href=https://data.pa.gov/Census-Economic/Federal-Information-Processing-Standard-FIPS-Codes/44ch-j9ei/about_data>PA County FIPS codes</a>
          <br>
          
          <br><p class = title><b>AUTHOR</b></p>
          Jack Starobin
          <br>Voting Rights Litigation Associate
          <br>ACLU-PA, 2026
          <br>
          <br>")
  })
  
}

shinyApp(ui = ui, server = server)