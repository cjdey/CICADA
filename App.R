#setwd("C:/Users/DeyC/Desktop/Cody Work/GLLFAS Round 2/Projects/CE Geodatabase/Shiny Tool/CEGDB_App")

#Load required packages 
############
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
require(shinycssloaders)
library(DT)
require(leaflet)
require(sf)
require(readxl)
require(htmltools)
require(raster)
require(shinyjs)
require(viridisLite)
############

ui <- navbarPage(

  #UI global settings 
  ############
  useShinyjs(),
  theme = shinytheme("simplex"),
  title = "Beta version: CICADA (CumulatIve effeCts spAtial DAta tool)",
  ############

  #About tab
  ############
  tabPanel(
    "About",
    p(
      "This tool compiles, summarizes and
      displays spatial information on fish and fish habitat in freshwaters across Canada, in support of DFO decision-making."
    ),
    p(
      strong(
        "This tool is currently in development and should not be used to inform decision making at this time"
      )
    ),
    hr(),
    p(
      "Development of this tool is being led by the Chu Lab in DFO's Ecosystems and Oceans Science Sector, in partnership with
      other DFO Science groups, external researchers and the Fish and Fish Habitat Protection Program. Funding to support this tool is currently
      provided by DFO's Competitive Science Research Fund."
    ),
    p(
      "For inquiries related to this project please contact Cindy Chu (cindy.chu@dfo-mpo.gc.ca). For questions or issues related to this app please contact Cody Dey (cody.dey@dfo-mpo.gc.ca)"
    ),
    p(
      "This tool relies on data shared by variety of internal and external data providers including: the Government of Ontario,
      the Government of British Columbia, the Government of Alberta, the Government of British Columbia, ESRI, Microsoft, Natural Resources Canada, Statistics Canada,
      Dr. Nick Mandrak (University of Toronto), Dr. David Theobald (Colorado State University) et al., and the Canadian Wildlife Federation.      
      "
    )
  ),
  ############ 

  #Interactive map tab
  ############
  tabPanel(
    "Interactive map",
    tags$style("
        #controls {
          opacity: 0.4;
        }
        #controls:hover{
          opacity: 0.9;
        }
               "),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    #  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    #  leafletOutput("map", width = "100%", height = "100%"),

    # page header
    p("View spatial data on fish and fish habitat for a site in Canada and the watershed in which it sits"),
    p(
      "Enter the site of interest below in decimal degrees",

      a(href = "http://rcn.montana.edu/Resources/Converter.aspx", " (convert from other units)", target = "_blank"),
      "or click on the map"
    ),

    # first row of Map tab, is a header with the input boxes and buttons
    sidebarLayout(
      sidebarPanel(
        width = 3,
        wellPanel(
          "Step 1 - Enter a location",
          textInput(inputId = "focal_y", label = "Latitude (°N)"),
          textInput(inputId = "focal_x", label = "Longitude (°E)"),
          actionButton(inputId = "check_watershed", label = "List available watersheds")
        ),
        wellPanel(
          "Step 2 - Select a watershed unit",
          selectInput(
            inputId = "watershed_scale",
            label = NULL,
            choices = c("[complete step 1 above]")
          ),
          actionButton(inputId = "go", label = "Generate map")
        )
      ),
      mainPanel(
        width = 9,
        # main section of Interactive map tab (i.e. the map)

        shinycssloaders::withSpinner(leafletOutput("map", height = "100%")),

        # floating panel with toggles
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 300,
          left = "auto",
          right = 50,
          bottom = "auto",
          width = 200,
          height = "auto",
          h3("Map layers"),
          materialSwitch("fish_presencedata", label = "Fish presence", value = FALSE),
          materialSwitch("hab_wq", label = "Water quality", value = FALSE),
          materialSwitch("an_barriers", label = "Aquatic barriers", value = FALSE),
          materialSwitch("an_humfoot", label = "Landscape modification", value = FALSE),
          materialSwitch("critical_habitat", label = "Critical habitat", value = FALSE)
        ) # end of floating panel
      ) # end of main panel
    ) # end of sideBarLayout
  ),
  ############ 

#Methods tab
#############

tabPanel(
  "Data and Methods",
  
  p(strong("Watershed units"),
    br(),
    em("Map Layer"),
    br(),br(),
    " Four sets of watershed units are available:",br(),br(),
    "National Hydrological Network (NHN) Tertiary Watersheds. National Coverage, Produced by Natural Resources Canada",
    br(),
    "Ontario (ON) Quaternary Watersheds. Provincial Coverage, Produced by Ontario Ministry of Natural Resources and Forestry",br(),
    "British Columbia (BC) Freshwater Atlas watersheds. Provincial Coverage, Produced by BC Ministry of Agriculture and Lands",br(),
    "Alberta (AB) Hydrological Unit Code Level 8 watersheds. Provincial Coverage, Produced by Alberta Environment and Parks"
  ),
  
  br(),
  p(strong("Fish Presence"),
    br(),
    em("Map Layer and Data Tab"),
    br(),br(),
    "Most recent observation of fish species at each sampling site. Based on a compilation of national, provincial and regional datasets conducted by the CICADA project team."
  ),
  
  br(),
  p(strong("Water quality"),
    br(),
    em("In development - Map Layer and Data Tab"),
    br(),br(),
    "Data on 11 water quality parameters compiled from national, provincial and regional datasets conducted by the CICADA project team.",
    br(),br(),
    "Currently implemented - 2015-2020 medians or averages for water temperature, chloride, conductivity, nitrates, total phosphorus and dissolved oxygen within areas of Alberta and Ontario only",
  ),
  
  br(),
  p(strong("Aquatic barriers"),
    br(),
    em("Map Layer and Data Tab"),
    br(),br(),
    "Dams, waterfall and fishways compiled in the Canadian Aquatic Barrier Database by the Canadian Wildlife Federation."
  ),
  
  br(),
  p(strong("Landscape Modification"),
    br(),
    em("Map Layer"),
    br(),br(),
    "This dataset visualizes degree of human modification on a scale of 0 (low modification) to 1 (high modification). 
  These values are based on a variety of landscape stressors including urban and built-up areas, crop and pasture lands, livestock grazing, oil and gas production, mining and quarrying, power generation (renewable and nonrenewable), roads, railways, power lines and towers, logging and wood harvesting, human intrusion, reservoirs, and air pollution.
  These data were assembled and analyzed by Theobald et. al. (2020) in", 
  em("Earth transformed: detailed mapping of global human modification from 1990 to 2017."), "Earth Systems Science Data 12, p 1953–1972.",
  "Resolution is 0.09 km2.  The dataset has been clipped by watershed unit for web-based display."
  ),
  
  br(),
  p(strong("Critical Habitat"),
    br(),
    em("Map Layer and Data Tab"),
    br(),br(),
    "Designated critical habitat for fish and mussels listed on Schedule 1 of the Species At Risk Act. Spatial data from DFO's publically available Species at Risk Critical Habitat and Distribution Map.
  Data for freshwater and diadromous species shown, while marine species are excluded."
  ),
  
  br(),
  p(strong("Fish Species List"),
    br(),
    em("Data Tab"),
    br(),br(),
    "List of species present in the NHN tertiary watershed, based on catch data (e.g. fish presence data described above) and expert opinion"
  ),
  
  br(),
  p(strong("Land cover summary"),
    br(),
    em("Data Tab"),
    br(),br(),
    "Summary of land cover within the watershed area across 9 different land cover types.
   Land cover classifications are conducted at 10m resolution based on Sentinel-2 imagery. Data provided by ESRI and Microsoft."
  )
),
###############

  #Data tabs
  ############
  
  #Data Tab 1
  tabPanel(
    "Fish species list",
    p(
      "List of species present in the NHN tertiary watershed, based on catch data and expert opinion"
    ),
    fluidRow(column(2, ""), column(8, DTOutput("table1")), column(2, "")),
    downloadButton("downloadTable1", "Download species list"),
  ),

  #Data tab 2
  tabPanel(
    "Fish presence",
    p(
      "Most recent capture of each species at each sampling site within the selected watershed"
    ),
    fluidRow(column(12, DTOutput("table2"))),
    downloadButton("downloadTable2", "Download fish sampling data")
  ),

  #Data tab 3
  tabPanel(
    "Water quality",
    p("Chloride (mg/L), conductivity (uS/cm), dissolved oxygen (mg/L), total phosphorus (mg/L) and nitrates (mg/L) data are median values from each site across samples collected between 2015 and 2020"), 
    p("Summer water temperature (degrees C) values are means from July and August over the same 5 year period."),
    p(
      "Water quality thresholds are based on existing guidelines for the protection of aquatic life and are further outlined in Dey et al. 2022. Preliminary assessment of the State of Fish and Fish Habitat in Fisheries and Oceans Canada's Ontario and Prairies Region. Canadian Science Advisory Secretariat Research Document"
    ),
    fluidRow(column(12, DTOutput("table3"))),
    downloadButton("downloadTable3", "Download water quality data")
  ),

  #Data tab 4
  tabPanel(
    "Aquatic barriers",
    p(
      "Dams, waterfalls and fishways occurring in the watershed"
    ),
    fluidRow(column(12, DTOutput("table4"))),
    downloadButton("downloadTable4", "Download barrier data")
  ),

  #Data Tab 5
  tabPanel(
    "Land cover summary",
    p(
      "Percentage of watershed with each land cover type"
    ),
    fluidRow(column(3, ""), column(6, DTOutput("table5")), column(3, "")),
    downloadButton("downloadTable5", "Download land cover values"),
  ),

  #Data Tab 6
  tabPanel(
    "Critical habitat",
    p(
      "Species with designated critical habitat within the selected watershed"
    ),
    fluidRow(column(1, ""), column(10, DTOutput("table6")),column(1, "")),
   
    downloadButton("downloadTable6", "Download species list"),
  )
  ############

) # end of UI


server <- function(input, output, session) {

  #Server globals
  ############
  
  sf_use_s2(FALSE)
  
  ############
  
  #Disable buttons unless conditions are met
  ############
  # disable check watershed unless lat long are entered
  observe({
    if (is.null(input$focal_y) || input$focal_y == "" || is.null(input$focal_x) || input$focal_x == "") {
      shinyjs::disable("check_watershed")
    } else {
      shinyjs::enable("check_watershed")
    }
  })


  # disable go unless focal site is set
  observe({
    if (input$watershed_scale == "[complete step 1 above]") {
      shinyjs::disable("go")
    } else {
      shinyjs::enable("go")
    }
  })
  ############ 
  
  #Load initial map
  ############
  # show base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      fitBounds(
        lat1 = 48.5,
        lng1 = -142,
        lat2 = 80,
        lng2 = -53
      )
  })

  # load watershed boundary layers
  O_QWS <- read_sf("Watershed_Layers/CE_Watershed_Cover_Datasets/ON_quat_wshd_cvr/ONT_WSHED_BDRY_QUAT_CVR.shp")
  O_QWS <- st_transform(O_QWS, crs = 4326)

  HUC8 <- read_sf("Watershed_Layers/CE_Watershed_Cover_Datasets/AB_HUC_8_cvr/AB_HUC_8_cvr.shp")
  HUC8 <- st_transform(HUC8, crs = 4326)

  NHN3 <- read_sf("Watershed_Layers/CE_Watershed_Cover_Datasets/NHN_CVR/NHN_CVR.shp")
  NHN3 <- st_transform(NHN3, crs = 4326)

  BCFWA <- read_sf("Watershed_Layers/CE_Watershed_Cover_Datasets/BC_FWA_wshd_cvr/BC_FWA_WATERSHED_GROUPS_cvr.shp")
  BCFWA <- st_transform(BCFWA, crs = 4326)
  ############ 

  #Clicking on map
  ############
  
  # Create site icon
  siteIcon <- makeAwesomeIcon(
    markerColor = "black",
    iconColor = "black"
  )
  
  # when the map gets clicked, fill in the site lat/lng and add a project marker
  observeEvent(input$map_click, {
    event <- input$map_click
    lat <- round(event$lat, 6)
    lng <- round(event$lng, 6)
    updateTextInput(session, "focal_y", value = lat)
    updateTextInput(session, "focal_x", value = lng)

    leafletProxy("map") %>%
      clearGroup(group = "project_site") %>%
      addAwesomeMarkers(
        group = "project_site", # project site
        lng = lng,
        lat = lat,
        label = "Project site",
        icon = siteIcon
      )
   
  #reset watershed_scale options
    updateSelectInput(session, "watershed_scale", choices = "[complete step 1 above]")
  })
  ############ 

  #Check watershed button 
  ############

  observeEvent(input$check_watershed, {
    showModal(modalDialog("Checking for available watershed layers",
      footer =
        NULL
    ))

    avail_watersheds <- c()

    site_x <- abs(as.numeric(input$focal_x)) * -1
    site_y <- as.numeric(input$focal_y)
    focal_site <- st_sfc(st_point(c(site_x, site_y)), crs = 4326)

    # check if site is covered by each of the watershed layers, and produce a list of the layers that it falls in
    if (any(st_contains(O_QWS, focal_site) %>% lengths() > 0)) {
      avail_watersheds <- append(avail_watersheds, "ON Quaternary Watersheds")
    }

    if (any(st_contains(HUC8, focal_site) %>% lengths() > 0)) {
      avail_watersheds <- append(avail_watersheds, "AB Hydrological Unit Code 8")
    }

    if (any(st_contains(NHN3, focal_site) %>% lengths() > 0)) {
      avail_watersheds <- append(avail_watersheds, "NHN Tertiary Watersheds")
    }

    if (any(st_contains(BCFWA, focal_site) %>% lengths() > 0)) {
      avail_watersheds <- append(avail_watersheds, "BC Freshwater Atlas")
    }

    if (length(avail_watersheds > 0)) {
      updateSelectInput(session, "watershed_scale", choices = avail_watersheds)
    } else {
      updateSelectInput(session, "watershed_scale", choices = c("[no data available]"))
    }

    removeModal()
  }) 
  ############ 
 
  #Generate map and data tables button
  observeEvent(input$go, {
    showModal(
      modalDialog("Preparing data for the project site and watershed",
      footer = NULL
    ))

    #Identify focal watershed
    ############
    sf_use_s2(FALSE)
    site_x <- abs(as.numeric(input$focal_x)) * -1
    site_y <- as.numeric(input$focal_y)
    focal_site <- st_sfc(st_point(c(site_x, site_y)), crs = 4326)

    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      watersheds <- read_sf("Watershed_Layers/CE_Watershed_Datasets/NHN_WSCSSDA_Units/NHN_WSCSSDA_Units.shp")
      watersheds <- st_transform(watersheds, crs = 4326)
      focal_watershed <- watersheds[st_contains(watersheds, focal_site) %>% lengths() > 0, ]
      focal_watershed_ID <- focal_watershed$WSCSSDA[1]
    } else {
      if (input$watershed_scale == "AB Hydrological Unit Code 8") {
        watersheds <- read_sf("Watershed_Layers/CE_Watershed_Datasets/HydrologicUnitCodeWatersheds8OfAlberta/HydrologicUnitCode8WatershedsOfAlberta.shp")
        watersheds <- st_transform(watersheds, crs = 4326)
        focal_watershed <- watersheds[st_contains(watersheds, focal_site) %>% lengths() > 0, ]
        focal_watershed_ID <- focal_watershed$HUC_8[1]
      } else {
        if (input$watershed_scale == "BC Freshwater Atlas") {
          watersheds <- read_sf("Watershed_Layers/CE_Watershed_Datasets/BC_FWA_Watershed_Groups_Poly/BC_FWA_WATERSHED_GROUPS_POLY.shp")
          watersheds <- st_transform(watersheds, crs = 4326)
          focal_watershed <- watersheds[st_contains(watersheds, focal_site) %>% lengths() > 0, ]
          focal_watershed_ID <- str_c("BC_", focal_watershed$WATERSHED_[1])
        } else {
          if (input$watershed_scale == "ON Quaternary Watersheds") {
            watersheds <- read_sf("Watershed_Layers/CE_Watershed_Datasets/OWBQUAT/LIO-2022-10-19/ONT_WSHED_BDRY_QUAT_DERIVED.shp")
            watersheds <- st_transform(watersheds, crs = 4326)
            focal_watershed <- watersheds[st_contains(watersheds, focal_site) %>% lengths() > 0, ]
            focal_watershed_ID <- str_c("ON-", focal_watershed$CODE[1])
          }
        }
      }
    }
    ############ 

    #Prep data layers
    ############

    # fish presence data
    fish_data <- read_csv("Fish data/Nat_Fish_Recent_01-12-2023.csv")

    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      fish_data_clip <- fish_data %>% filter(NHN4DWSH == focal_watershed_ID)
    } else {
      fish_data_clip <- fish_data %>% filter(PRPWSHID == focal_watershed_ID)
    }

    fish_data_sum <- fish_data_clip %>%
      group_by(CE_Site_ID) %>%
      mutate(fish_present = paste0(Common_name, collapse = "<br>")) %>%
      slice(1)

    fish_plot <- st_as_sf(
      x = fish_data_sum,
      coords = c("Longitude", "Latitude"),
      crs = 4269
    )

    fish_plot <- st_transform(fish_plot, crs = 4326)

    fish_labels <- lapply(seq(nrow(fish_data_sum)), function(i) {
      paste0(
        "Site: ", fish_data_sum[i, "CE_Site_ID"], "<br>",
        "Fish Present:", "<br>",
        fish_data_sum[i, "fish_present"]
      )
    })

    #SAR Critical Habitat

    crithab <- read_sf("Critical_habitat/DFO_SARA_CritHab_22_V5/DFO_SARA_CritHab_22.shp")
    
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
	crithab_lookup = read_csv("Critical_habitat/DFO_SARA_CritHab_22_V5/WSHD_Joins/SARA_NHN_Join.csv")
      keeps <- crithab_lookup %>% filter(WSCSSDA_ID == focal_watershed_ID)
	crithab_clip <- crithab %>% filter(SARA_ID %in% keeps$SARA_ID)
    } else {
	crithab_lookup = read_csv("Critical_habitat/DFO_SARA_CritHab_22_V5/WSHD_Joins/SARA_PRV_Join.csv")
      keeps <- crithab_lookup %>% filter(PRPWSHID == focal_watershed_ID)
	crithab_clip <- crithab %>% filter(SARA_ID %in% keeps$SARA_ID)
		} 
    
    crithab_plot <- st_transform(crithab_clip, crs = 4326)
    crithab_clip <- st_drop_geometry(crithab_clip)

    ch_labels <- lapply(seq(nrow(crithab_clip)), function(i) {
      paste0(
        "Critical habitat for ", crithab_clip[i, "Common_Nam"], "<br>",
        "SARA Status: ", crithab_clip[i, "SARA_Statu"]
      )
    })

    #water quality data
   wq<-read_csv("Water quality data/SOFFH_WQ.csv")

    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      wq_clip <- wq %>% filter(NHN_TWS == focal_watershed_ID)
    } else {
      wq_clip <- wq %>% filter(Prov_wsh == focal_watershed_ID)
    }

    wq_plot <- st_as_sf(
      x = wq_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4269
    )

    wq_plot <- st_transform(wq_plot, crs = 4326)

    wq_labels <- lapply(seq(nrow(wq_clip)), function(i) {
      paste0(
        "StationID: ", wq_clip[i, "StationID"], "<br>",
        "Waterbody: ", wq_clip[i, "Station_name"], "<br>",
        "Summer Water Temp (C): ", wq_clip[i, "Summer_Water_Temp"], "<br>",
        "Dissolved oxygen below threshold: ", wq_clip[i, "Dissolved_oxygen_below_threshold"], "<br>",
        "Chloride above threshold: ", wq_clip[i, "Chloride_above_threshold"], "<br>",
        "Conductivity above threshold: ", wq_clip[i, "Conductivity_above_threshold"], "<br>",
        "Total Phosphorus above threshold: ", wq_clip[i, "Total_phosphorus_above_threshold"], "<br>",
        "Nitrates above threshold: ", wq_clip[i, "Nitrates_above_threshold"], "<br>"
      )
    })

    # LULC Sentinel data
    if (input$watershed_scale == "NHN Tertiary Watersheds"){
    LULC <- read_csv("Sentinel_LULC/Sent21_NHN_Summary.csv")} else {
    LULC <- read_csv("Sentinel_LULC/Sent21_PRV_Summary.csv")} 
	
    colnames(LULC)[1]<-"Watershed_ID"   

    LULC_clip = LULC %>% filter(Watershed_ID == focal_watershed_ID) 
    
    LULC_clip = LULC_clip %>% pivot_longer(cols = Water:Clouds, 
                                             values_to = "percent_cover",
                                             names_to = "land_cover_class") %>%
                dplyr::select(-Watershed_ID)

    # CABD dams
    dams <- read_csv("Can_Aquatic_Barrier_database_WSHD/cabddams__WSHD.csv")

    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      dams_clip <- dams %>% filter(NHN4DWSH == focal_watershed_ID)
    } else {
      dams_clip <- dams %>% filter(PRPWSHID == focal_watershed_ID)
    }

    dams_clip <- dams_clip %>% mutate(height_m = ifelse(height_m == 0, NA, height_m))

    dams_plot <- st_as_sf(
      x = dams_clip,
      coords = c("longitude", "latitude"),
      crs = 4269
    )
    
    dams_plot <- st_transform(dams_plot, crs = 4326)

    dam_labels <- lapply(seq(nrow(dams_clip)), function(i) {
      paste0(
        "Dam", "<br>",
        "ID: ", dams_clip[i, "cabd_id"], "<br>",
        "Use: ", dams_clip[i, "dam_use"], "<br>",
        "Height: ", dams_clip[i, "height_m"], "<br>",
        "Passability: ", dams_clip[i, "passabilit"]
      )
    })

    # Theobald Human Modification data
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      fp <- paste0(
        "Human modification/GHM_17_NHN/GHM17_", focal_watershed_ID,
        "/GHM17_", focal_watershed_ID, ".tif"
      )
    } else {
	fp <- paste0(
        "Human modification/GHM_17_PRV/GHM17_", focal_watershed_ID,
        "/", focal_watershed_ID, ".tif"
      )

	} 

    hum <- raster(fp)
    projection(hum) <- "+init=epsg:3857"

    hum_pal <- colorNumeric(
      palette = "magma",
      domain = c(0, 1),
      na.color = NA
    )

    # CABD waterfalls
    wfs <- read_csv("Can_Aquatic_Barrier_database_WSHD/cabd_waterfalls_wshd.csv")

    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      wfs_clip <- wfs %>% filter(NHN4DWSH == focal_watershed_ID)
    } else {
      wfs_clip <- wfs %>% filter(PRPWSHID == focal_watershed_ID)
    }

    wfs_clip <- wfs_clip %>% mutate(fallheight = ifelse(fallheight == 0, NA, fallheight))

    wfs_plot <- st_as_sf(
      x = wfs_clip,
      coords = c("longitude", "latitude"),
      crs = 4269
    )

    wfs_plot <- st_transform(wfs_plot, crs = 4326)

    wfs_labels <- lapply(seq(nrow(wfs_clip)), function(i) {
      paste0(
        "Waterfall", "<br>",
        "ID: ", wfs_clip[i, "cabd_id"], "<br>",
        "Height: ", wfs_clip[i, "fallheight"], "<br>",
        "Passability: ", wfs_clip[i, "passabilit"]
      )
    })

    # CABD fishways
    fishways <- read_csv("Can_Aquatic_Barrier_database_WSHD/cabdfishways_wshd.csv")

    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      fishways_clip <- fishways %>% filter(NHN4DWSH == focal_watershed_ID)
    } else {
      fishways_clip <- fishways %>% filter(PRPWSHID == focal_watershed_ID)
    }

    fishways_clip <- fishways_clip %>% mutate(elevationm = ifelse(elevationm == 0, NA, elevationm))


    fishways_plot <- st_as_sf(
      x = fishways_clip,
      coords = c("longitude", "latitude"),
      crs = 4269
    )

    fishways_plot <- st_transform(fishways_plot, crs = 4326)

    fishways_labels <- lapply(seq(nrow(fishways_clip)), function(i) {
      paste0(
        "Fishway", "<br>",
        "ID: ", fishways_clip[i, "cabd_id"], "<br>",
        "Type: ", fishways_clip[i, "fishpasst1"]
      )
    })
    ############
    
    #Render map
    ############

    fishIcon <- makeIcon(
      iconUrl = "Image_files/fish_green.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    )

    wqIcon <- makeIcon(
      iconUrl = "Image_files/water_drop.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    )

    # fishwayIcon <- makeIcon(
    #   iconUrl = "Image_files/fishway.png",
    #   iconWidth = 20, iconHeight = 20,
    #   iconAnchorX = 10, iconAnchorY = 10)
    #
    # damIcon <- makeIcon(
    #   iconUrl = "Image_files/dam.png",
    #   iconWidth = 20, iconHeight = 20,
    #   iconAnchorX = 10, iconAnchorY = 10)
    #
    # waterfallIcon <- makeIcon(
    #   iconUrl = "Image_files/waterfall.png",
    #   iconWidth = 20, iconHeight = 20,
    #   iconAnchorX = 10, iconAnchorY = 10)

    output$map <- renderLeaflet({
      map <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>% # basemap
        addRasterImage(hum,
          opacity = 0.75,
          colors = hum_pal,
          group = "theobald"
        ) %>%
        addLegend(
         colors =  viridis(11, alpha = 1, begin = 0, end = 1, direction = 1, option = "magma"),
          labels = c("0 - less", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6","0.7","0.8","0.9", "1 - more"),
          title = "Landscape<br>modification"
        ) %>%
        addPolygons(
          data = crithab_plot,
          stroke = 0.05,
          group = "critical_habitat",
          color = "purple",
          label = lapply(ch_labels, htmltools::HTML),
          fillColor = "purple",
          opacity = 0.5,
          fillOpacity = 0.2
        ) %>%
        addPolygons( # focal watershed
          data = focal_watershed,
          stroke = 0.1,
          color = "black",
          fillColor = "transparent"
        ) %>%
        addMarkers( # fish presence
          data = fish_plot,
          group = "fish_presence",
          label = lapply(fish_labels, htmltools::HTML),
          icon = fishIcon
        ) %>%
        addCircleMarkers( # waterfalls
          data = wfs_plot,
          group = "barriers",
          label = lapply(wfs_labels, htmltools::HTML),
          radius = 5,
          weight = 1,
          opacity = 1,
          color = "black",
          fillColor = "orange",
          fillOpacity = 1
        ) %>%
        addCircleMarkers( # fishways
          data = fishways_plot,
          group = "barriers",
          label = lapply(fishways_labels, htmltools::HTML),
          radius = 5,
          weight = 1,
          opacity = 1,
          color = "black",
          fillColor = "yellow",
          fillOpacity = 1
        ) %>%
        addCircleMarkers( # dams
          data = dams_plot,
          group = "barriers",
          label = lapply(dam_labels, htmltools::HTML),
          radius = 5,
          weight = 1,
          opacity = 1,
          color = "black",
          fillColor = "red",
          fillOpacity = 1
        ) %>%
        addMarkers( # water quality
          data = wq_plot,
          group = "hab_wq",
          label = lapply(wq_labels, htmltools::HTML),
          icon = wqIcon
        ) %>%
        addAwesomeMarkers(
          group = "project_site", # project site
          lng = site_x,
          lat = site_y,
          label = "Project site",
          icon = siteIcon
        )

      map
    }) 
    ############

    #Build and render data tables
    ############

    # Table 1 (fish species list)
    

  
data_t1<- reactive({
 if (input$watershed_scale == "NHN Tertiary Watersheds") {
      NHN_splist <- read_csv("Fish data/NHN_TWS_FishSpp_01-13-2023.csv")
      tmp <- NHN_splist %>%
        filter(NHN_TWS == focal_watershed$WSCSSDA[1]) %>%
        dplyr::select(-NHN_TWS)
    } else {
      NHN <- read_sf("Watershed_Layers/CE_Watershed_Datasets/NHN_WSCSSDA_Units/NHN_WSCSSDA_Units.shp")
      NHN <- st_transform(NHN, crs = 4326)
      focal_NHN <- NHN[st_contains(NHN, focal_site) %>% lengths() > 0, ]
      focal_NHN_ID <- focal_NHN$WSCSSDA[1]
      NHN_splist <- read_csv("Fish data/NHN_TWS_FishSpp_01-13-2023.csv")
      tmp <- NHN_splist %>%
        filter(NHN_TWS == focal_NHN_ID) %>%
        dplyr::select(-NHN_TWS)
    }
tmp <- tmp %>% dplyr::select(Common_name, Scientific_name, Species_origin) %>% arrange(Common_name)
tmp
})

    output$table1 <- renderDT(
      {
        data_t1()
      },
      rownames = FALSE,
      options = list(pageLength = 20)
    )

    output$downloadTable1 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_Species_list.csv")
      },
      content = function(file) {
        write_csv(data_t1(), file)
      }
    )


    # Table 2 (fish captures)

    data_t2 <- reactive( {
            tmp = fish_data_clip %>% dplyr::select(
            Common_name,
            Scientific_name,
            CE_Site_ID,
            LastOfDate,
            Latitude,
            Longitude,
            Waterbody_name)
           
           tmp <- tmp %>% arrange(CE_Site_ID, Common_name, LastOfDate)

         colnames(tmp) <-
            c(
              "Common name",
              "Scientific name",
              "Site ID",
              "Last caught",
              "Latitude",
              "Longitude",
              "Waterbody name"
            )
	   tmp$Latitude <-round(tmp$Latitude, 6)
	   tmp$Longitude <-round(tmp$Longitude, 6)
             tmp })


    output$table2 <- DT::renderDataTable(

        {data_t2()}, 

        rownames = FALSE,
        options = list(pageLength = 20)
      )
    

    output$downloadTable2 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_Fish_presence_by_site.csv")
      },
      content = function(file) {
        write_csv(data_t2(), file)
      }
    )


    ## Table 3 - water quality

	data_t3 <- reactive({
        tmp <- wq_clip %>% dplyr::select(-Prov_wsh) %>% dplyr::select(-NHN_TWS)
        tmp <- tmp %>% relocate(Total_phosphorus, .before = Summer_Water_Temp) %>%
                    relocate(Nitrates, .before = Summer_Water_Temp) 
         tmp$Latitude <-round(tmp$Latitude, 6)
	   tmp$Longitude <-round(tmp$Longitude, 6)

	  tmp }) 

    output$table3 <- renderDataTable(
      { data_t3()  },
      rownames = FALSE,
      options = list(pageLength = 20)
    )

    output$downloadTable3 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_Water_quality_by_site.csv")
      },
      content = function(file) {
        write_csv(data_t3(), file)
      }
    )

    ## Table 4 - barriers

	data_t4<-reactive({

tmp_dam <- dams_clip %>% dplyr::select(
          featuretyp, cabd_id, latitude, longitude, height_m, passabilit, dam_use
        )
        names(tmp_dam)[7] <- "known_use"

        tmp_wfs <- wfs_clip %>% dplyr::select(
          featuretyp, cabd_id, latitude, longitude, fallheight, passabilit
        )
        names(tmp_wfs)[5] <- "height_m"

        tmp_fw <- fishways_clip %>% dplyr::select(
          featuretyp, cabd_id, latitude, longitude, elevationm, known_use
        )
        tmp_fw$passabilit <- "assumed"
        names(tmp_fw)[5] <- "height_m"

        sp <- str_replace_all(tmp_fw$known_use, ",", "XXXX")
        sp <- str_replace_all(sp, " \\s*\\([^\\)]+\\)", "")
        sp <- str_replace_all(sp, "[:punct:]", "")
        sp <- str_replace_all(sp, "XXXX", ", ")
        tmp_fw$known_use <- sp

        tmp <- bind_rows(tmp_dam, tmp_fw, tmp_wfs)
        names(tmp) <- c(
          "Barrier_type", "ID", "Latitude", "Longitude", "Height_m",
          "Passability", "Known_use"
        )
        tmp$Latitude <-round(tmp$Latitude, 6)
	   tmp$Longitude <-round(tmp$Longitude, 6)

       tmp
})

    output$table4 <- renderDataTable(
      {
        data_t4()
      },
      rownames = FALSE,
      options = list(pageLength = 20)
    )

    output$downloadTable4 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_aquatic_barriers.csv")
      },
      content = function(file) {
        write_csv(data_t4(), file)
      }
    )


	#Table 5 - LULC summary

	data_t5 <- reactive({
        LULC_clip %>% arrange(-percent_cover)
        })

    output$table5 <- renderDT(
      {
        data_t5()
      },
      rownames = FALSE,
      options = list(pageLength = 15)
    )

    output$downloadTable5 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_landcover_summary.csv")
      },
      content = function(file) {
        write_csv(data_t5(), file)
      }
    )

	#Table 6 - Critical habitat

data_t6<-reactive({
        tmp = crithab_clip %>% 
          dplyr::select(Common_Nam, Scientific, Population, SARA_Statu) %>%
          group_by(Common_Nam) %>% distinct(Population, .keep_all = TRUE) %>% arrange(Common_Nam)
        colnames(tmp)<-c("Common name", "Scientific name", "Population/DU", "SARA Status")
         tmp })

    output$table6 <- renderDT(
      {data_t6()},
      rownames = FALSE,
      options = list(pageLength = 20)
    )

    output$downloadTable6 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "species_with_critical_habitat.csv")
      },
      content = function(file) {
        write_csv(data_t6(), file)
      }
    )



    ############
    
    #Toggle Layers
    ############
    
    observeEvent(input$fish_presencedata, {
      if (input$fish_presencedata == FALSE) {
        leafletProxy("map") %>% hideGroup("fish_presence")
      } else {
        leafletProxy("map") %>% showGroup("fish_presence")
      }
    })
    
    observeEvent(input$an_barriers, {
      if (input$an_barriers == FALSE) {
        leafletProxy("map") %>% hideGroup("barriers")
      } else {
        leafletProxy("map") %>% showGroup("barriers")
      }
    })
    
    observeEvent(input$critical_habitat, {
      if (input$critical_habitat == FALSE) {
        leafletProxy("map") %>% hideGroup("critical_habitat")
      } else {
        leafletProxy("map") %>% showGroup("critical_habitat")
      }
    })
    
    observeEvent(input$an_humfoot, {
      if (input$an_humfoot == FALSE) {
        leafletProxy("map") %>% hideGroup("theobald")
      } else {
        leafletProxy("map") %>% showGroup("theobald")
      }
    })
    
    observeEvent(input$hab_wq, {
      if (input$hab_wq == FALSE) {
        leafletProxy("map") %>% hideGroup("hab_wq")
      } else {
        leafletProxy("map") %>% showGroup("hab_wq")
      }
    })
    
    ############
    
    removeModal()
    
  }
  ) # closes the generate map button
  
} # end of server


shinyApp(ui = ui, server = server)
