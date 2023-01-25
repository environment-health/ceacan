#' Script to prepare the data for analysis
#'
#' @export
data_prep <- function() {
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)
  sf::sf_use_s2(FALSE)

  # ================================================================================================
  # Data import and format
  ## Geolocated placenames 
  placenames <- importdat("b5433840")[[1]] |>
                dplyr::select(uid = PNuid_NLidu, name = Name_en) |>
                dplyr::mutate(
                  uid = as.character(uid),
                  type = "Geolocated placenames"
                )
                
  ## First Nations location
  fn <- importdat("ce594316")[[1]] |>
        dplyr::select(uid = BAND_NBR, name = BAND_NAME) |>
        dplyr::mutate(
          uid = as.character(uid),
          type = "First Nations"
        ) |>
        sf::st_transform(crs = 4326)
  
  ## Inuit communities location
  ic <- importdat("621e9a76")[[1]] |>
        dplyr::select(uid = ID, name = NAME) |>
        dplyr::mutate(type = "Inuit communities") |>
        sf::st_transform(crs = 4326)
  
  ## Combine 
  uid <- c(
    sf::st_nearest_feature(fn, placenames),
    sf::st_nearest_feature(ic, placenames)
  ) |>
  unique()
  placenames <- dplyr::bind_rows(placenames[-uid, ], fn, ic)
  
  # Alternate plan 
  can <- pipedat:::basemap$can |>
         sf::st_make_valid()

  placenames <- stars::read_stars("data/grid/grid.tif") |>
                as.data.frame() |>
                dplyr::select(x,y) |>
                sf::st_as_sf(coords = c("x","y"), crs = 4326) |>
                dplyr::mutate(uid = 1:dplyr::n())
  
  placenames2 <- placenames[can]
  
  ## Covid data
  covid <- importdat("a56e753b")
  hr <- covid[["covid_timeline_canada-a56e753b-health_regions_wgs84.geojson"]]
  hrpop <- covid[["covid_timeline_canada-a56e753b-health_regions.csv"]] |>
           dplyr::select(hruid, pop) |>
           dplyr::mutate(hruid = as.character(hruid))
           
  ### NOTE: First go at cases and deaths, just take the cumulative total 
  cases <- covid[["covid_timeline_canada-a56e753b-CovidTimelineCanada_hr.csv"]] |>
           dplyr::group_by(name, sub_region_1) |>
           dplyr::summarize(value = max(value)) |>
           dplyr::ungroup()
  uid <- cases$name == "cases"
  deaths <- cases[!uid, ] |>
            dplyr::select(hruid = sub_region_1, deaths = value) |>
            dplyr::mutate(hruid = as.character(hruid))
  cases <- cases[uid, ] |>
           dplyr::select(hruid = sub_region_1, cases = value) |>
           dplyr::mutate(hruid = as.character(hruid))
  
  ## Proximity measures data 
  px <- importdat("ee7295d7")
  px <- px[["proximity_measures_database-ee7295d7-polygons.geojson"]] |>
        dplyr::select(
          -Shape_Length, -Shape_Area, -prov_name, -cd_name, -ccs_name, 
          -cs_name, -er_name, -fed_name, -cma_name, -db_pop,          
          -employment_exists, -pharma_exists, -childcare_exists,
          -health_exists, -grocery_exists, -educpri_exists,
          -educsec_exists, -library_exists, -park_exists,
          -transit_exists
        )
        
  ## Statistics Canada census subdivisions
  sc2021 <- importdat("5e4be996")[[1]]

  ## Census 2021 housing suitability
  hs <- importdat("852db1a3")[[1]] 
  hs <- dplyr::filter(
    hs,
    Housing.suitability..3. != "Total - Housing suitability" &
    Number.of.persons.per.room..3. == "Total - Number of persons per room" &
    Statistics..3C. == "Number of private households" &
    Tenure..4. == "Total - Tenure" &
    Number.of.rooms.and.number.of.bedrooms..12. == "Total - Number of rooms and number of bedrooms"
  ) |>
  dplyr::select(
    DGUID,
    housing_suitability = Housing.suitability..3.,
    household_size = Household.size..8..Total...Household.size.1.,
    average_household_size = Household.size..8..Average.household.size.8.
  ) |>
  tidyr::pivot_wider (
    id_cols = DGUID,
    names_from = housing_suitability,
    values_from = c(household_size, average_household_size)
  ) |>
  dplyr::rename(
    household_size_suitable = household_size_Suitable,
    household_size_not_suitable = "household_size_Not suitable",
    average_household_size_suitable = average_household_size_Suitable,
    average_household_size_not_suitable = "average_household_size_Not suitable"
  ) |>
  dplyr::mutate(
    percent_household_not_suitable = 
      household_size_not_suitable / 
      (household_size_suitable + household_size_not_suitable),
    household_not_suitable = percent_household_not_suitable / 
                             max(percent_household_not_suitable, na.rm = TRUE)
  ) |>
  dplyr::select(
    DGUID, 
    household_not_suitable
  )

  ## Census 2021 dwelling condition
  dc <- importdat("b48b01d6")[[1]]
  dc <- dplyr::filter(
    dc,
    Period.of.construction..13. == "Total - Period of construction" &
    Structural.type.of.dwelling..10. == "Total - Structural type of dwelling" &
    Statistics..3C. == "Number of private households" &
    Dwelling.condition..4. != "Total - Dwelling condition"    
  ) |>
  dplyr::select(
    DGUID,
    dwelling_condition = Dwelling.condition..4.,
    tenure = Tenure..4..Total...Tenure.1.
  ) |>
  tidyr::pivot_wider(
    id_cols = DGUID,
    names_from = dwelling_condition,
    values_from = tenure
  ) |>
  dplyr::rename(
    regular_maintenance_needed = "Regular maintenance needed",
    minor_repairs_needed = "Minor repairs are needed",
    major_repairs_needed = "Major repairs needed"
  ) |>
  dplyr::mutate(
    percent_major_repairs_needed = 
      major_repairs_needed / 
      (regular_maintenance_needed + minor_repairs_needed + major_repairs_needed),
    major_repairs_needed = percent_major_repairs_needed / max(percent_major_repairs_needed, na.rm = TRUE)
  ) |>
  dplyr::select(
    DGUID, 
    major_repairs_needed
  )

  
  ## Census 2021 acceptable housing
  ah <- importdat("f4abec86")[[1]]
  ah <- dplyr::filter(
    ah,
    Residence.on.or.off.reserve..3. == "Total - Residence on or off reserve" &
    Core.housing.need..5. == "Total - Core housing need" &
    Household.type.including.census.family.structure..16. == 
      "Total - Household type including census family structure" &
    Statistics..3C. == "Number of private households" &
    Acceptable.housing..9. != "Total - Acceptable housing"
  ) |>
  dplyr::select(
    DGUID,
    acceptable_housing = Acceptable.housing..9.,
    tenure = Tenure..4..Total...Tenure.1.
  ) |>
  dplyr::mutate(
    acceptable_housing = tolower(stringr::str_replace(acceptable_housing," ","_"))
  ) |>
  tidyr::pivot_wider(
    id_cols = DGUID,
    names_from = acceptable_housing,
    values_from = tenure
  ) 
  uid <- stringr::str_detect(colnames(ah), "below")
  ah$below_thresholds <- rowSums(ah[, uid])
  ah$percent_below_thresholds <- ah$below_thresholds / (ah$below_thresholds + ah$acceptable)
  ah$below_thresholds <- ah$percent_below_thresholds / max(ah$percent_below_thresholds, na.rm = TRUE)
  ah <- dplyr::select(
    ah,
    DGUID,
    below_thresholds
  ) 

  ## National Pollutant Release Inventory
  npri <- importdat("d2f44fdf")[[1]] 
  npri <- dplyr::filter(
    npri,
    SectorDescriptionEn %in% c("Waste Treatment and Disposal", "Water and Wastewater Systems")
  ) |>
  dplyr::select(
    NpriID,
    SectorDescriptionEn    
  ) |>
  unique()
  
  ### Waste treatment and disposal vs Water and Wastewater Systems
  wtd <- dplyr::filter(npri, SectorDescriptionEn == "Waste Treatment and Disposal")
  wws <- dplyr::filter(npri, SectorDescriptionEn == "Water and Wastewater Systems")
  
  ### Health care facilities 
  health <- importdat("8b0bbc44")[[2]]
  type <- read.csv("data/data-raw/open_database_healthcare_facilities-8b0bbc44/classes.csv")
  health <- dplyr::left_join(health, type, by = "source_facility_type")
  critical <- health[health$type %in% "critical",]
  longterm <- health[health$type %in% c("long-term","diagnostics","cnbnl"),]
  # ------------------------------------------------------------------------------------------------
  
  
  # ================================================================================================
  # Unique ids of features to geolocated placenames 
  ## Covid health regions: hruid
  # NOTE: using sf::st_nearest_feature because the coastline in the covid dataset is too coarse
  uid <- sf::st_nearest_feature(placenames, hr)
  placenames$hruid <- hr$hruid[uid]

  ## Statistics Canada census subdivisions 2021
  uid <- sf::st_nearest_feature(
    x = sf::st_transform(placenames, crs = sf::st_crs(sc2021)),
    y = sc2021
  )
  placenames$dguig <- sc2021$DGUID[uid]
  # rm(sc2021) # Big dataset, remove to save memory
  
  ## Proximity measures data 
  uid <- sf::st_nearest_feature(
    x = sf::st_transform(placenames, crs = sf::st_crs(px)),
    y = px
  )
  placenames$pxuid <- px$db_uid[uid]
  # ------------------------------------------------------------------------------------------------
  
    
  # ================================================================================================
  # Join data to placenames 
  ## Proximity dataset
  placenames <- dplyr::left_join(
    placenames, 
    sf::st_drop_geometry(px),
    by = c("pxuid" = "db_uid")
  )
  
  ## Housing suitability 
  placenames <- dplyr::left_join(placenames, hs, by = c("dguig" = "DGUID"))
  
  ## Dwelling condition
  placenames <- dplyr::left_join(placenames, dc, by = c("dguig" = "DGUID"))
  
  ## Acceptable housing
  placenames <- dplyr::left_join(placenames, ah, by = c("dguig" = "DGUID"))
  
  ## Covid cases 
  placenames <- dplyr::left_join(placenames, cases, by = "hruid")

  ## Covid deaths
  placenames <- dplyr::left_join(placenames, deaths, by = "hruid")
  
  ## Covid populations
  placenames <- dplyr::left_join(placenames, hrpop, by = "hruid")
  
  # ------------------------------------------------------------------------------------------------
  
  # ================================================================================================
  # Calculate distances 
  # WARNING: max_dist is completely arbitrary
  dist_calc <- function(pts, max_dist = 25) {
    dat <- sf::st_distance(placenames, pts) |>
           units::set_units("km") |>
           apply(MARGIN = 1, FUN = min)
    # Cap to max dist
    max_dist <- max(dat, na.rm = TRUE)
    dat <- ifelse(dat > max_dist, max_dist, dat) 
    
    # Transform as an index between 0 and 1, with 1 being the farthest, and 0 the closest
    dat <- dat/max_dist
  }
  
  ## Waste treatment and disposal vs Water and Wastewater Systems
  placenames$waste_treatment_disposal <- dist_calc(wtd)
  placenames$water_waster_systems <- dist_calc(wws)
  
  # Health care 
  placenames$critical_healthcare <- dist_calc(critical)
  placenames$longterm_healthcare <- dist_calc(longterm)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ================================================================================================
  ## Latitude 
  coord <- sf::st_coordinates(placenames)
  x <- coord[,"Y"] |> unname()
  x <- x-min(x)
  x <- x/max(x)
  placenames$latitude <- x
  
  ## Select variables of interest 
  placenames <- dplyr::select(
    placenames, 
    -pharma_prox,
    -health_prox,
    -grocery_prox,
    -library_prox,
    -transit_prox,
    -park_prox
  )  
  # ------------------------------------------------------------------------------------------------
  
  # ================================================================================================
  # Export data 
  output <- here::here("data","data-format")
  if (!file.exists(output)) dir.create(output)
  sf::st_write(placenames, here::here(output,"exposure2.gpkg"), delete_dsn = TRUE)
  # ------------------------------------------------------------------------------------------------
}
