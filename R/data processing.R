### libraries ----
require(plyr)
require(dplyr)
require(tidyr)
require(purrr)
require(readr)
require(rgears)
require(sp)
require(rgeos)
require(rgdal)

# environment cleanup
rm(list = ls()); gc()

### directories ----
dir.data <- "./data/raw"
dir.output <- "./data"

### data ----
df.raw_pop <- read_csv(file.path(dir.data, "Population Projection.csv"))
df.raw_MD <- read_csv(file.path(dir.data, "Prevalence Percentage Estimate.csv")) 
spldf.raw_nz <- readOGR(dsn = file.path(dir.data, "shapefile"), layer = "TA2016_GV_Clipped")
df.db_meta <- read_csv(file.path(dir.data, "Observer Control Meta.csv"))

### data processing ----
## population and MD data
df.input_pop <- df.raw_pop %>% 
  change_names(names(df.raw_pop), toupper(names(df.raw_pop))) %>% 
  change_names("YEAR AT 30 JUNE", "YEAR") %>% 
  change_names("AREA", "TA") %>% 
  filter(SEX %in% "Total people") %>% 
  filter(AGE %in% c("Total people, all ages"
                    , "45-49 years"
                    , "50-54 years"
                    , "55-59 years"
                    , "60-64 years"
                    , "65-69 years"
                    , "70-74 years"
                    , "75-79 years"
                    , "80-84 years")) %>% 
  filter(ETHNICITY %in% c("European or Other (including New Zealander)"
                          , "Asian"
                          , "Total New Zealand population")) %>% 
  filter(!TA %in% "Total, New Zealand by territorial authority") %>% 
  select(-SEX, -FLAGS) %>% 
  filter(!grepl("local board area", TA)) %>% 
  filter(!grepl("region", TA)) %>% 
  mutate(TA = gsub("district", "District", TA)) %>% 
  mutate(TA = gsub("city", "City", TA)) %>% 
  mutate(TA = gsub("Wanganui", "Whanganui", TA)) %>% 
  mutate(TA = tolower(TA))


df.input_national <- df.input_pop %>% 
  filter(ETHNICITY %in% "Total New Zealand population") %>%
  filter(AGE %in% "Total people, all ages") %>% 
  change_names("VALUE", "BASE") %>% 
  select(-ETHNICITY, -AGE)


df.db_input <- df.input_pop %>%
  filter(!ETHNICITY %in% "Total New Zealand population") %>%
  filter(!AGE %in% "Total people, all ages") %>%
  left_join(df.raw_MD) %>% 
  mutate(MD = VALUE * PROP_MD) %>% 
  group_by(YEAR, TA, PROJECTION) %>% 
  summarise(MD = sum(MD, na.rm = TRUE),
            VALUE = sum(VALUE, na.rm = TRUE)) %>%
  left_join(df.input_national) %>% 
  mutate(MD_STD = MD / BASE)

## shapefiles
spldf.db_nz <- spldf.raw_nz %>% 
  spTransform(CRS("+init=epsg:4326"))

spldf.db_nz@data <- spldf.db_nz@data %>% 
  select(TA2016_NAM) %>% 
  change_names("TA2016_NAM", "TA") %>% 
  mutate(TA = tolower(TA))

spldf.db_nz <- subset(spldf.db_nz, !TA %in% "chatham islands territory")

spldf.simplify <- gSimplify(spldf.db_nz, topologyPreserve = TRUE, tol = 0.01)
spldf.db_nz@polygons <- spldf.simplify@polygons


### export ----
save(df.db_input, df.db_meta, spldf.db_nz, file = file.path(dir.output, "dashboard input.rda"))



