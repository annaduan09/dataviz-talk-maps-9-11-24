# Maps for 9/1/2024 Philly Dataviz Meetup Talk
# Anna Duan
# annaduan@sas.upenn.edu

#### 1 SET UP ####
# Required libraries
library(tidyverse) #data manipulation & visualization
library(sf) #spatial data manipulation
library(tidycensus) #census api access
library(mapview) #preview spatial layers
library(tigris) #census geometries
library(conflicted) #manage package conflicts
conflict_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)


#### 2 DATA PROCESSING #### 

##### American Community Survey (Census) ##### 
# Poverty, income, unemployment
# Source: US Census Bureau
# Variable names reference: https://api.census.gov/data/2022/acs/acs5/variables.html
acs_vars_list <- load_variables(2022, "acs5", cache = TRUE)

census <- get_acs(geography = "tract", 
                  survey = "acs5",
                  variables = c("B01003_001", #pop
                                "B17020_002", #poverty
                                "B19013_001", #medhhinc
                                # MALE
                                "B23001_006", #16-19, workforce
                                "B23001_008", #16-19, unemployed
                                "B23001_013", #20-21, workforce
                                "B23001_015", #20-21, unemployed
                                "B23001_020", #22-24, workforce
                                "B23001_022", #22-24, unemployed
                                "B23001_027", #25-29, workforce
                                "B23001_029", #25-29, unemployed
                                "B23001_034", #30-34, workforce
                                "B23001_036", #30-34, unemployed
                                "B23001_041", #35-44, workforce
                                "B23001_043", #35-44, unemployed
                                "B23001_048", #45-54, workforce
                                "B23001_050", #45-54, unemployed
                                "B23001_055", #55-59, workforce
                                "B23001_057", #55-59, unemployed
                                "B23001_062", #60-61, workforce
                                "B23001_064", #60-61, unemployed
                                "B23001_069", #62-64, workforce
                                "B23001_071", #62-64, unemployed
                                # FEMALE
                                "B23001_092", #16-19, workforce
                                "B23001_094", #16-19, unemployed
                                "B23001_099", #20-21, workforce
                                "B23001_101", #20-21, unemployed
                                "B23001_106", #22-24, workforce
                                "B23001_108", #22-24, unemployed
                                "B23001_113", #25-29, workforce
                                "B23001_115", #25-29, unemployed
                                "B23001_120", #30-34, workforce
                                "B23001_122", #30-34, unemployed
                                "B23001_127", #35-44, workforce
                                "B23001_129", #35-44, unemployed
                                "B23001_134", #45-54, workforce
                                "B23001_136", #45-54, unemployed
                                "B23001_141", #55-59, workforce
                                "B23001_143", #55-59, unemployed
                                "B23001_148", #60-61, workforce
                                "B23001_150", #60-61, unemployed
                                "B23001_155", #62-64, workforce
                                "B23001_157"), #62-64, unemployed
                  state = 42, 
                  county = 101,
                  geometry = TRUE,
                  output = 'wide',
                  year = 2022) %>%
  mutate(pop = B01003_001E,
         poverty_pct = ifelse(pop == 0, NA, B17020_002E / pop),
         medhhinc = B19013_001E,
         unemployed_f = B23001_094E + B23001_101E + B23001_108E + B23001_115E + B23001_122E + B23001_129E + B23001_136E + B23001_143E + B23001_150E + B23001_157E,
         unemployed_m = B23001_008E + B23001_015E + B23001_022E + B23001_029E + B23001_036E + B23001_043E + B23001_050E + B23001_057E + B23001_064E + B23001_071E,
         unemployed_pct = ifelse(pop == 0, NA, (unemployed_f + unemployed_m) / pop)) %>%
  erase_water() %>%
  st_transform("EPSG:2272")

##### Crime #####
# Variables: Shooting victims
# Source: Philadelphia Police District via OpenDataPhilly.org
# Link: https://metadata.phila.gov/#home/datasetdetails/5719551277d6389f3005a610/representationdetails/5719551277d6389f3005a614/
shootings <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings&filename=shootings&format=geojson&skipfields=cartodb_id") %>%
  filter(year == 2024) %>%
  select() %>%
  st_transform("EPSG:2272")

##### Amenities #####
# Source: Yelp API, Philly Parks and Rec via OpenDataPhilly
# Link: Yelp API workflow notebook (in this repo), https://metadata.phila.gov/#home/datasetdetails/5dc1aeb93741fa001504b10b/representationdetails/5dc1aeb93741fa001504b10f/
# Kids activities
kids_activities <- st_read("data/kids_activities.geojson") %>%
  select() %>%
  st_transform("EPSG:2272") %>%
  st_crop(census)

# Restaurants
restaurants <- st_read("data/restaurants.geojson") %>%
  select() %>%
  st_transform("EPSG:2272") %>%
  st_crop(census)

# Parks 
parks <- st_read("https://opendata.arcgis.com/datasets/d52445160ab14380a673e5849203eb64_0.geojson") %>%
  select() %>%
  st_transform("EPSG:2272")

##### Section 8 ##### 
# ZIP groups and rent limits
# Source: Philadelphia Housing Authority
# Link: https://www.pha.phila.gov/wp-content/uploads/2023/09/HCV-SAFMR-Payment-Standard-Schedule-effective-October-1-2023.pdf
voucher_limits <- data.frame(
  zip_code = c(19120, 19124, 19126, 19132, 19133, 19134, 19136, 19139, 19140, 19141, 19142, 19143, 19151, # Group 1
               19101, 19104, 19105, 19109, 19110, 19111, 19112, 19114, 19115, 19116, 19119,  # Group 2
               19121, 19122, 19131, 19135, 19137, 19138, 19144, 19145, 19148, 19149, 19150, 19152,
               19125, 19128, 19129, 19153, 19154,  # Group 3
               19118, 19127, 19146, 19147, # Group 4
               19102, 19103, 19106, 19107, 19123, 19130), # Group 5
  safmr = rep(c(1, 2, 3, 4, 5), times = c(13, 23, 5, 4, 6)),
  safmr_label = c(rep("Basic", 13), rep("Traditional", 23), rep("Mid Range", 5), rep("Opportunity", 4), rep("High Opportunity", 6)),
  cost_sro = c(rep(828, 13), rep(990, 23), rep(1197, 5), rep(1449, 4), rep(1584, 6)),
  cost_0br = c(rep(1104, 13), rep(1320, 23), rep(1596, 5), rep(1932, 4), rep(2112, 6)),
  cost_1br = c(rep(1236, 13), rep(1476, 23), rep(1776, 5), rep(2160, 4), rep(2352, 6)),
  cost_2br = c(rep(1476, 13), rep(1764, 23), rep(2124, 5), rep(2580, 4), rep(2820, 6)),
  cost_3br = c(rep(1788, 13), rep(2136, 23), rep(2568, 5), rep(3120, 4), rep(3408, 6)),
  cost_4br = c(rep(2064, 13), rep(2460, 23), rep(2964, 5), rep(3600, 4), rep(3936, 6)),
  cost_5br = c(rep(2373, 13), rep(2829, 23), rep(3408, 5), rep(4140, 4), rep(4526, 6)),
  cost_6br = c(rep(2683, 13), rep(3198, 23), rep(3853, 5), rep(4680, 4), rep(5116, 6)),
  cost_7br = c(rep(2992, 13), rep(3567, 23), rep(4297, 5), rep(5220, 4), rep(5707, 6)),
  cost_8br = c(rep(3302, 13), rep(3936, 23), rep(4742, 5), rep(5760, 4), rep(6297, 6))
)

# Voucher households 
vouchers <- st_read("data/vouchers_2023.csv") %>%
  select(GEOID) %>%
  group_by(GEOID) %>%
  summarise(vouchers = n())

#### 3 MAPPING PREP ####
# Tract geometries
phila_bounds <- tracts(state = '42', county = '101', year = 2023) %>% #PA FIPS = 42, Philadelphia FIPS = 101
  erase_water() %>% #great tigris package feature for clipping water features
  st_transform("ESPG:2272")

# rent limits
rent_limits <- zctas(year = 2020) %>% #PA FIPS = 42
  filter(substr(ZCTA5CE20, 1, 3) == 191) %>% #Philadelphia ZIPs start with 191
  mutate(zip_code = as.numeric(ZCTA5CE20)) %>%
  left_join(voucher_limits, by = "zip_code") %>%
  erase_water() %>%
  st_transform("EPSG:2272")

# voucher households
voucher_hh <- vouchers %>%
  full_join(tracts(state = '42', county = '101', year = 2023), by = c("GEOID" = "GEOID")) %>%
  st_as_sf() %>%
  st_transform("EPSG:2272") %>%
  filter(!is.na(GEOID)) %>%
  select(GEOID, vouchers)

# relevel safmr_label
rent_limits$safmr_label <- factor(rent_limits$safmr_label, levels = c("Basic", "Traditional", "Mid Range", "Opportunity", "High Opportunity"))

## Basemap 
basemap <- states(cb = TRUE) %>% 
  st_transform("EPSG:2272") %>%
  filter(NAME %in% c("New Jersey", "Pennsylvania")) %>%
  st_crop(st_buffer(census, 1000)) %>% #buffer around tracts, then crop NJ and PA for basemap
  erase_water() 

## Background rectangle
water_rect <- st_as_sfc(st_bbox(basemap)) %>%
  st_as_sf() %>%
  st_transform("EPSG:2272")

#### 4 MAPPING ####
##### Shootings #####
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = census, fill = "gray85", color = "gray90") +
  geom_sf(data = shootings, aes(color = "1 Shooting Victim"), alpha = 0.5, size = 2) +
  scale_color_manual(name = "", values = "orange") + 
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        text = element_text(color = "gray40"))

ggsave("shooting_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')
  
  
##### Amenities ##### 
# Restaurants
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = census, fill = "gray85", color = "gray90") +
  geom_sf(data = restaurants, aes(color = "1 restaurant"), alpha = 0.3, size = 2) +
  scale_color_manual(name = "", values = "darkcyan") + 
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        text = element_text(color = "gray40"))

ggsave("restaurant_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')


# Kids activities
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = census, fill = "gray85", color = "gray90") +
  geom_sf(data = kids_activities, aes(color = "1 venue"), alpha = 0.3, size = 1) +
  scale_color_manual(name = "", values = "salmon") + 
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        text = element_text(color = "gray40"))

ggsave("kids_activities_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')

# Parks
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = census, fill = "gray85", color = "gray90") +
  geom_sf(data = parks, aes(fill = "green space"), color = "transparent") +
  scale_fill_manual(name = "", values = "olivedrab") +
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        text = element_text(color = "gray40"))

ggsave("parks_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')

##### Demographics ##### 
# Poverty
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = census, aes(fill = poverty_pct*100), color = "transparent") +
  scale_fill_distiller(palette = 'YlOrBr',
                      breaks = c(20, 40, 60),
                      labels = c("20%", "40%", "60%"),
                      direction = 1,
                      na.value = "gray95") +
  labs(fill = "Percent below\npoverty line") +
  theme_void() +
  theme(legend.position = c(0.7, 0.1),
        legend.direction = "horizontal")

ggsave("poverty_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')

# Unemployment
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = census, aes(fill = unemployed_pct*100), color = "transparent") +
  scale_fill_distiller(palette = 'YlOrBr',
                      breaks = c(3, 6, 9, 12),
                      labels = c("3%", "6%", "9%", "12%"),
                      direction = 1,
                      na.value = "gray95") +
  labs(fill = "Percent unemployed") +
  theme_void() +
  theme(legend.position = c(0.7, 0.1),
        legend.direction = "horizontal")

ggsave("unemployment_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')

# Median household income
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = census, aes(fill = medhhinc), color = "transparent") +
  scale_fill_distiller(palette = 'YlGnBu',
                      breaks = c(40000, 80000,  120000, 160000),
                      labels = c("$40k", "$80k", "$120k", "$160k"),
                      direction = 1,
                      na.value="gray95") +
  labs(fill = "Household Income ($)") +
  theme_void() +
  theme(legend.position = c(0.7, 0.1),
        legend.direction = "horizontal")

ggsave("income_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')

##### Section 8 #####
# Rent limits
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = rent_limits %>% filter(!is.na(cost_2br)), aes(fill = as.factor(cost_2br)), color = "gray95") +
  scale_fill_brewer(palette = 'YlGnBu',
                    labels = c("$1,476", "$1,764", "$2,124", "$2,580", "$2,820"),
                    direction = 1) +
  labs(fill = "2-bedroom\nvoucher limit") +
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        legend.direction = "vertical",
        text = element_text(color = "gray40"))

ggsave("rent_limits_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')

# Opportunity and high opportunity areas
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = census, fill = "gray85", color = "gray90") +
  geom_sf(data = rent_limits %>% filter(safmr %in% c(4, 5)), aes(fill = "Opportunity ZIPs"), color = "transparent") +
  scale_fill_manual(values = "darkcyan", name = "") +
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        legend.direction = "vertical",
        text = element_text(color = "gray40"))

ggsave("opportunity_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')

# Voucher households
ggplot() +
  geom_sf(data = water_rect, color = "transparent", fill = "lightblue2") +
  geom_sf(data = basemap, fill = "gray95", color = "gray90") +
  geom_sf(data = voucher_hh, aes(fill = vouchers), color = "gray95") +
  scale_fill_distiller(palette = 'YlGnBu',
                    direction = 1,
                    na.value = "gray85") +
  labs(fill = "Voucher\nHouseholds") +
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        legend.direction = "vertical",
        text = element_text(color = "gray40"))

ggsave("voucher_hh_map.png", last_plot(), width = 8, height = 8, dpi = 'retina')
