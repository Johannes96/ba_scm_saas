
# Establish connection with SQL-Database
con_saas <- dbConnect(RSQLite::SQLite(), paste0(getwd(), "/data/saas_database.db"))

# Define SELECT statements
str_SQL_Adr <- "SELECT * FROM Adresses"
str_SQL_Saas <- "SELECT * FROM SaaSData"
str_SQL_Exp <- "SELECT * FROM expenses"
str_SQL_Opp <- "SELECT * FROM Opportunities"

# Query first table and wrangle data
saas_adr <- dbGetQuery(con_saas, str_SQL_Adr) %>%
  tbl_df() %>%
  mutate(latitude = as.numeric(gsub(pattern = ",", replacement = ".", latitude))) %>% #set column as numeric
  mutate(longitude = as.numeric(gsub(pattern = ",", replacement = ".", longitude))) %>% 
  mutate(City = trimws(City), Country = trimws(Country)) #trim leading and trailing whitespaces

# Get second table, wrangle data and left join it with adresses
saas_data <- dbGetQuery(con_saas, str_SQL_Saas) %>%
  tbl_df() %>%
  mutate(Period = dmy(Period)) %>% #convert to dateformat
  mutate(PositionPrice = as.numeric(gsub(pattern = ",", replacement = ".", PositionPrice))) %>%
  mutate(PositionSum = as.numeric(gsub(pattern = ",", replacement = ".", PositionSum))) %>%
  mutate(ARR = as.numeric(gsub(pattern = ",", replacement = ".", ARR))) %>%
  mutate(CustomerIndustry = case_when(CustomerIndustry == "Transportation and Traffic" ~ "Transport and Traffic",
                                      CustomerIndustry == "Pharmaceuticals" ~ "Pharmaceutical",
                                      CustomerIndustry == "Finance Insurance and Legal" ~ "Finance, Insurance and Legal",
                                      CustomerIndustry == "Telecommunication" ~ "Information Technology and Telecommunication",
                                      CustomerIndustry == "Information Technology" ~ "Information Technology and Telecommunication",
                                      TRUE ~ CustomerIndustry)) %>%
  left_join(x = ., y = saas_adr, by = "CustomerID")

# Get third table expenses
saas_exp <- dbGetQuery(con_saas, str_SQL_Exp)


# Get fourth table opportunities
saas_opp <- dbGetQuery(con_saas, str_SQL_Opp) %>%
  tbl_df() %>%
  mutate(ARR = as.numeric(gsub(pattern = ",", replacement = ".", ARR))) %>%
  mutate(Value = as.numeric(gsub(pattern = ",", replacement = ".", Value)))


#Disconnet DB

dbDisconnect(con_saas)


# Data for map ------------------------------------------------------------

#Read shape file with the rgdal library.

world_spdf <- readOGR(
  paste0(getwd(),"/data/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")
)


# Clean the data object
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

# Check if all countries in saas_data exist in world_spdf
lapply(unique(saas_data$Country), function(country) paste(country, country %in% world_spdf@data$NAME))

# Replace wrongly named countries
world_spdf@data <- world_spdf@data %>%
  mutate(NAME = case_when(NAME == "Czech Republic" ~ "Czechia",
                            NAME == "United States" ~ "USA",
                            TRUE ~ NAME))

# Data for clustering -----------------------------------------------------

data_clustering <- saas_data %>%
  group_by(CustomerID, CustomerName, CustomerIndustry, SalesChannel, SalesTyp, latitude, longitude) %>%
  summarise(PositionQuantity = sum(PositionQuantity),
            PositionSum = sum(PositionSum),
            ARR = sum(ARR)) %>%
  ungroup()

# dataset for results of clustering
clustering_results <- data_clustering

# k-means data
kmeans_results <- NULL




  
