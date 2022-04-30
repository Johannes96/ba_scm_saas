
# establish connection with SQL-Database
con_saas <- dbConnect(RSQLite::SQLite(), paste0(getwd(), "/data/saas_database.db"))

# define SELECT statements
str_SQL_Adr <- "SELECT * FROM Adresses"
str_SQL_Saas <- "SELECT * FROM SaaSData"

# query first table and wrangle data
saas_adr <- dbGetQuery(con_saas, str_SQL_Adr) %>%
  tbl_df() %>%
  mutate(latitude = as.numeric(gsub(pattern = ",", replacement = ".", latitude))) %>% #set column as numeric
  mutate(longitude = as.numeric(gsub(pattern = ",", replacement = ".", longitude))) %>% 
  mutate(City = trimws(City)) #trim leading and trailing whitespaces

# get second table, wrangle data and left join it with adresses
saas_data <- dbGetQuery(con_saas, str_SQL_Saas) %>%
  tbl_df() %>%
  mutate(Period = dmy(Period)) %>% #convert to dateformat
  mutate(ContractStart = dmy(ContractStart)) %>% 
  mutate(PositionPrice = as.numeric(gsub(pattern = ",", replacement = ".", PositionPrice))) %>%
  mutate(PositionSum = as.numeric(gsub(pattern = ",", replacement = ".", PositionSum))) %>%
  mutate(ARR = as.numeric(gsub(pattern = ",", replacement = ".", ARR))) %>%
  left_join(x = ., y = saas_adr, by = "CustomerID")

# Define values for filter
Periods <- ordered(saas_data$Period, levels = unique(saas_data$Period))
Industries <- ordered(saas_data$CustomerIndustry, levels = unique(saas_data$CustomerIndustry))

dbDisconnect(con_saas)
