
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
Periods <- ordered(saas_data$Period, levels = c("2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01", "2022-03-01"))
Industries <- ordered(saas_data$CustomerIndustry, levels = c("Information Technology", "Professional Services and Agencies", "Health", "Telecommunication", "Information Technology and Telecommunication", "Finance, Insurance and Legal", "State and Administration", "Manufacturing", "Pharmaceutical", "Transport and Traffic", "Retailing", "Automotive", "Transportation and Traffic", "Energy and Water", "Pharmaceuticals", "Finance", "Media and Culture", "Food", "Education and Non-Profit", "Legal", "Public", "Other", "Real Estate", "Finance Insurance and Legal"))

dbDisconnect(con_saas)
