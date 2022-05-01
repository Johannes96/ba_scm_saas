

#probe <- dcast(saas_data, CustomerID ~ Period)

saas_data_temp <- saas_data %>% select(CustomerID, Period, ARR)
saas_data_temp <- saas_data_temp %>% 
                  group_by(CustomerID, Period) %>%
                  summarise(TotalARR = sum(ARR)) %>%
                  ungroup()

bridge <- pivot_wider(saas_data_temp, names_from = Period, values_from = TotalARR) 
