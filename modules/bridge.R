

#probe <- dcast(saas_data, CustomerID ~ Period)

saas_data_temp <- saas_data %>% select(CustomerID, Period, CustomerIndustry, SalesChannel, SalesTyp, ARR)
saas_data_temp <- saas_data_temp %>% 
                  group_by(CustomerID, CustomerIndustry, SalesChannel, SalesTyp, Period) %>%
                  summarise(TotalARR = sum(ARR)) %>%
                  ungroup()

bridge <- pivot_wider(saas_data_temp, names_from = Period, values_from = TotalARR) 

bridge[is.na(bridge)] <- 0

#bridge[,4] = bridge[,2]-bridge[,3]

bridge$"Dif_2020-09" <- (bridge$"2020-09-01" - bridge$"2020-10-01")

bridge$"Change_2020-09" <- ifelse(bridge$"2020-09-01" > bridge$"2020-10-01", "Expansion",
                     ifelse(bridge$"2020-09-01" < bridge$"2020-10-01", "Contraction", "NoChange"))