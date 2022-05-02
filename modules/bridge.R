

#probe <- dcast(saas_data, CustomerID ~ Period)


# Prepara dataframe -------------------------------------------------------

saas_data_temp <- saas_data %>% select(CustomerID, Period, CustomerIndustry, SalesChannel, SalesTyp, ARR)
saas_data_temp <- saas_data_temp %>% 
                  group_by(CustomerID, CustomerIndustry, SalesChannel, SalesTyp, Period) %>%
                  summarise(TotalARR = sum(ARR)) %>%
                  ungroup()


# Prepare Bridge ----------------------------------------------------------

bridge <- pivot_wider(saas_data_temp, names_from = Period, values_from = TotalARR) 

bridge[is.na(bridge)] <- 0.00
TotalARR <- format(round(saas_data_temp$TotalARR, 2), nsmall = 2)

bridge$"Dif_2020-09" <- format(round((bridge$"2020-10-01" - bridge$"2020-09-01"), 2), nsmall = 2)

#Create categories
bridge$"Change_2020-09" <- ifelse((bridge$"2020-09-01" > bridge$"2020-10-01"& bridge$"2020-10-01" != 0), "Contraction",
                                  ifelse((bridge$"2020-09-01" < bridge$"2020-10-01"& bridge$"2020-09-01" != 0), "Expansion",
                                         ifelse((bridge$"2020-09-01" ==  0 & bridge$"Dif_2020-09" != 0 & bridge$"2020-10-01" != 0), "New",
                                                ifelse((bridge$"2020-10-01" ==  0 & bridge$"Dif_2020-09" != 0 & bridge$"2020-09-01" != 0), "Churn", "No change"))))

