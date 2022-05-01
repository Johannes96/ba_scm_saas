

#probe <- dcast(saas_data, CustomerID ~ Period)


# Prepara dataframe -------------------------------------------------------

saas_data_temp <- saas_data %>% select(CustomerID, Period, CustomerIndustry, SalesChannel, SalesTyp, ARR)
saas_data_temp <- saas_data_temp %>% 
                  group_by(CustomerID, CustomerIndustry, SalesChannel, SalesTyp, Period) %>%
                  summarise(TotalARR = sum(ARR)) %>%
                  ungroup()


# Prepara Bridge ----------------------------------------------------------

TotalARR <- format(round(saas_data_temp$TotalARR, 2), nsmall = 2)

bridge <- pivot_wider(saas_data_temp, names_from = Period, values_from = TotalARR) 

bridge[is.na(bridge)] <- 0


bridge$"Dif_2020-09" <- format(round((bridge$"2020-10-01" - bridge$"2020-09-01"), 2), nsmall = 2)

#not correct
bridge$"Change_2020-09" <- ifelse(bridge$"2020-09-01" > bridge$"2020-10-01", "Contraction",
                                  ifelse(bridge$"2020-09-01" < bridge$"2020-10-01", "Expansion",
                                         ifelse((bridge$"2020-09-01" ==  0 & bridge$"Dif_2020-09" != 0), "New",
                                                ifelse((bridge$"2020-10-01" ==  0 & bridge$"Dif_2020-09" != 0), "Churn", "No change"))))

#not correct
bridge$"Change_2020-09" <- ifelse(bridge$"2020-09-01" > bridge$"2020-10-01", "Contraction",
                                  ifelse(bridge$"2020-09-01" < bridge$"2020-10-01", "Expansion",
                                         ifelse(bridge$"2020-09-01" ==  -1*(bridge$"Dif_2020-09"), "Churn",
                                                ifelse(bridge$"2020-10-01" ==  bridge$"Dif_2020-09", "New", "No change" ))))



# bridge$"Abs_2020-09" <- bridge$"Dif_2020-09" %>% 
#                         mutate_if(is.numeric, funs(. * -1))
