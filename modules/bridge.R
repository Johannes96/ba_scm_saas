
# UI ----------------------------------------------------------------------

bridge_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        titlePanel("Filter"),
        fluidRow(column(12,
                        # Select Period
                        selectInput(inputId = ns("Period"),
                                    label = "Period",
                                    choices = unique(saas_data$Period),
                                    selected = "2022-03-01")
                ) 
              )               
          , width=2),
          mainPanel(
            plotOutput(ns("ARRBridge"))
      )
    )
  )
}


bridge <- function(input, output, session) {
  

## Server ------------------------------------------------------------------

# Prepare dataframe -------------------------------------------------------

saas_data_temp <- saas_data %>% select(CustomerID, Period, CustomerIndustry, SalesChannel, SalesTyp, ARR)
saas_data_temp <- saas_data_temp %>% 
                  group_by(CustomerID, CustomerIndustry, SalesChannel, SalesTyp, Period) %>%
                  summarise(TotalARR = sum(ARR)) %>%
                  ungroup()


# Prepare Bridge ----------------------------------------------------------

bridge <- pivot_wider(saas_data_temp, names_from = Period, values_from = TotalARR) 

bridge[is.na(bridge)] <- 0.00
TotalARR <- format(round(saas_data_temp$TotalARR, 2), nsmall = 2)

#Calculate differences

bridge$"Dif202010" <- (bridge$"2020-10-01" - bridge$"2020-09-01")
transform(bridge, Dif202010 = as.numeric(Dif202010))
     

#Create categories

bridge$"Change202010" <- ifelse((bridge$"2020-09-01" > bridge$"2020-10-01"& bridge$"2020-10-01" != 0), "Contraction",
                                  ifelse((bridge$"2020-09-01" < bridge$"2020-10-01"& bridge$"2020-09-01" != 0), "Expansion",
                                         ifelse((bridge$"2020-09-01" ==  0 & bridge$"Dif202010" != 0 & bridge$"2020-10-01" != 0), "New",
                                                ifelse((bridge$"2020-10-01" ==  0 & bridge$"Dif202010" != 0 & bridge$"2020-09-01" != 0), "Churn", "No change"))))


# Sum ARR for each month --------------------------------------------------

PeriodARR <- data.table(Period = c("2020-09-01",
                                   "2020-10-01",
                                   "2020-11-01",
                                   "2020-12-01",
                                   "2021-01-01",
                                   "2021-02-01",
                                   "2021-03-01",
                                   "2021-04-01",
                                   "2021-05-01",
                                   "2021-06-01",
                                   "2021-07-01",
                                   "2021-08-01",
                                   "2021-09-01",
                                   "2021-10-01",
                                   "2021-11-01",
                                   "2021-12-01",
                                   "2022-01-01",
                                   "2022-02-01",
                                   "2022-03-01"
),
PeriodARR = c("2020-09-01"=sum(bridge$"2020-09-01"),
              "2020-10-01"=sum(bridge$"2020-10-01"),
              "2020-11-01"=sum(bridge$"2020-11-01"),
              "2020-12-01"=sum(bridge$"2020-12-01"),
              "2021-01-01"=sum(bridge$"2021-01-01"),
              "2021-02-01"=sum(bridge$"2021-02-01"),
              "2021-03-01"=sum(bridge$"2021-03-01"),
              "2021-04-01"=sum(bridge$"2021-04-01"),
              "2021-05-01"=sum(bridge$"2021-05-01"),
              "2021-06-01"=sum(bridge$"2021-06-01"),
              "2021-07-01"=sum(bridge$"2021-07-01"),
              "2021-08-01"=sum(bridge$"2021-08-01"),
              "2021-09-01"=sum(bridge$"2021-09-01"),
              "2021-10-01"=sum(bridge$"2021-10-01"),
              "2021-11-01"=sum(bridge$"2021-11-01"),
              "2021-12-01"=sum(bridge$"2021-12-01"),
              "2022-01-01"=sum(bridge$"2022-01-01"),
              "2022-02-01"=sum(bridge$"2022-02-01"),
              "2022-03-01"=sum(bridge$"2022-03-01")
))



# Create bridge -----------------------------------------------------------

bridge_2020_10 <- bridge %>% 
                  group_by(Change202010) %>%
                  summarise(ARRGrouped = sum(Dif202010)) %>%
                  ungroup()

bridge_2020_10 <- rbindlist(list(as.list(PeriodARR[1,]), bridge_2020_10), use.names=FALSE)






# Create plot -------------------------------------------------------------


output$ARRBridge <- renderPlot({
  
  waterfall(bridge_2020_10, calc_total = TRUE, total_axis_text="Actual", draw_lines = FALSE, fill_by_sign = FALSE, fill_colours = 3:8, rect_border = NA)
  
})








}