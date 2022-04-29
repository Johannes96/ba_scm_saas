
name_x <- "Johannes"

for (i in 1:5) {
  
  print(paste(name_x,"-",i))
}


my_vect <- c(1,2,3,4)


max(my_vect)

funct_1 <- function(zahl) {
  
  ergebnis <- 1 + zahl
  return(ergebnis)
  
}

funct_1(1000)
