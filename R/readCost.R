library(tidyverse)

RR_cost <- function(){


  fnames <- list.files(path = "data-raw/RR/", pattern = "*.csv", full.names = T)
  basår <- 2011
  result <- data.frame()


  for (i in 1:length(fnames)){
    temp <- read.csv2(file = fnames[i], header = F)
    names(temp) <- c("Realtidsresultatrapport..", "current_year",  "last_year")
    result <- rbind( result, cbind(year = basår + i, temp ))

  }
  result <-
    result %>%
    filter(
      str_detect(result$Realtidsresultatrapport.., "Summa Fastighetsskötsel och städ") |
        str_detect(result$Realtidsresultatrapport.., "Summa Reparationer") |
        str_detect(result$Realtidsresultatrapport.., "Summa Planerat underhåll") |
        str_detect(result$Realtidsresultatrapport.., "Summa Taxebundna kostnader") |
        str_detect(result$Realtidsresultatrapport.., "Summa Fastighetsskatt") |
        str_detect(result$Realtidsresultatrapport.., "Summa Övriga driftskostnader")|
    ) %>%
    select(-last_year) %>%
    # separate(
    #   Realtidsresultatrapport..,
    #   sep = ";" ,
    #   into = c("lable", "value1", "value2", "value3")
    # ) %>%
    mutate(current_year = str_replace_all(current_year, " ", "")) %>%
    mutate(current_year = as.numeric(current_year))

  pivot_wider(result, names_from = c("Realtidsresultatrapport.."), values_from = current_year)

  # rr2019 <-
  #   result  %>%
  #   filter(year == 2019) %>%
  #   mutate(value1 = value2, value2 = value3, value3 = "") %>%
  #   select(-value3)
  #
  # result <-
  #   result %>%
  #   filter(year != 2019) %>%
  #   select(-value3) %>%
  #   bind_rows( rr2019)%>%
  #   arrange(year)



}

cost <- RR_cost()


write.csv(cost, "data/cost.csv", fileEncoding = "UTF8")
