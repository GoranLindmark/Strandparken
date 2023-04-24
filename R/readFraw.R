library(tidyverse)


RR <- function(){


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
    str_detect(result$Realtidsresultatrapport.., "Resultat före Avskrivningar") |
    str_detect(result$Realtidsresultatrapport.., "Summa avskrivningar") |
    str_detect(result$Realtidsresultatrapport.., "Rörelseresultat") |
    str_detect(result$Realtidsresultatrapport.., "Resultat efter finansiella poster") |
    str_detect(result$Realtidsresultatrapport.., "Årets resultat")
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

BR <- function(){
fnames <- list.files(path = "data-raw/BR", pattern = "*.csv", full.names = T)
basår <- 2011
result <- data.frame()
for (i in 1:length(fnames)){
  result <- rbind( result, cbind(year = basår + i, read.csv(fnames[i]) ))

}
result <-
result %>%
  filter(str_detect(Realtidsbalansrapport.., "Summa långfristiga skulder") |
           str_detect(Realtidsbalansrapport.., "Summa kortfristiga skulder")) %>%
  separate(Realtidsbalansrapport.., sep = ";" , into = c("lable", "value1", "value2", "value3")) %>%
  mutate( value1 = str_replace_all(value1, " ", "")) %>%
  mutate( value2 = str_replace_all(value2, " ", "")) %>%
  mutate( value3 = str_replace_all(value3, " ", "")) %>%
  mutate(value1 = as.numeric(value1)) %>%
  mutate(value2 = as.numeric(value2)) %>%
  mutate(value3 = as.numeric(value3))

names(result) <- c("year", "Lable", "IB", "Period", "UB")

result <-
  result %>%
  group_by(year) %>%
  summarize(ib = sum(IB),
            period = sum(Period),
            ub = sum(UB))

result
}

rr <- RR()

br <- BR()


write.csv(rr, "data/rr.csv", fileEncoding = "UTF8")
write.csv(br, "data/br.csv", fileEncoding = "UTF8")
