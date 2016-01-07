################################################################################
# Habitat type data mungeing

rm(list = ls())

dir= "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\kimberley_various"
setwd(dir)

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

gain = 1.0472
offset = -0.0547

hab_df <- read.csv("Hab_type_data_import.csv", header = TRUE, 
                   stringsAsFactors = FALSE)

hab_df$avg <- rowMeans(hab_df[, 2:51], na.rm = TRUE)
hab_df$adj_avg <- ifelse(!is.na(hab_df$sat), gain * hab_df$avg + offset, 
                         hab_df$avg)




yrlymin <- hab_df%>%
  mutate(yr = year(dmy(hab_df$date)))%>%
  select(yr, adj_avg)%>%
  group_by(yr)%>%
  summarise(min = min(adj_avg, na.rm = TRUE),
            median = median(adj_avg, na.rm = TRUE))


ggplot(yrlymin, aes(x=yr, y=min))+
  geom_point()+
  geom_line()





