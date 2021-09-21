install.packages("nycflights13")
library(nycflights13)
library(ggplot2)
library(dplyr)
#Bài 1: Vẽ đồ thị scatter
alaska_flights <- flights %>%
  filter(carrier == "AS")
ggplot(data = alaska_flights,
       mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_point()

#Bài 2: Vẽ đồ thị Linegraphs
early_january_weather <- weather %>%
  filter(origin == "EWR" & month == 1 & day <= 15)
ggplot(data = early_january_weather,
       mapping = aes(x = time_hour, y = temp)) +
  geom_line()

#Bài 3: vẽ đồ thị Histogram
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram()
'stat_bin()` using `bins = 30`. Pick better value with `binwidth'

#Bài 4: Vẽ biểu đồ boxplot 
ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot()

#Bài 5: Vẽ biểu đồ barplot
ggplot(data = flights, mapping = aes(x = carrier)) +
  geom_bar()