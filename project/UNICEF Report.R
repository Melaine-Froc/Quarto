install.packages("tidyverse")
install.packages("gapminder")
install.packages("plotly")
install.packages("ggplot2")
library(tidyverse)
library(gapminder)
library(plotly)
library(ggplot2)

# JoinTheData
indicator_1 <- read_delim("indicator_1.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
indicator_2 <- read_delim("indicator_2.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
metadata <- read_delim("metadata.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)
localisation <- read_delim("localisation.pdf",
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Simplify_name
colnames(metadata)[4] <- "Life_Exp"

# Join_Data_For_World_Map_chart 
data_join <- indicator_1 %>%
  full_join(indicator_2, by = c("country", "time_period")) %>%
  full_join(metadata, by = c("country"="country", "time_period"= "year")) %>%
  full_join(localisation, by = c("country" = "country"))

# World_Map_chart 
map_world <- map_data("world")
map_world$region <- as.character(map_world$region)
map_world$region[map_world$region == "USA"] <- "United States of America"
map_world$region[map_world$region == "UK"] <- "United Kingdom"
map_world$region[map_world$region == "Republic of Congo"] <- "Republic of the Congo"
map_world$region[map_world$region == "Democratic Republic of the Congo"] <- "Congo"

map_data_join <- full_join(data_join, map_world, by = c("country"="region"))

data_2021 <- filter(map_data_join, time_period == 2021)

# Map
ggplot(data_2021) +
  aes(x = long, y = lat, group = group, fill = Life_Exp) +
  geom_polygon()+
  labs(
    title = "Life Expectancy at birth around the world (measured in years)",
    subtitle = "Year 2021",
    fill = "Life Exp"
  )


# TimeSeries

# Sample data
data <- data.frame(x = c("Yemen", "Sierra Leone", "Nicaragua", "Senegal", "Liberia", "Burundi", "Bangladesh", "Jordan", "Peru", "Guyana"),
                   y = c(0,10,20,30,40,50))

# Filter data where 'y' is above 0
filtered_data <- subset(data, y > 0)

# TimeSeries 
ggplot(filtered_data, aes(x = x, y = y)) +
  geom_point() +
  labs(x = "Country", y = "Number of schools with a limited access to sanitation services (%)", title = "Percentage of schools with a limited access to sanitation services per country")

# Prepare_ScatterPlot
colnames(metadata)[3] <- "Pop"
colnames(data_join)[9] <- "Pop"

# Prepare_ScatterPlot
time_serie <- full_join(select(data_join, Life_Exp , time_period, Region, Pop),localisation, by = c("country"))

# ScatterPlot
ggplot(time_serie)+
  aes(Life_Exp , Region, color = Pop) +
  geom_point()
  labs(
    x = "Region", 
    y = "Number of refugees by host country",
    title = "Life Expectancy at birth in the population of the region"
  )
options(scipen = 999)


# Prepare_BarChart
time_serie <- full_join(select(data_join, Region, indicator.x, time_period, Life_Exp, country),localisation, by = c("country")

# BarChart
BarChart <- data_join %>%
  group_by(Region, indicator.x,time_period) %>%
  summarise(m_Life_Exp = mean(Life_Exp, na.rm = TRUE)) %>%
  ggplot() +
  aes(Region, m_Life_Exp, fill = Region) +
  geom_col() +
  facet_wrap(~ indicator.x, nrow = 1) + 
  labs(
    x = "",
    y = "Number_of_refugees",
    fill = "indicator.x",
    title = "Life expectancy in countries & number of refugees by host county in 2020s"
  )
theme_classic()+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "chartreuse4","#9933FF"))
ggplotly(BarChart)
