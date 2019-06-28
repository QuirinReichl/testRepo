### Aufgabe 1

getwd()

# Pakete laden

library(nycflights13)
#library(dplyr)
library(tidyverse)
library(forcats)

# alle Datensätze untersuchen 

airlines
airports
flights
planes
weather


# 1a)


# Datensatz planes benutzen wegen Variable "seats"

str(planes)
summary(planes)
View(planes)
glimpse(planes)

# Grenzwerte festlegen

min_seats <- 400
max_seats <- 500

# Funktion "get_planes_by_seats" schreiben

get_planes_by_seats <- planes %>% filter(seats >= min_seats & seats <=max_seats) %>% select(tailnum, model)

get_planes_by_seats

# oder


get_planes_by_seats1 <- filter(planes, seats >= min_seats, seats <= max_seats) %>% select(tailnum, model)

get_planes_by_seats1


# Prüfen ob Output der Funktion von Datentyp "tibble" ("tbl_df") ist

class(get_planes_by_seats)


# 1b)

###

# plane_model_by_seats <- as.character(distinct(get_planes_by_seats, model) %>% arrange(desc(model)))
# plane_model_by_seats
# class(plane_model_by_seats)

# Lösung
plane_model_by_seats <- as.character(get_planes_by_seats %>% group_by(model) %>% summarize() %>% arrange(desc(model)) %>% pull(model)) 
plane_model_by_seats
class(plane_model_by_seats)

# 2a)

# Gesamtdatensatz erstellen"


get_avg_distance_by_model <- flights %>% left_join(planes, by = c("tailnum", "year")) %>% 
  group_by(model) %>% mutate(n = n(), distance = mean(distance)) %>% 
 select(model, n, distance) %>% na.omit() %>% unique() %>% arrange(desc(distance))         

get_avg_distance_by_model <- unique(get_avg_distance_by_model)

View(get_avg_distance_by_model)

#2b)


# Datensatz vorbereiten

data <- flights %>% left_join (planes, by = c("tailnum", "year")) %>% 
  group_by(model) %>% mutate (n = n(), distance = mean(distance)) %>% 
  select(manufacturer, model, n, distance) %>% na.omit() 

# Neue Variable "type" erstellen

type <- data %>% group_by(model) %>% mutate(type = gsub("\\-.*$","", model)) %>% ungroup()

# neue Funktion "get_avg_distance_by_type" schreiben

get_avg_distance_by_type <- filter(type, manufacturer == "AIRBUS" | manufacturer == "BOEING") %>%  
  select(manufacturer, type, n, distance) %>% unique() %>% arrange(desc(distance))

# Kontrolliere Funktion get_avg_distance_by_type


View(get_avg_distance_by_type)
get_avg_distance_by_type
View(planes)


# A3a)

data <- planes %>% mutate(manufacturer = recode(
  manufacturer, "AIRBUS INDUSTRIE" = "AIRBUS")) 

data$manufacturer <- as.factor(data$manufacturer)

class(data$manufacturer)

repair_manufacturer <- data %>% forcats::fct_lump(manufacturer, n=2)





