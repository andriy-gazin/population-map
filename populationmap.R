library(ggplot2)

source("https://raw.githubusercontent.com/andriy-gazin/yandex-geocodeR/master/yaGeocode.R")

#prepair data-------------------------------------------------------------------

population <- read.csv("https://raw.githubusercontent.com/andriy-gazin/population-map/master/population_processed.csv", 
                       stringsAsFactors = F)

population$address <- paste("Україна", population$oblast, population$region, sep = ", ")

#geocode data-------------------------------------------------------------------

geocoded <- geocode(population$address)

#combine population data and coordinates----------------------------------------

population <- population %>% 
  cbind.data.frame(geocoded) %>% 
  dplyr::select(address, X2013:X2015, lon, lat) %>% 
  dplyr::mutate(type = ifelse(grepl("смт", address), "смт", "місто"))

#load the polygons data from gadm.org-------------------------------------------

ukraine <- raster::getData(name = "GADM", country = "UKR", level = 1)
ukraine <- fortify(ukraine)

#create map---------------------------------------------------------------------
png(filename = "~/Pictures/population_circle.png", 
    width = 900, height = 825, type = "cairo")

ggplot(ukraine)+
  geom_polygon(aes(long, lat, group = group), 
               color = "#3A3F4A", fill = "#EFF2F4", size = 0.1)+
  geom_point(data = na.omit(population[population$type == "місто",]),
             aes(lon, lat, size = X2015, color = type), 
             shape = 1, alpha = 0.75, stroke = 1)+
  geom_point(data = na.omit(population[population$type=="смт",]), 
             aes(lon, lat, size = X2015, color = type), 
             shape = 1, alpha = 0.75, stroke = 0.45)+
  scale_color_manual(name = "Тип:", values = c("місто" = "#007685", "смт" = "#91074C"))+
  scale_size(name = "Чисельність:",
             range = c(0.05, 20), 
             breaks = c(10000, 100000, 500000, 1000000, 2500000),
             labels = c("10 тис", "100 тис","500 тис", "1 млн", "2.5 млн"))+
  labs(title = "Чисельність наявного населення",
       subtitle = "В розрізі міст і селищ міського типу",
       caption = "Дані Державної служби статистики України станом на 1 січня 2015 року")+
  coord_map(projection = "mercator", 
            xlim = range(ukraine$long), ylim = range(ukraine$lat),
            orientation = c(90, 0, 0))+
  theme_minimal(base_family = "Ubuntu Condensed")+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        legend.position = "top",
        legend.margin  = unit(0.1, "lines"),
        legend.text  = element_text(family = "Ubuntu Condensed", size = 12),
        legend.title = element_text(family = "Ubuntu Condensed", face = "bold", size = 12),
        legend.text.align = 0,
        legend.box = "horizontal",
        legend.box.just = "left",
        plot.title = element_text(face = "bold", size = 36, margin = margin(b = 10), hjust = 0.030),
        plot.subtitle = element_text(size = 16, margin = margin(b = 20), hjust = 0.030),
        plot.caption = element_text(size = 14, margin = margin(b = 10, t = 25), color = "#5D646F"),
        plot.background = element_rect(fill = "#EFF2F4"))

dev.off()

