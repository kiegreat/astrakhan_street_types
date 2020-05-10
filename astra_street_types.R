
library(sf)
library(tidyverse)
library(extrafont)

# 1. Set coordinates of the city center and radius for cropping geodata // Зададим координаты центра города и радиус для обрезки геоданных
lat <- 46.347869
lon <- 48.033574
rad <- 8000

# 2. Set street types // Обозначим типы улиц
street_types <- c('аллея', 'бульвар', 'мост', 'набережная', 'переулок', 'площадь', 'проезд', 'проспект', 'шоссе', 'тупик', 'улица')

# 3. Set styling for graphics // Зададим оформление для графики
blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_line(colour = "transparent"),
                panel.grid.minor=element_blank(),plot.background=element_blank(),
                title = element_text(family = 'Roboto'), 
                plot.subtitle = element_text(family = 'Roboto Thin'), 
                plot.caption = element_text(family = 'Roboto Light'), 
                legend.title = element_text(family = 'Roboto Light'),
                text = element_text(family = 'Roboto'))

# 4. Load and transform data // Загрузим данные
#
# Pick a region and download/unzip the .shp.zip file from: http://download.geofabrik.de/
# Нужно выбрать регион, скачать файл и разархивировать .shp.zip файл отсюда: http://download.geofabrik.de/

allroads <- read_sf("data/gis_osm_roads_free_1.shp")
water <- read_sf("data/gis_osm_water_a_free_1.shp")

pt <- data.frame(lat = lat, lon = lon)
pt <- pt %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% st_transform(crs = 'ESRI:102013')
circle <- st_buffer(pt, dist = rad) %>% st_transform(st_crs(allroads))

allroads <- st_intersection(circle, allroads)
allroads <- allroads[!(allroads$fclass  == "footway" & is.na(allroads$name)),]

water <- st_intersection(circle, water)

rm(pt, circle, rad, lat, lon); gc()

# 5. Extract types of streets from thier names // Извлечем типы улиц из их названий
df <- allroads %>% 
  mutate(
    type = str_replace_all(
      string = tolower(name), 
      pattern = '.*(аллея|бульвар|мост|набережная|переулок|площадь|проезд|проспект|шоссе|тупик|улица).*',
      replacement = '\\1'
    ),
    # fix incorrect regexp replacements // пофиксим неправильное срабатывание регулярных выражений
    type = ifelse(name %in% c('1-й проезд Мостостроителей', '4-й проезд Мостостроителей'), 'проезд', type)
  ) %>% 
  filter(type %in% street_types)

# 6. Create map // Создадим карту
street_colors <-  c(
  'проезд' = '#f72585',
  'улица' = '#ffe66d', 
  'проспект' ='red', 
  'бульвар' = '#7BFF23', 
  'переулок' = '#59c8e5',
  'мост' = 'black', 
  'аллея' = '#fe9ea5', 
  'набережная' = '#0a7abf', 
  'шоссе' = "#ff9223",
  'площадь' = 'brown'
)

ggplot() +
  blankbg + 
  geom_sf(data = water, color = NA) +
  geom_sf(data = df %>% filter(type %in% c('улица', 'переулок')), size = 0.4, aes(color = type, fill = type)) + 
  geom_sf(data = df %>% filter(type %in% c('мост', 'набережная', 'площадь', 'проезд', 'шоссе')), size = 0.6, aes(color = type, fill = type)) + 
  geom_sf(data = df %>% filter(type %in% c('бульвар', 'проспект', 'аллея')), size = 0.7, aes(color = type, fill = type)) + 
  scale_fill_manual(values = street_colors) +
  scale_color_manual(values = street_colors) +
  labs(
    title = 'Типы улиц Астрахани',
    subtitle = 'vk.com/astr.city.data // Инструмент - R',
    fill = '',
    color = ''
  ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = 'map.png', plot = last_plot(), device = 'png', dpi = 600)











