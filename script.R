library(dplyr)
library(sf)
library(ggplot2)
library(purrr)

Data <- read.delim("data/data-rf-2021/2021_S1_NB_FER.txt") %>%
  bind_rows(read.delim("data/data-rf-2021/2021_S2_NB_FER.txt"))

NombreDeValidationsParArrets <- Data %>%
  group_by(ID_REFA_LDA) %>%
  summarise(
    NbValidations = sum(NB_VALD)
  ) %>%
  rename(
    NomArret = ID_REFA_LDA
  )

Stations <- st_read("data/emplacement-des-gares-idf.geojson")

StationsAvecValidations <- NombreDeValidationsParArrets %>%
  left_join(Stations, by= c("NomArret"="id_ref_lda")) %>%
  filter(metro==1) %>%
  group_by(NomArret) %>% 
  slice(1) %>%
  ungroup() %>%
  select(NbValidations,nom_long,geometry) %>%
  mutate(
    latitude = unlist(map(geometry,2)),
    longitude = unlist(map(geometry,1))
  ) %>% st_as_sf()

Communes <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes-version-simplifiee.geojson") %>%
  st_crop(xmin = 2.22, xmax = 2.47,
          ymin = 48.76, ymax = 48.95)

LignesDeMetro <- st_read("data/traces-du-reseau-ferre-idf.geojson") %>%
  filter((mode=="METRO")) %>%
  st_crop(xmin = 2.22, xmax = 2.47,
          ymin = 48.76, ymax = 48.95)


CarteMetro <- ggplot() +
  geom_sf(data = Communes, fill="#f5f5f5", color="#7a7a7a") +
  geom_sf(data = LignesDeMetro, aes(color = paste0("#",colourweb_hexa)),lwd=1.5, show.legend = FALSE) +
  geom_point(data = StationsAvecValidations, aes(x = longitude, y = latitude, size=NbValidations), colour = "black", alpha = 0.6, show.legend = FALSE) +
  theme_void() +
  scale_size_continuous(range = c(2,10))
  
ggsave((file="result.svg"), plot= CarteMetro,   scale = 2)
ggsave((file="result.png"), plot= CarteMetro,   scale = 2)