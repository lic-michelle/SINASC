#ETL
library(read.dbc)
library(microdatasus)
library(DBI)
library(RPostgres)

#data manipulation
library(tidyverse)
library(janitor)
library(cid10)

#statistics
library(rstatix)

#data viz
library(ggpubr)
library(ggplot2)
library(geobr)
library(ggspatial)

#read the environment variables with the credentials
readRenviron(".env")

con = dbConnect(drv = RPostgres::Postgres(),
                dbname="data_sus",
                host = Sys.getenv("HOST_ADDRESS"),
                port = 5432,
                user = Sys.getenv("USER"),
                password = Sys.getenv("PASSWORD"))

#download coordinates of the states
br_states = read_state(year = 2020)

#query count of genetic anomalies
query = paste0("
SELECT 
  EXTRACT(YEAR FROM dtnasc) AS ano_nasc,
  mun_res_uf,
  idanomal,
COUNT(*) AS n
FROM 
  sinasc
GROUP BY 
  ano_nasc, mun_res_uf, idanomal
")

df4 = dbGetQuery(con, query)

#Data at scale 1:250,000, using Geodetic reference system "SIRGAS2000" and CRS(4674)

df4 %>% 
  pivot_wider(names_from = idanomal,
              values_from = n) %>%
  rowwise() %>% 
  mutate(incidencia = (sim/(sim+não))*1e5) %>% 
  drop_na(mun_res_uf) %>% 
  mutate(mun_res_uf = gsub("Amazonas", "Amazônas", mun_res_uf)) %>% 
  mutate(mun_res_uf = gsub(" de ", " De ", mun_res_uf)) %>% 
  mutate(mun_res_uf = gsub(" do ", " Do ", mun_res_uf)) %>% 
  left_join(., br_states[,c(3,5,6)], join_by(mun_res_uf == name_state)) %>% 
  filter(ano_nasc == 2023) %>% 
  mutate(incidencia_categ = cut(incidencia, 
                                breaks = c(0, 500, 1000, 1500, 2000, Inf), 
                                labels = c("< 500", "500-1000", "1000-1500", "1500-2000", "> 2000"))) %>%
  ggplot(aes(geometry = geom,
             fill = incidencia_categ)) +
  geom_sf() +
  coord_sf(crs = 4674, expand = FALSE) +
  annotation_scale(location = "br", style = "ticks", plot_unit = "km") +
  annotation_north_arrow(location = "tr", 
                         style = north_arrow_fancy_orienteering) +
  scale_fill_viridis_d(name = "Incidência por 100 mil hab.",
                       guide=guide_legend(
                         direction='horizontal',
                         title.position='top',
                         title.hjust = 0.5,
                         label.hjust = 0.5,
                         label.position = 'bottom',
                         keywidth = 3,
                         keyheight = 0.5
                       )) +
  labs(title = "Incidência por 100 mil hab. de anomalias congênitas por Estado em 2023",
       captin = "Source: DataSUS - SINASC") +
  theme_minimal() +
  theme(title=element_text(face='bold'),
        legend.position = 'bottom')