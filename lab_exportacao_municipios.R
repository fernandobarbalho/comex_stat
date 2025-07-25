library(geobr)
library(tidyverse)
library(colorspace)
library(sf)
library(spData)

uf_mun <- readRDS("~/Github/comex_stat/uf_mun.rds")
exportacao_municipios <- readRDS("~/Github/comex_stat/exportacao_municipios.RDS")
ncm_sh <- readRDS("~/github/comex_stat/ncm_sh.rds")

data("world")

estados<- geobr::read_state()
municipios<-  geobr::read_municipality()
municipios_seat<- geobr::read_municipal_seat()


dados_mapa<-
exportacao_municipios %>%
  rename(co_sh4 = sh4) %>%
  rename(co_mun_geo = co_mun) %>%
  filter(co_pais %in% c(249,396,873)) %>%
  inner_join(uf_mun) %>%
  inner_join(
    ncm_sh %>%
      distinct(co_sh4, no_sh4_por)
  ) %>%
  summarise(
    valor_total = sum(vl_fob),
    .by = c(co_mun_geo, no_mun_min, sg_uf_mun))%>%
  mutate(valor_total =  valor_total/10^9) %>%
  slice_max(order_by = valor_total, n =100) 


sum(exportacao_municipios$vl_fob)

dados_mapa %>%
  summarise(quantidade=  n(),
            .by = sg_uf_mun) %>%
  arrange(desc(quantidade))



-43.22788 -22.87665

municipios_seat %>%
  inner_join(
    dados_mapa %>%
      mutate(code_muni = as.numeric(co_mun_geo))
  ) %>%
  ggplot() +
  geom_sf(data = world) +
  geom_sf(data= estados, fill = "white") +
  geom_sf(aes( size= valor_total), pch= 21, fill= "black", color = "black", alpha = 0.4 ) + #alpha= valor_total
  coord_sf(xlim = c(-75,-35), ylim=c(-33,5))+
  geom_text(data = tibble(lon= -38.22788, lat=-24.87665 ) , 
            aes(x=lon, y=lat, label= str_wrap("Rio de Janeiro-RJ US$ 4,9 bi",17)),
            color = "white",
            fontface = "bold") +
  geom_text(data = tibble(lon= -40.22788, lat=-28.87665 ) , 
            aes(x=lon, y=lat, label= str_wrap("São Paulo: 34 cidades",30)),
            color = "white",
            fontface = "bold") +
  geom_text(data = tibble(lon= -40.22788, lat= 0 ) , 
            aes(x=lon, y=lat, label= str_wrap("NE + N + CO: 16 cidades",30)),
            color = "white",
            fontface = "bold") +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void() +
  theme(
    panel.background = element_rect(fill= "#0077be"),
    plot.title = element_text(size =14, color = "blue", face = "bold", hjust= 0.5),
    plot.subtitle = element_text(size =12, color = "blue", face = "bold", hjust= 0.5) 
  ) +
  labs(
    size = str_wrap("Valor em US$ Bi",8),
    title = "Cem cidades com maior exportação para os EUA",
    subtitle = "Ano: 2024",
    caption = "Fonte: Comex Stat. Elaboração: Fernando Barbalho"
  ) +
  annotate("segment",
           x = -48.22788, y = -23.87665, xend = -40.22788, yend = -27.87665,
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
           color = "white")





municipios_seat %>%
  inner_join(
    dados_mapa %>%
      mutate(code_muni = as.numeric(co_mun_geo))
  ) %>%
  ggplot() +
  geom_sf(data = world) +
  geom_sf(data= estados, fill = "black") +
  geom_sf(aes( fill= valor_total), pch= 21,  color = "black", alpha = 1, size= 2 ) + #alpha= valor_total
  coord_sf(xlim = c(-75,-35), ylim=c(-33,5))+
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void() +
  theme(
    panel.background = element_rect(fill= "#0077be")
  )


