library(geobr)
library(tidyverse)
library(colorspace)

uf_mun <- readRDS("~/Github/comex_stat/uf_mun.rds")
exportacao_municipios <- readRDS("~/Github/comex_stat/exportacao_municipios.RDS")

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
    .by = c(co_mun_geo, no_mun, sg_uf_mun))%>%
  mutate(valor_total =  valor_total/10^9) %>%
  arrange(desc(valor_total)) 


fab<-
  municipios_seat %>%
  filter(abbrev_state == "SP") %>%
  inner_join(
    dados_mapa %>%
      mutate(code_muni =co_mun_geo)
  )

fab1<-   
  municipios_seat %>%
  filter(abbrev_state == "SP")


fab2<- 
  dados_mapa %>%
  filter(sg_uf_mun== "SP") %>%
  mutate(code_muni =co_mun_geo)



dados_mapa %>%
  filter(sg_uf_mun == "SP") %>%
  mutate(cod_mun = as.numeric(co_mun_geo))

municipios_seat %>%
  inner_join(
    dados_mapa %>%
      mutate(code_muni = as.numeric(co_mun_geo))
  ) %>%
  ggplot() +
  #geom_sf(data= estados, fill = "white") +
  geom_sf()


municipios_seat %>%
  inner_join(
    dados_mapa %>%
      mutate(code_muni = as.numeric(co_mun_geo))
  ) %>%
  ggplot() +
  geom_sf(data= estados, fill = "white") +
  geom_sf(aes( size= valor_total, alpha= valor_total), pch= 21, fill= "black", color = "black") +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void() +
  theme(
    panel.background = element_rect(fill= "gray")
  )
