library(readxl)
library(ggrepel)
library(colorspace)

EXP_2024 <- 
  read_delim("EXP_2024.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE) %>%
  janitor::clean_names()

exportacao_municipios %>%
  rename(co_sh4 = sh4) %>%
  rename(co_mun_geo = co_mun) %>%
  filter(co_pais %in% c(249,396,873)) %>%
  inner_join(uf_mun) %>%
  inner_join(
    ncm_sh %>%
      distinct(co_sh4, no_sh4_por, no_sh4_ing)
  ) 


anexo_I_Executive_Orders <- 
  read_excel("anexo_I_Executive_Orders.xlsx") %>%
  janitor::clean_names()


anexo_I_Executive_Orders %>%
  mutate(htsus_original = htsus) %>%
  mutate(htsus = str_remove_all(htsus,"[.]")) %>%
  mutate(htsus = str_sub(htsus,1,6)) %>%
  inner_join(
    ncm_sh %>%
      rename(htsus = co_sh6) %>%
      distinct(htsus, no_sh6_por)
  ) %>%
  rename(descricao = no_sh6_por) %>%
  select(htsus_original, articles_of_civil_aircraft_only, descricao, description ) %>%
  writexl::write_xlsx("tabela_anexo_1_traduzido.xlsx")

dados_trabalho<-
EXP_2024 %>%
  filter(co_pais %in% c(249,396,873)) %>%
  mutate(htsus = str_sub(co_ncm,1,6)) %>%
  inner_join(
    anexo_I_Executive_Orders %>%
      mutate(htsus_original = htsus) %>%
      mutate(htsus = str_remove_all(htsus,"[.]")) %>%
      mutate(htsus = str_sub(htsus,1,6)) %>%
      distinct(htsus)
  )

sum(dados_trabalho$vl_fob)

dados_sem_restricao_2024_estados<-
dados_trabalho %>%
  summarise(valor_total_sem_restricao = sum(vl_fob),
            .by = sg_uf_ncm) %>%
  arrange(desc(valor_total_sem_restricao))

dados_total_eua_2024_estados<-
  EXP_2024 %>%
  filter(co_pais %in% c(249,396,873)) %>%
  summarise(valor_total_eua = sum(vl_fob),
            .by = sg_uf_ncm) %>%
  arrange(desc(valor_total_eua))


dados_total_mundo_2024_estados<-
  EXP_2024 %>%
  summarise(valor_total_mundo = sum(vl_fob),
            .by = sg_uf_ncm) %>%
  arrange(desc(valor_total_mundo))

dados_total_mundo_2024_estados %>%
  inner_join(dados_sem_restricao_2024_estados) %>%
  inner_join(dados_total_eua_2024_estados) %>%
  mutate(valor_total_com_restricao = valor_total_eua - valor_total_sem_restricao) %>%
  mutate(indice_exposicao_eua = valor_total_eua/valor_total_mundo) %>%
  mutate(indice_impacto_restricao = valor_total_com_restricao/valor_total_eua) %>%
  arrange(desc(indice_impacto_restricao)) %>%
  writexl::write_xlsx("impatos_tarifas_estados.xlsx")

exportacao_total_eua <- 
  (EXP_2024 %>%
  filter(co_pais %in% c(249,396,873)) %>%
  summarise(valor_total = sum(vl_fob)))$valor_total



dados_total_mundo_2024_estados %>%
  inner_join(dados_sem_restricao_2024_estados) %>%
  inner_join(dados_total_eua_2024_estados) %>%
  mutate(valor_total_com_restricao = valor_total_eua - valor_total_sem_restricao) %>%
  mutate(indice_exposicao_eua = valor_total_eua/valor_total_mundo) %>%
  mutate(indice_impacto_restricao = valor_total_com_restricao/valor_total_eua) %>%
  summarise(median(indice_impacto_restricao),
            median(indice_exposicao_eua)) 


fab<-
  dados_total_mundo_2024_estados %>%
  inner_join(dados_sem_restricao_2024_estados) %>%
  inner_join(dados_total_eua_2024_estados) %>%
  mutate(valor_total_com_restricao = valor_total_eua - valor_total_sem_restricao) %>%
  mutate(indice_exposicao_eua = valor_total_eua/valor_total_mundo) %>%
  mutate(indice_impacto_restricao = valor_total_com_restricao/valor_total_eua) %>%
  mutate(participacao_exportacao_eua = valor_total_eua/exportacao_total_eua) %>%
  arrange(desc(indice_impacto_restricao))

sum(fab$participacao_exportacao_eua)

dados_total_mundo_2024_estados %>%
  inner_join(dados_sem_restricao_2024_estados) %>%
  inner_join(dados_total_eua_2024_estados) %>%
  mutate(valor_total_com_restricao = valor_total_eua - valor_total_sem_restricao) %>%
  mutate(indice_exposicao_eua = valor_total_eua/valor_total_mundo) %>%
  mutate(indice_impacto_restricao = valor_total_com_restricao/valor_total_eua) %>%
  mutate(participacao_exportacao_eua = valor_total_eua/exportacao_total_eua) %>%
  arrange(desc(indice_impacto_restricao)) %>%
  ggplot(aes(x=indice_exposicao_eua, indice_impacto_restricao)) +
  geom_vline(xintercept = 0.2, color = "lightgray")+
  geom_hline(yintercept = 0.5, color = "lightgray")+
  geom_point() +
  geom_text_repel(aes(label = sg_uf_ncm)) +
  scale_x_continuous( labels = scales::percent) +
  scale_y_continuous( labels = scales::percent) +
  theme_light() +
  theme(
    panel.grid = element_blank(), 
  ) +
  labs(
    x= "Índice de exposição aos EUA",
    y= "Índice do impacto das tarifas",
    caption = "Fonte: Comex Stat. Elaboração: VPR/DIESO 01/08/2025",
    title = "Exposição e impacto das sobretarifas dos EUA nas UFs brasileiras",
    subtitle = "Valores baseados em dados de 2024"
  ) 



dados_total_mundo_2024_estados %>%
  inner_join(dados_sem_restricao_2024_estados) %>%
  inner_join(dados_total_eua_2024_estados) %>%
  mutate(valor_total_com_restricao = valor_total_eua - valor_total_sem_restricao) %>%
  mutate(indice_exposicao_eua = valor_total_eua/valor_total_mundo) %>%
  mutate(indice_impacto_restricao = valor_total_com_restricao/valor_total_eua) %>%
  mutate(participacao_exportacao_eua = valor_total_eua/exportacao_total_eua) %>%
  arrange(desc(indice_impacto_restricao)) %>%
  ggplot(aes(x=indice_exposicao_eua, indice_impacto_restricao)) +
  geom_vline(xintercept = 0.2, color = "lightgray")+
  geom_hline(yintercept = 0.5, color = "lightgray")+
  geom_point(aes(size= participacao_exportacao_eua)) +
  geom_text_repel(aes(label = sg_uf_ncm)) +
  scale_x_continuous( labels = scales::percent) +
  scale_y_continuous( labels = scales::percent) +
  scale_color_continuous_sequential(palette = "Heat 2", labels = scales::percent)+
  scale_size_continuous(labels = scales::percent) +
  theme_light() +
  theme(
    panel.grid = element_blank(), 
    legend.position = "bottom"
  ) +
  labs(
    x= "Índice de exposição aos EUA",
    y= "Índice do impacto das tarifas",
    caption = "Fonte: Comex Stat. Elaboração: VPR/DIESO 01/08/2025",
    title = "Exposição e impacto das sobretarifas dos EUA nas UFs brasileiras",
    subtitle = "Valores baseados em dados de 2024",
    size = "Participação exportação EUA"
  ) 



dados_total_mundo_2024_estados %>%
  inner_join(dados_sem_restricao_2024_estados) %>%
  inner_join(dados_total_eua_2024_estados) %>%
  mutate(valor_total_com_restricao = valor_total_eua - valor_total_sem_restricao) %>%
  mutate(indice_exposicao_eua = valor_total_eua/valor_total_mundo) %>%
  mutate(indice_impacto_restricao = valor_total_com_restricao/valor_total_eua) %>%
  mutate(participacao_exportacao_eua = valor_total_eua/exportacao_total_eua) %>%
  mutate(sg_uf_ncm = reorder(sg_uf_ncm, indice_exposicao_eua)) %>%
  ggplot(aes(y=sg_uf_ncm,  x= indice_exposicao_eua))  +
  geom_text(aes(x=indice_exposicao_eua+0.005, y=sg_uf_ncm, label = str_c(round(indice_exposicao_eua,2)*100,"%")),
            hjust=0,
            size =3) +
  geom_col()+
    scale_x_continuous( labels = scales::percent) +
    scale_color_continuous_sequential(palette = "Heat 2", labels = scales::percent)+
    scale_size_continuous(labels = scales::percent) +
    theme_light() +
    theme(
      panel.grid = element_blank(), 
      legend.position = "bottom",
      axis.text.x = element_blank()
    ) +
    labs(
      x= "Índice de exposição aos EUA",
      y="",
      caption = "Fonte: Comex Stat. Elaboração: VPR/DIESO 01/08/2025",
      title = "Exposição às exportação aos EUA nas UFs brasileiras",
      subtitle = "Valores baseados em dados de 2024"
    )  

dados_total_mundo_2024_estados %>%
  inner_join(dados_sem_restricao_2024_estados) %>%
  inner_join(dados_total_eua_2024_estados) %>%
  mutate(valor_total_com_restricao = valor_total_eua - valor_total_sem_restricao) %>%
  mutate(indice_exposicao_eua = valor_total_eua/valor_total_mundo) %>%
  mutate(indice_impacto_restricao = valor_total_com_restricao/valor_total_eua) %>%
  mutate(participacao_exportacao_eua = valor_total_eua/exportacao_total_eua) %>%
  mutate(sg_uf_ncm = reorder(sg_uf_ncm, indice_impacto_restricao)) %>%
  ggplot(aes(y=sg_uf_ncm,  x= indice_impacto_restricao))  +
  geom_text(aes(x=indice_impacto_restricao+0.005, y=sg_uf_ncm, label = str_c(round(indice_impacto_restricao,2)*100,"%")),
            hjust=0,
            size =3) +
  geom_col()+
  scale_x_continuous( labels = scales::percent) +
  scale_color_continuous_sequential(palette = "Heat 2", labels = scales::percent)+
  scale_size_continuous(labels = scales::percent) +
  theme_light() +
  theme(
    panel.grid = element_blank(), 
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  labs(
    x= "Índice do impacto das tarifas",
    y="",
    caption = "Fonte: Comex Stat. Elaboração: VPR/DIESO 01/08/2025",
    title = "Impacto das tarifas dos EUA nas UFs brasileiras",
    subtitle = "Valores baseados em dados de 2024"
  )  

