library(colorspace)

dados_trabalho_exportacao %>%
  summarise(volume = sum(kg_liquido),
            .by = co_ano) %>%
  ggplot(aes(x= co_ano, y= volume)) +
  geom_col() +
  scale_x_continuous(breaks = 1997:2025) 


dados_trabalho_exportacao %>%
  summarise(valor = sum(vl_fob),
            .by = co_ano) %>%
  mutate(valor = valor/10^9) %>%
  ggplot(aes(x= co_ano, y= valor)) +
  geom_col(aes(fill= valor), show.legend = FALSE) +
  geom_text(aes(x=co_ano, y= valor+0.1, label= round(valor,1)), size= 3.1) +
  scale_x_continuous(breaks = 1997:2025) +
  scale_fill_continuous_sequential(palette = "Heat 2") +
  theme_light() +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    title = "Exportação de aço para os EUA",
    subtitle = "Valores em US$ bilhões",
    x= "",
    y="",
    caption =  "Fonte: ComexStat. Elaboração própria"
  )

dados_trabalho_exportacao %>%
  summarise(valor = sum(vl_fob),
            .by = c(co_ano, no_sh2_por)) %>%
  mutate(valor = valor/10^9) %>%
  ggplot(aes(x= co_ano, y= valor, fill =  no_sh2_por)) +
  geom_col() +
  geom_text(aes(x=co_ano, y= valor+0.1, label= round(valor,1)), size= 3.1) +
  scale_x_continuous(breaks = 1997:2025) +
  scale_fill_discrete_qualitative(palette = "Dark 2") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Exportação de aço para os EUA",
    subtitle = "Valores em US$ bilhões",
    x= "",
    y="",
    fill = "",
    caption =  "Fonte: ComexStat. Elaboração própria"
  )


#Dimensões 1026 x 729


dados_trabalho_exportacao %>%
  summarise(valor = sum(kg_liquido),
            .by = co_ano) %>%
  mutate(valor = valor/10^9) %>%
  ggplot(aes(x= co_ano, y= valor)) +
  geom_col(aes(fill= valor)) +
  geom_text(aes(x=co_ano, y= valor+0.2, label= round(valor,1)), size= 3.1) +
  scale_x_continuous(breaks = 1997:2025) +
  scale_fill_continuous_sequential(palette = "Heat 2") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Exportação de aço para os EUA",
    subtitle = "Em milhões de toneladas líquidas",
    x= "",
    y="",
    caption =  "Fonte: ComexStat. Elaboração própria",
    fill = "Milhões de toneladas"
  )

