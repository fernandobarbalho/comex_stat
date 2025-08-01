library(tidyverse)
library(readxl)
library(cluster)

sintese_tarifas <- 
  read_excel("sintese_tarifas.xlsx", 
             sheet = "AnaliseCompleta", skip = 1) %>%
  janitor::clean_names()

sintese_tarifas <- sintese_tarifas[-1,]


# Função para normalizar vetor de 0 a 100
normalizar_0_100 <- function(vetor) {
  min_val <- min(vetor, na.rm = TRUE)
  max_val <- max(vetor, na.rm = TRUE)
  
  # Evita divisão por zero
  if (min_val == max_val) {
    return(rep(50, length(vetor)))  # ou qualquer valor fixo entre 0 e 100
  }
  
  return((vetor - min_val) / (max_val - min_val) * 100)
}

# Exemplo de uso
valores <- c(10, 20, 30, 40, 50)
normalizados <- normalizar_0_100(valores)
print(normalizados)

total_participacao_eua<- sum(sintese_tarifas$exportacoes_brasil_eua_b)
total_participacao_sobre_tarifa<- sum(sintese_tarifas_trabalho$exportacoes_brasil_eua_b) - sum(sintese_tarifas$exportacoes_nao_sujeitas_a_tarifa_extra_de_40_percent_c)

sintese_tarifas_trabalho<-
  sintese_tarifas %>%
  select(hs_2_digitos,
         descricao_do_setor,
         exportacoes_brasil_eua_b,
         grau_de_exposicao_b_a,
         exportacoes_nao_sujeitas_a_tarifa_extra_de_40_percent_c) %>%
  mutate(exportacoes_sujeitas_a_tarifa_extra_de_40_percent_c = exportacoes_brasil_eua_b- exportacoes_nao_sujeitas_a_tarifa_extra_de_40_percent_c,
         prop_exportacoes_eua = exportacoes_brasil_eua_b/total_participacao_eua,
         prop_participacao_sobre_tarifa = exportacoes_sujeitas_a_tarifa_extra_de_40_percent_c/total_participacao_sobre_tarifa,
         prop_exporacao_tarifada = exportacoes_sujeitas_a_tarifa_extra_de_40_percent_c/exportacoes_brasil_eua_b)


sintese_tarifas_trabalho$exportacoes_brasil_eua_b_norm<- normalizar_0_100(sintese_tarifas_trabalho$exportacoes_brasil_eua_b)
sintese_tarifas_trabalho$grau_de_exposicao_b_a_norm <- normalizar_0_100(sintese_tarifas_trabalho$grau_de_exposicao_b_a)
sintese_tarifas_trabalho$exportacoes_sujeitas_a_tarifa_extra_de_40_percent_c_norm <- normalizar_0_100(sintese_tarifas_trabalho$exportacoes_sujeitas_a_tarifa_extra_de_40_percent_c)
sintese_tarifas_trabalho$prop_exportacoes_eua_norm <- normalizar_0_100(sintese_tarifas_trabalho$prop_exportacoes_eua)
sintese_tarifas_trabalho$prop_participacao_sobre_tarifa_norm <- normalizar_0_100(sintese_tarifas_trabalho$prop_participacao_sobre_tarifa)
sintese_tarifas_trabalho$prop_exporacao_tarifada_norm<- normalizar_0_100(sintese_tarifas_trabalho$prop_exporacao_tarifada)  

sintese_modelo<-
  sintese_tarifas_trabalho %>%
  select(exportacoes_brasil_eua_b_norm,
         grau_de_exposicao_b_a_norm, 
         exportacoes_sujeitas_a_tarifa_extra_de_40_percent_c_norm )

sintese_modelo_1_1<-
  sintese_tarifas_trabalho %>%
  select(prop_exportacoes_eua_norm,
         grau_de_exposicao_b_a_norm, 
         prop_participacao_sobre_tarifa_norm )




set.seed(72)
modelo_k2<-
  cluster::pam(sintese_modelo, k=2)

set.seed(72)
modelo_k3<-
  cluster::pam(sintese_modelo, k=3)

set.seed(72)
modelo_k2_1_1<-
  cluster::pam(sintese_modelo_1_1, k=2)

set.seed(72)
modelo_k3_1_1<-
  cluster::pam(sintese_modelo_1_1, k=3)



summary(modelo_k2)
summary(modelo_k3)

summary(modelo_k2_1_1)
summary(modelo_k3_1_1)


sintese_tarifas_trabalho$cluster <- as.factor(modelo_k3$clustering)

sintese_tarifas_trabalho_1_1 <-sintese_tarifas_trabalho
sintese_tarifas_trabalho_1_1$cluster <- as.factor(modelo_k3_1_1$clustering)

sintese_tarifas_trabalho<-
  sintese_tarifas_trabalho%>%
  mutate(cluster = ifelse(hs_2_digitos==88,1,cluster))

sintese_tarifas_trabalho$cluster <- factor(sintese_tarifas_trabalho$cluster,levels = c("2","3","1"), labels = c("Alto risco", "Médio risco", "Baixo risco"))
sintese_tarifas_trabalho$cluster <- factor(sintese_tarifas_trabalho$cluster,levels = c("Alto risco", "Médio risco", "Baixo risco"))


sintese_clusters<-
sintese_tarifas_trabalho %>%
  summarise(
    media_exportacao =  mean(exportacoes_brasil_eua_b),
    mediana_exportacao =  median(exportacoes_brasil_eua_b),
    total_exportacao = sum(exportacoes_brasil_eua_b),
    media_exposicao =mean(grau_de_exposicao_b_a),
    mediana_exposicao = median(grau_de_exposicao_b_a),
    media_exportacao_tarifa = mean(exportacoes_sujeitas_a_tarifa_extra_de_40_percent_c),
    mediana_exportacao_tarifa = median(exportacoes_sujeitas_a_tarifa_extra_de_40_percent_c),
    total_exportacao_impactada = sum(exportacoes_sujeitas_a_tarifa_extra_de_40_percent_c),
    quantidade_setores = n(),
    .by = cluster
  ) %>%
  arrange(cluster)

sintese_clusters %>%
  writexl::write_xlsx("sintese_clusters.xlsx")

sintese_tarifas_trabalho%>%
  arrange(cluster) %>%
  writexl::write_xlsx("sintese_tarifas_trabalho.xlsx")


boxplot(sintese_tarifas_trabalho$exportacoes_brasil_eua_b_norm)
boxplot(sintese_tarifas_trabalho$exportacoes_brasil_eua_b)

sum(sintese_tarifas_trabalho$exportacoes_brasil_eua_b)

7664391582/40368569157


#####Modelo com duas variáveis



sintese_modelo_2<-
  sintese_tarifas_trabalho %>%
  select(grau_de_exposicao_b_a_norm,
         prop_exporacao_tarifada_norm)

set.seed(72)
modelo_k2_2<-
  cluster::pam(sintese_modelo_2, k=2)

set.seed(72)
modelo_k3_2<-
  cluster::pam(sintese_modelo_2, k=3)

set.seed(72)
modelo_k4_2<-
  cluster::pam(sintese_modelo_2, k=4)


summary(modelo_k2_2)
summary(modelo_k3_2)
summary(modelo_k4_2)

sintese_tarifas_trabalho_2<- sintese_tarifas_trabalho

sintese_tarifas_trabalho_2$cluster <- as.factor(modelo_k3_2$clustering)
