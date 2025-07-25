library(tidyverse)
library(cluster)

normalize_to_100 <- function(x, na.rm = FALSE) {
  # Check if input is numeric
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }
  
  # Handle NA values if specified
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    stop("Vector contains NA values. Set na.rm = TRUE to remove them.")
  }
  
  # Check if all values are identical (to avoid division by 0)
  if (length(unique(x)) == 1) {
    warning("All values are identical. Returning 50 for all elements.")
    return(rep(50, length(x)))
  }
  
  # Perform normalization
  min_x <- min(x)
  max_x <- max(x)
  normalized <- (x - min_x) / (max_x - min_x) * 100
  
  return(normalized)
}

comercio_bens_selecionados_usa<-read_csv("TradeData_7_23_2025_16_32_4.csv")   # read_csv("TradeData_7_23_2025_15_14_9.csv")

dados_modelo_pais<-  
  comercio_bens_selecionados_usa %>%
  select(reporterISO, cmdCode, fobvalue) %>%
  pivot_wider(names_from = cmdCode,   values_from = fobvalue)

dados_modelo<-
  dados_modelo_pais%>%
  select(- reporterISO)%>%
  mutate(across(everything(), ~replace_na(., 0)))

dados_modelo$`0901` <- normalize_to_100(dados_modelo$`0901`, na.rm = TRUE)
dados_modelo$`2709` <- normalize_to_100(dados_modelo$`2709`, na.rm = TRUE)
dados_modelo$`72`<- normalize_to_100(dados_modelo$`72`, na.rm = TRUE)
dados_modelo$`88`<- normalize_to_100(dados_modelo$`88`, na.rm = TRUE)

modelo<- pam(dados_modelo, k=4)

boxplot(dados_modelo$`0901`)


modelo$clusinfo



dados_modelo_pais$cluster<- modelo$clustering

dados_modelo_pais_normalizado<- dados_modelo
dados_modelo_pais_normalizado$reporterISO <-dados_modelo_pais$reporterISO

paises_proximo_brasil_exportacao_eua<-
dados_modelo_pais %>%
  filter(cluster %in% c(2,3))
