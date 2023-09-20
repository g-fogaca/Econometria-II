# Importando pacotes
library(tidyverse) # Manipulação de dados

# Função para importar dados --------------------------------------------------

read_data <- function(file) {
  
  full_path <- paste0("https://github.com/scunning1975/mixtape/raw/master/", file)
  
  df <- haven::read_dta(full_path) # lê arquivos STATA
  
  return(df)
  
}

# Exemplo 1 -------------------------------------------------------------------
# Importando dados
yule <- read_data("yule.dta")

# Regressão linear múltipla 
mod <- lm(paup ~ outrelief + old + pop, yule)
summary(mod)

# Exemplo 2 -------------------------------------------------------------------
# Função que gera grupo tratamento e controle aleatórios
gap <- function() {
  
  # Cria dataframe com resultados potenciais e valores aleatórios
  sdo <- tibble(
    y1 = c(7, 5, 5, 7, 4, 10, 1, 5, 3, 9),
    y0 = c(1, 6, 1, 8, 2, 1, 10, 6, 7, 8),
    random = rnorm(10)) %>% 
    # Ordena observações pelo valor aleatório
    arrange(random) %>% 
    # Cria resultado observado a partir da switching equation
    mutate(
      d = c(rep(1, 5), rep(0,5)),
      y = d * y1 + (1 - d) * y0
    ) %>% 
    # Retorna vetor com resultados observados
    pull(y)
  
  # Calcula a diferença de médias entre os dois grupos
  sdo <- mean(sdo[1:5] - sdo[6:10])
  
  return(sdo)
  
}

# Simulando o experimento 10.000 vezes
sim <- replicate(10000, gap())
mean(sim) # Este é o ATE, pois o tratamento foi atribuido aleatoriamente 

# Exemplo 3 ------------------------------------------------------------------
# Importando base 
ri <- read_data("ri.dta") %>%
  mutate(id = c(1:8)) %>% 
  mutate_at(vars(y0,y1), as.numeric) %>% 
  mutate(
    y0 = if_else(is.na(y0), 0, y0), 
    y1 = if_else(is.na(y1), 0, y1)
    )
