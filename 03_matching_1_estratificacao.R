# importando pacotes
library(tidyverse) # manipulação e visualização de dados
library(readxl) # le arquivos em excel

# Importando dados ------------------------------------------------------------
titanic_raw <- read_excel("dados/titanic.xls")

### EXERCÍCIO 1 ---------------------------------------------------------------
# Objetivo: encontrar o efeito causal de estar na 1a classe do titanic sobre
# a taxa de sobrevivência no incidente

# Criando dummies a partir das variáveis categóricas
titanic <- titanic_raw %>% 
  # Transmute = mutate + select (descarta as demais variáveis do df original)
  transmute(
    d = case_when(
      class == "1st class" ~ 1,
      TRUE ~ 0
    ),
    sobrevivente = case_when(
      survived == "yes" ~ 1,
      TRUE ~ 0
    ),
    sexo = case_when(
      sex == "man" ~ 1,
      TRUE ~ 0
      ),
    faixa_etaria = case_when(
      age == "adults" ~ 1,
      TRUE ~ 0
    )
  )

# Calculando a média dos grupos de tratamento e controle (1a classe e demais)
media_grupos <- titanic %>% 
  group_by(d) %>% 
  summarize(sobrevivente = mean(sobrevivente)) %>% 
  pull(sobrevivente)

# Calculando SDO (não quer dizer nada em termos de causalidade)
SDO <- media_grupos[2] - media_grupos[1]

# Criando estratos 
titanic <- titanic %>% 
  mutate(
    s = case_when(
      sexo == 0 & faixa_etaria == 1 ~ 1, # mulheres adultas
      sexo == 0 & faixa_etaria == 0 ~ 2, # meninas crianças
      sexo == 1 & faixa_etaria == 1 ~ 3, # homens adultos
      sexo == 1 & faixa_etaria == 0 ~ 4, # meninos crianças
      TRUE ~ 0
      )
    )

# Verificando distribuição dos estratos
titanic %>% count(s)

# Taxas de sobrevivência de cada grupo
medias_estratos <- titanic %>% 
  group_by(s, d) %>% 
  summarize(sobrevivente = mean(sobrevivente)) %>% 
  # Transforma os resultados de tratados e controle em duas colunas diferentes
  pivot_wider(names_from = d, values_from = sobrevivente) %>% 
  # Calcula a diferença de médias entre tratados e controle em cada estrato
  mutate(dif = `1` - `0`)

# Vetor com as diferenças nos estratos
diferencas <- medias_estratos$dif

# Pesos de cada grupo
obs <- nrow(titanic %>% filter(d ==0)) # Total controle

pesos <- titanic %>% 
  filter(d == 0) %>% 
  group_by(s) %>% 
  count() %>% # Calcula a quantidade de controles em cada estrato
  pull(n) / obs # Retorna vetor com os pesos

# Calculando ATE
ATE <- sum(diferencas * pesos)

### ATE é bem menor que SDO