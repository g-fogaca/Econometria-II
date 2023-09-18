# importando pacotes
library(tidyverse) # manipulação e visualização de dados
library(readxl) # le arquivos em excel

# Importando dados ------------------------------------------------------------
dados <- read_excel(
  path = "dados/training_example.xls",
  n_max = 20,
  col_types = rep("numeric", 9)
  )

### EXERCÍCIO 2 ---------------------------------------------------------------
# Objetivo: Identificar o efeito causal do treinamento sobre os salários, con-
# trolando o efeito da idade dos indivíduos por um modelo de pareamento.

# Identificando grupo tratamento
tratamento <- dados %>% 
  select(contains("treat")) %>% 
  drop_na() %>% 
  mutate(D = 1)

# Identificando grupo controle
controle <- dados %>% 
  select(contains("control")) %>% 
  drop_na() %>% 
  mutate(D = 0)

# Agregando dados
col_names <- names(tratamento) %>% sub("_treat", "", .)

df <- bind_rows(
  tratamento %>% set_names(col_names),
  controle %>% set_names(col_names)
)

# Calculando médias dos grupos
medias <- df %>% 
  group_by(D) %>% 
  summarize(mean_age = mean(age), mean_earnings = mean(earnings))

# Calculando SDO
medias_earnings <- medias %>% 
  pull(mean_earnings)

SDO <- medias_earnings[2] - medias_earnings[1]

# Trabalhadores que fizeram o treinamento ganham na média $26.25 a menos, mas 
# este não é o efeito causal

# Visualizando diferença de idade nos grupos
ggplot(df, aes(x = age)) +
  geom_density(aes(fill = as.factor(D)), alpha = 0.5) +
  labs(fill = "Grupo")

# Há uma clara diferença de idade entre os grupos

# Unidades pareamento (selecionadas a partir da idade)
pareamento <- dados %>% 
  select(contains("matched")) %>% 
  drop_na()

# Juntando tratamento e pareamento
df_matching <- bind_cols(tratamento, pareamento)

# Encontrando o efeito causal do tratamento sobre earnings
ATE <- df_matching %>% 
  mutate(dif = earnings_treat - earnings_matched) %>% 
  pull(dif) %>% 
  mean()

print(ATE)

# Controlando pela idade, podemos concluir que o treinamento levou a um salário
# $1,695.00 maior 