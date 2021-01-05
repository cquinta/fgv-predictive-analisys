# Setup Inicial
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("gridExtra")
install.packages("ggcorrplot")
install.packages("randomForest")
install.packages("caret")
install.packages("doParallel")
install.packages("ranger")
install.packages("tidymodels")
install.packages("rsample")
install.packages("vip")
install.packages("usemodels")


library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(ranger)
library(vip)
library(readr)
library(dplyr)
library(tidyverse)
library(tibble)
library(gridExtra)
library(corrplot)
library(ggcorrplot)
library(randomForest)
library(caret)
library(tidymodels)
library(rsample)
library(yardstick)
library(usemodels)
theme_set(theme_linedraw())

# Lendo os DataSets que devem estar localizados no diretório raiz
comp_prices <- read_csv("comp_prices.csv")
sales <- read_csv("sales.csv")


# *** TRATANDO O DATASET COMP_PRICES ***

# 1 - Agrupando e Removendo valores duplicados

comp_prices_grouped <- comp_prices %>% group_by(PROD_ID, DATE_EXTRACTION,COMPETITOR,PAY_TYPE) 
comp_prices_grouped_distinct <- comp_prices_grouped %>% distinct()
head(comp_prices_grouped_distinct)

# 2 - Dando dimensão para a coluna e PAY_TYPE

comp_prices_grouped_distinct <- comp_prices_grouped_distinct %>%
  mutate(PAY_TYPE = as.factor(PAY_TYPE))
    
# 3 - Plotando os valores

comp_prices_grouped_distinct %>% ggplot(aes(x = PAY_TYPE, y = COMPETITOR_PRICE, fill = COMPETITOR))+
  geom_boxplot()+  
  facet_wrap(~PROD_ID)+
  xlab("Tipo de Pagamento") +
  labs(fill="Competidores") 

# Nota-se alguns outliers muito fora da média o que pode ser creditado a equivocos de digitação ou coleta. 
# A visualização fica muito prejudicada. Vamos plotar os valores em escala retirando da visualização os outliers 
# mais fora do normal.

comp_prices_grouped_distinct %>% ggplot(aes(x = PAY_TYPE,y = COMPETITOR_PRICE, fill = COMPETITOR))+
  geom_boxplot()+  
  facet_wrap(~PROD_ID)+
  xlab("Tipo de Pagamento") +
  labs(fill="Competidores") +
  scale_y_continuous(limits = c(0, 2000))

# Visualmente ainda é possível verificar alguns outliers. 


# 4 - Removendo Outliers de forma a deixar apenas valores entre [Q1- (1.5)IQR] e  [Q3+(1.5)IQR] 

# Criando os limites inferior e superior (Q1,Q3)

Q1 <- quantile(comp_prices_grouped_distinct$COMPETITOR_PRICE, .25)
Q3 <- quantile(comp_prices_grouped_distinct$COMPETITOR_PRICE, .75)
IQRPRICE <- IQR(comp_prices_grouped_distinct$COMPETITOR_PRICE)

# Criando o Dataset sem outliers pelo critério proposto
comp_prices_ajusted <- subset(comp_prices_grouped_distinct, comp_prices_grouped_distinct$COMPETITOR_PRICE > (Q1 - 1.5*IQRPRICE) & comp_prices_grouped_distinct$COMPETITOR_PRICE < (Q3 + 1.5*IQRPRICE))
head(comp_prices_ajusted)

# Plotando os valores sem outliers
comp_prices_ajusted %>% ggplot(aes(x = PAY_TYPE,y = COMPETITOR_PRICE, fill = COMPETITOR))+
  geom_boxplot()+  
  facet_wrap(~PROD_ID)+
  xlab("Tipo de Pagamento") +
  labs(fill="Competidores") 

# 5 - Preparando o DataSet por protudo
comp_prices_nested <- comp_prices_ajusted %>% group_by(PROD_ID) %>% nest()

# 6 Testando por produto para verificar se o tipo de venda faz diferença nas medianas dos preços
comp_prices_type_nested <- comp_prices_nested %>% mutate (pvalor = map(.x= data, .f = ~kruskal.test(.x$COMPETITOR_PRICE,.x$PAY_TYPE)$p.value))
comp_prices_type_ploted <- comp_prices_type_nested %>% unnest(pvalor) %>% 
  select (PROD_ID,pvalor)  
comp_prices_type_ploted[comp_prices_type_ploted$pvalor > 0.05,]

# Apenas o P4 confirma a hipótese nula de que as medianas são iguais, o que 
# significa que, para a maioria dos produtos o tipo de venda faz diferença
# para as medianas. 

# 6 - Testando por produto para verificar se a medianas dos preços e diferente por concorrente.

comp_prices_competitor_nested <- comp_prices_nested %>% mutate (pvalor = map(.x= data, .f = ~kruskal.test(.x$COMPETITOR_PRICE,.x$COMPETITOR)$p.value))
comp_prices_competitor_ploted <- comp_prices_competitor_nested  %>% unnest(data) %>% unnest(pvalor) %>%
  select (PROD_ID,pvalor)  
comp_prices_competitor_ploted[comp_prices_competitor_ploted$pvalor > 0.05,]

# Nenhum produto confirma a hipótese nula de que as medianas são iguais 
# confirmando que as medianas são diferentes por concorrente para cada um 
# dos produtos.

# 7 - Retirando o horário da Data das medições
comp_prices_ajusted$DATE_EXTRACTION <- as.Date(comp_prices_ajusted$DATE_EXTRACTION)

# 8 - Agrupando por data de extração e sumarisando pela média diária por produto por concorrente. 

comp_prices_ajusted <- comp_prices_ajusted %>% group_by(PROD_ID,DATE_EXTRACTION,COMPETITOR,PAY_TYPE) %>% summarise(COMPETITOR_PRICE = mean(COMPETITOR_PRICE))

head(comp_prices_ajusted)

# 9 - Pivotando o DataSet para trabalhar. 

comp_pivoted <- comp_prices_ajusted %>% 
                pivot_wider(names_from = c(COMPETITOR,PAY_TYPE), values_from = c(COMPETITOR_PRICE)) 

head(comp_pivoted)

# 10 - Adicionando lag de 1 dia, uma vez que a previsão será feita no dia anterior 

comp_pivoted <- comp_pivoted %>% 
  group_by(PROD_ID) %>% 
  nest() %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C1_", 1:2), lag) )) %>%
  mutate(data = map(data, ~mutate_at(.x, paste0("C2_", 1:2), lag) )) %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C3_", 1:2), lag) )) %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C4_", 1:2), lag) )) %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C5_", 1:2), lag) )) %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C6_", 1:2), lag) )) %>% 
  unnest(cols = c(data)) %>% 
  ungroup()

head(comp_pivoted)


# 11 - Tratando os NA´s

# Quando o competidor não vende um determinado produto em um determinado tipo de pagamento toda a coluna passa 
# a valer 0


comp_pivoted <- comp_pivoted %>% nest(data = c(DATE_EXTRACTION, C1_1,C1_2, C2_1,C2_2, C3_1,C3_2,C4_1,C4_2, C5_1, C5_2, C6_1, C6_2)) %>% 
  mutate(data = map(data, ~{
    .x %>% 
      # Caso toda coluna seja toda NA, input 0
      mutate_if(~all(is.na(.x)), ~0)
        
  })) %>% 
  unnest(cols = c(data))

head(comp_pivoted)

# Preenchendo os NA´s com a média por concorrente por produto

  comp_pivoted <- comp_pivoted %>% 
    #group_by(PROD_ID) %>% 
    nest() %>% 
    mutate(data = map(data, ~mutate_at(.x, paste0("C1_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>%
    mutate(data = map(data, ~mutate_at(.x, paste0("C2_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
    mutate(data = map(data, ~mutate_at(.x, paste0("C3_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
    mutate(data = map(data, ~mutate_at(.x, paste0("C4_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
    mutate(data = map(data, ~mutate_at(.x, paste0("C5_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
    mutate(data = map(data, ~mutate_at(.x, paste0("C6_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
    unnest(cols = c(data))
      

head(comp_pivoted)


# *** TRATANDO O DATASET SALES *** 


# 1 - Agrupando a quantidade de vendas e o preço unitário por produto e por dia.

sales <- sales %>% 
  group_by(PROD_ID, DATE_ORDER) %>% 
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum(REVENUE))

# 2 - Criando a coluna preço unitário
sales <- sales %>% mutate(P_Unitario = REVENUE/QTY_ORDER)
head(sales)

# Plotando os valores para o preço unitário por produto

p1 <- sales %>% ggplot(aes(x = PROD_ID, y = P_Unitario, fill=PROD_ID ))+
  geom_boxplot()

# Plotando os valores de contagem de vendas por produto 

p2 <- sales %>% ggplot(aes(x = PROD_ID, y = QTY_ORDER, fill=PROD_ID ))+
  geom_boxplot()


# Plotando os valores de receita por produto

p3 <- sales %>% ggplot(aes(x = PROD_ID, y = REVENUE, fill=PROD_ID ))+
  geom_boxplot()


# Visualizando 
grid.arrange(p1,p2,p3,nrow=3)

# Comparando os 3 plots é possível notar que na maioria dos casos em que há outliers 
# na quantidade de orDens também há outliers na receita e que o preço unitário possui poucos outliers 
# Vamos optar por manter os outliers, muito embora o caso específico do produto P6 merecesse uma avaliação mais detalhada
# se houvesse tempo. 

# A base não possui NA´s . 

# 3 - Adicionando o lag de 1 dia a exemplo do que foi feito para a base de preços dos concorrentes. 
 
sales <- sales %>% 
    group_by(PROD_ID) %>% 
    nest() %>% 
    mutate(data = map(data, ~mutate_at(.x, "QTY_ORDER", lag)))  %>%
    mutate(data = map(data, ~mutate_at(.x, "REVENUE", lag)))  %>%
    mutate(data = map(data, ~mutate_at(.x, "P_Unitario", lag)))  %>%
    unnest(cols = c(data))

head(sales)


# *** UNIFICANDO AS BASES *** 

# 1 - Ajustando o nome da coluna tipo data nos dois datasets

sales <- sales %>% rename("DATA" = "DATE_ORDER")
comp_pivoted <- comp_pivoted %>% rename("DATA"="DATE_EXTRACTION")

# 2 - Unificando os datasets
DF <- left_join(sales,comp_pivoted)
head(DF)


# 3 - Retirando a primeira data de cada produto por conta do lag 
DF <- DF %>% 
  nest(data = c(DATA, QTY_ORDER, REVENUE, P_Unitario, C1_1,C1_2, C2_1,C2_2, C3_1,C3_2,C4_1,C4_2, C5_1, C5_2, C6_1, C6_2)) %>% 
  mutate(data = map(data, ~.x %>% filter(DATA > first(DATA)))) %>% 
  unnest(cols = c(data))



# 4 Tratando os NA´s 


# Preenchendo os NA´s com a média por produto por concorrente. 

DF <- DF %>% 
  group_by(PROD_ID) %>% 
  nest() %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C1_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>%
  mutate(data = map(data, ~mutate_at(.x, paste0("C2_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C3_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C4_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C5_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
  mutate(data = map(data, ~mutate_at(.x, paste0("C6_", 1:2), ~ifelse(is.na(.x),mean(.x,na.rm=T), .x)))) %>% 
  unnest(cols = c(data))

# Retirando a variável REVENUE por ter relação direta com o preço unitário
DF <- select(DF,-c("REVENUE"))

# Informações gerais sobre o dataframe a ser utilizado para a modelagem.

skimr::skim(DF)

# Iniciando a Modelagem 

set.seed(123)

# construindo o modelo

head(DF)

## Criando treino e teste
DF_split <- initial_split(DF,strata=QTY_ORDER)
DF_train <- training(DF_split)
DF_test <- testing(DF_split)

DF_split
# De 2153 registros 1617 serão utiizados para treino e 536 para teste

head(DF_train)
head(DF_test)


# Medodo de CV


DF_folds <- bootstraps(DF_train, strata=QTY_ORDER)

# Ao tentar utilizar o CV a quantidade de dados ficou pequena então optamos por utilizar o bootstrap

# Criando o Modelo de Referência

use_ranger(formula = QTY_ORDER ~., data = DF_train)

# Vamos utilizar o código sugerido utilizando rando forest e fazendo algumas modificações no featureset.

## Preprocessamento
ranger_recipe <- 
  recipe(formula = QTY_ORDER ~ ., data = DF_train) %>% 
  step_dummy(all_nominal(),-all_outcomes()) %>%  # Criando dummy variables para PROD_ID
  step_date(DATA, features = c("decimal", "year","dow","month"),label=FALSE) %>% # Transformando a Data em fetures numéricas
  step_rm(DATA) # removendo a data

ranger_prep <- prep(ranger_recipe)
juice(ranger_prep)

## Criando o modelo
ranger_spec <- 
  rand_forest(mtry = tune(), 
              min_n = tune(), 
              trees = 1000) %>% # este valor fará com que o tune_grid demore entre 5 a 10 minuto.
  set_mode("regression") %>% 
  set_engine("ranger") 



## Criando o workflow

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 


## Rodando o Modelo ( DEMORA UNS 10 MINUTOS DEPENDENDO DA MÁQUINA)
doParallel::registerDoParallel()
set.seed(50890)
ranger_tune <-
  tune_grid(ranger_workflow, 
            resamples = DF_folds, 
            grid = 10)

# Plotando Modelo
autoplot(ranger_tune)

# Selecionando melhor modelo
lowest_rmse <- show_best(ranger_tune,metric="rmse")
lowest_rsq <- show_best(ranger_tune,metric="rsq")
lowest_rsq

# Finalizando o modelo
final_rf <- ranger_workflow %>% 
  finalize_workflow(select_best(ranger_tune,metric="rsq"))

model_fit <- last_fit(final_rf,DF_split) # Demora uns 2 minutos dependendo da máquina

collect_predictions(model_fit) %>%
  ggplot(aes(QTY_ORDER, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.5, color = "midnightblue") +
  coord_fixed()


# Avaliando importancia das variáveis

imp_spec <- ranger_spec %>%
  finalize_model(select_best(ranger_tune,metric="rsq")) %>%
  set_engine("ranger", importance = "permutation")

workflow() %>%
  add_recipe(ranger_recipe) %>%
  add_model(imp_spec) %>%
  fit(DF_train) %>%
  pull_workflow_fit() %>%
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))
