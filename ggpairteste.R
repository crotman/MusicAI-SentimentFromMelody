
library(tidyverse)
library(GGally)


sentimentos <- read_csv("sentimentos.csv")


features <- read_csv("features.csv")

selecionados <- features %>% 
  select(volatilidade_ewma_99,
         dp_duracao
  )


ggpairs(selecionados)
