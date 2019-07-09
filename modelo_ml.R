library(caret)

library(tidyverse)


sentimentos <- read_csv("sentimentos.csv") %>% 
    mutate_if(is.numeric , replace_na, replace = 0) %>% 
    mutate_if(is.integer , replace_na, replace = 0)


features <- read_csv("features_sem_coral.csv") %>%
    filter(arquivo != "nao") %>% 
    mutate_if(is.numeric , replace_na, replace = 0) %>% 
    mutate_if(is.integer , replace_na, replace = 0) %>% 
    filter(dp_duracao <1.5 ) %>% 
    filter(dp_intensidade <1 ) %>% 
    filter(volatilidade_ewma_995 < 10)



dados = sentimentos %>% 
    inner_join(features, by = c("arquivo_completo" = "arquivo"))

set.seed(13)


rows <- sample(nrow(dados))

dados <- dados[rows,]


split <- round(nrow(dados) * .80 )

treino <- dados[1:split, ]

teste <- dados[(split + 1):nrow(dados), ]



treino_positive <- treino %>% 
    select(
        #positive,
        -negative,            
        -artista,               
        -musica,                   
        -anger,                   
        -anticipation,
        -disgust,                 
        -fear,
        -joy,                   
        -sadness,
        -surprise,
        -trust,
        -arquivo_completo
    )


treino_aleat <- treino_positive %>%
    select(positive) %>% 
    rename(new_positive = positive ) %>% 
    sample_frac(1) %>% 
    bind_cols(treino_positive) %>% 
    select(-positive) %>% 
    rename(positive = new_positive)
    
    


controle_cv <- trainControl(
    
    verboseIter = TRUE,
    #index = folds, 
    returnResamp = "all",
    number = 5,
    returnData = TRUE,
    savePredictions = "all",
    method = "cv",
    #,
    allowParallel = FALSE,
    # seeds = 1:41
)


model_lm_positive <- train(
    positive ~ . ,
    treino_positive,
    metric = "RMSE",
    method = "lm",
    trControl = controle_cv,
    preProcess = c( "zv", "nzv", "center", "scale", "pca")
    # tuneGrid = expand.grid(
    #     mstop = c(10, 20),
    #     prune = c("yes")
    # )
)

model_lm_positive


model_lm_aleat_positive <- train(
    positive ~ . ,
    treino_positive,
    metric = "RMSE",
    method = "lm",
    trControl = controle_cv,
    preProcess = c( "zv", "nzv", "center", "scale", "pca")
    # tuneGrid = expand.grid(
    #     mstop = c(10, 20),
    #     prune = c("yes")
    # )
)


model_lm_aleat_positive



model_glmnet_positive <- train(
    positive ~ . ,
    treino_positive,
    metric = "RMSE",
    method = "glmnet",
    trControl = controle_cv,
    preProcess = c( "zv", "nzv", "center", "scale", "pca"),
    tuneGrid = expand.grid(
        alpha = c(0, 0.5, 1),
        lambda = c(0.3, 0.2, 0.1)
    )
)

model_glmnet_positive

plot(model_glmnet_positive)


model_rf_positive <- train(
    positive ~ . ,
    treino_positive,
    metric = "RMSE",
    method = "rf",
    trControl = controle_cv,
    preProcess = c( "zv", "nzv", "center", "scale", "pca"),
    tuneGrid = expand.grid(
         mtry = c(10, 15, 20, 30, 50, 100)
    )
)

model_rf_positive

plot(model_rf_positive)



# model_loes <- train(
#     positive ~ . ,
#     treino_positivo,
#     metric = "RMSE",
#     method = "gamLoess",
#     trControl = controle_cv,
#     preProcess = c( "zv", "nzv", "center", "scale")
#     # tuneGrid = expand.grid(
#     #     size = c(100)
#     # )
# )
# 
# model_loes$pred
# 
# plot(model_loes)





