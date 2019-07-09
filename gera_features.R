library(tidyverse)

library(magrittr)

notas <- readRDS("notas.rds")

sentimentos <- read_csv("sentimentos.csv")

notas_por_musica <- notas %>% 
    group_by(arquivo) %>% 
    nest() %>% 
    mutate(row = row_number())
    


pega_features <- function(row, notas_musica, arquivo) 
{

    print(arquivo)   
    print(row)

    notas_musica <- notas_musica %>% 
        arrange(time) %>% 
        mutate(row = row_number() ) %>%   
        filter(row < 2000) %>% 
        select(-row)
    
    
        
    template_escala_maior = tibble(
        nome = "maior",
        notas = c(0, 2, 4, 5, 7, 9, 11),
        vantagem = 0.1
    )
    
    
    template_escala_dorio = tibble(
        nome = "doria",
        notas = c(0, 2, 3, 5, 7, 9, 10),
        vantagem = 0
    )
    
    
    template_escala_frigio = tibble(
        nome = "frigio",
        notas = c(0, 1, 3, 5, 7, 8, 10),
        vantagem = 0
        
    )
    
    
    template_escala_lidio = tibble(
        nome = "lidio",
        notas = c(0, 2, 4, 6, 7, 9, 11),
        vantagem = 0
        
    )
    
    
    template_escala_mixolidio = tibble(
        nome = "mixolidio",
        notas = c(0, 2, 4, 5, 7, 9, 10),
        vantagem = 0
        
    )
    
    
    template_escala_menor = tibble(
        nome = "menor",
        notas = c(0, 2, 3, 5, 7, 8, 10),
        vantagem = 0.1
        
    )
    
    template_escala_locrio = tibble(
        nome = "locrio",
        notas = c(0, 1, 3, 5, 6, 8, 10, 11),
        vantagem = 0
        
    )
    
    
    template_escala_pentatonica_maior = tibble(
        nome = "pentatonica maior",
        notas = c(0, 2, 4, 7, 9),
        vantagem = 0.05
        
    )
    
    
    template_escala_pentatonica_menor = tibble(
        nome = "pentatonica menor",
        notas = c(0, 3, 5, 7, 10),
        vantagem = 0.05
        
    )
    
    
    nota_inicial <- tibble(
        nota_inicial = 0:10
    ) 
    
    
    templates <- bind_rows(
        
        template_escala_maior,
        template_escala_dorio,
        template_escala_frigio,
        template_escala_lidio,
        template_escala_mixolidio,
        template_escala_menor,
        template_escala_locrio,
        template_escala_pentatonica_maior,
        template_escala_pentatonica_menor
        
    ) %>% 
        group_by(nome) %>% 
        mutate(grau = row_number() )
    
    
    escalas <- templates %>% 
        crossing(nota_inicial) %>% 
        mutate(nota_no_tom = (notas + nota_inicial) %% 12) %>% 
        arrange(nome, nota_inicial)
    
    
    
    notas_musica_na_escala <- notas_musica %>% 
        mutate(note_abs = note %% 12) %>% 
        left_join(escalas, by = c("note_abs" = "nota_no_tom")) 
    
    
    escala_escolhida <-  notas_musica_na_escala %>% 
        group_by(nome, nota_inicial, vantagem, grau) %>% 
        summarize(frequencia_no_grau = n() * (1 + max(vantagem))) %>% 
        ungroup() %>% 
        group_by(nome, nota_inicial) %>% 
        mutate(frequencia_escala = sum(frequencia_no_grau)) %>% 
        arrange(desc(frequencia_escala),nome, nota_inicial, grau ) %>% 
        ungroup() %>% 
        group_by(nome, nota_inicial) %>% 
        mutate(freq_grau_mais_frequente = max(frequencia_no_grau) ) %>%
        filter(freq_grau_mais_frequente == frequencia_no_grau ) %>% 
        mutate(frequencia_escala = if_else(grau == 1, frequencia_escala + 1 , frequencia_escala )) %>% 
        ungroup() %>% 
        top_n(1, frequencia_escala ) %>% 
        mutate(row = row_number()) %>% 
        filter(row == 1) %>% 
        select(-row) %>% 
        select(nome, nota_inicial)
    
    
    
    escala_escolhida_com_notas <- escala_escolhida %>% 
        inner_join(escalas, by = c("nome", "nota_inicial"))
    
    notas_musica_na_escala_escolhida <- notas_musica %>% 
        mutate(note_abs = note %% 12) %>% 
        left_join(escala_escolhida_com_notas, by = c("note_abs" = "nota_no_tom")) %>% 
        mutate(grau = if_else(is.na(grau), as.integer(0), grau))
    
    
    intervalos_possiveis <- tibble(origem = 0:8) %>% 
        crossing(destino = 0:8) %>% 
        mutate(nome_intervalo = paste0(origem,"_a_", destino) )
    
    
    frequencia_graus <- notas_musica_na_escala_escolhida %>% 
        group_by(grau) %>% 
        summarise(n = n(), tempo = sum(length)) %>%         
        mutate(n = n / sum(n), tempo = tempo /sum(tempo))
    
    
    saida_frequencia_graus_tempo <- frequencia_graus %>% 
        mutate(grau = paste0("freq_tempo_grau_", grau)) %>% 
        select(-n) %>% 
        spread(grau, tempo) 
    
    saida_frequencia_graus_n <- frequencia_graus %>% 
        mutate(grau = paste0("freq_n_grau_", grau)) %>% 
        select(-tempo) %>% 
        spread(grau, n) 
    
    
    frequencia_intervalos <-  notas_musica_na_escala_escolhida %>% 
        arrange(time) %>% 
        mutate( destino = grau, origem = lag(grau), length_origem = lag(length)) %>% 
        left_join(intervalos_possiveis) %>% 
        group_by(nome_intervalo) %>% 
        summarise(n = n(), tempo = sum(length)) %>%         
        mutate(n = n / sum(n), tempo = tempo /sum(tempo)) 
    
    saida_frequencia_intervalos_n <- frequencia_intervalos %>% 
        mutate(nome_intervalo = paste0("freq_n_intervalo_", nome_intervalo)) %>% 
        select(-tempo) %>% 
        spread(nome_intervalo, n) 
    
    saida_frequencia_intervalos_tempo <- frequencia_intervalos %>% 
        mutate(nome_intervalo = paste0("freq_tempo_intervalo_", nome_intervalo)) %>% 
        select(-n) %>% 
        spread(nome_intervalo, tempo) 

    desvio_padrao_duracao_notas <- notas_musica_na_escala_escolhida %>% 
        summarise(dp_duracao = sd(length)/mean(length)) 
    
    
    desvio_padrao_intensidade_notas <- notas_musica_na_escala_escolhida %>%
        summarise(dp_intensidade = sd(velocity)/mean(velocity))
    
    
    amplitude_melodia <- notas_musica_na_escala_escolhida %>% 
        summarise(nota_max = max(note), nota_min = min(note)) %>% 
        mutate(amplitude = nota_max - nota_min) %>% 
        select(amplitude) 
    
    
    tempo_continuo <- tibble(tempo_continuo = seq(1, max(notas_musica_na_escala_escolhida$time), max(notas_musica_na_escala_escolhida$time)/1000))
    
    ewma.filter <- function (x, ratio) {
        c(stats::filter(x * ratio, 1 - ratio, "recursive", init = x[1]))
    }
    
    notas_tempo_continuo <- tempo_continuo %>% 
        crossing(notas_musica_na_escala_escolhida) %>% 
        filter(time <= tempo_continuo) %>% 
        group_by(tempo_continuo) %>% 
        mutate(max_time = max(time)) %>%
        filter(time == max_time) %>% 
        ungroup() %>% 
        arrange(time) 
    
    ewma_995 <- tibble(ewma_995 = ewma.filter(notas_tempo_continuo$note, 0.005))
    
    ewma_99 <- tibble(ewma_99 = ewma.filter(notas_tempo_continuo$note, 0.01))
    
    notas_tempo_continuo_vol <- bind_cols(notas_tempo_continuo, ewma_995, ewma_99 )
    
    volatilidade <- notas_tempo_continuo_vol %>% 
        summarise(volatilidade_ewma_995 = mean(abs(ewma_995-note)), volatilidade_ewma_99 = mean(abs(ewma_99-note) ))
    
    notas_uma <- notas_musica %>% 
        mutate(fim = time + length) %>% 
        arrange(time) %>% 
        filter( time < lag(fim)   ) %>% 
        nrow()
    
    
    if (notas_uma == 0)
    {
        bind_cols(
            saida_frequencia_graus_tempo,
            saida_frequencia_graus_n,
            saida_frequencia_intervalos_tempo,
            saida_frequencia_intervalos_n,
            desvio_padrao_duracao_notas,
            desvio_padrao_intensidade_notas,
            amplitude_melodia,
            volatilidade
        ) %>% 
            mutate(nome_escala = escala_escolhida$nome ) %>% 
            mutate(arquivo = arquivo)
    }
    else
    {
        tibble(arquivo = "nao")
    }
    
}   


retorno <- pmap_df( list (row = notas_por_musica$row, notas_musica = notas_por_musica$data, arquivo = notas_por_musica$arquivo),  possibly( pega_features, tibble(arquivo ="nao" ))   )

write_csv(retorno, "features_sem_coral.csv")




