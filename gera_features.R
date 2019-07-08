library(tidyverse)

library(magrittr)

# notas <- read_csv("c:/musicameta/notas.csv")


# notas_musica <- notas %>% 
#     filter(arquivo == "c:/musica/130000_Pop_Rock_Classical_Videogame_EDM_MIDI_Archive[6_19_15]/F/F/Frank Sinatra - Love and Marriage.mid") %>% 
#     filter(track == 2) 


notas_musica <- read_csv("musicateste.csv")

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
    select(nome, nota_inicial)

escala_escolhida_com_notas <- escala_escolhida %>% 
    inner_join(escalas, by = c("nome", "nota_inicial"))

notas_musica_na_escala_escolhida <- notas_musica %>% 
    mutate(note_abs = note %% 12) %>% 
    left_join(escala_escolhida_com_notas, by = c("note_abs" = "nota_no_tom"))     



#escala
#frequencia dos graus unidade
#frequencia dos graus em tempo
#frequencia dos intervalos
#frequencia dos intervalos em tempo
#media da duracao das notas
#desvio padrao da duracao das notas
#desvio padrao da "velocidade"
#velocidade media de mudanca das notas
#amplitude da melodia



















