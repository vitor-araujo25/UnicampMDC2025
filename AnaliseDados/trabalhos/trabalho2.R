########################################
# Trabalho 2 - INF-0612          
# Nome(s): Vitor de Oliveira Fernandez Araujo
#          Vitor Sancho Cardoso
########################################
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

names <- c("horario", "temperatura", "vento", "umidade", "sensacao")
#setwd("")
cepagri <- read.csv(".\\cepagri.csv", header = FALSE, sep = ";", col.names = names)
head(cepagri)

# analisando o tipo de dado para cada coluna
sapply(cepagri, class)

## ajustando tipo de dado das colunas temperatura e horário
cepagri$temperatura <- as.numeric(cepagri$temperatura)

# usando formato timestamp para facilitar análises por data com maior performance
cepagri$horario <- as.POSIXct(cepagri$horario,
                              format = '%d/%m/%Y-%H:%M', 
                              tz = "America/Sao_Paulo")

# incluindo colunas ano e mes para facilitar tratamento
cepagri$ano <- as.POSIXlt(cepagri$horario,
                          format = '%d/%m/%Y-%H:%M', 
                          tz = "America/Sao_Paulo")$year + 1900

cepagri$mes <- as.POSIXlt(cepagri$horario,
                          format = '%d/%m/%Y-%H:%M', 
                          tz = "America/Sao_Paulo")$mon + 1

cepagri$dia <- as.POSIXlt(cepagri$horario,
                          format = '%d/%m/%Y-%H:%M', 
                          tz = "America/Sao_Paulo")$mday

# removendo registros dos anos 2014 e 2025, que serão desconsiderados da análise
# por não considerarem os anos completos

cepagri <- cepagri[!(cepagri$ano == 2014 | cepagri$ano == 2025),]

summary(cepagri)
###############################################

# A coluna de sensação térmica tem um volume de NAs muito maior do que as outras,
# conforme problema na fonte de dados foi explicado em aula.
# Visando a não descartar dados demais, começaremos removendo as linhas em que
# temperatura, vento ou umidade sejam NA, permitindo registros onde sensacao=NA mas
# que tenham valores válidos nas outras colunas

## limpeza de registros NA
registros_com_NA <- c(is.na(cepagri$umidade) | is.na(cepagri$vento) | is.na(cepagri$temperatura))

registros_com_NA

cepagri_sem_NAs <- cepagri[!registros_com_NA,]
cepagri_sem_NAs

paste("Foram removidos", nrow(cepagri) - nrow(cepagri_sem_NAs),"pois continham algum campo com valor NA", sep = " ")
nrow(cepagri)
nrow(cepagri_sem_NAs)

# sobrescrevendo para simplificar o resto do código
cepagri <- cepagri_sem_NAs 

summary(cepagri)
#######################################

## o dataset aprsenta valores minimos absurdos para temperatura e umidade, que 
## iremos remover

# extrapolando uma temperatura impossivel em Campinas
cepagri <- cepagri[(!cepagri$temperatura < -50),]
# umidade do ar entre 0 e 100
cepagri <- cepagri[(!cepagri$umidade < 0),]


summary(cepagri)
#######################################

## removendo registros com leituras consecutivas
## que podem ser capturadas por problemas nos sensores
## ou alterações equivocadas na metodologia de captura
## dos dados

consecutive <- function(vector, k = 2) {
  n <- length(vector)
  result <- logical(n)
  for (i in k:n)
    if (all(vector[(i-k+1):i] == vector[i]))
      result[(i-k+1):i] <- TRUE
  return(result)
}

filtro_consecutivos <- consecutive(cepagri$temperatura, 144) # 144 leituras correspondem a 1 dia

cepagri_sem_consecutivos <- cepagri[!filtro_consecutivos,]
cepagri_sem_consecutivos

summary(cepagri_sem_consecutivos)

cepagri <- cepagri_sem_consecutivos
######################################################

# Neste ponto, ainda percebemos anomalias nos valores de sensacao termica, que têm
# minimos e máximo -8.0 e 99.9, respectivamente. 


# Com o filtro abaixo, podemos ver que claramente o 99.9 se trata de um erro isolado, e devemos remove-los
# Para não afetar os dados dos dias em que a sensacao térmica foi calculada incorretamente,
# iremos apenas atribuir NA para esta coluna.
sensacao_menor_99 <- cepagri[order(-cepagri[cepagri$sensacao < 99.9,]$sensacao),]

# O filtro de igualdade também dá match com valores NA, então precisamos desconsidera-los
# na busca. Ao executar a consulta, vemos que tínhamos 142 registros com sensacao = 99.9
nrow(cepagri[!is.na(cepagri$sensacao) & cepagri$sensacao == 99.9,])

cepagri[!is.na(cepagri$sensacao) & (cepagri$sensacao == 99.9), 5] <- NA
summary(cepagri)

######################################################
cepagri_sem_sensacao_NA <- cepagri[!is.na(cepagri$sensacao),]
summary(cepagri_sem_sensacao_NA)


######################################################
### ANALISE DOS DADOS ################################
######################################################

## BOX PLOT DE SENSAÇÃO TÉRMICA AO LONGO DOS ANOS

# A partir da metade do ano de 2023, os dados de sensação termica estiveram com erro,
# então excluímos os anos de 2023 e 2024 da visualização
ggplot(cepagri_sem_sensacao_NA[cepagri_sem_sensacao_NA$ano < 2023,], 
       aes(x = factor(ano), y = sensacao)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(outlier.alpha = 0.2, fill="#bf522e") +
  labs(x = "Ano", y = "Sensação Térmica (°C)") +
  labs(caption = "Distribuição de Sensação Térmica por Ano") + 
  theme_minimal() +
  theme(
    plot.caption = element_text(
      hjust=0.5,
      size=10,
      margin = margin(t=10),
      color = "grey50"
    )
  )

###################################################################
## HISTOGRAMA DE UMIDADE RELATIVA

histogram_umidade <- ggplot(cepagri[cepagri$umidade > 0,], aes(x = umidade)) +
  
  geom_histogram(
    aes(y = after_stat(100*(count/sum(count)))),
    binwidth = 5, 
    boundary = 0, 
    color="white",
    fill="grey30"
  ) +
  annotate("rect", xmin = 0, xmax = 30, ymin = 0, ymax = 15,
           alpha = .4, fill="#990000") +
  annotate("rect", xmin = 80, xmax = 100, ymin = 0, ymax = 15,
           alpha = .5, fill="#0000cc") +
  labs(x = "Umidade Relativa do Ar (%)", y = "Frequência de Observações (%)") +
  labs(caption = "Frequências Relativas de Umidade Relativa do Ar no período 2015 a 2024") + 
  theme_minimal() +
  theme(
    plot.caption = element_text(
      hjust=0.5,
      size=10,
      margin = margin(t=10),
      color = "grey50"
    )
  )

#adicionando labels nas áreas sombreadas
umidade_data <- ggplot_build(histogram_umidade)$data[[1]]
histogram_umidade <- 
  histogram_umidade + 
  annotate("text", x = 15, y = 7.5, label = paste("Total < 30%: ", format(sum(umidade_data[umidade_data$xmin < 30,]$y), digits=3), "%") ,colour = "white") +
  annotate("text", x = 90, y = 13, label = paste("Total > 80%: \n", format(sum(umidade_data[umidade_data$xmin > 80,]$y), digits=3), "%") ,colour = "white")


seletor_estacao <- function(dia, mes){
  case_when(
    (mes == 12 & dia >= 21) | mes %in% c(1,2) | (mes == 3 & dia <= 20) ~ "Verão",
    (mes == 3 & dia >= 21) | mes %in% c(4,5) | (mes == 6 & dia <= 20) ~ "Outono",
    (mes == 6 & dia >= 21) | mes %in% c(7,8) | (mes == 9 & dia <= 20) ~ "Inverno",
    (mes == 9 & dia >= 21) | mes %in% c(10,11) | (mes == 12 & dia <= 20) ~ "Primavera"
  )
}

cepagri_estacoes <- cepagri %>% 
  mutate(
    estacao = seletor_estacao(dia, mes)
  )

## compilando medianas da umidade por estacao
tabela_umidade_estacao <- cepagri_estacoes %>% 
  group_by(estacao) %>% 
  summarise(mediana_umidade = median(umidade, na.rm = TRUE), .groups = "drop")

#########################################################
### ANÁLISE DE VELOCIDADE DOS VENTOS POR MES

cepagri_vento_metro_por_sengundo <- cepagri
## convertendo velocidade para m/s
cepagri_vento_metro_por_sengundo$vento_ms <- cepagri_vento_metro_por_sengundo$vento / 3.6


# função gera percentuais de ocorrencias de ventos em cada uma
# das faixas contidas no vetor limites 
conta_vento_percentual <- function(df, limites) {
  limites <- sort(limites)
  
  breaks <- c(-Inf, limites, Inf)
  labels <- c(
    paste0("abaixo_", limites[1]," m/s"),
    paste0("entre_", limites[1], "_", limites[2]," m/s"),
    paste0("entre_", limites[2], "_", limites[3]," m/s"),
    paste0("acima_", limites[3] ," m/s")
  )
  
  # Classifica cada observação no intervalo correto
  df$faixa <- cut(df$vento_ms, breaks = breaks, labels = labels, right = FALSE)
  
  # Conta por mês
  tabela <- as.data.frame.matrix(table(df$mes, df$faixa))
  
  # Converte para percentual (linha por mês soma 100%)
  tabela_percentual <- tabela / rowSums(tabela) * 100
  
  # Adiciona coluna de mês
  tabela_percentual$mes <- rownames(tabela_percentual)
  
  # Reorganiza ordem dos meses
  tabela_percentual <- tabela_percentual[order(as.numeric(as.character(tabela_percentual$mes))), ]
  
  rownames(tabela_percentual) <- NULL
  
  return(tabela_percentual)
}

faixas_de_velocidade_do_vento <- c(4, 12, 25) 
faixa_vento <- conta_vento_percentual(cepagri_vento_metro_por_sengundo, faixas_de_velocidade_do_vento)

## ajustando para long, facilita a montagem do grafico
df_faixa_vento_long <- melt(faixa_vento, id.vars = "mes", 
                            variable.name = "faixa", 
                            value.name = "percentual")

df_long <- df_faixa_vento_long %>%
  group_by(mes) %>%
  mutate(dominante = percentual == max(percentual)) %>%
  ungroup()

# garantindo ordenação de meses no gráfico 
df_long$mes <- factor(df_long$mes, levels = 1:12)

# Heatmap
ggplot(df_long, aes(x = factor(mes), y = faixa, fill = percentual)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(percentual, 1)), color = "black", size = 3) +
  scale_fill_gradient(
    low = "#cce5ff",  # azul claro
    high = "#003366", # azul escuro
    name = "Percentual (%)"
  ) +
  labs(
    x = "Mês",
    y = "Faixa",
    caption = "Heatmap das faixas de velocidade de vento por mês"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(
      hjust = 0.5,
      size = 10,
      margin = margin(t = 10),
      color = "grey50"
    )
  )

##########################################################
#### ANALISE DE SENSAÇÃO TÉRMICA POR ESTAÇÕES DO ANO AGRUPADAS POR HORÁRIO

cepagri_com_flag_de_estacao_do_ano <- cepagri_sem_sensacao_NA %>%
  mutate(
    ano_estacao = case_when(
      # Verão: dezembro (ano atual), janeiro e fevereiro (ano anterior) e março até dia 20
      (mes == 12 & dia >= 21) ~ ano,       # dezembro → verão do ano atual
      (mes %in% c(1,2)) ~ ano - 1,            # jan, fev → verão do ano anterior
      (mes == 3 & dia <= 20) ~ ano - 1,   # março até 20 → verão do ano anterior
      
      # Outono
      (mes == 3 & dia >= 21) | mes %in% c(4,5) | (mes == 6 & dia <= 20) ~ ano,
      
      # Inverno
      (mes == 6 & dia >= 21) | mes %in% c(7,8) | (mes == 9 & dia <= 20) ~ ano,
      
      # Primavera
      (mes == 9 & dia >= 21) | mes %in% c(10,11) | (mes == 12 & dia <= 20) ~ ano
    ),
    estacao = seletor_estacao(dia, mes),
    estacao_ano = paste(estacao, ano_estacao)
  ) %>%
  select(-dia, -ano_estacao)

ordem_estacoes <- c("Verão", "Outono", "Inverno", "Primavera")
cepagri_com_flag_de_estacao_do_ano_agrupadas_ordenados <- cepagri_com_flag_de_estacao_do_ano %>%
  mutate(
    estacao_ano = paste(estacao, ano)
  ) %>%
  group_by(estacao_ano) %>%
  summarise(
    temp_media = mean(temperatura, na.rm = TRUE),
    temp_max = max(temperatura, na.rm = TRUE),
    temp_min = min(temperatura, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    estacao = sub(" .*", "", estacao_ano),
    ano_num = as.numeric(sub(".* ", "", estacao_ano)),
    estacao = factor(estacao, levels = ordem_estacoes)
  ) %>%
  arrange(ano_num, estacao)

cepagri_com_flag_de_estacao_do_ano_agrupadas_ordenados

df_media_sensacao <- cepagri_com_flag_de_estacao_do_ano %>%
  mutate(hora = format(as.POSIXct(horario), "%H:00:00")) %>%
  group_by(hora, estacao) %>%
  summarise(
    sensacao_media = mean(sensacao, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(hora)

## ajustando representação de horario
df_media_sensacao_horario_formatado <- df_media_sensacao %>%
  mutate(hora = sprintf("%02dH", as.integer(substr(hora, 1, 2))))

## ajustando formado long para wide, para melhor visalização
df_media_sensacao_wide <- df_media_sensacao_horario_formatado %>%
  pivot_wider(
    names_from = hora,           
    values_from = sensacao_media 
  )

ggplot(df_media_sensacao_horario_formatado, aes(x = hora, y = sensacao_media, color = estacao, group = estacao)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    x = "Hora do dia",
    y = "Sensação térmica (°C)",
    color = "Estação",
    caption = "Sensação térmica média por hora do dia"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(
      hjust = 0.5,
      size = 10,
      margin = margin(t = 10),
      color = "grey50"
    )
  )

###################################################################
## COMPARAÇÃO COM MÉDIAS MENSAIS DE TEMPERATURA EM CAMPINAS, SEGUNDO CLIMATE_DATA.ORG

climate_data_org_temperatura_mensal_historica <- data.frame(
  mes_extenso = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                  "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
  mes = 1:12,
  temp_media_historica = c(23.4, 23.7, 22.9, 21.8, 19.0, 18.0,
                           17.9, 19.3, 21.3, 22.5, 22.3, 23.2),
  temp_max_historica = c(27.9, 28.4, 27.5, 26.8, 24.2, 23.7,
                         23.9, 25.8, 27.6, 28.3, 27.4, 28.0),
  temp_min_historica = c(19.9, 19.9, 19.2, 17.7, 14.7, 13.5,
                         13, 13.9, 16.1, 17.8, 18.2, 19.4)
)


cepagri_media_temperatura_diaria <- cepagri %>%
  group_by(ano, mes, dia) %>%
  summarise(temperatura_max = mean(temperatura, na.rm = TRUE))

## unindo dataframes cepagri e climate_data.org
cepagri_media_temperatura_diaria <- merge(cepagri_media_temperatura_diaria, climate_data_org_temperatura_mensal_historica, by = "mes")

## criando limiares de temperatura nas faixas em que a média foi superior a 1°C,1.5°C,2°C e 2.5°C
temperaturas_acima_media_mais_1_grau <- cepagri_media_temperatura_diaria[cepagri_media_temperatura_diaria$temperatura_max > cepagri_media_temperatura_diaria$temp_media_historica + 1,]
temperaturas_acima_media_mais_1_grau <- temperaturas_acima_media_mais_1_grau %>%
  mutate(limiar = "+1°C")

temperaturas_acima_media_mais_1.5_grau <- cepagri_media_temperatura_diaria[cepagri_media_temperatura_diaria$temperatura_max > cepagri_media_temperatura_diaria$temp_media_historica + 1.5,]
temperaturas_acima_media_mais_1.5_grau <- temperaturas_acima_media_mais_1.5_grau %>%
  mutate(limiar = "+1.5°C")

temperaturas_acima_media_mais_2_grau <- cepagri_media_temperatura_diaria[cepagri_media_temperatura_diaria$temperatura_max > cepagri_media_temperatura_diaria$temp_media_historica + 2,]
temperaturas_acima_media_mais_2_grau <- temperaturas_acima_media_mais_2_grau %>%
  mutate(limiar = "+2°C")

temperaturas_acima_media_mais_2.5_grau <- cepagri_media_temperatura_diaria[cepagri_media_temperatura_diaria$temperatura_max > cepagri_media_temperatura_diaria$temp_media_historica + 2.5,]
temperaturas_acima_media_mais_2.5_grau <- temperaturas_acima_media_mais_2.5_grau %>%
  mutate(limiar = "+2.5°C")


## Unindo todos os dataframes com os limiares de temperatura
df_limiares_temperatura <- bind_rows(temperaturas_acima_media_mais_1_grau, temperaturas_acima_media_mais_1.5_grau, temperaturas_acima_media_mais_2_grau, temperaturas_acima_media_mais_2.5_grau)

df_total_registro_por_limiar_ano <- df_limiares_temperatura %>%
  count(ano, limiar, name = "total_registros")

## para garantir a ordem na apresentação do gráfico
df_total_registro_por_limiar_ano$limiar <- factor(df_total_registro_por_limiar_ano$limiar, levels = c("+1°C", "+1.5°C", "+2°C", "+2.5°C"))

ggplot(df_total_registro_por_limiar_ano, aes(x = factor(ano), y = total_registros, fill = limiar)) +
  geom_col(position = "dodge") +
  labs(
    x = "Ano",
    y = "Total de dias",
    fill = "Limiar",
    caption = "Total de dias com temperatura máxima maior que a média histórica"
  ) +
  scale_fill_brewer(
    type = "seq",
    palette = "Blues",
    direction = -1
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 10),
    plot.caption = element_text(
      hjust = 0.5,
      size = 10,
      margin = margin(t = 10),
      color = "grey50"
    )
  )

## para melhor observação no formato de tabela
df_total_registro_por_limiar_ano_wide <- df_total_registro_por_limiar_ano %>%
  pivot_wider(
    names_from = limiar,
    values_from = total_registros,
    values_fill = 0  # preenche com zero se algum limiar não ocorrer em determinado ano
  )
