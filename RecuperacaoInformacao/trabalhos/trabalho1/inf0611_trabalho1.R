######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Vitor de Oliveira Fernandez Araujo                             #
#   - Vitor Sancho Cardoso                                           #
#   -                                                                #
#                                                                    #
######################################################################

######################################################################
# Configurações Preliminares                                         #
######################################################################

# Carregando as bibliotecas
library(tokenizers)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)


# Carregando os arquivos auxiliares
source("./ranking_metrics.R", encoding = "UTF-8")
source("./trabalho1_base.R", encoding = "UTF-8")

# Configure aqui o diretório onde se encontram os arquivos do trabalho
# setwd("")


######################################################################
#
# Questão 1
#
######################################################################

# Lendo os documentos (artigos da revista TIME)
# sem processamento de texto (não mude essa linha)
docs <- process_data("time.txt", "XX-Text [[:alnum:]]", "Article_0", 
                     convertcase = TRUE, remove_stopwords = FALSE)
# Visualizando os documentos (apenas para debuging)
head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)
# Visualizando as consultas (apenas para debuging)
head(queries)
# Exemplo de acesso aos tokens de uma consulta
# q1 <- queries[queries$doc_id == "Query_01",]; q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header = TRUE)

# Visualizando os ground_truths (apenas para debuging)
head(ground_truths)
# Exemplo de acesso vetor de ground_truth da consulta 1:
# ground_truths[1,]
# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
names(ground_truths)[ground_truths[1,]==1]


# Computando a matriz de termo-documento
term_freq <- document_term_frequencies(docs, term="word")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq, k=1.2, b=0.75))
# Visualizando as estatísticas da coleção (apenas para debuging)
# head(docs_stats)

######################################################################
#
# Questão 2
#
######################################################################


# query: Elemento da lista de consultas, use a segunda coluna desse 
#        objeto para o cálculo do ranking
# ground_truth: Linha do data.frame de ground_truths referente a query
# stats: data.frame contendo as estatísticas da base
# stat_name: Nome da estatística de interesse, como ela está escrita 
#            no data.frame stats
# top: Tamanho do ranking a ser usado nos cálculos de precisão 
#      e revocação
# text: Título adicional do gráfico gerado, deve ser usado para 
#       identificar a questão e a consulta
computa_resultados <- function(query, ground_truth, stats, stat_name, 
                               top, text) {
  # Criando ranking (função do arquivo base)
  # Dica: você pode acessar a segunda coluna da query a partir de $word ou [["word"]]
  ranking <- get_ranking_by_stats(stat_name, stats, query$word)
  # Visualizando o ranking (apenas para debuging)
  #head(ranking, n = 5)
  
  # Calculando a precisão
  # Dica: para calcular a precisão, revocação e utilizar a função plot_prec_e_rev,
  # utilize a coluna doc_id do ranking gerado (você pode acessar com $doc_id)
  p <- precision(ground_truth, ranking$doc_id, top)

  # Calculando a revocação
  r <- recall(ground_truth, ranking$doc_id, top)

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p, 
            "\tRevocação: ", r, "\n"))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id, ground_truth, k = top, text = text) 
}

# Definindo a consulta 1 
# Dicas para as variáveis consulta1 e n_consulta1:
# Para a variável consulta1, você deve acessar os tokens de uma consulta, conforme
# o exemplo da linha 52 e 53.
# Para a variável n_consulta1, você deve informar o número da consulta. Por exemplo,
# se usar a Query_01 como consulta, n_consulta1 deve receber o valor 1.
n_consulta1 <- 2
str_consulta1 <- paste("Query_0", n_consulta1, sep=""); str_consulta1
consulta1 <- queries[queries$doc_id == str_consulta1,]; consulta1

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 15, "titulo")

### análises extras
# rk1 <- get_ranking_by_stats("bm25", docs_stats, consulta1$word)
# head(rk, n=10)$doc_id
# ground_truths[ground_truths[n_consulta1,] == 1]
# precision(ground_truths[n_consulta1,], rk1$doc_id,4)
# average_precision(ground_truths[n_consulta1,], rk1$doc_id,4)
# recall(ground_truths[n_consulta1,], rk1$doc_id, 4)
###

# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1, ground_truths[n_consulta1, ], docs_stats,
                   "tf_idf", top = 20, paste("TF-IDF - ",str_consulta1, sep=""))

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1, ground_truths[n_consulta1, ], docs_stats,
                   "bm25", top = 20, paste("BM25 - ",str_consulta1, sep=""))


# Definindo a consulta 2 
n_consulta2 <- 33
str_consulta2 <- paste("Query_0", n_consulta2, sep=""); str_consulta2
consulta2 <- queries[queries$doc_id == str_consulta2,]; consulta2

### análises extras
# rk2 <- get_ranking_by_stats("bm25", docs_stats, consulta2$word)
#head(rk2, n=10)$doc_id # top n do ranking
# precision(ground_truths[n_consulta2,], rk2$doc_id,5)
# recall(ground_truths[n_consulta2,], rk2$doc_id, 10)
# average_precision(ground_truths[n_consulta2,], rk2$doc_id, 10)
# precision(ground_truths[n_consulta2,], rk2$doc_id,10)
###

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2, ground_truths[n_consulta2, ], docs_stats,
                   "tf_idf", top = 20, paste("TF-IDF - ",str_consulta2, sep=""))

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2, ground_truths[n_consulta2, ], docs_stats,
                   "bm25", top = 20, paste("BM25 - ",str_consulta2, sep=""))

######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
#
# Para a Query_02 (consulta 1), o modelo que apresentou melhor performance foi o BM25.
# Para um K = 20, comparando TF-IDF e BM25, foi possível observar um valor igual para 
# revocação e precisão, o que num primeiro momento poderia indicar que os dois têm 
# performances equivalentes, porém, ao analisar os gráficos em função do top K, percebemos 
# que no TF-IDF o ranking só alcança os máximos de precisão e revocação em K = 11, 
# enquanto que no BM25, os resultados relevantes já são completamente retornados com K = 4.
# 
# Para a Query_033 (consulta 2), foram observados valores superiores de revocação e
# precisão para BM25, o que poderia evidenciar uma melhor performance em
# relação do TF-IDF. Porém, levando em consideração a precisão média, o modelo TF-IDF
# performou melhor. Ou seja, este modelo foi capaz de produzir resultados relevantes mais
# rápido que o BM25, que errou no seu primeiro item retornado. Este comportamento também
# é evidenciado na análise gráfica, onde a curva de precisão do TF-IDF se inicia e mantém
# em 1 por mais tempo do que o BM25, que aumenta gradativamente a precisão 
# conforme o aumento do K.
#

######################################################################
#
# Questão 3
#
######################################################################
# Na função process_data está apenas a função para remoção de 
# stopwords está implementada. Sinta-se a vontade para testar 
# outras técnicas de processamento de texto vista em aula.

# Lendo os documentos (artigos da revista TIME) 
# com processamento de texto
docs_proc <- process_data("time.txt", "XX-Text [[:alnum:]]",  
                          "Article_0", convertcase = TRUE, 
                          remove_stopwords = TRUE)
# Visualizando os documentos (apenas para debuging)
# head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
head(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(docs_proc, term="word")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, k=1.2, b=0.75))

# Definindo a consulta 1 
n_consulta1_proc <- 2
str_consulta1_proc <- paste("Query_0", n_consulta1_proc, sep=""); str_consulta1_proc
consulta1_proc <- queries_proc[queries_proc$doc_id == str_consulta1_proc,]; consulta1_proc

# rk1_proc <- get_ranking_by_stats("bm25", docs_stats_proc, consulta1_proc$word)
# head(rk, n=10)$doc_id
# ground_truths[ground_truths[n_consulta1,] == 1]
# precision(ground_truths[n_consulta1_proc,], rk1_proc$doc_id,4)
# average_precision(ground_truths[n_consulta1_proc,], rk1_proc$doc_id,4)
# recall(ground_truths[n_consulta1_proc,], rk1_proc$doc_id, 4)


# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ], docs_stats_proc,
                   "tf_idf", top = 20, paste("TF-IDF - ",str_consulta1_proc," - sem stopwords", sep=""))

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ], docs_stats_proc,
                   "bm25", top = 20, paste("BM25 - ",str_consulta1_proc," - sem stopwords", sep=""))


# Definindo a consulta 2 
n_consulta2_proc <- 33
str_consulta2_proc <- paste("Query_0", n_consulta2_proc, sep=""); str_consulta2_proc
consulta2_proc <- queries_proc[queries_proc$doc_id == str_consulta2_proc,]; consulta2_proc


# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ], docs_stats_proc,
                   "tf_idf", top = 20, paste("TF-IDF - ",str_consulta2_proc," - sem stopwords", sep=""))

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ], docs_stats_proc,
                   "bm25", top = 20, paste("BM25 - ",str_consulta2_proc," - sem stopwords", sep=""))

### cálculo de média das precisões médias
map_for_queries <- function(q_ids, stats_df, stat = "bm25", top = 20, queries_df, gtruth_df) {
  pairs <- lapply(q_ids, function(i) {
    qname <- paste("Query_0", i, sep="")
    q_tokens <- queries_df[queries_df$doc_id == qname, ]$word
    rk <- get_ranking_by_stats(stat, stats_df, q_tokens)$doc_id
    list(gtruth_df[i,], rk)
  })
  mean_average_precision(pairs, top)
}

map_tfidf <- map_for_queries(1:59, docs_stats, "tf_idf", 20, queries, ground_truths)
map_bm25 <- map_for_queries(1:59, docs_stats, "bm25", 20, queries, ground_truths)
map_tfidf_proc <- map_for_queries(1:59, docs_stats_proc, "tf_idf", 20, queries_proc, ground_truths)
map_bm25_proc <- map_for_queries(1:59, docs_stats_proc, "bm25", 20, queries_proc, ground_truths)

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
# 
# a) 
#  Para a Query_02, o impacto da remoção de stopwords parece ter piorado ambos os modelos,
#  o que parece surpreendente e talvez possa ser explicado pela baixa quantidade de
#  documentos relevantes para a consulta (apenas 2). No cenário do modelo BM25, 
#  por exemplo, a precisão média para a Query_02 para K = 4 caiu de 0.5 para 0.25
#  após a remoção de stopwords, mostrando uma maior dificuldade em trazer documentos 
#  relevantes no topo do ranking. No cenário que os métodos BM25 e TF-IDF são utilizados
#  com a remoção de stop words, o BM25 performou melhor, já que o valor de revocação 
#  atingiu 1 em K = 5, valor inferior o que foi observado no TF-IDF, onde foi observado apenas em K = 12. 
#
#  A remoção das stop words não melhorou a performance para a consulta dos k = 20
#  primeiros elementos da Query_033 (consulta 2) utilizando TF-IDF. Porém, ao analisar o gráfico
#  de precisão e revocação por top K, nota-se que o valor de máximo de revocação obtido para
#  esse método (0.61) foi reduzido para o elemento K = 17, ante o K = 18 observado
#  quando as stop words não foram removidas. Com isso, é possível afirmar que ao remover
#  as stop words a performance do TF-IDF melhorou. Sobre o BM25, a performance para
#  os K = 20 elementos foi igual, ao comparar o cenário com e sem stop words. Porém,
#  analisando o gráfico, é possível notar que para K = 14, no cenário com stop words
#  observamos 2 itens não relevantes (gerando uma precisão próxima a 0.86), já ao remover
#  as stop words temos somente 1 item não relevante quando K = 14 (precisão próxima a 0.94).
#  Assim, é possível afirmar que ao remover as stop words a performance do BM25 melhorou.


# 
# b) 
#  Ao gerar os resultados de MAP para os modelos, nas versões com e sem stopwords, podemos
#  ter uma visão mais geral sobre o conjunto de consultas e a performance genérica de
#  diferentes modelos e preprocessamentos. Com os dados gerados, é nítida a superioridade
#  do BM25 com a remoção de stopwords, atingindo uma MAP de aproximadamente 0.6. Em
#  contraponto, o pior foi o TF-IDF sem remoção de stopwords, que apresentou uma MAP de ~0.477. 
#  É interessante observar que a remoção de stopwords foi capaz de melhorar ambos os modelos
#  em cerca de 5%.


######################################################################
#
# Extra
#
# # Comando para salvar todos os plots gerados e que estão abertos no 
# Rstudio no momemto da execução. Esse comando pode ajudar a comparar 
# os gráfico lado a lado.
# 
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics",
#                               full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png",
#                                full.names = TRUE);
# file.copy(from=plots.png.paths, to="C:\\Users\\Vitor\\Desktop\\Graficos\\")
######################################################################