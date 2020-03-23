#------------------------------------------------#
# INF-0611 Recuperacao de Informacao             #
#                                                #
# Trabalho Avaliativo 3                          #
#------------------------------------------------#
# Nome COMPLETO Aluna (o) 1:                     #
#                                                #
# Nome COMPLETO Aluna (o) 2:                     #
#                                                #
# Nome COMPLETO Aluna (o) 3:                     #
#                                                #
# Nome COMPLETO Aluna (o) 4:                     #                                              #
#------------------------------------------------#

library(IM)
library(e1071)
library(wvtool)
library(ecodist)
library(imager)
library(ggplot2)

#------------------------------------------------#
# Configuracao dos arquivos auxiliares           #
#------------------------------------------------#
setwd("/home/diogo/MDC/inf-0611/trabalho 3") # configure o caminho antes de descomentar essa linha
source("./ranking_metrics.R")
source("./trabalho3_base.R")

# caminho da pasta de imagens
path_soccer = './soccer/'

#------------------------------------------------#
# Leitura das imagens                            #
#------------------------------------------------#
imagens <- read_images(path_soccer)

#------------------------------------------------#
# Obtem classe de cada imagem                    #
#------------------------------------------------#
nome_classes <- get_classes(path_soccer)

#------------------------------------------------#
# Define classe relevante                        #
#------------------------------------------------#
classe_relevante <- 'barcelona'

#------------------------------------------------#
# obtem ground_truth                             #
#------------------------------------------------#

ground_truth <- get_ground_truth(nome_classes, classe_relevante)

#------------------------------------------------#
# define consulta classe relevante               #
#------------------------------------------------#
consulta <- #utilizar as mesmas do Trabalho 2
  
#------------------------------------------------#
# define tamanho do topK analisado               #
#------------------------------------------------#
top <- 20

#################################################
#################################################


#------------------------------------------------#
# Questao 1                                      #
#------------------------------------------------#


# obtem caracteristicas de cor                   

features_color <- function(imagens){
  #entrada: o conjunto de imagens carregadas
  #saida: uma matriz de caracteristicas de cor onde cada linha é referente a uma imagem
  
  features <- c()
  cnt <- 1
  for(img_ in imagens){
    print(sprintf("Processando imagem %d de %d", cnt, length(imagens)))
    
    channels <- dim(img_[])
    img_features <- c()
    for (color in 1:channels[4]){
      # seleciona um canal de cor para ser processado
      img <- img_[ , , 1, color]
      
      # tranforma intensidade de pixels em valores de 0 a 255
      min_v <- min(img)
      max_v <- max(img)
      img <- ((img - min_v) / (max_v - min_v)) * 255
      # calcula histograma
      h <- hist(img, plot=FALSE, breaks=0:255)$counts
      # junta os histogramas de cor em um único vetor
      img_features <- c(img_features, h)
    }
    # adiciona uma nova linha na matriz de caracteristicas (uma nova imagem)
    features <- rbind(features, img_features)
    cnt <- cnt + 1
  }
  
  return(features)
}

#------------------------------------------------#

# obtem caracteristicas de textura               

features_texture <- function(imagens){
  #entrada: o conjunto de imagens carregadas
  #saida: uma matriz de caracteristicas de textura onde cada linha é referente a uma imagem
  
  #se a imagem nao estiver em escala de cinza deve-se transforma-la
  features <- NULL
  cnt <- 1
  for(img_ in imagens){
    print(sprintf("Processando imagem %d de %d", cnt, length(imagens)))
    
    # transforma a imagem para escala cinza
    img <- drop(grayscale(img_[]))
    
    # tranforma intensidade de pixels em valores de 0 a 255
    min_v <- min(img)
    max_v <- max(img)
    img <- ((img - min_v) / (max_v - min_v)) * 255
    
    # obtem catacteristicas de textura
    values <- criarMatrizCoocorrencia(img, c(1,0))
    
    # adiciona uma nova linha na matriz de caracteristicas (uma nova imagem)
    features <- rbind(features,values)
    cnt <- cnt + 1
  }
  
  return(features)
}

#------------------------------------------------#

# obtem caracteristicas de forma                 

features_shape <- function(imagens){
  # entrada: o conjunto de imagens carregadas
  # saida: uma matriz de caracteristicas de forma onde cada linha é referente a uma imagem
  
  #se a imagem nao estiver em escala de cinza deve-se transforma-la
  features <- NULL
  cnt <- 1
  for(img_ in imagens){
    print(sprintf("Processando imagem %d de %d", cnt, length(imagens)))
    
    # transforma a imagem para escala cinza
    img <- drop(grayscale(img_[]))
    
    aux_line <- NULL
    for(i in 0:10){
      for(j in 0:10){
        #adiciona um novo momento como caracteristica no vetor de caracteristicas da imagem
        aux_line <- cbind(aux_line,moment(img, order=c(i,j), center=TRUE))
        
      }}
    #adiciona uma nova linha na matriz de caracteristicas (uma nova imagem)
    features <- rbind(features, aux_line)
    cnt <- cnt + 1
  }
  return(features)
}

features_c <- features_color(imagens)
features_t <- features_texture(imagens)
features_s <- features_shape(imagens)


#################################################
#################################################

#------------------------------------------------#
# Questao 2                                      #
#------------------------------------------------#


# agregando rankings por valor                          


#escolha duas consultas da classe barcelona (posicoes de 11 a 20 do vetor ground_truth)
#construa os rankings para cada metodo de agregacao e para cada consulta (3 rankings para cada consulta)

q1 <- 13
q2 <- 14

generate_distances <- function(features, query){
  #entrada: conjunto de caracteristicas que serao utilizadas para calculo de distancia e indice da consulta no vetor de caracteristicas
  #saida: vetor não-ordenado de distancias das imagens para a consulta

  ## calcular distancia euclidiana de todas as imagens (representada pelas caracteristicas) para a consulta
  distancias <- full(ecodist::distance(features, "euclidean"))
  distancias <- distancias[,query]
  
  return(distancias)
}

# consulta 1
distancia_c1 <- generate_distances(features_c, q1)
distancia_t1 <- generate_distances(features_t, q1)
distancia_s1 <- generate_distances(features_s, q1)

# consulta 2
distancia_c2 <- generate_distances(features_c, q2)
distancia_t2 <- generate_distances(features_t, q2)
distancia_s2 <- generate_distances(features_s, q2)

##FAZER para cada consulta
## calcular rankings para a agregacao por COMBMIN
ranking_combmin1 <- combmin(distancia_c1, distancia_t1, distancia_s1)
ranking_combmin2 <- combmin(distancia_c2, distancia_t2, distancia_s2)

## calcular rankings para a agregacao por COMBMAX
ranking_combmax1 <- combmax(distancia_c1, distancia_t1, distancia_s1)
ranking_combmax2 <- combmax(distancia_c2, distancia_t2, distancia_s2)

## calcular rankings para a agregacao por COMBSUM
ranking_combsum1 <- combsum(distancia_c1, distancia_t1, distancia_s1)
ranking_combsum2 <- combsum(distancia_c2, distancia_t2, distancia_s2)

#################################################
#################################################

# comparando  rankings                              

##FAZER
## utilize as funções do arquivo ranking_metrics.R para calcular a precisao e revocacao utilizando o ground_truth e os rankings obtidos (como o parametro predictions)

# precisao no topk para combmin
p_combmin1 <- mapply(precision, 1:top, 
                    MoreArgs = list(ground_truth = ground_truth, 
                                    prediction = ranking_combmin1))
p_combmin2 <- mapply(precision, 1:top, 
                     MoreArgs = list(ground_truth = ground_truth, 
                                     prediction = ranking_combmin2))
# precisao no topk para combmax
p_combmax1 <- mapply(precision, 1:top,
                    MoreArgs = list(ground_truth = ground_truth, 
                                    prediction = ranking_combmax1))
p_combmax2 <- mapply(precision, 1:top,
                     MoreArgs = list(ground_truth = ground_truth, 
                                     prediction = ranking_combmax2))
# precisao no topk para combsum
p_combsum1 <- mapply(precision, 1:top, 
                    MoreArgs = list(ground_truth = ground_truth, 
                                    prediction = ranking_combsum1))
p_combsum2 <- mapply(precision, 1:top, 
                     MoreArgs = list(ground_truth = ground_truth, 
                                     prediction = ranking_combsum2))

pr1 <- data.frame(precision = NULL, recall = NULL)
pr2 <- data.frame(precision = NULL, recall = NULL)

## gere um grafico de precisao X topK para cada consulta (contendo as curvas de todas os rankings para uma consulta)

### CONSULTA 1 ###
ggplot(pr1, aes(x = 1:top)) + 
  geom_point(aes(y = p_combmin1), color = 'red') + 
  geom_line(aes(y = p_combmin1), color = 'red') +
  geom_text(aes(0, 1,label = "CombMin"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = p_combmax1), color = 'blue') + 
  geom_line(aes(y = p_combmax1),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "CombMax"), vjust= -0.3, color = 'blue') +
  geom_point(aes(y = p_combsum1),color = 'green') + 
  geom_line(aes(y = p_combsum1),color = 'green') +
  geom_text(aes(0, 0.8, label = "CombSum"), vjust= -0.3, color = 'green') +
  theme_light() +
  labs(colour = element_blank(), title = "Precisão x TopK - Consulta 1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Precisão", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

### CONSULTA 2 ###
ggplot(pr2, aes(x = 1:top)) + 
  geom_point(aes(y = p_combmin2), color = 'red') + 
  geom_line(aes(y = p_combmin2), color = 'red') +
  geom_text(aes(0, 1,label = "CombMin"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = p_combmax2), color = 'blue') + 
  geom_line(aes(y = p_combmax2),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "CombMax"), vjust= -0.3, color = 'blue') +
  geom_point(aes(y = p_combsum2),color = 'green') + 
  geom_line(aes(y = p_combsum2),color = 'green') +
  geom_text(aes(0, 0.8, label = "CombSum"), vjust= -0.3, color = 'green') +
  theme_light() +
  labs(colour = element_blank(), title = "Precisão x TopK - Consulta 2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Precisão", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

## gere um grafico de revocacao X topK para cada consulta (contendo as curvas de todas os rankings para uma consulta)

# revocação no topk para combmin
r_combmin1 <- mapply(recall, 1:top, 
                     MoreArgs = list(ground_truth = ground_truth, 
                                     prediction = ranking_combmin1))
r_combmin2 <- mapply(recall, 1:top, 
                     MoreArgs = list(ground_truth = ground_truth, 
                                     prediction = ranking_combmin2))
# revocação no topk para combmax
r_combmax1 <- mapply(recall, 1:top,
                     MoreArgs = list(ground_truth = ground_truth, 
                                     prediction = ranking_combmax1))
r_combmax2 <- mapply(recall, 1:top,
                     MoreArgs = list(ground_truth = ground_truth, 
                                     prediction = ranking_combmax2))
# revocação no topk para combsum
r_combsum1 <- mapply(recall, 1:top, 
                     MoreArgs = list(ground_truth = ground_truth, 
                                     prediction = ranking_combsum1))
r_combsum2 <- mapply(recall, 1:top, 
                     MoreArgs = list(ground_truth = ground_truth, 
                                     prediction = ranking_combsum2))

### CONSULTA 1 ###
ggplot(pr1, aes(x = 1:top)) + 
  geom_point(aes(y = r_combmin1), color = 'red') + 
  geom_line(aes(y = r_combmin1), color = 'red') +
  geom_text(aes(0, 1,label = "CombMin"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = r_combmax1), color = 'blue') + 
  geom_line(aes(y = r_combmax1),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "CombMax"), vjust= -0.3, color = 'blue') +
  geom_point(aes(y = r_combsum1),color = 'green') + 
  geom_line(aes(y = r_combsum1),color = 'green') +
  geom_text(aes(0, 0.8, label = "CombSum"), vjust= -0.3, color = 'green') +
  theme_light() +
  labs(colour = element_blank(), title = "Revocação x TopK - Consulta 1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Revocação", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

### CONSULTA 2 ###
ggplot(pr2, aes(x = 1:top)) + 
  geom_point(aes(y = r_combmin2), color = 'red') + 
  geom_line(aes(y = r_combmin2), color = 'red') +
  geom_text(aes(0, 1,label = "CombMin"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = r_combmax2), color = 'blue') + 
  geom_line(aes(y = r_combmax2),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "CombMax"), vjust= -0.3, color = 'blue') +
  geom_point(aes(y = r_combsum2),color = 'green') + 
  geom_line(aes(y = r_combsum2),color = 'green') +
  geom_text(aes(0, 0.8, label = "CombSum"), vjust= -0.3, color = 'green') +
  theme_light() +
  labs(colour = element_blank(), title = "Revocação x TopK - Consulta 2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Revocação", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)


#################################################
#################################################

##NAO SE ESQUECA DO RELATORIO

#################################################
#################################################

#------------------------------------------------#
# Questao 3                                      #
#------------------------------------------------#

# agregando rankings por posicao                          


#escolha duas consultas da classe barcelona (posicoes de 11 a 20 do vetor ground_truth)
#construa os rankings para cada metodo de agregacao e para cada consulta (3 rankings para cada consulta)


#utilize a funcao da questão anterior generate_distance seguida pela generate_ranking para
#cada vetor de caracteristicas individualmente

generate_rankings <- function(distancias){
  #entrada: conjunto de caracteristicas que serao utilizadas para calculo de distancia e indice da consulta no vetor de caracteristicas
  #saida: vetor ordenado pelas distancias com os indices das imagens mais proximas à consulta
  
  ##FAZER
  ## ordenar distancias
  ranking <- order(distancias)
  
  return(ranking)
}

##FAZER para cada consulta
## calcular ranking para o metodo de agregacao BORDA

### CONSULTA 1 ###
ranking_c1 <- generate_rankings(distancia_c1)
ranking_t1 <- generate_rankings(distancia_t1)
ranking_s1 <- generate_rankings(distancia_s1)
ranking_borda1 <- bordacount(ranking_c1, ranking_t1, ranking_s1)

### CONSULTA 2 ###
ranking_c2 <- generate_rankings(distancia_c2)
ranking_t2 <- generate_rankings(distancia_t2)
ranking_s2 <- generate_rankings(distancia_s2)
ranking_borda2 <- bordacount(ranking_c2, ranking_t2, ranking_s2)

#################################################
#################################################

# comparando  rankings                              

##FAZER
## utilize as funções do arquivo ranking_metrics.R para calcular a precisao e revocacao utilizando o ground_truth e os rankings obtidos (como o parametro predictions)

## gere um grafico de precisao X topK para cada consulta (contendo as curvas dos rankings gerados pelo BORDA e pelo COMB escolhido)

# precisao no topk para bordacount
p_borda1 <- mapply(precision, 1:top, 
                   MoreArgs = list(ground_truth = ground_truth, 
                                   prediction = ranking_borda1))
p_borda2 <- mapply(precision, 1:top, 
                   MoreArgs = list(ground_truth = ground_truth, 
                                   prediction = ranking_borda2))

### CONSULTA 1 ###
ggplot(pr1, aes(x = 1:top)) + 
  geom_point(aes(y = p_combmin1), color = 'red') + 
  geom_line(aes(y = p_combmin1), color = 'red') +
  geom_text(aes(0, 1,label = "CombMin"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = p_borda1), color = 'blue') + 
  geom_line(aes(y = p_borda1),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "Borda"), vjust= -0.3, color = 'blue') +
  theme_light() +
  labs(colour = element_blank(), title = "Precisão x TopK - Consulta 1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Precisão", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

### CONSULTA 2 ###
ggplot(pr2, aes(x = 1:top)) + 
  geom_point(aes(y = p_combsum2), color = 'red') + 
  geom_line(aes(y = p_combsum2), color = 'red') +
  geom_text(aes(0, 1,label = "CombSum"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = p_borda2), color = 'blue') + 
  geom_line(aes(y = p_borda2),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "Borda"), vjust= -0.3, color = 'blue') +
  theme_light() +
  labs(colour = element_blank(), title = "Precisão x TopK - Consulta 2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Precisão", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

## gere um grafico de revocacao X topK para cada consulta (contendo as curvas dos rankings gerados pelo BORDA e pelo COMB escolhido)

# revocação no topk para bordacount
r_borda1 <- mapply(recall, 1:top, 
                   MoreArgs = list(ground_truth = ground_truth, 
                                   prediction = ranking_borda1))
r_borda2 <- mapply(recall, 1:top, 
                   MoreArgs = list(ground_truth = ground_truth, 
                                   prediction = ranking_borda2))

### CONSULTA 1 ###
ggplot(pr1, aes(x = 1:top)) + 
  geom_point(aes(y = r_combmin1), color = 'red') + 
  geom_line(aes(y = r_combmin1), color = 'red') +
  geom_text(aes(0, 1,label = "CombMin"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = r_borda1), color = 'blue') + 
  geom_line(aes(y = r_borda1),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "Borda"), vjust= -0.3, color = 'blue') +
  theme_light() +
  labs(colour = element_blank(), title = "Revocação x TopK - Consulta 1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Revocação", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

### CONSULTA 2 ###
ggplot(pr2, aes(x = 1:top)) + 
  geom_point(aes(y = r_combsum2), color = 'red') + 
  geom_line(aes(y = r_combsum2), color = 'red') +
  geom_text(aes(0, 1,label = "CombSum"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = r_borda2), color = 'blue') + 
  geom_line(aes(y = r_borda2),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "Borda"), vjust= -0.3, color = 'blue') +
  theme_light() +
  labs(colour = element_blank(), title = "Revocação x TopK - Consulta 2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Revocação", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)


#################################################
#################################################

##NAO SE ESQUECA DO RELATORIO

#################################################
#################################################

#------------------------------------------------#
# Questao 4                                      #
#------------------------------------------------#


# concatenando caracteristicas                      

## FAZER -- pode ser utilizado mesmo metodo do trabalho anterior
## obter vetores finais de caracteristicas pela concatenação de cada tipo de caracteristica (forma, cor, textura):
## - dos 3

features_cts <- cbind(features_c, features_t, features_s)

## utilizar a funcao generate_distance da questao 2 seguida da funcao generate_ranking da questao 3 para cada novo vetor de caracteristicas (com as mesmas consultas)
# distâncias
distancia_cts1 <- generate_distances(features_cts, q1)
distancia_cts2 <- generate_distances(features_cts, q2)

# rankings
ranking_cts1 <- generate_rankings(distancia_cts1)
ranking_cts2 <- generate_rankings(distancia_cts2)

#################################################
#################################################

# comparando  rankings                              

##FAZER
## utilize as funções do arquivo ranking_metrics.R para calcular a precisao e revocacao utilizando o ground_truth e os rankings obtidos (como o parametro predictions)

## gere um grafico de precisao X topK para cada consulta (contendo as curvas dos rankings da agregacao escolhida e da concatenacao de caracteristicas)
# precisao no topk para bordacount
p_concat1 <- mapply(precision, 1:top, 
                   MoreArgs = list(ground_truth = ground_truth, 
                                   prediction = ranking_cts1))
p_concat2 <- mapply(precision, 1:top, 
                   MoreArgs = list(ground_truth = ground_truth, 
                                   prediction = ranking_cts2))

### CONSULTA 1 ###
ggplot(pr1, aes(x = 1:top)) + 
  geom_point(aes(y = p_concat1), color = 'red') + 
  geom_line(aes(y = p_concat1), color = 'red') +
  geom_text(aes(0, 1,label = "Concatenação"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = p_combmin1), color = 'blue') + 
  geom_line(aes(y = p_combmin1),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "CombMin"), vjust= -0.3, color = 'blue') +
  theme_light() +
  labs(colour = element_blank(), title = "Precisão x TopK - Consulta 1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Precisão", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

### CONSULTA 2 ###
ggplot(pr2, aes(x = 1:top)) + 
  geom_point(aes(y = p_concat2), color = 'red') + 
  geom_line(aes(y = p_concat2), color = 'red') +
  geom_text(aes(0, 1,label = "Concatenação"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = p_borda2), color = 'blue') + 
  geom_line(aes(y = p_borda2),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "Borda"), vjust= -0.3, color = 'blue') +
  theme_light() +
  labs(colour = element_blank(), title = "Precisão x TopK - Consulta 2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Precisão", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

## gere um grafico de revocacao X topK para cada consulta (contendo as curvas dos rankings da agregacao escolhida e da concatenacao de caracteristicas)
r_concat1 <- mapply(recall, 1:top, 
                    MoreArgs = list(ground_truth = ground_truth, 
                                    prediction = ranking_cts1))
r_concat2 <- mapply(recall, 1:top, 
                    MoreArgs = list(ground_truth = ground_truth, 
                                    prediction = ranking_cts2))

### CONSULTA 1 ###
ggplot(pr1, aes(x = 1:top)) + 
  geom_point(aes(y = r_concat1), color = 'red') + 
  geom_line(aes(y = r_concat1), color = 'red') +
  geom_text(aes(0, 1,label = "Concatenação"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = r_combmin1), color = 'blue') + 
  geom_line(aes(y = r_combmin1),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "CombMin"), vjust= -0.3, color = 'blue') +
  theme_light() +
  labs(colour = element_blank(), title = "Revocação x TopK - Consulta 1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Revocação", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

### CONSULTA 2 ###
ggplot(pr2, aes(x = 1:top)) + 
  geom_point(aes(y = r_concat2), color = 'red') + 
  geom_line(aes(y = r_concat2), color = 'red') +
  geom_text(aes(0, 1,label = "Concatenação"), vjust= -0.3, color = 'red') +
  geom_point(aes(y = r_borda2), color = 'blue') + 
  geom_line(aes(y = r_borda2),  color = 'blue') +
  geom_text(aes(0, 0.9,label = "Borda"), vjust= -0.3, color = 'blue') +
  theme_light() +
  labs(colour = element_blank(), title = "Revocação x TopK - Consulta 2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Revocação", limits = c(0, 1), 
                     breaks = 0:10 * 0.1,
                     minor_breaks = NULL) +
  scale_x_continuous(name = "TopK", limits = c(0, top), 
                     breaks = 0:top,
                     minor_breaks = NULL)

## serao entao um total de 4 graficos (dois para cada consulta)

#################################################
#################################################

##NAO SE ESQUECA DO RELATORIO

#################################################
#################################################





