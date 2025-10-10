# UNIVERSIDADE FEDERAL DO PARANÁ
# Programa de Pós-Graduação em Engenharia de Recursos Hídricos e Ambiental
# ERHA7016 – Hidrologia Estocástica
# Autor: Daniel Detzel
# Data: 08 out. 2025
# Aula 4: Análise de Séries Temporais (pt. 3)
#         Geração de Séries Sintéticas (pt. 1)

# Bibliotecas utilizadas
library(tidyverse)  # contém diversas bibliotecas, incluindo ggplot2
library(patchwork)  # para ajustes de gráficos lado a lado
library(Rlibeemd)   # para decomposição de séries via CEEMDAN
library(forecast)   # para modelos ARIMA
library(dplyr)      # manejo de dados

# Função utilizada
source("cod2serieMensal.R") # extração dos dados de vazão do ONS

# Leitura dos dados (usados em diferentes partes do código)
Qmensal <- read.csv("Vazoes_Mensais_1931_2023.csv",
                    check.names = FALSE)

# Decomposição ------------------------------------------------------------

# |------------------------------------|
# |1. Decomposição de séries temporais |
# |   clássica, STL e CEEMDAN          |
# |------------------------------------|

# Uso de séries mensais
serie <- cod2serieMensal(Qmensal,169) # Sobradinho
vazao <- serie$Vazao
tempo <- serie$Ano + (serie$Mes - 1)/12

# É preciso transformar a série para um formato 'ts' (time series)
serieTS <- ts(vazao, start = c(min(serie$Ano), 1), frequency = 12)

# 6.1. Decomposição clássica
decClassica <- decompose(serieTS, type = "additive")

# Montagem do data frame
classica <- data.frame(tempo = rep(tempo, 4),
                       valor = c(vazao,
                                 decClassica$trend,
                                 decClassica$seasonal,
                                 decClassica$random),
                       componente = factor(rep(c("Obs.",
                                                 "Tend.",
                                                 "Saz.",
                                                 "Res."),
                                               each = length(tempo)),
                                           levels = c("Obs.",
                                                      "Tend.",
                                                      "Saz.",
                                                      "Res.")))

# Obtenção dos gráficos
# tiff('decomposicaoClassica.tif', height=720, width = 1780, res=300)
ggplot(classica, aes(x = tempo, y = valor, color = componente)) +
  geom_line() +
  facet_wrap(~componente, ncol = 1, scales = "free_y", strip.position = "right") +
  scale_color_manual(
    values = c(
      "Obs."      = "firebrick",
      "Tend."     = "darkgreen",
      "Saz."  = "steelblue",
      "Res."       = "gray50")) +
  labs(x = "Ano", y = "Vazão [m³/s]") +
  theme_gray() +
  theme(legend.position = "none")
# dev.off()

# 6.2. Decomposição STL
# Sazonalidade periódica e tendência suavizada com período de 10 anos
stl <- stl(serieTS, s.window = "periodic", t.window = 120)

# Montagem do data frame
decSTL <- data.frame(tempo = rep(tempo, 4),
                     valor = c(vazao,
                               stl$time.series[,"trend"],
                               stl$time.series[,"seasonal"],
                               stl$time.series[,"remainder"]),
                     componente = factor(rep(c("Obs.",
                                               "Tend.",
                                               "Saz.",
                                               "Res."),
                                             each = length(tempo)),
                                         levels = c("Obs.",
                                                    "Tend.",
                                                    "Saz.",
                                                    "Res.")))

# Obtenção dos gráficos
# tiff('decomposicaoSTL.tif', height=720, width = 1780, res=300)
ggplot(decSTL, aes(x = tempo, y = valor, color = componente)) +
  geom_line() +
  facet_wrap(~componente, ncol = 1, scales = "free_y", strip.position = "right") +
  scale_color_manual(
    values = c(
      "Obs."      = "firebrick",
      "Tend."     = "darkgreen",
      "Saz."  = "steelblue",
      "Res."       = "gray50")) +
  labs(x = "Ano", y = "Vazão [m³/s]") +
  theme_gray() +
  theme(legend.position = "none")
# dev.off()

# 6.3. Decomposição CEEMDAN
# Usando 4 IMFs (intrinsic mode functions)
ceemdan <- ceemdan(serieTS, num_imfs = 6)

# Montagem do data frame
decCEEMDAN <- data.frame(tempo = rep(tempo, ncol(ceemdan) + 1),
                         valor = c(vazao, as.vector(ceemdan)),
                         componente = factor(c("Obs.",
                                               paste0("IMF", 1:ncol(ceemdan))),
                                             levels = c("Obs.",
                                                        paste0("IMF", 1:ncol(ceemdan)))
                         ) %>% rep(each = length(tempo)))

# Obtenção dos gráficos
# tiff('decomposicaoCEEMDAN.tif', height=1147, width = 1780, res=300)
ggplot(decCEEMDAN, aes(x = tempo, y = valor, color = componente)) +
  geom_line() +
  facet_wrap(~componente, ncol = 1, scales = "free_y", strip.position = "right") +
  scale_color_manual(
    values = c(
      "Obs." = "firebrick",
      "IMF1"     = "gray50",
      "IMF2"     = "gray50",
      "IMF3"     = "gray50",
      "IMF4"     = "gray50",
      "IMF5"     = "gray50",
      "IMF6"     = "gray50")) +
  labs(x = "Ano", y = "Vazão [m³/s]", color = "Componente") +
  theme_gray() +
  theme(legend.position = "none")
# dev.off()


# Modelos ARIMA: identificação --------------------------------------------

# |-----------------------------------|
# |2. Leitura das séries de interesse |
# |   + dessazonalização              |
# |-----------------------------------|

# Séries utilizadas como exemplo para comparação entre modelos
# serie <- cod2serieMensal(Qmensal,222) # Salto Caxias
serie <- cod2serieMensal(Qmensal,252) # Itiquira II

# Processo para dessazonalizar a série (nessa parte ainda estamos trabalhando
# com modelos não sazonais)
serieDS <- serie %>%
  group_by(Mes) %>%
  mutate(Q = (Vazao - mean(Vazao)) / sd(Vazao)) %>%
  ungroup()

# O pacote 'forecast' requer que as séries estejam no formato ts (time series)
serie <- ts(serieDS$Q)

# |------------------------------------------------------|
# |3. Funções de autocorrelação e autocorrelação parcial |
# |   FAC/FACP                                           |
# |------------------------------------------------------|

# O pacote forecast traz funções prontas para isso
p1 <- ggAcf(serie, lag.max = 36) + ggtitle("FAC")
p2 <- ggPacf(serie, lag.max = 36) + ggtitle("FACP")

# tiff('FACvsFACP.tif', height=1200, width = 1800, res=300)
# Posiciona um gráfico sobre o outro
p1 / p2
# dev.off()

# |---------------------------|
# |4. Critérios de Informação |
# |   AIC, AICc, BIC          |
# |---------------------------|

# Aqui é escrita uma função para tornar possível a determinação de todos os
# critérios de informação estudados para todas as combinações de modelos até
# a segunda ordem (suficiente para aplicações hidrológicas)
# Parâmetros
maxp <- 2
maxq <- 2

# Função
ajusteBateladaARMA <- function(x, max.p = maxp, max.q = maxq) {
  # Planeja a estrutura do data frame de resultados
  resultados <- data.frame(p = integer(),
                           q = integer(),
                           AIC = numeric(),
                           AICc = numeric(),
                           BIC = numeric())
  
  # Loops para variação das ordens dos modelos
  for (p in 0:max.p) {
    for (q in 0:max.q) {
      
      # Pula o ARMA(0,0)
      if (p == 0 && q == 0) next
      
      fit <- Arima(x, order = c(p, 0, q), include.mean = FALSE)
      resultados <- rbind(resultados, data.frame(p = p,
                                                 q = q,
                                                 AIC = fit$aic,
                                                 AICc = fit$aicc,
                                                 BIC = fit$bic))
    }
  }
  return(resultados)
}

# Chamada da função
ajuste <- ajusteBateladaARMA(serie, maxp, maxq)

# Obtenção do mapa de calor com os resultados
# tiff('heatmapBIC.tif', height=720, width = 1780, res=300)
ggplot(ajuste, aes(x = factor(p), y = factor(q), fill = BIC)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(BIC, 1)), color = "white", size = 6) +
  scale_fill_gradient(low = "steelblue", high = "midnightblue", name = "BIC") +
  labs(x = "AR(p)",
       y = "MA(q)") +
  theme_gray(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right")
# dev.off()

# Alternativa!!
# Obtenção do melhor modelo de forma automática, usando o critério fornecido
# no argumento 'ic' e a ordem máxima 2 (modelo ARMA - sem diferenciação)
ajuste_auto <- auto.arima(
  serie,             # série temporal
  max.p = 2,         # máxima ordem AR
  max.d = 0,         # máxima ordem I
  max.q = 2,         # máxima ordem MA
  seasonal = FALSE,  # modelo não sazonal
  allowmean = FALSE, # não incluir constante (série possui média zero)
  ic = "bic",        # critério de informação utilizado
  stepwise = FALSE,  # processo interno do otimizador
  approximation = FALSE)

# Exibe os resultados
summary(ajuste_auto)
