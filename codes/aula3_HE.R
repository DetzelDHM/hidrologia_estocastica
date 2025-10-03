# UNIVERSIDADE FEDERAL DO PARANÁ
# Programa de Pós-Graduação em Engenharia de Recursos Hídricos e Ambiental
# ERHA7016 – Hidrologia Estocástica
# Autor: Daniel Detzel
# Data: 01 out. 2025
# Aula 3: Análise de Séries Temporais (pt. 2)

# Bibliotecas utilizadas
library(tidyverse)  # contém diversas bibliotecas, incluindo ggplot2
library(dplyr)      # manejo de dados
library(hydroTSM)   # para conversão de vazões entre escalas temporais
library(modifiedmk) # para o teste de Mann-Kendall (e variações)
library(forecast)   # para simulações de séries não estacionárias
library(patchwork)  # para ajustes de gráficos lado a lado
library(Rlibeemd)   # para decomposição de séries via CEEMDAN

# Função utilizada
source("cod2serieMensal.R") # extração dos dados de vazão do ONS


# Tendências --------------------------------------------------------------

# |----------------------------------------------------------|
# |1. Série com tendência vs. série sem tendência            |
# |   vazões médias mensais afluentes a usinas hidrelétricas |
# |----------------------------------------------------------|

# Leitura dos dados
Qmensal <- read.csv("Vazoes_Mensais_1931_2023.csv",
                    check.names = FALSE)

# Chamada da função que extrai a série de vazões para uma usina específica,
# a partir do fornecimento do seu código ONS
# serie <- cod2serieMensal(Qmensal,17) # Marimbondo
serie <- cod2serieMensal(Qmensal,74) # Foz do Areia

# Obtenção das vazões médias anuais usando o pacote hydroTSM (requer variável
# tipo zoo)
serie    <- serie %>% mutate(Vazao = as.numeric(as.character(Vazao)))
serieZoo <- zoo(serie$Vazao, order.by = as.yearmon(paste(serie$Ano, serie$Mes), "%Y %m"))
Qanual   <- monthly2annual(serieZoo, FUN = mean, na.rm = TRUE)
Qanual   <- data.frame(Datas = index(Qanual),
                       Vazao = coredata(Qanual))


# Ajuste da regressão para representar a linha de tendência
Qanual$Datas <- as.numeric(format(Qanual$Datas,"%Y"))
regressao <- lm(Vazao ~ Datas, data = Qanual)
# Obtendo-se a tendência
Qanual$tendencia <- predict(regressao)

# tiff('tendenciaFA.tif', height = 1000, width = 1000, res=300)
ggplot(Qanual, aes(x = Datas)) +
  geom_line(aes(y = Vazao, colour="Vazões")) + 
  geom_line(aes(y = tendencia, colour="Tendência")) + 
  labs(x="Anos",y="Vazões (m³/s)",
       subtitle = "Foz do Areia (rio Iguaçu, PR)") +
  scale_x_continuous(breaks = seq(1930,2030, by = 15)) +
  scale_color_manual(name = "Legenda",
                     values = c("Vazões" = "gray70",
                                "Tendência" = "darkgreen")) +
  theme_gray() +
  theme(legend.position = "bottom")
# dev.off()

# |----------------------------------------|
# |2. Significância da correlação de lag 1 |
# |   baseado no teste t                   |
# |----------------------------------------|

# Uso da variável Qanual definida no bloco anterior em função da série escolhida
x <- Qanual$Vazao
n <- length(x)

# Cálculo da correlação de lag 1
r1 <- acf(x, lag.max = 1, plot = FALSE)$acf[2]

# Estatística do teste
t0 <- r1 * sqrt(n - 2) / sqrt(1 - r1^2)

# Valor tabelado da distribuição (alpha de 5%)
alpha <- 0.05
t <- qt(1 - alpha/2,n-2)

# P-valor do teste
pval <- 2 * (1 - pt(abs(t0), df = n - 2))

# Exibir os resultados
resultado <- data.frame(r1 = round(r1, digits = 2),
                        t0 = round(t0, digits = 2),
                        t = round(t, digits = 2),
                        p.valor = round(pval, digits = 3))
print(resultado)

# |------------------------------------|
# |3. Identificação de tendência       |
# |   baseado no teste de Mann-Kendall |
# |------------------------------------|

# Uso da variável Qanual definida no bloco anterior em função da série escolhida
x <- Qanual$Vazao

# Mann-Kendall original (não recomendado em casos de persistência!!)
mkOriginal <- mkttest(x)

# Trend-Free Pre-Whitening Mann-Kendal [Yue et al., 2002]
mkTFPW <- tfpwmk(x)

# Mann-Kendall baseado no tamanho efetivo da amostra [Hamed, 2009]
mkHR <- mmkh(x,ci = 0.95)

# Resultados
resMK <- data.frame(Original = round(mkOriginal[5],digits = 3),
                    Yue_et_al = round(mkTFPW[4], digits = 3),
                    Hamed_Rao = round(mkHR[2], digits = 3))
print(resMK)




# Transformações numéricas ------------------------------------------------

# |----------------------------------------|
# |4. Diferenciação                        |
# |   séries homogêneas vs. não homogêneas |
# |----------------------------------------|

# Aqui são usadas séries simuladas apenas para fins ilustrativos
n <- 100 # elementos da série

# Série não estacionária homogênea simulada
xNE <- arima.sim(n = n, list(order = c(1,1,0), ar = 0.7))
xNE <- as.numeric(xNE[1:n])

# Aplica a diferenciação
xNEdiff <- diff(xNE)

# Monta os data.frames
xOri <- data.frame(x = 1:n, y = xNE)
xDif <- data.frame(x = 2:n, y = xNEdiff)

# Obtenção dos gráficos
# Série original
# tiff('serieNEhomog.tif', height=720, width = 1780, res=300)
p1 <- ggplot(xOri, aes(x = x, y = y)) +
  geom_line(color = "darkgreen") +
  labs(subtitle = "Original", x = "Tempo") +
  theme_gray() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank())
# Série diferenciada
p2 <- ggplot(xDif, aes(x = x, y = y)) +
  geom_line(color = "firebrick") +
  labs(subtitle = "Diferenciada", x = "Tempo") +
  theme_gray() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank())
# União dos gráficos
p1 + p2
# dev.off()

# Série não estacionária não homogênea simulada. Para tanto, é utilizado um modelo
# baseado em crescimento exponencial
X0 <- 1
r  <- 0.05
sd_eps <- 0.1

# Modelo exponencial
eps <- rnorm(n, mean = 0, sd = sd_eps)
logX <- log(X0) + cumsum(r + eps)
xNENH <- exp(logX)

# Aplica a diferenciação
xNENHdiff <- diff(xNENH, differences = 10)

# Monta os data.frames
xOriNH <- data.frame(x = 1:n, y = xNENH)
xDifNH <- data.frame(x = 11:n, y = xNENHdiff)

# Obtenção dos gráficos
# Série original
# tiff('serieNENhomog.tif', height=720, width = 1780, res=300)
p1 <- ggplot(xOriNH, aes(x = x, y = y)) +
  geom_line(color = "darkgreen") +
  labs(subtitle = "Original", x = "Tempo") +
  theme_gray() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank())
# Série diferenciada
p2 <- ggplot(xDifNH, aes(x = x, y = y)) +
  geom_line(color = "firebrick") +
  labs(subtitle = "Diferenciada (10x)", x = "Tempo") +
  theme_gray() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank())
# União dos gráficos
p1 + p2
# dev.off()

# |-----------------------------------------------|
# |5. Estabilização de variância (e normalização) |
# |   Box-Cox e Logarítmica                       |
# |-----------------------------------------------|

# 5.1. Transformação Box-Cox
# Uso da série de vazões médias anuais lida no item 1
x <- Qanual$Vazao
n <- length(x)

# Estima o parâmetro Box-Cox
lambda <- BoxCox.lambda(X)

# Aplica a trasnformação
xBC <- BoxCox(x, lambda)

# Monta os data.frames
xOrigin <- data.frame(x = x)
xTransf <- data.frame(x = xBC)

# Obtenção dos gráficos
# Série original
# tiff('transfBoxCox.tif', height=720, width = 1780, res=300)
p1 <- ggplot(xOrigin, aes(x = x)) +
  geom_histogram(colour="steelblue", fill="steelblue", alpha = 0.5, bins = 10) +
  labs(x="Classes de vazões (m³/s)",y="Frequência",
       subtitle = "Original") +
  theme_gray()
# Série diferenciada
p2 <- ggplot(xTransf, aes(x = x)) +
  geom_histogram(colour="firebrick", fill="firebrick", alpha = 0.5, bins = 10) +
  labs(x="Classes de vazões (m³/s)",y="Frequência",
       subtitle = "Transformada (\u03BB = 0,67)") +
  theme_gray()
# União dos gráficos
p1 + p2
# dev.off()

# 5.2. Transformação Logarítmica

# Aplica a trasnformação
xLog <- log(x)

# Monta os data.frames
xOrigin <- data.frame(x = x)
xTransf <- data.frame(x = xLog)

# Obtenção dos gráficos
# Série original
# tiff('transfLog.tif', height=720, width = 1780, res=300)
p1 <- ggplot(xOrigin, aes(x = x)) +
  geom_histogram(colour="steelblue", fill="steelblue", alpha = 0.5, bins = 10) +
  labs(x="Classes de vazões (m³/s)",y="Frequência",
       subtitle = "Original") +
  theme_gray()
# Série diferenciada
p2 <- ggplot(xTransf, aes(x = x)) +
  geom_histogram(colour="firebrick", fill="firebrick", alpha = 0.5, bins = 10) +
  labs(x="Classes de vazões (m³/s)",y="Frequência",
       subtitle = "Transformada") +
  theme_gray()
# União dos gráficos
p1 + p2
# dev.off()


# Decomposição ------------------------------------------------------------

# |------------------------------------|
# |6. Decomposição de séries temporais |
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
# Usando 6 IMFs (intrinsic mode functions)
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
  labs(subtitle = "Decomposição CEEMDAN", x = "Ano", y = "Vazão [m³/s]", color = "Componente") +
  theme_gray()
# dev.off()

