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

# Função utilizada
source("cod2serieMensal.R") # extração dos dados de vazão do ONS

# Leitura dos dados (usados em diferentes partes do código)
Qmensal <- read.csv("Vazoes_Mensais_1931_2023.csv",
                    check.names = FALSE)

# Tendências --------------------------------------------------------------

# |----------------------------------------------------------|
# |1. Série com tendência vs. série sem tendência            |
# |   vazões médias mensais afluentes a usinas hidrelétricas |
# |----------------------------------------------------------|

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




