# UNIVERSIDADE FEDERAL DO PARANÁ
# Programa de Pós-Graduação em Engenharia de Recursos Hídricos e Ambiental
# ERHA7016 – Hidrologia Estocástica
# Autor: Daniel Detzel
# Data: 14 out. 2025
# Aula 4: Geração de Séries Sintéticas (pt. 2)

# Bibliotecas utilizadas
library(tidyverse)  # contém diversas bibliotecas, incluindo ggplot2
library(patchwork)  # para ajustes de gráficos lado a lado
library(forecast)   # para modelos ARIMA
library(dplyr)      # manejo de dados
library(lmtest)     # manejo de dados

# Estimação de parâmetros -------------------------------------------------

# |----------------------------------------------|
# |1. Representação da estabilidade de um modelo |
# |   caso do modelo AR(1)                       |
# |----------------------------------------------|

# Fixa uma semente para reprodutibilidade do processo
set.seed(10)

# Parâmetros
nSim = 200  # tamanho da série
phi1 <- 0.7 # modelo estável
phi2 <- 1.01 # modelo instável

# Processo estável |ar| < 1
x1 <- arima.sim(model = list(ar = phi1), n = nSim)

# Processo instável |ar| > 1. Nota: a função 'arima.sim' não permite modelos 
# não estáveis (não estacionários). É preciso implementar manualmente.
x2 <- numeric(nSim)

# Números pseudoaleatórios normalmente distribuídos (média 0; desv.pad. 1)
at <- rnorm(nSim, 0, 1)

# Simulação via loop
for (i in 2:nSim) {
  x2[i] <- phi2 * x2[i-1] + at[i]
}


# Montagem do data frame
df <- data.frame(t = 1:nSim,
                 est = as.numeric(x1),
                 ins = as.numeric(x2))

# Obtenção do gráfico
# tiff('estabilidadeProcesso.tif', height=720, width = 1780, res=300)
p1 <- ggplot(df, aes(x = t, y = est)) +
  geom_line(color="steelblue") +
  labs(x = "Tempo", y = "Z", subtitle = "Processo estável - \u03C6 = 0,7") +
  theme_gray()
p2 <- ggplot(df, aes(x = t, y = ins)) +
  geom_line(color="firebrick") +
  labs(x = "Tempo", y = "Z", subtitle = "Processo instável - \u03C6 = 1,01") +
  theme_gray()
p1 + p2
# dev.off()

# |-------------------------------------------------------|
# |2. Representação dos parâmetros AR no círculo unitário |
# |   caso do modelo AR(1)                                |
# |-------------------------------------------------------|

# Parâmetros (mesmos do item anterior)
phi     <- c(phi1, phi2)
rotulos <- c('Estacionário', 'Não Estacionário')

# Raiz do polinômio característico para o processo AR(1)
raiz <- 1/phi

# Montagem do data frame
df <- data.frame(Re  = Re(raiz), # porção real
                 Im  = Im(raiz), # porção imaginária
                 phi = phi,
                 rot = rotulos)

# Círculo unitário
angulo  <- seq(0, 2 * pi, length.out = 300)
circulo <- data.frame(x = cos(angulo),
                      y = sin(angulo))

# Obtenção do gráfico
# tiff('circuloUnitario.tif', height=1500, width = 1500, res=300)
ggplot() +
  geom_path(data = circulo, aes(x = x, y = y), color = "gray40", linewidth = 1) +
  geom_point(data = df, aes(x = Re, y = Im, color = rot), size = 4) +
  geom_text(data = df, aes(x = Re, y = Im, label = sprintf("%.2f", Re)),
            vjust = -1.2, hjust = 1.2,size = 3.5) +
  coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  scale_color_manual(values = c("Estacionário" = "steelblue", "Não Estacionário" = "firebrick")) +
  labs(x = "Parte real", y = "Parte Imaginária", color = NULL) +
  theme_gray()
# dev.off()

# |------------------------------------------------------|
# |3. Exemplo - verificação da estabilidade de um modelo |
# |   estacionariedade e invertibilidade                 |
# |------------------------------------------------------|

# Coeficientes do modelo
phi   <- c(0.6, 0.2)  # porção AR
theta <- c(-0.1, 0.8)  # porção MA 

# Verificação da condição de estacionariedade. O R calcula as raízes de um 
# polinômio por meio da função 'polyroot'. Essa função deve ser implementada
# com os coeficientes em ordens crescentes.
# Ex.: 1 - phi1 * B - phi2 * B^2 --> ordem:(1, phi1, phi2)
raizesAR <- polyroot(c(1, -phi)) # notar que os parâmetros entram negativos!

# Resultado - notar a importância de se usar o operador Mod(), que retorna a 
# magnitude do vetor. Para raízes complexas, essa conta precisa ser feita para
# que a verificação do círculo unitário seja realizada adequadamente. Para
# raízes reais, Mod() apenas alterará o sinal
cat("Raízes polinômio AR:", Mod(raizesAR))

# Verificação da condição de invertibilidade. Cuidar que o polinômio para o 
# processo MA tem diferenças nos sinais. Isso reflete a forma de entrar com os
# coeficientes na função
raizesMA <- polyroot(c(1, -theta)) 
# Resultado 
cat("Raízes polinômio MA:", Mod(raizesMA))

# |----------------------------------------------------|
# |4. Exemplo - função soma dos quadrados dos resíduos |
# |   modelo ARMA(1,1) ajustado via MLE                |
# |----------------------------------------------------|
# Processo para se obter um mapa de calor com o comportamento dos parâmetros 
# AR e MA estimados para um modelo ARMA(1,1). Inicialmente, é feita uma 
# simulação com parâmetros definidos. Na sequência, é feito um processo 
# combinatório para parâmetros phi e theta variando em (quase) todo o domínio
# que garante um modelo estacionário e invertível

# Semente fixa para reprodutibilidade
set.seed(123)

# Simulação de um processo "teórico" (com parâmetros fixos conhecidos)
n            <- 300 # tamanho da série
phiTeorico   <- 0.6 # parâmetro AR
thetaTeorico <- 0.4 # parâmetro MA
x <- arima.sim(model = list(ar = phiTeorico, ma = thetaTeorico), n = n)

# Variação dos parâmetros que servem como a estimativa do processo teórico
phiEstimado   <- seq(-0.95, 0.95, by = 0.05)
thetaEstimado <- seq(-0.95, 0.95, by = 0.05)

# A função 'expand.grid' gera todas as combinações possíveis dos dois parâmetros
Sgrid <- expand.grid(phi = phiEstimado, theta = thetaEstimado)

# Cálculo da função S (soma dos quadrados dos resíduos) para as combinações dos
# parâmetros
Sgrid$S <- mapply(function(phi, theta) {
  # O recurso 'try' é utilizado para que eventuais erros nos ajustes das diver-
  # sas combinações das porções AR e MA não façam o processo ser interrompido.
  # Ele é complementado pela condicional 'if' logo abaixo, que atribui NA a
  # qualquer eventual erro
  fit <- try(arima(x,
                   order = c(1,0,1),
                   fixed = c(phi, theta, NA),
                   transform.pars = FALSE,
                   method = "CSS"),
             silent = TRUE)
  if (inherits(fit, "try-error")) return(NA)
  sum(residuals(fit)^2) # soma dos quadrados dos resíduos
}, Sgrid$phi, Sgrid$theta)

# Omite eventuais NAs
Sgrid <- na.omit(Sgrid)

# Limita os valores da função S, pois para certas combinações de phi e theta o
# resultado "explode". Isso prejudica a visualização
Sgrid <- Sgrid %>%
  mutate(SgridLim = pmin(S, quantile(S, 0.85)))  

# Obtenção do gráfico
# tiff('somaQuadrados.tif', height=1300, width = 1600, res=300)
ggplot(Sgrid, aes(x = phi, y = theta)) +
  geom_tile(aes(fill = SgridLim)) +
  # geom_contour(aes(z = rss), color = "black", alpha = 0.4) +
  annotate("point", x = phiTeorico, y = thetaTeorico,
           color = "red", size = 3, shape = 21, fill = "white") +
  scale_fill_viridis_c(option = "E", direction = -1, na.value = "white") +
  labs(subtitle = "Ponto vermelho: parâmetros teóricos (\u03C6 = 0.6, \u03B8 = 0.4)",
    x = "\u03C6 (parâmetro AR)",
    y = "\u03B8 (parâmetro MA)",
    fill = "S(\u03C6,\u03B8)") +
  # theme_gray(base_size = 14)
  theme_gray()
# dev.off()


# Validação ---------------------------------------------------------------

# |------------------------------------------------------------|
# |5. Representação das propriedades do ruído branco gaussiano |
# |   resíduos de um modelo bem ajustado                       |
# |------------------------------------------------------------|

# A função 'rnorm' produz ruídos brancos gaussianos
n <- 1000
ruidoBranco <- rnorm(n, mean = 0, sd = 1)
df <- data.frame(t = 1:n,
                 y = ruidoBranco)


# As três propriedades de interesse são:
# - Independência
# - Homoscedasticidade
# - Distribuição normal

# Gráficos
# 5.1. Independência (via FAC)
p1 <- ggAcf(ruidoBranco, lag.max = 100) + ggtitle("FAC")

# 5.2. Homocedasticidade (via plot da série temporal)
p2 <- ggplot(df, aes(x = t, y = y)) + 
  geom_line(color="deepskyblue3", alpha = 0.8, na.rm = TRUE) +
  geom_hline(yintercept = 3.5, color = "black", linetype = "dashed") +
  geom_hline(yintercept = -3.5, color = "black", linetype = "dashed") +
  labs(x = 'Tempo', y = 'Z',
       subtitle = 'Série Homoscedástica') +
  theme_gray() 

# 5.3. Distribuição normal (via histograma)
p3 <- ggplot(df, aes(x = y)) +
  geom_histogram(aes(y = ..density.., color = "Resíduos"), bins = 15,
                 fill = "steelblue", alpha = 0.5) +
  stat_function(aes(color = "Dist. Normal"), fun = dnorm,
                args = list(mean = 0, sd = 1),
                size = 1) +
  scale_color_manual(name = "Legenda", values = c("Resíduos" = "steelblue",
                                                  "Dist. Normal" = "navy")) +
  labs(x = "Z", y = "Densidade", subtitle = "Distribuição Normal") +
  theme_gray()
# Unindo os gráficos
tiff('ruidoBranco.tif', height=1200, width = 1780, res=300)
p1 / p2 / p3
dev.off()

# |-----------------------|
# |6. Validação do modelo |
# |   testes estatísticos |
# |-----------------------|
# Uso da série gerada no item anterior

# 6.1. Independência: teste de Ljung-Box
Box.test(ruidoBranco, lag = 20, type = "Ljung-Box")

# 6.2. Homoscedasticidade: teste de Breusch-Pagan
bptest(df$y ~ df$t)

#6.3. Normalidade: teste de Shapiro-Wilk
shapiro.test(ruidoBranco)
