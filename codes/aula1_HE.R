# UNIVERSIDADE FEDERAL DO PARANÁ
# Programa de Pós-Graduação em Engenharia de Recursos Hídricos e Ambiental
# ERHA7016 – Hidrologia Estocástica
# Autor: Daniel Detzel
# Data: 16 set. 2025
# Aula 1: Introdução - conceitos iniciais e revisão de estatística

# Bibliotecas utilizadas
library(ggplot2)   # para os gráficos
library(patchwork) # para ajustes de gráficos lado a lado
library(hydroTSM)  # para séries hidrológicas
library(zoo)       # para interpretação de dados em formato 'zoo'
library(longmemo)  # para série dos níveis mínimos do rio Nilo
library(tidyr)     # para manejo de dados
library(goftest)   # para de Anderson-Darling 
library(ppcc)      # para teste de PPCC

# Modelos determinísticos e estocásticos ----------------------------------

# |------------------------------------|
# |1. Exemplo de modelo determinístico |
# |   função senoidal                  |
# |------------------------------------|

# Criação dos valores da abscissa até o limite de 6*pi, usando 1000 pontos 
# igualmente espaçados
x <- seq(0, 6*pi, length.out = 1000)
# Aplicação da função seno em criação do data frame
serie <- data.frame(x = x, y = sin(x))

# Processo opcional somente para atribuir rótulos textuais em função de pi no
# eixo das abscissas
pi_breaks <- seq(0, 6*pi, by = pi/2)
pi_labels <- sapply(0:(length(pi_breaks)-1), function(i) {
  if (i == 0) "0"
  else if (i %% 2 == 0) paste0(i/2, "\u03C0")
  else paste0(i, "\u03C0/2")
})

# Obtenção do gráfico
# tiff('serieDeterministica.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="blue3") +
  scale_x_continuous(breaks = pi_breaks,
                     labels = pi_labels,
                     name = expression(x)) +
  labs(y = expression(sin(x))) +
  theme_gray() 
# dev.off()

# |------------------------------------|
# |2. Exemplo de modelo estocástico    |
# |   modelo AR(1)                     |
# |------------------------------------|

# O R possui uma função nativa para simular valores com modelos ARIMA. Aqui
# vamos definir um parâmetro AR de 0,9 e 100 valores gerados
phi <- 0.9
n <- 100
y <- arima.sim(model = list(ar = phi), n = n)

# Criação do data frame
serie <- data.frame(x <- 1:n,y = as.numeric(y))

# Obtenção do gráfico
# tiff('serieEstocastica.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="blue3") +
  theme_gray() 
# dev.off()

# Natureza estocásticas de variáveis hidrológicas -------------------------

# |-------------------------------------|
# |3. Exemplo séries temporais diversas |
# |   [Exemplo 3] (slides)              |
# |-------------------------------------|

# 3.1. Precipitação em escala diária
# Estação Maquehue Temuco, Chile
data(MaquehueTemuco)
# Montagem do data frame
serie <- data.frame(x = index(MaquehueTemuco),
                    y = coredata(MaquehueTemuco$pcp))
# Obtenção do gráfico
# tiff('seriePrecipMaquehueTemuco.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="deepskyblue3") +
  labs(x = 'Data', y = 'Precipitação [mm]',
       subtitle = 'Precipitação diária em Maquehue Temuco (Chile)') +
  scale_x_date(date_breaks = "10 years",date_labels = "%Y") +
  ylim(0,200) +
  theme_gray() 
# dev.off()

# 3.2. Precipitação em escala mensal
# Bacia do rio Ebro, Espanha
data(EbroPPtsMonthly)
# Montagem do data frame
serie <- data.frame(x = EbroPPtsMonthly$Date,
                    y = EbroPPtsMonthly$P9008X)
# Obtenção do gráfico
# tiff('seriePrecipMensal.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="green4") +
  labs(x = 'Data', y = 'Precipitação [mm]',
       subtitle = 'Precipitação mensal em Ebro (Espanha)') +
  theme_gray() 
# dev.off()

# 3.3. Temperatura máxima em escala diária
# Estação Cauquenes en El Arrayan, Chile
data(Cauquenes7336001)
# Montagem do data frame
serie <- data.frame(x = index(Cauquenes7336001),
                    y = coredata(Cauquenes7336001$Tmx_degC))
# Obtenção do gráfico
# tiff('serieTmaxCauquenes.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="darkorange") +
  labs(x = 'Data', y = 'Temperatura Máxima [°C]',
       subtitle = 'Temperatura máxima diária em Cauquenes (Chile)') +
  theme_gray() 
# dev.off()

# 3.4. Evapotranspiração potencial em escala diária
# Estação Cauquenes en El Arrayan, Chile
data(Cauquenes7336001)
# Montagem do data frame
serie <- data.frame(x = index(Cauquenes7336001),
                    y = coredata(Cauquenes7336001$PET_mm))
# Obtenção do gráfico
# tiff('serieEVTPCauquenes.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="goldenrod") +
  labs(x = 'Data', y = 'EVT Potencial [mm]',
       subtitle = 'Evapotranspiração potencial diária em Cauquenes (Chile)') +
  theme_gray() 
# dev.off()

# 3.5. Vazão em escala horária
# Rio Karamea, Nova Zelândia
data(KarameaAtGorgeQts)
# Montagem do data frame
serie <- data.frame(x = index(KarameaAtGorgeQts),
                    y = coredata(KarameaAtGorgeQts))
# Obtenção do gráfico
# tiff('serieVazaoHoraria.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="darkorchid4",na.rm = TRUE) +
  labs(x = 'Data', y = 'Vazão [m³/s]',
       subtitle = 'Vazão horária no rio Karamea (Nova Zelândia)') +
  ylim(0,3000) +
  theme_gray() 
# dev.off()

# |-----------------------------|
# |4. Exemplo de persistência   |
# |   dependência temporal      |
# |-----------------------------|

# Avaliação do correlograma dos níveis mínimos do rio Nilo
data(NileMin)
# Ano de início (622 d.C.)
anoIni <- 622
# Montagem do data frame
serie <- data.frame(x = as.numeric(time(NileMin)) + anoIni - 1,
                    y = as.numeric(NileMin))
# Obtenção do gráfico
# tiff('serieNilo.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="sandybrown") +
  labs(x = 'Data', y = 'Nível [m]',
       subtitle = 'Nível mínimo no rio Nilo (estação de Roda, Cairo)') +
  scale_x_continuous(breaks = round(seq(min(serie$x), max(serie$x), length.out = 10))) +
  ylim(900,1500) +
  theme_gray() 
# dev.off()

# |------------------------------|
# |5. Exemplo de correlograma    |
# |   (função de autocorrelação) |
# |------------------------------|

# O R possui uma maneira direta de plotar o correlograma, mas apresenta de
# uma forma simples. Opta-se, portanto, pelo uso do ggplot2 para deixá-lo
# mais apresentável

# Determinação do correlograma (sem plotar)
acfNilo <- acf(serie$y, lag.max = 100, plot = FALSE)
# Intervalos de confiança (95%)
ic <- 1.96/sqrt(length(serie$y))
# Obtenção do data frame
serie <- data.frame(x = acfNilo$lag[,1,1],
                    y = acfNilo$acf[,1,1],
                    sup = ic,
                    inf = -ic,
                    leg = "Correlograma")
# Obtenção do correlograma
# tiff('correlogramaNilo.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x = x)) + 
  geom_bar(aes(y = y, fill = "Correlograma"), stat = "identity") +
  geom_hline(yintercept = 0, color = "black") +
  geom_ribbon(aes(ymin = inf, ymax = sup, fill = "95% conf."), alpha = 0.5) +
  scale_fill_manual(values = c("Correlograma" = "sandybrown",
                               "95% conf." = "sandybrown"),
                               name = "Legenda") +
  labs(x = 'Defasagem (anos)', y = 'Correlação',
       subtitle = 'Correlograma dos níveis mínimos do rio Nilo') +
  theme_gray() +
  theme(
    legend.position = c(0.85, 0.75),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8))
# dev.off()

# Variáveis aleatórias ----------------------------------------------------

# |-----------------------------------------|
# |6. Representação de variáveis aleatórias |
# |   VA contínuas por meio de VA discretas |
# |-----------------------------------------|

# Uso da série de vazões diárias no rio Oca, Espanha
data(OcaEnOnaQts)
# Somente 1 mês de amostra é necessário
dataIni <- as.Date("1962-01-01")
dataFim   <- as.Date("1962-01-31")
amostra <- window(OcaEnOnaQts, start = dataIni, end = dataFim)
# Montagem do data frame
serie <- data.frame(x = index(amostra),
                    y = coredata(amostra))
# Obtenção do gráfico
# tiff('VAcontVAdisc.tif', height=720, width = 1780, res=300)
pdisc <- ggplot(serie, aes(x=x, y=y)) + 
  geom_point(color="springgreen3") +
  labs(x = 'Data', y = 'Vazão [m³/s]',
       subtitle = 'VA discreta (função das medições)') +
  scale_x_date(date_breaks = "6 days",date_labels = "%d %b") +
  theme_gray() 
pcont <- ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="springgreen3") +
  labs(x = 'Data', y = 'Vazão [m³/s]',
       subtitle = 'VA contínua (representação)') +
  scale_x_date(date_breaks = "6 days",date_labels = "%d %b") +
  theme_gray() 
pdisc + pcont
# dev.off()

# Distribuições de probabilidade ------------------------------------------

# |----------------------------------------|
# |7. Representação da distribuição Normal |
# |   FDP e FDA (genéricas)                |
# |----------------------------------------|

# Criação de uma variável aleatória
va <- seq(-4, 4, length.out = 400)

# Montagem do data frame com as transformações para obtenção das funções
serie <- data.frame(x = va,
                    fdp = dnorm(va, mean = 0, sd = 1),
                    fda = pnorm(va, mean = 0, sd = 1))

# tiff('NormalFDP_FDA.tif', height=720, width = 1780, res=300)
p_fpd <- ggplot(serie, aes(x = x, y = fdp)) +
  geom_line(color = "steelblue", size = 1.2) +
  labs(title = "FDP Distribuição Normal",
       x = "x",
       y = "Densidade") +
  theme_gray()
p_fpa <- ggplot(serie, aes(x = x, y = fda)) +
  geom_line(color = "firebrick", size = 1.2) +
  labs(title = "FDA Distribuição Normal",
       x = "x",
       y = "Probabilidade") +
  theme_gray()
p_fpd + p_fpa
# dev.off()

# |----------------------------------------|
# |8. Representação da distribuição Normal |
# |   comportamento dos parâmetros         |
# |----------------------------------------|

# Representação da distribuição Normal (FDP)
va <- seq(-8, 8, length.out = 1000)

# Criação das diferentes FDPs
serie <- data.frame(x = va,
                    fdp1 = dnorm(va, mean = 0, sd = 1),
                    fdp2 = dnorm(va, mean = 2, sd = 1),
                    fdp3 = dnorm(va, mean = -4, sd = 1.5),
                    fdp4 = dnorm(va, mean = 0, sd = 2))
# Reorganização do data frame para formato longo (facilita inclusão de 
# legenda no gráfico)
serie <- serie |> pivot_longer(cols = starts_with("fdp"),
                               names_to  = "Distribuicao",
                               values_to = "Densidade")
# Montagem da legenda
leg <- c(fdp1 = "\u03bc = 0,  \u03c3 = 1",
         fdp2 = "\u03bc = 2,  \u03c3 = 1",
         fdp3 = "\u03bc = -4, \u03c3 = 1.5",
         fdp4 = "\u03bc = 0,  \u03c3 = 2")

# Obtenção do gráfico
# tiff('NormalFDP_parametros.tif', height = 720, width = 1780, res=300)
ggplot(serie, aes(x = x, y = Densidade, color = Distribuicao)) +
  geom_line() +
  scale_color_manual(values = c("steelblue", "firebrick",
                                "forestgreen", "tan3"),
                     labels = leg,
                     name = "Parâmetros") +
  xlim(-8,8) +
  labs(x = "x",
       y = "Densidade") +
  theme_gray()
# dev.off()

# |--------------------------------------------|
# |9. Representação da distribuição Log-Normal |
# |   comportamento dos parâmetros             |
# |--------------------------------------------|

# Criação da variável aleatória
va <- seq(0, 10, length.out = 1000)

# Criação das diferentes FDPs
serie <- data.frame(x = va,
                    fdp1 = dlnorm(va, meanlog = 0.2, sdlog = 1),
                    fdp2 = dlnorm(va, meanlog = 1, sdlog = 1),
                    fdp3 = dlnorm(va, meanlog = 0.8, sdlog = 1.5),
                    fdp4 = dlnorm(va, meanlog = 0.7, sdlog = 0.5))
# Reorganização do data frame para formato longo (facilita inclusão de 
# legenda no gráfico)
serie <- serie |> pivot_longer(cols = starts_with("fdp"),
                               names_to  = "Distribuicao",
                               values_to = "Densidade")
# Montagem da legenda
leg <- c(fdp1 = "\u03bc = 0,2,  \u03c3 = 1",
         fdp2 = "\u03bc = 1,  \u03c3 = 1",
         fdp3 = "\u03bc = 0,8, \u03c3 = 1.5",
         fdp4 = "\u03bc = 0,7,  \u03c3 = 0,5")

# Obtenção do gráfico
# tiff('LNormal2FDP_parametros.tif', height = 720, width = 1780, res=300)
ggplot(serie, aes(x = x, y = Densidade, color = Distribuicao)) +
  geom_line() +
  scale_color_manual(values = c("steelblue", "firebrick",
                                "forestgreen", "tan3"),
                     labels = leg,
                     name = "Parâmetros") +
  xlim(0,10) +
  labs(x = "x",
       y = "Densidade") +
  theme_gray()
# dev.off()

# |--------------------------------------------------|
# |10. Representação da distribuição Log-Normal a 3P |
# |    comportamento dos parâmetros                  |
# |--------------------------------------------------|

# Criação da variável aleatória
va <- seq(5, 15, length.out = 1000)

# É necessário implementar uma função específica, pois o R não disponibiliza
# a versão de 3 parâmetros da Log-Normal
dlnorm3 <- function(x, meanlog, sdlog, loc) {
  dlnorm(x - loc, meanlog = meanlog, sdlog = sdlog)}

# Criação das diferentes FDPs
serie <- data.frame(x = va,
                    fdp1 = dlnorm3(va, meanlog = 1, sdlog = 0.5,loc = 3.5),
                    fdp2 = dlnorm3(va, meanlog = 1, sdlog = 0.5,loc = 4.5),
                    fdp3 = dlnorm3(va, meanlog = 2, sdlog = 0.2,loc = 1),
                    fdp4 = dlnorm3(va, meanlog = 0.7, sdlog = 0.5,loc = 4.5))
# Reorganização do data frame para formato longo (facilita inclusão de 
# legenda no gráfico)
serie <- serie |> pivot_longer(cols = starts_with("fdp"),
                               names_to  = "Distribuicao",
                               values_to = "Densidade")
# Montagem da legenda
leg <- c(fdp1 = "\u03bc = 1,  \u03c3 = 0,5, \u0394 = 3,5",
         fdp2 = "\u03bc = 1,  \u03c3 = 0,5, \u0394 = 4,5",
         fdp3 = "\u03bc = 2, \u03c3 = 0,2, \u0394 = 1,0",
         fdp4 = "\u03bc = 0,7,  \u03c3 = 0,5, \u0394 = 4,5")

# Obtenção do gráfico
# tiff('LNormal3FDP_parametros.tif', height = 720, width = 1780, res=300)
ggplot(serie, aes(x = x, y = Densidade, color = Distribuicao)) +
  geom_line() +
  scale_color_manual(values = c("steelblue", "firebrick",
                                "forestgreen", "tan3"),
                     labels = leg,
                     name = "Parâmetros") +
  xlim(5,15) +
  labs(x = "x",
       y = "Densidade") +
  theme_gray()
# dev.off()


# Verificação de adequação de ajuste --------------------------------------

# |------------------------|
# |11. Métodos gráficos    |
# |    FDP, FDA e Q-Q plot |
# |------------------------|

# Usando novamente a série dos níveis mínimos do rio Nilo
data(NileMin)
# Montagem do data frame
serie <- data.frame(x = as.numeric(NileMin))

# 11.1. Histograma/densidade
# Comparação gráfica entre o histograma (empírico) e as densidades (teóricas)
# tiff('histdensNilo.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x = x)) +
  geom_histogram(aes(y = ..density.., color = "Observado"), bins = 15,
                 fill = "steelblue", alpha = 0.5) +
  stat_function(aes(color = "Dist. Normal"), fun = dnorm,
                args = list(mean = mean(NileMin), sd = sd(NileMin)),
                size = 1) +
  stat_function(aes(color = "Dist. Log-Normal"), fun = dlnorm,
                args = list(meanlog = mean(log(NileMin)),
                            sdlog = sd(log(NileMin))),
                size = 1) +
  # scale_fill_manual(name = "", values = c("Observado" = "red2")) +
  scale_color_manual(name = "Legenda", values = c("Observado" = "steelblue",
                                           "Dist. Normal" = "firebrick",
                                           "Dist. Log-Normal" = "sandybrown")) +
  labs(x = "Vazão (m³/s)", y = "Densidade", subtitle = "Níveis mínimos anuais rio Nilo") +
  theme_gray()
# dev.off()

# 11.2. Função Distribuição de Probabilidades Acumulada
# Comparação gráfica entre as FDAs empírica e teóricas
# tiff('FDANilo.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x = x)) +
  stat_ecdf(geom = "point", aes(color = "Observado")) +
  stat_function(aes(color = "Normal"), fun = pnorm,
                args = list(mean = mean(NileMin), sd = sd(NileMin)),
                size = 1) +
  stat_function(aes(color = "Log-Normal"), fun = plnorm,
                args = list(meanlog = mean(log(NileMin)),
                            sdlog = sd(log(NileMin))),
                size = 1) +
  scale_color_manual(name = "Legenda", values = c("Observado" = "steelblue","Normal" = "firebrick","Log-Normal" = "sandybrown")) +
  labs(x = "Vazão (m³/s)", y = "Probabilidade acumulada",
       subtitle = "Níveis mínimos anuais rio Nilo") +
  theme_gray()
# dev.off()

# 11.3. Q-Q plots
# O pacote 'ggplot2' possui uma função automática para o Q-Q plot, mas está
# limitada à distribuição Normal apenas. Então para o gráfico comparativo
# é preciso determinar os quantis e plotar o gráfico manualmente

# Cálculo dos quantis teóricos
# Posição de plotagem de Weibull
p <- ppoints(length(serie$x), a = 0)
# Quantis
teoricoN <- qnorm(p, mean = mean(serie$x), sd = sd(serie$x))
teoricoLN <- qlnorm(p, meanlog = mean(log(serie$x)), sdlog = sd(log(serie$x)))

# Criação do data frame
pontos <- data.frame(amostra = sort(x),
                     N = teoricoN,
                     LN = teoricoLN)

# Obtenção do gráfico
# tiff('QQplot.tif', height=920, width = 1980, res=300)
ggplot(pontos, aes(y = amostra)) +
  geom_point(aes(x = N, color = "Normal")) +
  geom_point(aes(x = LN, color = "Log-Normal")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "steelblue") +
  scale_color_manual(name = "Legenda",
                     values = c("Normal" = "firebrick",
                                "Log-Normal" = "sandybrown")) +
  labs(subtitle = "Níveis mínimos anuais rio Nilo",
       x = "Quantis teóricos (m³/s)", 
       y = "Quantis amostrais (m³/s)") +
  coord_equal() +
  theme_gray()
# dev.off()

# |------------------------------------------|
# |12. Testes de hipótese                    |
# |    Shapiro-Wilk, Anderson-Darling e PPCC |
# |------------------------------------------|

# 12.1. Verificação quanto à normalidade
# Shapiro-Wilk
shapiro.test(serie$x)
# Anderson-Darling
ad.test(serie$x, null = "pnorm", mean = mean(serie$x),
        sd = sd(serie$x), estimated = TRUE)
# PPCC
ppccTest(serie$x, qfn = "qnorm")

# 12.2. Verificação quanto à log-normalidade
# Anderson-Darling
ad.test(serie$x, null = "plnorm", meanlog = mean(log(serie$x)),
         sdlog = sd(log(serie$x)), estimated = TRUE)
# PPCC
ppccTest(serie$x, qfn = "qlnorm")
