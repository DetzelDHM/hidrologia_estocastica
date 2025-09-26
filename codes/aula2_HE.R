# UNIVERSIDADE FEDERAL DO PARANÁ
# Programa de Pós-Graduação em Engenharia de Recursos Hídricos e Ambiental
# ERHA7016 – Hidrologia Estocástica
# Autor: Daniel Detzel
# Data: 24 set. 2025
# Aula 2: Introdução (pt. 2)
#         Análise de Séries Temporais (pt. 1)

# Bibliotecas utilizadas
library(tidyverse)  # contém diversas bibliotecas, incluindo ggplot2
library(climate)    # acesso à série da Oscilação Muitidecadal do Atlântico
library(patchwork)  # para ajustes de gráficos lado a lado
library(hydroTSM)   # para conversão de vazões entre escalas temporais
library(forecast)   # para correlograma
library(longmemo)   # para série dos níveis mínimos do rio Nilo
library(goftest)    # para de Anderson-Darling 
library(ppcc)       # para teste de PPCC


# Verificação de adequação de ajuste --------------------------------------

# |------------------------------------------|
# |1. Testes de hipótese                     |
# |    Shapiro-Wilk, Anderson-Darling e PPCC |
# |------------------------------------------|

# Usando a série dos níveis mínimos do rio Nilo
data(NileMin)
# Montagem do data frame
serie <- data.frame(x = as.numeric(NileMin))

# 1.1. Verificação quanto à normalidade
# Shapiro-Wilk
shapiro.test(serie$x)
# Anderson-Darling
ad.test(serie$x, null = "pnorm", mean = mean(serie$x),
        sd = sd(serie$x), estimated = TRUE)
# PPCC
ppccTest(serie$x, qfn = "qnorm")

# 1.2. Verificação quanto à log-normalidade
# Anderson-Darling
ad.test(serie$x, null = "plnorm", meanlog = mean(log(serie$x)),
        sdlog = sd(log(serie$x)), estimated = TRUE)
# PPCC
ppccTest(serie$x, qfn = "qlnorm")

# Processos estocásticos - definições -------------------------------------

# |-----------------------------------------------------------|
# |2. Representação de realizações de um processo estocástico |
# |   uso de dados simulados                                  |
# |-----------------------------------------------------------|

# Representação de realizações de um processo estocástico
# Assim como na aula 1, vamos usar um modelo autorregressivo. Porém, um primeiro
# processo para representar o que é observado (amostra de uma realização) e 
# outros 5 para representar as outras possíveis realizações
phi <- 0.6
nObs <- 40
nSim <- 100

# Representação das realizações
sim <- replicate(n = 6,
                 expr = arima.sim(model = list(ar = phi), n = nSim))
# Posicionando as observações entre a janela de 100 intervalos de tempo, a 
# partir de t = 20
iniObs <- 20
obs <- c(rep(NA, iniObs - 1),
         sim[iniObs:(iniObs + nObs - 1),6],
         rep(NA, nSim - (iniObs - 1 + nObs)))

# Criação do data frame
serie <- data.frame(x    = 1:nSim,
                    obs  = obs,
                    sim1 = sim[,1],
                    sim2 = sim[,2],
                    sim3 = sim[,3],
                    sim4 = sim[,4],
                    sim5 = sim[,5],
                    obs1 = sim[,6])

# Produção dos gráficos - são 3: (1) somente o observado, (2) observado com
# a sua realização infinita associada e (3) observado e demais simulações

# Gráfico (1)
# tiff('PE - amostra.tif', height=720, width = 1780, res=300)
ggplot() +
  geom_line(data = serie,
            aes(x = x, y = obs, colour = "Observado"),
            linewidth = 1.2, na.rm = TRUE) +
  scale_colour_manual(values = c(Observado = "firebrick"),name = "Legenda") +
  scale_y_continuous(limits = c(-3,4), breaks = NULL) +
  labs(x = "Tempo", y = "Variável aleatória",
       subtitle = "Representação de uma amostra de uma realização de um PE") +
  theme_gray() +
  theme(legend.position = "right")
# dev.off()

# Gráfico (2)
# tiff('PE - realização.tif', height=720, width = 1780, res=300)
ggplot() +
  geom_line(data = serie,
            aes(x = x, y = obs1, colour = "Simulado"),
            linewidth = 0.5, alpha = 0.6) +
  geom_line(data = serie,
            aes(x = x, y = obs, colour = "Observado"),
            linewidth = 1.2, na.rm = TRUE) +
  scale_colour_manual(values = c(Simulado = "gray60",
                                 Observado = "firebrick"),name = "Legenda") +
  scale_y_continuous(limits = c(-3,4), breaks = NULL) +
  labs(x = "Tempo", y = "Variável aleatória",
       subtitle = "Representação de 1 realização de um PE") +
  theme_gray() +
  theme(legend.position = "right")
# dev.off()

# Gráfico (3)
# Reorganização do data frame para formato longo (facilita inclusão de 
# legenda no gráfico). A ideia é ter uma legenda única para todas as simulações
serieSim <- serie %>% pivot_longer(cols = starts_with("sim"),
                                  names_to  = "simID",
                                  values_to = "Valores") 
serieSim <- serieSim %>% mutate(groupID = simID,
                                simRotulo = "Simulado")

# Obtenção do gráfico
# tiff('PE - 5 realizações.tif', height=720, width = 1780, res=300)
ggplot() +
  geom_line(data = serieSim,
            aes(x = x, y = Valores, colour = simRotulo, group = groupID),
            linewidth = 0.5, alpha = 0.6) +
  geom_line(data = serie,
            aes(x = x, y = obs, colour = "Observado"),
            linewidth = 1.2, na.rm = TRUE) +
  scale_colour_manual(values = c(Simulado = "gray60",
                                 Observado = "firebrick"),name = "Legenda") +
  scale_y_continuous(limits = c(-3,4), breaks = NULL) +
  labs(x = "Tempo", y = "Variável aleatória",
       subtitle = "Representação de 5 realizações de um PE") +
  theme_gray() +
  theme(legend.position = "right")
# dev.off()


# Análise de séries temporais - definições e propriedades -----------------

# |-----------------------------------------------|
# |3. Exemplo de comportamentos não estacionários |
# |   série NAO                                   |
# |-----------------------------------------------|

# NAO: Oscilação Multidecadal do Atlântico. Fonte dos dados: https://climatedataguide.ucar.edu/climate-data/hurrell-north-atlantic-oscillation-nao-index-station-based
nao <- read.table("nao_station_seasonal.txt",
                  header = TRUE,
                  skip = 1,
                  sep = "",
                  na.strings = "-999")
nao[nao == -999] <- NA

# Conversão para vetor
naoVetor <- as.numeric(t(nao[,]))
anos <- as.numeric(rownames(nao))
anosVetor <- rep(anos, each = ncol(nao)) + rep(0:(ncol(nao)-1)/12,
                                                times = length(anos))

# Montagem do data frame
serie <- data.frame(x = anosVetor, y = naoVetor)

# Obtenção do gráfico
# tiff('serieAMO.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x=x, y=y)) + 
  geom_line(color="deepskyblue3", alpha = 0.8, na.rm = TRUE) +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = 'Anos', y = 'Anomalias [°C]',
       subtitle = 'Índice Oscilação multidecadal do Atlântico') +
  scale_x_continuous(breaks = round(seq(min(serie$x), max(serie$x), length.out = 10))) +
  theme_gray()  
# dev.off()

# Análise de séries temporais - persistência ------------------------------


# |--------------------------------------------|
# |4. Série persistente vs. série independente |
# |   série de níveis mínimos do rio Nilo      |
# |--------------------------------------------|

# Carregamento dos dados de níveis mínimos do rio Nilo
data(NileMin)
# Ano de início (622 d.C.)
anoIni <- 622
# Montagem do data frame
serie <- data.frame(x = as.numeric(time(NileMin)) + anoIni - 1,
                    y1 = as.numeric(NileMin))

# Geração de uma série aleatória (normalmente distribuída), com as mesmas 
# estatísticas da do rio Nilo
media   <- mean(serie$y1)
desvPad <- sd(serie$y1)
n       <- length(serie$y1)
aleatorio <- rnorm(n, mean = media, sd = desvPad)
# Inclusão no data frame
serie$y2 <- aleatorio

# Obtenção do gráfico
tiff('persistencia.tif', height=960, width = 2370, res=300)
# Série persistente
p1 <- ggplot(serie, aes(x = x, y = y1)) +
  geom_line(color="firebrick") +
  geom_hline(yintercept = media, color = "black") +
  scale_x_continuous(limits = c(600,1300), breaks = NULL) +
  scale_y_continuous(limits = c(900,1500), breaks = NULL) +
  labs(x = NULL, y = NULL,
       subtitle = 'Série persistente (níveis mínimos do rio Nilo)') +
  theme_gray() 

# Série independente
p2 <- ggplot(serie, aes(x = x, y = y2)) +
  geom_line(color="firebrick") +
  geom_hline(yintercept = media, color = "black") +
  scale_x_continuous(limits = c(600,1300), breaks = NULL) +
  scale_y_continuous(limits = c(900,1400), breaks = NULL) +
  labs(x = NULL, y = NULL,
       subtitle = 'Série independente') +
  theme_gray() 

# União em subplots
p2/p1
dev.off()

# |----------------------------------|
# |5. Montagem do correlograma       |
# |   série de vazões médias mensais |
# |----------------------------------|

# Leitura dos dados de vazões diárias do ONS. Os dados são originalmente 
# disponibilizados pelo ONS no portal SINtegre, em formato XLS. Para fins de
# eficiência na leitura do arquivo, o formato foi convertido externamente
# para CSV
dadosCompletos <- read.csv("Vazoes_Diarias_1931_2023.csv",
                           check.names = FALSE)

# Extração da série de interesse. Nota: a função cod2serie.R precisa estar
# na mesma pasta deste código
source("cod2serie.R")

# Extração da série de Foz do Areia (código 74). O código funciona para qualquer
# outra série do banco de dados, bastando que se altere o seu código
Qdiaria <- cod2serie(dadosCompletos, 74)

# Conversão em vazões mensais (necessita conversão para formato zoo)
QdiariaZoo <- zoo(Qdiaria$Vazao, order.by = as.Date(Qdiaria$Datas))
Qmensal    <- daily2monthly(QdiariaZoo,FUN = mean)
Qmensal    <- data.frame(Datas = index(Qmensal),
                         Vazao = coredata(Qmensal))
# Conversão em vazões anuais (para uso futuro)
Qanual     <- daily2annual(QdiariaZoo,FUN = mean)
Qanual     <- data.frame(Datas = index(Qanual),
                         Vazao = coredata(Qanual))

# Obtenção do correlograma
# O procedimento é similar ao que foi determinado na aula 1. Contudo, aqui é
# adicionado um critério mais preciso quanto ao cálculo dos intervalos de 
# confiança, os quais são dependentes do lag. Como consequência, eles não
# resultam em linhas horizontais
vazao <- Qmensal$Vazao
fac <- acf(vazao, lag.max = 36, plot = FALSE)

# Elementos auxiliares
facVetor <- as.numeric(fac$acf)
lags     <- as.numeric(fac$lag)
n        <- length(vazao)

# Intervalos de confiança (95%)
ic <- numeric(length(facVetor))
ic[1] <- 0  # lag 0
for(k in 2:length(facVetor)) {
  if (k < 2) {
    ic[k] <- sqrt( (1/n) * (1 + 2 * sum(facVetor[2:(k-1)]^2)) )
  } else {
    ic[k] <- sqrt(1/n)
  }
}
ic_sup <- 1.96 * ic
ic_inf <- -1.96 * ic

# Obtenção do data frame
serie <- data.frame(x = lags,
                    y = facVetor,
                    sup = ic_sup,
                    inf = ic_inf,
                    leg = "Correlograma")

# Obtenção dos gráficos
# Plotagem da série para visualização
# tiff('ACFserie.tif', height=720, width = 1780, res=300)
ggplot(vazao, aes(x = Datas, y = Vazao)) + 
  geom_line(color="deepskyblue3") +
  labs(x = 'Data', y = 'Vazão [m³/s]') +
  scale_x_date(date_breaks = "10 years",date_labels = "%Y") +
  theme_gray()
# dev.off()

# Correlograma sem intervalos de confiança
# tiff('ACFsemIC.tif', height=720, width = 1780, res=300)
ggplot(serie, aes(x = x)) + 
  geom_bar(aes(y = y), fill = "sandybrown", stat = "identity") +
  geom_hline(yintercept = 0, color = "black") +
  #scale_fill_manual(values = "Correlograma",
                    #name = "Legenda") +
  labs(x = 'Defasagem (anos)', y = 'Correlação') +
  theme_gray()
# dev.off()

# Correlograma com intervalos de confiança
# tiff('ACFcomIC.tif', height=720, width = 1780, res=300)
# tiff('ACFSobradinho.tif', height=900, width = 900, res=300)
ggplot(serie, aes(x = x)) + 
  geom_bar(aes(y = y, fill = "Correlograma"), stat = "identity") +
  geom_hline(yintercept = 0, color = "black") +
  geom_ribbon(aes(ymin = inf, ymax = sup, fill = "95% conf."), alpha = 0.5) +
  scale_fill_manual(values = c("Correlograma" = "sandybrown",
                               "95% conf." = "sandybrown"),
                    name = "Legenda") +
  labs(x = 'Defasagem (anos)', y = 'Correlação',
       subtitle = 'Escala anual') +
  theme_gray() 
  theme(
    legend.position = c(0.70, 0.75),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8))
# dev.off()

# Códigos auxiliares ------------------------------------------------------

# Plotagem simplificada de ACF
# tiff('ACFFozDoAreia.tif', height = 750, width = 750, res=300)
ggplot(serie, aes(x = x)) + 
  geom_bar(aes(y = y), fill = "sandybrown",stat = "identity") +
  geom_hline(yintercept = 0, color = "black") +
  geom_ribbon(aes(ymin = inf, ymax = sup), fill = "sandybrown", alpha = 0.5) +
  labs(x = 'Defasagem (anos)', y = 'Correlação',
       subtitle = 'Foz do Areia') +
  theme_gray() 
# dev.off()

