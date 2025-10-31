# UNIVERSIDADE FEDERAL DO PARANÁ
# Programa de Pós-Graduação em Engenharia de Recursos Hídricos e Ambiental
# ERHA7016 – Hidrologia Estocástica
# Autor: Daniel Detzel
# Data: 22 out. 2025
# Aula 6: Geração de Séries Sintéticas (pt. 3)

# Bibliotecas utilizadas
library(tidyverse)  # contém diversas bibliotecas, incluindo ggplot2
library(patchwork)  # para ajustes de gráficos lado a lado
library(forecast)   # para modelos ARIMA
library(dplyr)      # manejo de dados
library(goftest)    # para de Anderson-Darling 
library(ppcc)       # para teste de PPCC
library(lubridate)  # manejo de datas
library(lmtest)     # teste de Breusch-Pagan
library(moments)    # para coeficiente de assimetria

# Função utilizada
source("cod2serieMensal.R") # extração dos dados de vazão do ONS

# Este código é o passo-a-passo para a geração de séries sintéticas de vazão,
# conforme a sistemática metodológica proposta em aula. Trata-se de uma proposta
# deliberadamente detalhada em pontos específicos. Ou seja, o código pode ser
# otimizado para que as séries sejam obtidas sem muito esforço.

# |----------------------------------|
# |0. Leitura dos dados de interesse |
# |   qualquer vazão em escala anual |
# |----------------------------------|

# Leitura dos dados (usados em diferentes partes do código)
Qmensal <- read.csv("Vazoes_Mensais_1931_2023.csv", check.names = FALSE)

# Série utilizada como exemplo
cod   <- 81  # Baixo Iguaçu
serie <- cod2serieMensal(Qmensal,cod) 

# Conversão para a escala anual (estamos trabalhando com modelos não sazonais)
serieAnual <- serie %>%
  filter(Usina == cod) %>%
  group_by(Ano) %>%
  summarise(Vazao = mean(Vazao, na.rm = TRUE)) %>%
  ungroup()

# Conversão para o formato de série temporal
Qanual <- ts(serieAnual$Vazao,
             start = min(serieAnual$Ano),
             frequency = 1)


# Seção 1 -----------------------------------------------------------------
# |---------------------------------------------------|
# |1. Análises preliminares e identificação do modelo |
# |---------------------------------------------------|


# (1a) Plotar a série histórica e analisá-la ------------------------------

# Conversão para formato suportado pelo ggplot2
QanualDF <- tibble(Ano = floor(time(Qanual)),
                   Data = ymd(paste0(Ano, "-01-01")),
                   Vazao = as.numeric(Qanual))

# Obtenção do gráfico
# tiff('passo1a.tif', height=720, width = 1780, res=300)
ggplot(QanualDF, aes(x = Data, y = Vazao)) + 
  geom_line(color="steelblue", linewidth = 1) +
  labs(x = 'Data', y = 'Vazão [m³/s]') +
  scale_x_date(date_breaks = "10 years",date_labels = "%Y") +
  theme_gray()
# dev.off()


# (1b) Verificar se a série histórica possui distribuição normal ----------

# Histograma (empírico) vs. densidades (teóricas)
p1 <- ggplot(QanualDF, aes(x = Vazao)) +
  geom_histogram(aes(y = ..density.., color = "Observado"), bins = 8,
                 fill = "steelblue", alpha = 0.5) +
  stat_function(aes(color = "Dist. Normal"), fun = dnorm,
                args = list(mean = mean(QanualDF$Vazao), sd = sd(QanualDF$Vazao)),
                size = 1) +
  scale_color_manual(name = "Legenda", values = c("Observado" = "steelblue",
                                                  "Dist. Normal" = "navy")) +
  labs(x = "Vazão (m³/s)", y = "Densidade", subtitle = "Histograma/Densidade") +
  theme_gray() +
  theme(legend.position = "none")
  # theme(legend.position = c(0.75, 0.85),
  #       legend.background = element_rect(fill = alpha("white", 0.8)))

# Q-Q plots
# Cálculo dos quantis teóricos usando as posições de plotagem de Weibull
p <- ppoints(length(QanualDF$Vazao), a = 0)
# Quantis
teoricoN <- qnorm(p, mean = mean(QanualDF$Vazao), sd = sd(QanualDF$Vazao))

# Criação do data frame
pontos <- data.frame(amostra = sort(QanualDF$Vazao),
                     N = teoricoN)

# Gráfico
p2 <- ggplot(pontos, aes(y = amostra)) +
  geom_point(aes(x = N, color = "Normal")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "steelblue") +
  scale_color_manual(name = "Legenda",
                     values = c("Normal" = "navy")) +
  labs(subtitle = "Q-Q Plot",
       x = "Quantis teóricos (m³/s)", 
       y = "Quantis amostrais (m³/s)") +
  # coord_equal() +
  theme_gray() +
  theme(legend.position = "none")
  # theme(legend.position = c(0.20, 0.85),
  #       legend.background = element_rect(fill = alpha("white", 0.8)))

# Exbição dos gráficos lado a lado
# tiff('passo1b.tif', height=1200, width = 1920, res=300)
p1 + p2
# dev.off()

# Testes de hipótese
# Shapiro-Wilk
shapiro.test(QanualDF$Vazao)
# Anderson-Darling
ad.test(QanualDF$Vazao, null = "pnorm", mean = mean(QanualDF$Vazao),
        sd = sd(QanualDF$Vazao), estimated = TRUE)
# PPCC
ppccTest(QanualDF$Vazao, qfn = "qnorm")

# (1c) Transformar a série para aproximá-la de uma distribuição normal ----

# Aqui são oferecidas 2 opções: aplicando logaritmos ou Box-Cox. Escolher apenas
# uma delas, ou dispensar a transformação
# Logarítmica -> 1
# Box-Cox     -> 2
# Sem transf. -> 3
transf <- 2

if (transf == 1) {
  # Transformação Logarítmica
  Qtransf <- log(Qanual)
} else if (transf == 2) {
  # Transformação Box-Cox
  # Parâmetro lambda
  lambda <- BoxCox.lambda(Qanual)
  # Aplicação da transformação
  Qtransf <- BoxCox(Qanual, lambda)
} else {
  Qtransf <- Qanual
}

# As verificações do item (1b) podem ser repetidas para confirmar a normalidade.

# Conversão para formato suportado pelo ggplot2
QtransfDF <- tibble(Ano = floor(time(Qtransf)),
                    Data = ymd(paste0(Ano, "-01-01")),
                    Vazao = as.numeric(Qtransf))

# Histograma (empírico) vs. densidades (teóricas)
p1 <- ggplot(QtransfDF, aes(x = Vazao)) +
  geom_histogram(aes(y = ..density.., color = "Observado"), bins = 8,
                 fill = "firebrick", alpha = 0.5) +
  stat_function(aes(color = "Dist. Normal"), fun = dnorm,
                args = list(mean = mean(QtransfDF$Vazao), sd = sd(QtransfDF$Vazao)),
                size = 1) +
  scale_color_manual(name = "Legenda", values = c("Observado" = "firebrick",
                                                  "Dist. Normal" = "darkred")) +
  labs(x = "Vazão (m³/s)", y = "Densidade", subtitle = "Histograma/Densidade") +
  theme_gray() +
  theme(legend.position = "none")
  # theme(legend.position = c(0.75, 0.85),
  #       legend.background = element_rect(fill = alpha("white", 0.8)))

# Q-Q plots
# Cálculo dos quantis teóricos usando as posições de plotagem de Weibull
p <- ppoints(length(QtransfDF$Vazao), a = 0)
# Quantis
teoricoN <- qnorm(p, mean = mean(QtransfDF$Vazao), sd = sd(QtransfDF$Vazao))

# Criação do data frame
pontos <- data.frame(amostra = sort(QtransfDF$Vazao),
                     N = teoricoN)

# Gráfico
p2 <- ggplot(pontos, aes(y = amostra)) +
  geom_point(aes(x = N, color = "Normal")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "firebrick") +
  scale_color_manual(name = "Legenda",
                     values = c("Normal" = "darkred")) +
  labs(subtitle = "Q-Q Plot",
       x = "Quantis teóricos (m³/s)", 
       y = "Quantis amostrais (m³/s)") +
  # coord_equal() +
  theme_gray() +
  theme(legend.position = "none")
  # theme(legend.position = c(0.20, 0.85),
  #       legend.background = element_rect(fill = alpha("white", 0.8)))

# Exbição dos gráficos lado a lado
# tiff('passo1c.tif', height=1200, width = 1920, res=300)
p1 + p2
# dev.off()

# Shapiro-Wilk
shapiro.test(Qtransf)
# Anderson-Darling
ad.test(Qtransf, null = "pnorm", mean = mean(Qtransf),
        sd = sd(Qtransf), estimated = TRUE)
# PPCC
ppccTest(Qtransf, qfn = "qnorm")

# (1d e 1e) Determinar a FAC e FACP amostrais  ----------------------------

# O pacote forecast traz funções prontas para isso
p3 <- ggAcf(Qtransf, lag.max = 36) + ggtitle("FAC")
p4 <- ggPacf(Qtransf, lag.max = 36) + ggtitle("FACP")

# Posiciona um gráfico sobre o outro
# tiff('passos1d1e.tif', height=1200, width = 1800, res=300)
p3 / p4
# dev.off()

# (1f) [Alternativa] Critérios de informação ------------------------------

# Subtraindo a média (ver explicação nos slides da aula)
zt <- Qtransf - mean(Qtransf)

# Obtenção do melhor modelo de forma automática, usando o critério fornecido
# no argumento 'ic' e a ordem máxima 2 (modelo ARMA - sem diferenciação)
ajuste <- auto.arima(
  zt,                # série temporal
  max.p = 2,         # máxima ordem AR
  max.d = 0,         # máxima ordem I
  max.q = 2,         # máxima ordem MA
  seasonal = FALSE,  # modelo não sazonal
  allowmean = FALSE, # não incluir constante (série possui média zero)
  ic = "aicc",       # critério de informação utilizado
  stepwise = FALSE,  # processo interno do otimizador
  approximation = FALSE) # MLE exata

# Exibe os resultados
summary(ajuste)

# IMPORTANTE: a função 'auto.arima' não restringe modelos puramente MA. Como 
# para vazões essa variante não possui sentido físico, ela não é recomendada.
# Assim, caso a função retorne modelos puramente MA, sugere-se considerar o 
# modelo AR(1) diretamente.
ajuste <- Arima(zt, order = c(2, 0, 0), include.mean = FALSE)
# Exibe os resultados
summary(ajuste)




# Seção 2 -----------------------------------------------------------------
# |----------------------------|
# |2. Estimação dos parâmetros |
# |----------------------------|

# (2a) Estimar a média da série histórica ---------------------------------
# Utilizar a série transformada
media <- mean(Qtransf)

# (2b) Determinar a série Zt subtraindo a média da série histórica --------
# Procedimento feito anteriormente no passo opcional (1f)
zt <- Qtransf - media

# (2c) Estimar os parâmetros phi e theta ----------------------------------
# Os parâmetros já são extraídos no processo de identificação
parametros <- round(ajuste$coef, digits = 3)
parametros

# (2d) Calcular a variância da série de resíduos --------------------------
# A variância já é extraída no processo de identificação
var.a <- ajuste$sigma2

# (2e) Condições de estacionariedade e invertibilidade --------------------
# Tanto a função 'auto.arima' como 'Arima' do pacote 'forecast' estimam os 
# parâmetros por meio de um processo de otimização que garante o atendimento
# a ambas as condições. Portanto, se um modelo violá-las, a própria função
# retorna um erro e identifica o problema. Ainda assim, é possível fazer a 
# confirmação de que as raízes do polinômio característico caem fora do 
# círculo unitário
raizes_AR <- Mod(polyroot(c(1, -ajuste$model$phi)))
# raizes_MA <- Mod(polyroot(c(1, -ajuste$model$theta)))
raizes_AR
# raizes_MA


# Seção 3 -----------------------------------------------------------------
# |-----------------------|
# |3. Validação do modelo |
# |-----------------------|

# (3a) A partir de t=p+q, determinar os resíduos do modelo ----------------
# Os resíduos já são extraídos do processo de identificação
at <- ajuste$residuals

# Plotagem para visualização dos resíduos (erros)
# Tibble para ggplot2
atDF <- tibble(Ano  = floor(time(ajuste$residuals)),
               Data = ymd(paste0(Ano, "-01-01")),
               Res  = as.numeric(ajuste$residuals))

# Gráfico
# tiff('passo3a.tif', height=720, width = 1780, res=300)
ggplot(atDF, aes(x = Data, y = Res)) +
  geom_point(color = "darkseagreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_date(date_breaks = "10 years",date_labels = "%Y") +
  labs(x = "Tempo", y = "Resíduos (m³/s)") +
  theme_gray()
# dev.off()


# (3b) Verificar a independência da série de resíduos ---------------------
# Teste de Ljung-Box
Box.test(at, lag = 20, type = "Ljung-Box")


# (3c) Verificar a homocedasticidade da série de resíduos -----------------
# Teste de Breusch-Pagan
bptest(atDF$Res ~ atDF$Ano)

# (3d) Verificar a normalidade da série de resíduos -----------------------
# Histograma (empírico) vs. densidades (teóricas)
p5 <- ggplot(atDF, aes(x = Res)) +
  geom_histogram(aes(y = ..density.., color = "Resíduo"), bins = 8,
                 fill = "darkseagreen", alpha = 0.5) +
  stat_function(aes(color = "Dist. Normal"), fun = dnorm,
                args = list(mean = mean(atDF$Res), sd = sd(atDF$Res)),
                size = 1) +
  scale_color_manual(name = "Legenda", values = c("Resíduo" = "darkseagreen",
                                                  "Dist. Normal" = "darkgreen")) +
  labs(x = "Resíduo (m³/s)", y = "Densidade", subtitle = "Histograma/Densidade") +
  theme_gray() +
  theme(legend.position = "none")

# Q-Q plots
# Cálculo dos quantis teóricos usando as posições de plotagem de Weibull
p <- ppoints(length(atDF$Res), a = 0)
# Quantis
teoricoN <- qnorm(p, mean = mean(atDF$Res), sd = sd(atDF$Res))

# Criação do data frame
pontos <- data.frame(amostra = sort(atDF$Res),
                     N = teoricoN)

# Gráfico
p6 <- ggplot(pontos, aes(y = amostra)) +
  geom_point(aes(x = N, color = "Normal")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkseagreen") +
  scale_color_manual(name = "Legenda",
                     values = c("Normal" = "darkgreen")) +
  labs(subtitle = "Q-Q Plot",
       x = "Quantis teóricos (m³/s)", 
       y = "Quantis amostrais (m³/s)") +
  # coord_equal() +
  theme_gray() +
  theme(legend.position = "none")

# Exbição dos gráficos lado a lado
# tiff('passo3d.tif', height=1200, width = 1920, res=300)
p5 + p6
# dev.off()

# Teste de Shapiro-Wilk
shapiro.test(at)




# Seção 4 -----------------------------------------------------------------
# |--------------------------------|
# |4. Geração de séries sintéticas |
# |--------------------------------|
# Muito embora a aula traga os passos (4a) a (4f), aqui podemos unir todos eles 
# em uma sequência só. O pacote 'forecast' traz a função 'simulate' específica
# para a geração de séries sintéticas

# (4a a 4d) Geração de séries sintéticas --------------------------------

# Para garantir a reprodutibilidade
set.seed(6)

# Parâmetros da geração
nSeries <- 100         # número de séries
tamSeries <- length(zt) # tamanho desejado para as séries
delta     <- 0.1        # aquecimento do modelo (eliminação do viés inicial)
tamDelta  <- ceiling(tamSeries * (1 + delta)) # tamanho total a gerar

# Processo de geração
listaSinteticas <- replicate(
  n = nSeries,
  {
    sinteticas <- simulate(ajuste, nsim = tamDelta) # gera 10% a mais
    sinteticas <- tail(sinteticas, tamSeries)       # remove os primeiros 10% 
  },
  simplify = FALSE)

# Agrupamento das séries geradas em um data frame
sinteticasDF <- as.data.frame(listaSinteticas)
names(sinteticasDF) <- paste0("Serie_", 1:nSeries)


# (4e) Invertendo as transformações (média e log/Box-Cox) -----------------

# Devolvendo a média para as séries
QtransfSint <- sinteticasDF + mean(Qtransf, na.rm = TRUE)

# Invertendo a transformação para normal

if (transf == 1) {
  # Opção via transformação logarítmica
  Qsint <- exp(QtransfSint)
} else if (transf == 2) {
  # Opção via Box-Cox
  Qsint <- InvBoxCox(QtransfSint, lambda)
} else {
  Qsint <- QtransfSint
}

# Extra: gráfico comparativo entre histórico e séries sintéticas ----------
# Rodar apenas para os casos em que o tamanho da série histórica é o mesmo das
# séries sintéticas

# Adicionar rótulo de tempo para as séries sintéticas
QsintDF <- Qsint %>%
  mutate(t = time(Qanual)) %>%  # make sure time aligns
  pivot_longer(cols = starts_with("Serie_"),
               names_to = "SerieSS",
               values_to = "VazaoSS")

# Gráfico
ggplot() +
  # Séries sintéticas (linhas fracas)
  geom_line(data = QsintDF, aes(x = t, y = VazaoSS, group = SerieSS),
            color = "gray50", alpha = 0.5) +
  # Histórico
  geom_line(data = QanualDF, aes(x = Ano, y = Vazao),
            color = "steelblue", size = 1) +
  labs(subtitle = "Séries histórica vs. sintéticas",
       x = "Ano",
       y = "Vazões (m³/s)") +
  theme_gray()

# outra opção: intervalo hachurado representativo
envelopeDF <- Qsint %>%
  mutate(t = time(Qanual)) %>%
  reframe(Min = pmin(!!!syms(names(Qsint))), 
          Max = pmax(!!!syms(names(Qsint))), .by = t)

# Gráfico
# tiff('seriesSinteticasRibbon.tif', height=900, width = 1780, res=300)
ggplot() +
  # Hachura
  geom_ribbon(data = envelopeDF, aes(x = t, ymin = Min, ymax = Max,
                                     fill = "Gerado (envoltória)"),
                                 alpha = 0.3) +
  # Histórico
  geom_line(data = QanualDF, aes(x = Ano, y = Vazao, color = "Observado"),
                             size = 1) +
  labs(subtitle = "Séries histórica vs. sintéticas",
       x = "Ano",
       y = "Vazões (m³/s)",
       color = NULL,
       fill = NULL) +
  scale_color_manual(name = NULL, values = c("Observado" = "steelblue")) +
  scale_fill_manual(name = NULL, values = c("Gerado (envoltória)" = "steelblue")) +
  theme_gray() + 
  theme(legend.position = "bottom", legend.title = element_blank())
# dev.off()




# Verificação das séries sintéticas ---------------------------------------
# ATENÇÃO: CÓDIGO AINDA NÃO FINALIZADO!!

# |-------------------------|
# |5. Quantas séries gerar? |
# |-------------------------|
# Gráfico comparativo das distribuições de probabilidade acumuladas


# Verificação das séries sintéticas geradas -------------------------------

# |----------------------------|
# |6. Estatísticas descritivas |
# |----------------------------|
# Cálculo comparativo das estatísticas descritivas entre observado e gerado

# 6.1. Estatísticas do histórico observado
estObs <- QanualDF |> 
  summarise(
    media     = mean(Vazao, na.rm = TRUE),
    variancia = var(Vazao, na.rm = TRUE),
    desvpad   = sd(Vazao, na.rm = TRUE),
    assim     = skewness(Vazao, na.rm = TRUE),
    min       = min(Vazao, na.rm = TRUE),
    max       = max(Vazao, na.rm = TRUE),
    corrlag1  = cor(Vazao[-1], Vazao[-length(Vazao)]),
    corrlag2  = cor(Vazao[-c(1,2)], Vazao[-c(length(Vazao)-1, length(Vazao))])
  )

# 6.2. Estatísticas das séries sintéticas
estSint <- QsintDF |>
  group_by(SerieSS) |>
  summarise(
    media     = mean(VazaoSS, na.rm = TRUE),
    variancia = var(VazaoSS, na.rm = TRUE),
    desvpad   = sd(VazaoSS, na.rm = TRUE),
    assim     = e1071::skewness(VazaoSS, na.rm = TRUE),
    min       = min(VazaoSS, na.rm = TRUE),
    max       = max(VazaoSS, na.rm = TRUE),
    corrlag1  = acf(VazaoSS, plot = FALSE, lag.max = 2)$acf[2],
    corrlag2  = acf(VazaoSS, plot = FALSE, lag.max = 2)$acf[3]
  ) |>
  ungroup()


# 6.3. Intervalos de confiança
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = nSeries - 1)

# Inclusão dos intervalos
estIC <- estSint |>
  summarise(across(where(is.numeric),
                   list(Media = mean, sd = sd),
                   .names = "{.col}_{.fn}")) |>
  pivot_longer(everything(),
               names_to = c("Metrica", ".value"),
               names_sep = "_") |>
  mutate(
    se = sd / sqrt(nSeries),
    IC_inf = Media - t_crit * se,
    IC_sup = Media + t_crit * se
  )

# 6.4. Comparativo observado vs. sintético
comparativo <- estIC |>
  left_join(
    estObs |> pivot_longer(everything(), names_to = "Metrica", values_to = "Obs."),
    by = "Metrica"
  ) |>
  select(Metrica, Obs., IC_inf, Media, IC_sup)
comparativo

# |-----------------------|
# |7. Análises adicionais |
# |-----------------------|
# Cálculo comparativo das estatísticas descritivas entre observado e gerado

# 7.1. Análise baseada em sequências (runs)

# Função específica para contagem de sequências
runs <- function(x, ref = mean(x, na.rm = TRUE)) {
  # Posição da vazão em relação à referência: 1 acima, 0 abaixo (ou igual)
  acimaOuAbaixo <- as.integer(x > ref)
  
  # Identifica quando a sequência inicia
  iniRun <- c(1, which(diff(acimaOuAbaixo) != 0) + 1)
  
  # Comprimento das sequências (no tempo)
  tempoRun <- diff(c(iniRun, length(x) + 1))
  
  # Comprimento das sequências (vazões)
  vazaoRun <- sapply(seq_along(iniRun), function(i) {
    start <- iniRun[i]
    end <- start + tempoRun[i] - 1
    mean(x[start:end], na.rm = TRUE)
  })
  
  # Número de sequências
  nRuns <- length(tempoRun)
  
  # Valores médios e máximos
  tempoRunMedia <- mean(tempoRun)
  tempoRunMax   <- max(tempoRun)
  vazaoRunMedia <- mean(vazaoRun)
  vazaoRunMax   <- max(vazaoRun)
  
  # Tribble de resumo
  tibble(
    nRuns         = nRuns,
    tempoRunMedia = tempoRunMedia,
    tempoRunMax   = tempoRunMax,
    vazaoRunMedia = vazaoRunMedia,
    vazaoRunMax   = vazaoRunMax
  )
}

# Para séries observadas
runsObs <- runs(QanualDF$Vazao)

# Para séries sintéticas
runsSint <- QsintDF |>
  group_by(SerieSS) |>
  summarise(runs(VazaoSS), .groups = "drop")

# Intervalos de confiança
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = nrow(runsSint) - 1)

runsIC <- runsSint |>
  summarise(across(where(is.numeric),
                   list(Media = mean, sd = sd),
                   .names = "{.col}_{.fn}")) |>
  pivot_longer(
    cols = everything(),
    names_to = c("Metrica", "stat"),
    names_pattern = "(.*)_(.*)"
  ) |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(
    se = sd / sqrt(nrow(runsSint)),
    IC_inf = Media - t_crit * se,
    IC_sup = Media + t_crit * se
  )

# Comparativo observado vs. sintético
comparativoRuns <- runsIC |>
  left_join(
    runsObs |> pivot_longer(everything(), names_to = "Metrica", values_to = "Obs."),
    by = "Metrica"
  ) |>
  select(Metrica, Obs., IC_inf, Media, IC_sup)
comparativoRuns

# 8. Déficits médios e máximo déficit acumulado
# Nível de regularização considerado
delta = 0.8

# Função auxiliar para determinação dos déficits
deficitAcum <- function(x, delta) {
  media <- mean(x, na.rm = TRUE)
  n <- length(x)
  Def <- numeric(n)
  
  for (t in 2:n) {
    Def[t] <- max(0, Def[t - 1] - x[t] + delta * media)
  }
  
  tibble(
    media = mean(Def),
    max = max(Def)
  )
}

# Para séries observadas
defObs <- deficitAcum(QanualDF$Vazao, delta = delta)

# Para séries sintéticas
defSint <- QsintDF |>
  group_by(SerieSS) |>
  reframe(deficitAcum(VazaoSS, delta = delta))

# Intervalos de confiança
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = nrow(defSint) - 1)

defIC <- defSint |>
  summarise(across(where(is.numeric),
                   list(Media = mean, sd = sd),
                   .names = "{.col}_{.fn}")) |>
  pivot_longer(
    cols = everything(),
    names_to = c("Metrica", "stat"),
    names_pattern = "(.*)_(.*)"
  ) |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(
    se = sd / sqrt(nrow(defSint)),
    IC_inf = Media - t_crit * se,
    IC_sup = Media + t_crit * se
  )

# Comparativo observado vs. sintético
comparativoDeficits <- defIC |>
  left_join(
    defObs |> pivot_longer(everything(), names_to = "Metrica", values_to = "Obs."),
    by = "Metrica"
  ) |>
  select(Metrica, Obs., IC_inf, Media, IC_sup)
comparativoDeficits