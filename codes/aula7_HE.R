# UNIVERSIDADE FEDERAL DO PARANÁ
# Programa de Pós-Graduação em Engenharia de Recursos Hídricos e Ambiental
# ERHA7016 – Hidrologia Estocástica
# Autor: Daniel Detzel
# Data: 29 out. 2025
# Aula 7: Geração de Séries Sintéticas (pt. 4

# Bibliotecas utilizadas
library(tidyverse)  # contém diversas bibliotecas, incluindo ggplot2
library(patchwork)  # para ajustes de gráficos lado a lado
library(trend)      # para estimador de Sen
library(forecast)   # para modelos ARIMA
library(dplyr)      # manejo de dados
library(purrr)      # manejo de dados

# Função utilizada
source("cod2serieMensal.R") # extração dos dados de vazão do ONS

# Leitura dos dados (usados em diferentes partes do código)
Qmensal <- read.csv("Vazoes_Mensais_1931_2023.csv",
                    check.names = FALSE)


# Remoção de tendências ---------------------------------------------------

# Série utilizada como exemplo
cod   <- 74  # Foz do Areia
serie <- cod2serieMensal(Qmensal,cod) 

# Conversão para a escala anual 
serieAnual <- serie %>%
  filter(Usina == cod) %>%
  group_by(Ano) %>%
  summarise(Vazao = mean(Vazao, na.rm = TRUE)) %>%
  ungroup()

# |-----------------------------------------------------------|
# |1. Determinação do parâmetro de Sen e remoção da tendência |
# |-----------------------------------------------------------|

# 1.1. Determinação da declividade de Sen (estimador não paramétrico do coef.
# angular)
sen <- sens.slope(serieAnual$Vazao)
s   <- sen$estimates

# 1.2. Remoção da tendência
serieAnual <- serieAnual %>%  
  mutate(VazaoEstac = Vazao - s * row_number())

# 1.3. Gráfico comparativo
p1 <- ggplot(serieAnual, aes(x = Ano)) +
  geom_line(aes(y = Vazao, color = "Original"), linewidth = 0.7) +
  geom_line(aes(y = VazaoEstac, color = "Sem Tendência"), linewidth = 0.7) +
  scale_color_manual(
    name = NULL,
    values = c("Original" = "gray50", "Sem Tendência" = "firebrick")
  ) +
  labs(
    x = "Tempo (anos)",
    y = "Vazão (m³/s)",
    subtitle = paste0("Remoção de tendências - Sen = ", round(s, 2), " m³/s.ano")
  ) +
  theme_gray(base_size = 18) +
  theme(
    legend.position = "right")
# Salva o gráfico em arquivo externo  
ggsave(filename = "remocaoTendencia.png", 
       plot     = p1,
       width    = 26,
       height   = 10,
       units    = "cm",
       dpi      = 300)


# Modelos Sazonais --------------------------------------------------------

# Série utilizada como exemplo
cod   <- 169  # Sobradinho
serie <- cod2serieMensal(Qmensal,cod) 

# |----------------------------------------------|
# |2. Visualização de uma série com sazonalidade |
# |----------------------------------------------|

# 2.1. Separação dos últimos 30 anos, somente para melhorar a visualização
ultimosAnos <- max(serie$Ano) - 24
serieUltimos <- serie %>% filter (Ano >= ultimosAnos)

# 2.2. Plotagem

# Criação de coluna com datas
serieUltimos <- serieUltimos %>%
  mutate(Data = as.Date(paste(Ano, Mes, "01", sep = "-")))

# Gráfico
p2 <- ggplot(serieUltimos, aes(x = Data, y = Vazao)) +
  geom_line(color = "steelblue", linewidth = 0.9) +
  labs(subtitle = "Vazões no reservatório de Sobradinho",
              x = "Ano",
              y = "Vazão (m³/s)") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_gray(base_size = 18) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16))
# Salva o gráfico em arquivo externo  
ggsave(filename = "sazonalidadeExemplo.png", 
       plot     = p2,
       width    = 26,
       height   = 10,
       units    = "cm",
       dpi      = 300)

# |-----------------|
# |3. Modelo SARIMA |
# |-----------------|

# 3.1. FAC e FACP da série mensal em Sobradinho

# O pacote 'forecast' requer que as séries estejam no formato ts (time series)
Qmensal <- ts(serie$Vazao,
              start = c(min(serie$Ano), min(serie$Mes)),
              frequency = 12)

# O pacote forecast traz funções prontas para isso
p3 <- ggAcf(Qmensal, lag.max = 48) + ggtitle("FAC") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
p4 <- ggPacf(Qmensal, lag.max = 48) + ggtitle("FACP") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

# Posiciona um gráfico sobre o outro
p5 <- p3 + p4
# Salva o gráfico
ggsave(filename = "acfPacfSazonal.png", 
       plot     = p5,
       width    = 26,
       height   = 10,
       units    = "cm",
       dpi      = 300)

# 3.2. Ajuste do modelo SARIMA
# Nota: por simplicidade e para priorizar a didática, as verificações prévias
# quanto à remoção de tendências não foram incluídas no código. Pelo mesmo
# motivo foi adotada a transformação logarítmica aos dados diretamente
# Nova variável para manter 'serie' como dado original
QmensalDF <- serie

# Conversão para formato adequado
QmensalTS <- ts(QmensalDF$Vazao,
                start = c(min(QmensalDF$Ano), min(QmensalDF$Mes)),
                frequency = 12)

# Tansformação logarítmica
zt <- log(QmensalTS)
mediaZt <- mean(zt)
zt <- zt - mediaZt

# Procedimento de ajuste automático, usando AICc como critério para identifica-
# ção do melhor modelo
# OBS.: essa função pode demorar um bocado para rodar caso os argumentos 
# 'stepwise' e 'approximation' estejam configurados como FALSE
fit_sarima <- auto.arima(
  zt,
  max.p = 2,         # máxima ordem AR
  max.d = 0,         # máxima ordem I
  max.q = 2,         # máxima ordem MA
  max.P = 1,         # máxima ordem SAR
  max.D = 0,         # máxima ordem SI
  max.Q = 1,         # máxima ordem SMA
  seasonal = TRUE,   # modelo sazonal
  ic = "aicc",       # critério de informação adotado
  allowmean = FALSE,  # sem adicionar nível média
  stepwise = TRUE,   # processo interno do otimizador (busca simplificada)
  approximation = TRUE # MLE exata
)

# Modelo ajustado
summary(fit_sarima)

# |----------------|
# |4. Modelo PARMA |
# |----------------|

# Nota: por simplicidade e para priorizar a didática, as verificações prévias
# quanto à remoção de tendências não foram incluídas no código. Pelo mesmo
# motivo foi adotada a transformação logarítmica aos dados diretamente
# Nova variável para manter 'serie' como dado original
QmensalDF <- serie

# 4.1. FAC e FACP individuais para cada mês

# Extração dos meses de janeiros e julhos, para exemplos
QJan <-  QmensalDF %>% filter(Mes == 1) 
QJul <- QmensalDF %>% filter(Mes == 7) 

# Conversão para o formato adequado
QJanTS <- ts(QJan$Vazao, start = min(QJan$Ano), frequency = 1)
QJulTS <- ts(QJul$Vazao, start = min(QJul$Ano), frequency = 1)

# Janeiros
acfJan  <- ggAcf(QJanTS, lag.max = 12) + ggtitle("ACF - Janeiros") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
pacfJan <- ggPacf(QJanTS, lag.max = 12) + ggtitle("PACF - Janeiros") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

# Julhos
acfJul  <- ggAcf(QJulTS, lag.max = 12) + ggtitle("ACF - Julhos") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
pacfJul <- ggPacf(QJulTS, lag.max = 12) + ggtitle("PACF - Julhos") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

# Combinação dos gráficos em um só layout
p7 <- (acfJan + acfJul) / (pacfJan + pacfJul)
# Salva o gráfico
ggsave(filename = "acfPacfPeriodica.png", 
       plot     = p7,
       width    = 22,
       height   = 13,
       units    = "cm",
       dpi      = 300)

# 4.2. Processo de ajuste
# O pacote 'perARMA' faz o ajuste desse tipo de modelo, mas infelizmente a sua
# estrutura é relativamente complexa para interpretar. Então, a opção mais 
# direta é ajustar diferentes modelos ARMA, um para cada mês

# Conversão dos dados para posterior separação em meses
QmensalDF <- QmensalDF %>%
  mutate(Date = as.Date(paste(Ano, Mes, "01", sep = "-")),
         Mes = as.integer(Mes))

# Separa a série por mês
Qmes <- QmensalDF %>%  group_split(Mes)

# Ajusta automaticamente um ARMA para cada mês
fit_parma <- lapply(Qmes, function(fit_mes) {
  serieMes <- ts(fit_mes$Vazao, frequency = 1)
  auto.arima(serieMes,
             max.p = 2,         # máxima ordem AR
             max.d = 0,         # máxima ordem I
             max.q = 2,         # máxima ordem MA
             seasonal = FALSE,  # modelo não sazonal
             allowmean = FALSE, # não incluir constante (série possui média zero)
             ic = "aicc",       # critério de informação utilizado
             stepwise = FALSE,  # processo interno do otimizador
             approximation = FALSE) # MLE exata
})

# Rotula os ajustes com as iniciais dos meses
names(fit_parma) <- month.abb

# Resumo dos ajustes
resumo <- map_dfr(names(fit_parma), function(m) {
  mod <- fit_parma[[m]]
  tibble(
    Month = m,
    p = mod$arma[1],
    q = mod$arma[2],
    AICc = mod$aicc
  )
})

print(resumo)

# |--------------------------|
# |4. Modelo dessazonalizado |
# |--------------------------|

# Nova variável para manter 'serie' como dado original
QmensalDF <- serie
QmensalDF <- QmensalDF %>%
  mutate(Data = as.Date(paste(Ano, Mes, "01", sep = "-")),
         Mes = as.integer(Mes))

# Cálculo das médias e desvios padrão mensais
mediaMes <- QmensalDF %>%
  group_by(Mes) %>%
  summarise(MediaMes = mean(Vazao, na.rm = TRUE),
            DesvPadMes = sd(Vazao, na.rm = TRUE))

# Dessazonalização
QmensalDF <- QmensalDF %>%
  left_join(mediaMes, by = "Mes") %>%
  mutate(VazaoDeseas = (Vazao - MediaMes) / DesvPadMes)

# Gráfico de visualização
# Original
p8 <- ggplot(QmensalDF, aes(x = Data, y = Vazao)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  labs(subtitle = "Vazões originais",
       x = "Ano",
       y = "Vazão (m³/s)") +
  scale_x_date(date_breaks = "15 years", date_labels = "%Y") +
  theme_gray(base_size = 18) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16))
# Dessazonalizada
p9 <- ggplot(QmensalDF, aes(x = Data, y = VazaoDeseas)) +
  geom_line(color = "firebrick", linewidth = 0.6) +
  labs(subtitle = "Vazões dessazonalizadas",
       x = "Ano",
       y = "Vazão (m³/s)") +
  scale_x_date(date_breaks = "15 years", date_labels = "%Y") +
  theme_gray(base_size = 18) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16))
# Unindo em um único layout
p10 <- p8 + p9
# Salvando a figura
ggsave(filename = "dessaz.png", 
       plot     = p10,
       width    = 30,
       height   = 13,
       units    = "cm",
       dpi      = 300)

# Efeitos na ACF
p11  <- ggAcf(QmensalDF$Vazao, lag.max = 36) + ggtitle("ACF - Original") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
p12  <- ggAcf(QmensalDF$VazaoDeseas, lag.max = 36) + ggtitle("ACF - Dessazonalizada") +
  theme(
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
# Unindo em um único layout
p13 <- p11 + p12
# Salvando a figura
ggsave(filename = "dessazACF.png", 
       plot     = p13,
       width    = 30,
       height   = 13,
       units    = "cm",
       dpi      = 300)

# |-------------------------|
# |5. Método dos fragmentos |
# |-------------------------|
# Nova variável para manter 'serie' como dado original
QmensalDF <- serie

# Aqui apenas o cálculo dos fragmentos para ilustração
fragmentos <- QmensalDF %>%
  group_by(Ano) %>%
  mutate(Fragmento = Vazao / mean(Vazao, na.rm = TRUE)) %>%
  ungroup()

# Gráfico
p14 <- ggplot(fragmentos, aes(x = Mes, y = Fragmento, group = Ano)) +
  geom_line(alpha = 0.25, color = "steelblue") +
  stat_summary(aes(group = 1), fun = mean,geom = "line",
               color = "black", linewidth = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "point",
               color = "black", size = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Mês",y = "Fragmentos",
       subtitle = "Linhas azuis: anos individuais | Linha preta: média mensal") +
  theme_grey(base_size = 18) +
  theme(plot.subtitle = element_text(size = 16))

# Salvando a figura
ggsave(filename = "fragmentos.png", 
       plot     = p14,
       width    = 30,
       height   = 13,
       units    = "cm",
       dpi      = 300)

# Anexo: trabalho em progresso --------------------------------------------

# # 3.3. Geração de séries sintéticas
# 
# # Parâmetros da geração
# nSeries <- 10           # número de séries
# tamSeries <- length(zt) # tamanho desejado para as séries
# delta     <- 60         # aquecimento do modelo (eliminação do viés inicial)
# # importante ser múltiplo de 12 para manter a coerência
# # entre os meses observado e sintéticos
# tamDelta  <- tamSeries + delta # tamanho total a gerar
# 
# # Processo de geração
# listaSinteticas <- replicate(
#   n = nSeries,
#   {
#     sinteticas <- simulate(fit_sarima, nsim = tamDelta) # gera 10% a mais
#     sinteticas <- tail(sinteticas, tamSeries)           # remove os primeiros 10% 
#   },
#   simplify = FALSE)
# 
# # Agrupamento das séries geradas em um data frame
# sinteticasDF <- as.data.frame(listaSinteticas)
# names(sinteticasDF) <- paste0("Serie_", 1:nSeries)
# 
# # Invertendo as transformações
# QtransfSint <- sinteticasDF + mediaZt
# Qsint <- exp(QtransfSint)
# 
# # Adicionando um vetor de data
# datas <- serie %>% select(Ano, Mes)
# Qsint <- Qsint %>%  mutate(Ano = datas$Ano,
#                            Mes = datas$Mes) %>%
#   relocate(Ano, Mes)
# Qsint_long <- Qsint %>%
#   pivot_longer(
#     cols = starts_with("Serie_"),
#     names_to = "Serie",
#     values_to = "VazaoSS"
#   )
# 
# monthly_means <- Qsint_long %>%
#   group_by(Mes) %>%
#   summarise(MeanSim = mean(VazaoSS))
# 
# # 3.4. Gráficos para melhor exibição
# # 3.4.1. Padrão com envolvória
# 
# # Determina a envoltória (min/max)
# envoltoriaDF <- Qsint %>%
#   rowwise() %>%
#   mutate(
#     Min = min(c_across(starts_with("Serie_")), na.rm = TRUE),
#     Max = max(c_across(starts_with("Serie_")), na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   select(Ano, Mes, Min, Max)
# 
# # Restrição dos últimos anos para melhorar a visualização
# ultimosAnos <- max(serie$Ano) - 24
# serieUltimos <- serie %>% filter (Ano >= ultimosAnos)
# 
# # Criação de coluna com datas
# serieUltimos <- serieUltimos %>%
#   mutate(Data = as.Date(paste(Ano, Mes, "01", sep = "-")))
# 
# # Determinação das envoltórias
# envoltoriaUltimos <- envoltoriaDF %>% filter (Ano >= ultimosAnos)
# envoltoriaUltimos <- envoltoriaUltimos %>%
#   mutate(Data = as.Date(paste(Ano, Mes, "01", sep = "-")))
# 
# # Gráfico
# ggplot() +
#   geom_ribbon(data = envoltoriaUltimos, aes(x = Data, ymin = Min, ymax = Max),
#               fill = "steelblue", alpha = 0.3) +
#   geom_line(data = serieUltimos, aes(x = Data, y = Vazao),
#             color = "steelblue", size = 1) +
#   labs(
#     title = "Observed vs. SARIMA Synthetic Series",
#     subtitle = "Envelope of synthetic series",
#     x = "Year",
#     y = "Flow (m³/s)"
#   ) +
#   theme_gray() +
#   theme(
#     axis.title = element_text(size = 14),
#     axis.text = element_text(size = 12),
#     plot.subtitle = element_text(size = 12)
#   )

