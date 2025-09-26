# Função para a extração facilitada de dados de uma determinada usina, bastando
# que seja inserido o código ONS
cod2serie <- function(df, cod) {
  
  # Conversão do nome da estação para caracter
  cod <- as.character(cod)
  
  # Verifica a existência da estação
  if (!cod %in% colnames(df)) {
    stop(paste("Não há série com código", cod, "na base de dados."))
  }
  
  # Cria o data.frame com duas colunas: data e série
  serie <- data.frame(Datas = df$Datas,Série = df[[cod]])
  
  # Renomeia a coluna com o código da estação
  colnames(serie)[2] <- "Vazao"
  
  # Remove os valores vazios para séries que iniciam depois de 1931
  # serie <- serie[!is.na(serie[[cod]]), ]
  serie <- serie[!is.na(serie$Vazao), ]
  
  return(serie)
}