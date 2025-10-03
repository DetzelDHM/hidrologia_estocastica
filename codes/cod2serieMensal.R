# Função para a extração facilitada de dados de uma determinada usina, bastando
# que seja inserido o código ONS. Versão para escala mensal
cod2serieMensal <- function(df,cod){
  # Nome dos meses para truca futura
  meses <- c("Jan","Fev","Mar","Abr","Mai","Jun",
             "Jul","Ago","Set","Out","Nov","Dez")
  
  df %>%
    # Extrai somente a série do código fornecido
    filter(Usina == as.character(cod)) %>%
    # Altera o formato da série de matriz para vetor (mais fácil de operar)
    pivot_longer(cols = all_of(meses),
                 names_to = "Mes",
                 values_to = "Vazao") %>%
    # Converte as abreviações textuais dos meses para números
    mutate(Mes = match(Mes, meses)) %>%
    # Assegura que os anos são números inteiros
    mutate(Ano = as.integer(Ano)) %>%
    # Organiza o formato final
    arrange(Ano, Mes)
}