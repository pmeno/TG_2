ClassificarBigSmall <- function(dados = data.table())
{
    dados <- dados[, S_B := ifelse(valorDeMercado >= median(valorDeMercado), 'B', 'S'), by = year]
    dados
}