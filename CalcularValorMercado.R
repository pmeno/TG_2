CalcularValorMercado <- function(precos = data.table(), qtdAcoes = data.table())
{
    
  precoTotal <- dcast.data.table(precos, CD_CVM + dataPreco ~ tickerTipo, value.var = 'preco', fun.aggregate = sum)
  precoTotal[is.na(ON), ON := 0]
  precoTotal[is.na(PN), PN := 0]
  
  qtdAcoesFinal <- dcast.data.table(qtdAcoes, CD_CVM ~ stock.type, value.var = 'qtd.issued', fun.aggregate = sum)
  valorMercado <- merge(precoTotal, qtdAcoesFinal, by = 'CD_CVM')
  
  setnames(valorMercado, colnames(valorMercado), c('CD_CVM', 'dataPreco', 'preco_ON', 'preco_PN','exluir', 'qtdON', 'qtdPN'))
  
  valorMercado[, valorDeMercado := preco_ON*qtdON + preco_PN*qtdPN]
  
}