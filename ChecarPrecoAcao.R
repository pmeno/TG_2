ChecarPrecoAcao <- function(dados = data.table(), dataMercado = "2010-01-01", tipoAcao)
{
  
  tickers <- dados[ , unique(get(paste0('Ticker_', tipoAcao, '_YAHOO')))]
  precos  <- BatchGetSymbols(tickers, first.date = dataMercado, last.date = offset(dataMercado, 1, 'Brazil/ANBIMA'), do.fill.missing.prices = F, thresh.bad.data = 0)
  
  precos <- precos$df.tickers
  setDT(precos)
  
  #precos[, .N, by = data.ref][order(N)]
  
  precos <- precos[ref.date == dataMercado, .(price.adjusted, ref.date, ticker)]
  setnames(precos, colnames(precos), c('preco', 'dataPreco', paste0('Ticker_', tipoAcao, '_YAHOO')))
  precos
  
}