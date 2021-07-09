CalcularRetornoMensal <- function(dados = data.table(), ticker)
{
  serieHist    <- BatchGetSymbols(paste0(ticker, '.SA'), first.date = '2010-01-01', last.date = '2020-01-01', thresh.bad.data = 0.2, do.cache = T, do.complete.data = T, do.fill.missing.prices = T)
  serieHist    <- setDT(serieHist$df.tickers)
  datasMaximas <- serieHist[!is.na(price.adjusted), max(ref.date), by = .(ticker, month(ref.date), year(ref.date))]
  datasMinimas <- serieHist[!is.na(price.adjusted), min(ref.date), by = .(ticker, month(ref.date), year(ref.date))]
  datasRetorno <- rbind(datasMinimas, datasMaximas)
  setnames(datasRetorno, 'V1', 'ref.date')
  retorno <- merge(datasRetorno, serieHist[, .(ticker, price.adjusted, ref.date)], by = c('ticker', 'ref.date'))
  retorno[, retMensal := Delt(price.adjusted, k = 1), by = .(ticker, month, year)]
  retorno[, ticker := str_remove(ticker, '.SA')]
  retorno
    
}


GetPricesComDatasMaxMin <- function(ticker, dataIni, dataFim)
{
  serieHist    <- BatchGetSymbols(paste0(ticker, '.SA'), first.date = dataIni, last.date = dataFim, thresh.bad.data = 0.2, do.cache = T, do.complete.data = F, do.fill.missing.prices = F)
  serieHist    <- setDT(serieHist$df.tickers)
  datasMaximas <- serieHist[!is.na(price.adjusted), max(ref.date), by = .(ticker)]
  datasMinimas <- serieHist[!is.na(price.adjusted), min(ref.date), by = .(ticker)]
  datasRetorno <- rbind(datasMinimas, datasMaximas)
  setnames(datasRetorno, 'V1', 'ref.date')
  retorno <- merge(datasRetorno, serieHist[, .(ticker, price.adjusted, ref.date)], by = c('ticker', 'ref.date'))
  retorno[, retPeriodo := Delt(price.adjusted, k = 1), by = .(ticker)]
  retorno[, ticker := str_remove(ticker, '.SA')]
  retorno
  
}
