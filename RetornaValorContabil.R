RetornaValorContabil <- function(tickersCVM = data.table(), datas, tipoBalanco = 'con')
{
  
  bpp <- get_dfp_data(tickersCVM,first_year = year(datas), last_year = year(datas), type_docs = 'BPP', type_format = tipoBalanco)
  bpp <- bpp[[1]]
  setDT(bpp)
  bpp[, DS_CONTA := iconv(DS_CONTA, from = 'UTF-8', to = 'ASCII//TRANSLIT')]
  patrimonioLiquido <- bpp[DS_CONTA %chin% 'Patrimonio Liquido Consolidado' | DS_CONTA %chin% 'Patrimonio Liquido']
  patrimonioLiquido <- patrimonioLiquido[, .(CD_CVM, DT_REFER, ESCALA_MOEDA, DS_CONTA, VL_CONTA)]
  patrimonioLiquido
}