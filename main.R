#### Libs utilizadas ######

require(data.table)
require(BatchGetSymbols)
require(GetDFPData2)
require(GetFREData)
require(bizdays)

### CAMINHOS NECESSÁRIOS #####

caminhoPrincipal <- "C:/Users/pedro/Desktop/TCC/PARTE_2/codes/TG_2/"
caminhoBases     <- "C:/Users/pedro/Desktop/TCC/PARTE_2/codes/TG_2/bases/"
caminhoTickers   <- paste0(caminhoBases, "tickerBolsa.csv")
caminhoQtdAcoes  <- paste0(caminhoBases, "baseQtdAcoes.csv")

### Sources #####

source(paste0(caminhoPrincipal, 'ChecarPrecoAcao.R'))


### CARREGAR TODAS AS EMPRESAS DA BOLSA DE 2010 ATÉ HOJE ########

empresasBolsa <- get_info_companies()
tickersBolsa  <- read.csv(caminhoTickers, sep = ';', encoding = 'UTF-8', header = T, na.strings = "", stringsAsFactors = FALSE, colClasses = c('character','character','character','character','character'))
acoesQtdBolsa <- read.csv(caminhoQtdAcoes, sep = ';', encoding = 'UTF-8', header = T, na.strings = "NA", stringsAsFactors = FALSE)

setDT(empresasBolsa)
setDT(tickersBolsa)
setDT(acoesQtdBolsa)

empresasBolsa <- empresasBolsa[, .(CD_CVM, DENOM_SOCIAL, DENOM_COMERC, SETOR_ATIV, CNPJ, DT_REG, DT_CANCEL, MOTIVO_CANCEL, SIT_REG, CATEG_REG, SIT_EMISSOR, TP_MERC)]

#Formatando colunas para as classes necessarias

empresasBolsa[, DT_REG := as.Date(DT_REG, '%d/%m/%Y')]
empresasBolsa[, DT_CANCEL := as.Date(DT_CANCEL, '%d/%m/%Y')]

acoesQtdBolsa[, 'DT_REFER'] <- acoesQtdBolsa[, as.Date(DT_REFER, origin = "1899-12-30")]

# Filtrando apenas empresas que queremos calcular os parâmetros.

empresasBolsa <- empresasBolsa[is.na(DT_CANCEL) & (SIT_REG == 'ATIVO') & (CATEG_REG == 'Categoria A') & (is.na(TP_MERC) | TP_MERC != 'BALCÃO NÃO ORGANIZADO')]
empresasBolsa <- empresasBolsa[SIT_EMISSOR %chin% c("FASE OPERACIONAL", "FASE PRÉ-OPERACIONAL")]
empresasBolsa <- merge(empresasBolsa, tickersBolsa, by = c('CNPJ', 'DENOM_SOCIAL'), all.x = TRUE)


valorMercado <- dcast(acoesQtdBolsa, CNPJ_CIA + DT_REFER + CD_CVM ~ stock.type, value.var = 'qtd.issued', fun.aggregate = sum)
valorMercado[, 'NA' := NULL]

valorMercado <- merge(valorMercado, empresasBolsa[, .(DENOM_SOCIAL, CD_CVM, DT_REG, Ticker_ON, Ticker_PN, Ticker_UN)], by = 'CD_CVM', all.y = T)

priceFinal <- data.table()

for(i in 1:length(b[, dataRef])){
  
  price <- ChecarPrecoAcao(b[, Ticker_ON_YAHOO][i], b[, dataRef][i])
  priceFinal <- rbind(priceFinal, price)
  
}
