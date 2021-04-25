#### Libs utilizadas ######

require(data.table)
require(BatchGetSymbols)
require(GetDFPData2)
require(GetFREData)

### CAMINHOS NECESSÁRIOS #####

caminhoPrincipal <- "C:/Users/pedro/Desktop/TCC/PARTE_2/codes/TG_2/"
caminhoTickers   <- paste0(caminhoPrincipal, "tickersListadas.csv")

### CARREGAR TODAS AS EMPRESAS DA BOLSA DE 2010 ATÉ HOJE ########

empresasBolsa <- get_info_companies()
tickersBolsa  <- read.csv(caminhoTickers, sep = ';', encoding = 'UTF-8', header = T, na.strings = "")

setDT(empresasBolsa)
setDT(tickersBolsa)

empresasBolsa <- empresasBolsa[, .(CD_CVM, DENOM_SOCIAL, DENOM_COMERC, SETOR_ATIV, CNPJ, DT_REG, DT_CANCEL, MOTIVO_CANCEL, SIT_REG, CATEG_REG, SIT_EMISSOR, TP_MERC)]

#Formatando datas
empresasBolsa[, DT_REG := as.Date(DT_REG, '%d/%m/%Y')]
empresasBolsa[, DT_CANCEL := as.Date(DT_CANCEL, '%d/%m/%Y')]

#Filtrando apenas empresas negociadas em bolsa

empresasBolsa <- empresasBolsa[is.na(DT_CANCEL) & CATEG_REG == 'Categoria A']






