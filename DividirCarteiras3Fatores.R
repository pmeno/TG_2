DividirCarteiras3Fatores <- function(dados = data.table())
{
    carteira_1 <- dados[carteira == 'S/L']
    carteira_2 <- dados[carteira == 'S/M']
    carteira_3 <- dados[carteira == 'S/H']
    carteira_4 <- dados[carteira == 'B/L']
    carteira_5 <- dados[carteira == 'B/M']
    carteira_6 <- dados[carteira == 'B/H']
    
    result <- list(carteira_S_L = carteira_1, carteira_S_M = carteira_2, carteira_S_H = carteira_3,
                   carteira_B_L = carteira_4, carteira_B_M = carteira_5, carteira_B_H = carteira_6)

}