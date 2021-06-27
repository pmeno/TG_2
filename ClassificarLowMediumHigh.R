ClassificarLowMediumHigh <- function(dados = data.table())
{
    dados[VC_VM < quantile(VC_VM, 0.3), L_M_H := 'L', by = year]
    dados[VC_VM > quantile(VC_VM, 0.7), L_M_H := 'H', by = year]
    dados[(VC_VM >= quantile(VC_VM, 0.3) & VC_VM <= quantile(VC_VM, 0.7)), L_M_H := 'M', by = year]
    dados
    
}