
clear

import excel "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/panelmeses.xlsx", sheet("Hoja1") firstrow clear

egen dep=group(depar)

list depar dep in 1/120, sepby(depar)
*tsset dep tiempo, monthly

gen t= tiempo+599 
format t %tm
tsset dep t, monthly

*Prueba Raiz Unitaria. https://www.stata.com/features/overview/panel-data-unit-root-tests/
*Un paso Para las ultimas 2 regresiones IV funciona de manera similar (la tabla principal)
eststo: xtabond2 infla l3.infla f3.infla cmar, gmm(infla,collapse) iv(l12.infla)  noleveleq nodiffsargan robust twostep
outreg2 using panel2, append excel dec(3) ctitle(a) 


eststo: xtabond2 infla l3.infla f3.infla l3.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep
outreg2 using panel2, append excel dec(3) ctitle(b) 


eststo: xtabond2 infla l3.infla f3.infla l6.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep
outreg2 using panel2, append excel dec(3) ctitle(d) 


eststo: xtabond2 infla l3.infla f3.infla l12.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep
outreg2 using panel2, append excel dec(3) ctitle(e) 

eststo: xtabond2 infla l3.infla f3.infla l18.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep
outreg2 using panel2, append excel dec(3) ctitle(f) 

eststo: xtabond2 infla l6.infla f6.infla l18.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep
outreg2 using panel2, append excel dec(3) ctitle(g) 

esttab,label se stats(N j ar1 ar1p ar2 ar2p hansen hansenp sargan sarganp, labels("Observarsions" "No. of instruments" "AR1" "AR1 (p-value)" "AR2" "AR2 (p-value)" "Hansen" "Hansen-J (p-value)" "Sargan" "Sargan(p-value)"))



xtabond2 infla l.(l3.infla f3.infla cmar), gmm(l3.infla f3.infla cmar, collapse eq(diff)) robust twostep

*eststo: xtabond2 infla l3.infla f3.infla l18.cmar, gmm(infla,collapse) iv(cmar) robust twostep
*xtabond2 infla l.infla f.infla cmar, gmm(l.infla, lag(1 1)) gmm(l.infla f.infla, collapse eq(diff)) robust twostep
*, gmm(var1 var2 var3 var4 var5
*var6 var7, collapse eq(diff))
*xtabond2 infla l3.infla f3.infla l.18cmar, gmm(infla) gmm(l.infla f.infla,collapse) iv(cmar)  robust twostep
*xtabond2 Vardep l.Vardep var1 var2 var3 var4 var6 var7, gmm(l.vardep, lag(1 1))
*gmm(var1 var2, collapse) iv(var3 var4 var5 var6 var7) robust twostep
*eststo: xtabond2 infla l6.infla f6.infla l18.cmar, gmm(infla,collapse)  noleveleq nodiffsargan robust twostep
*outreg2 using panel, append excel dec(3) ctitle(e) 
*esttab,label se stats(N j ar1p ar2p hansen hansenp, labels("Observarsions" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen" "Hansen-J (p-value)"))
*outreg2 using panel, append excel dec(3) ctitle(e) 
*esttab,label se stats(N j ar1p ar2p hansen sargan sarganp wald, labels("Observarsions" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen" "Sargan" "Sargan(p-value)" "Wald"))
*outreg2 using panel, append excel dec(3) ctitle(e) 
eststo clear
outreg2 clear



estimates store a

xtabond2 infla l.infla f.infla l24.cmar, gmm(infla,collapse)  noleveleq nodiffsargan robust 
estimates store b

*Dos pasos
xtabond2 infla l.infla f.infla l19.cmar, gmm(infla,collapse) noleveleq nodiffsargan robust  twostep
estimates store c

xtabond2 infla l.infla f.infla l24.cmar, gmm(infla,collapse) noleveleq nodiffsargan robust twostep
estimates store d



estimates table a b c d, star




****TENER EN CUENTA PARA SU EXTRACCIÓN
eststo result_1
esttab result_1 ,label se stats(N j ar1p ar2p hansenp, labels("Observarsions" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)"))



xtabond2 infla l6.infla f6.infla l18.cmar, gmm(infla,collapse) gmm (l6.infla f6.infla l18.cmar) noleveleq nodiffsargan robust twostep













****************************************************************
****************************************************************
****************************************************************





*****Primer intento*
 xtabond2 infla l.infla f.infla cmar m*, gmm(infla) iv(infla l.infla f.infla cmar m*) noleveleq

******Seggundo intento
xtabond2 cmar infla l3.infla f3.infla  m*, gmm(infla,collapse) iv(l3.infla l3.cmar m*) noleveleq nodiffsargan robust small

******Tercer intento
xtabond2 cmar infla l12.infla f12.infla  m*, gmm(infla,collapse) iv(l12.infla l12.cmar m*) noleveleq nodiffsargan robust small

*******+Cuarto intento
xtabond2 infla l12.infla f12.infla cmar m*, gmm(infla,collapse) iv(l12.infla l12.cmar m*) noleveleq nodiffsargan robust small

********Quinto intento los demas con el video
xtabond2 infla l6.infla f6.infla cmar, gmm(infla,collapse) iv(l.infla l.cmar m*) noleveleq nodiffsargan robust small

********Sexto intento*Libro p41  Mejores intentos                                                                                                                                     
gmm (infla-({b1}*l.infla+{b2}*f.infla+{b3}*cmar)) xtinstruments(1: infla, lags(2/4)) instruments(l(2/4).cmar l(2/7).infla, noconstant) onestep  winitial(xt D)

********Septimo intento* Libro dos etapas    Mejores intentos                                                                                                                                  
gmm (infla-({b1}*l.infla+{b2}*f.infla+{b3}*cmar)) xtinstruments(1: infla, lags(2/4)) instruments(l(2/4).cmar l(2/4).infla, noconstant) winitial(xt D) wmatrix(robust)
estat overid
estat overid, difference
estimates store model6
estat mmsc model6

********Octavo intento- Mejores intentos
xtabond2 infla f.infla l24.cmar, gmm(infla,collapse) iv(l3.infla l3.cmar) noleveleq nodiffsargan robust small  



*Opciones planteadas al director
xtabond infla f.infla l24.cmar, lags(1) maxldep(5) maxlags(5) artests(2) 
estat sargan
estimates store c
xtabond infla f.infla l12.cmar, lags(1) maxldep(5) maxlags(5) artests(2) vce(robust)
estimates store a
estat abond
test l.infla f.infla l12.cmar
*En dos pasos
xtabond infla f.infla l.cmar, lags(1) maxldep(5) maxlags(5) artests(2) twostep
estat sargan
estimates store d
xtabond infla f.infla l12.cmar, lags(1) maxldep(5) maxlags(5) artests(2)  twostep vce(robust)
estimates store d
estat abond
test l.infla f.infla l12.cmar
hausman c d

 

estat summarize //summary statistics for the estimation sample
estat vce //variance–covariance matrix of the estimators (VCE)
estimates //cataloging estimation results
forecast //dynamic forecasts and simulations
lincom //point estimates, standard errors, testing, and inference for linear combinations of coefficients
margins //marginal means, predictive margins, marginal effects, and average marginal effects
marginsplot //graph the results from margins (profile plots, interaction plots, etc.)
nlcom //point estimates, standard errors, testing, and inference for nonlinear combinations of coefficients
predict //predictions, residuals, influence statistics, and other diagnostic measures
predictnl //point estimates, standard errors, testing, and inference for generalized predictions
test //Wald tests of simple and composite linear hypotheses
testnl //Wald tests of nonlinear hypotheses










//     NO TENER EN CUENTA
*Modelo de criterio
webuse abdata
/// Base de datos utilizada por Arellano

xtdpdgmm L(0/1).n w k, noserial gmmiv(L.n, collapse model(difference)) iv(w k, difference model(difference)) twostep vce(robust)




xtdpdgmm infla l.infla f.infla cmar, gmm(infla, lag(1/2))  teffects two vce(r) overid
estimates store xlags
estat mmsc xlags

xtdpdgmm infla l.infla f.infla cmar, noserial gmmiv(infla, model(difference)collapse) iv(l.infla cmar) vce(robust)
xtdpdgmm L(0/1).infla cmar, model(diff) collapse gmm(infla, lag(2 4)) gmm(cmar, lag(1 3)) nl(noserial) teffects igmm vce(r)

quietly xtdpdgmm L(0/1).infla cmar, model(diff) gmm(infla, lag(2/4) collapse) gmm(cmar, lag(1/3) collapse) nl(noserial, collapse) teffects igmm vce(r)
estat overid



