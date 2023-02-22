************************************************************/
/**************** Econometría Básica***********************/
/**************Autor: Juan Pablo Cely***************/
/**********************10-08-2020****************************/
/************************************************************/

/******************************************************************************/
/******************************** Estimaciones finales******************/
/******************************************************************************/
import delimited "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/probitt33.csv", encoding(ISO-8859-1)

probit indep parexp ocupados prim crecimiento
estimates store probit1
outreg2 using promco, append excel dec(3) ctitle(probit1) 


probit indep parexp ocupados prim parpibcorr
estimates store probit2
outreg2 using promco, append excel dec(3) ctitle(probit2) 


probit indep parexp ocupados prim pper
estimates store probit3
outreg2 using promco, append excel dec(3) ctitle(probit3) 


estimates table probit1 probit2 probit3, star stat(N r2)



reg theta comp   pper terc crecimiento
estimates store reg1
outreg2 using promco2, append excel dec(3) ctitle(mco1) 


reg theta comp   pper terc ocupados 
estimates store reg2
outreg2 using promco2, append excel dec(3) ctitle(mco2) 


reg theta comp   pper terc parexp
estimates store reg3
outreg2 using promco2, append excel dec(3) ctitle(mco3) 



estimates table reg1 reg2 reg3 probit1 probit2 probit3, star stat(N r2)

vif
imtest, white
sktest residual
qnorm residual
