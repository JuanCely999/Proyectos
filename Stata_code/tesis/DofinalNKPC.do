/************************************************************/
/********DINÁICA DE LOS PRECIOS EN LOS DEPARTAMENTOS EN COLOMBIA:*/
/* UNA ESTIMACIÓN USANDO LA CURVA DE PHILLIPS NEOKEYNESIANA***/
/****************(2010-2019)*****************************/
/*******************************************************/


/******************************************************************************/
/******************************** CAPITULO 3 *************************************/
/******************************************************************************/

*******************************************
****3.1. NKPC por departamentos******
***********Residuos************************
***********Do:Residuos************************

clear

use "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/MesesCorridoCol2.dta" // PC Personal Recuerde cambiar el directorio


*2010
gen n=_n+599 
format n %tm
tsset n, monthly


tsfilter hp banti= anticm, trend(l1)
tsfilter hp batla= atlacm, trend(l2)
tsfilter hp bbogo= bogocm, trend(l3)
tsfilter hp bboli= bolicm, trend(l4)
tsfilter hp bboya= boyacm, trend(l5)
tsfilter hp bcald= caldcm, trend(l6)
tsfilter hp bcaqu= caqucm, trend(l7)
tsfilter hp bcauc= cauccm, trend(l8)
tsfilter hp bcesa= cesacm, trend(l9)
tsfilter hp bcord= cordcm, trend(l10)
tsfilter hp bcund= cundcm, trend(l11)
tsfilter hp bchoc= choccm, trend(l12)
tsfilter hp bhuil= huilcm, trend(l13)
tsfilter hp blagua= laguacm, trend(l14)
tsfilter hp bmagd= magdcm, trend(l15)
tsfilter hp bmeta= metacm, trend(l16)
tsfilter hp bnari= naricm, trend(l17)
tsfilter hp bnort= nortcm, trend(l18)
tsfilter hp bquin= quincm, trend(l19)
tsfilter hp brisa= risacm, trend(l20)
tsfilter hp bsant= santcm, trend(l21)
tsfilter hp bsucr= sucrcm, trend(l22)
tsfilter hp btoli= tolicm, trend(l23)
tsfilter hp bvall= vallcm, trend(l24)
tsfilter hp bcolombia= colombiacm, trend(l25)

*Antioquia
gmm (Phillips:infanti-({b0}+{b1}*l3.infanti+{b2}*f3.infanti+{b3}*banti)), instruments(l(2/7).banti l(1/6).infanti) wmatrix(hac bartlett opt) variables(banti infanti) vce(hac bartlett opt)
estat overid
estimates store ant2
predict ant

*Atlantico
gmm (Phillips:infatla-({b0}+{b1}*l3.infatla+{b2}*f3.infatla+{b3}*batla)), instruments(l(2/7).batla l(1/6).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid 
estimates store atla2
predict atla

*Bogota
gmm (Phillips:infbogo-({b0}+{b1}*l3.infbogo+{b2}*f3.infbogo+{b3}*bbogo)), instruments(l(2/7).bbogo l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt)
estat overid 
estimates store bogo2
predict bogo

*Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l3.infboli+{b2}*f3.infboli+{b3}*bboli)), instruments(l(2/7).bboli l(1/6).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid 
estimates store boli2
predict boli

*Boyaca
gmm (Phillips:infboya-({b0}+{b1}*l3.infboya+{b2}*f3.infboya+{b3}*bboya)), instruments(l(2/7).bboya l(1/6).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid 
estimates store boya2
predict boya

*Caldas
gmm (Phillips:infcald-({b0}+{b1}*l3.infcald+{b2}*f3.infcald+{b3}*bcald)), instruments(l(2/7).bcald l(1/6).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid 
estimates store cald2
predict cald

*Caqueta
gmm (Phillips:infcaqu-({b0}+{b1}*l3.infcaqu+{b2}*f3.infcaqu+{b3}*bcaqu)), instruments(l(2/7).bcaqu l(1/6).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid 
estimates store caqu2
predict caqu

*Cauca
gmm (Phillips:infcauc-({b0}+{b1}*l3.infcauc+{b2}*f3.infcauc+{b3}*bcauc)), instruments(l(2/7).bcauc l(1/6).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid 
estimates store cauc2
predict cauc

*Cesar
gmm (Phillips:infcesa-({b0}+{b1}*l3.infcesa+{b2}*f3.infcesa+{b3}*bcesa)), instruments(l(2/7).bcesa l(1/6).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid 
estimates store cesa2
predict cesa

*Cordoba
gmm (Phillips:infcord-({b0}+{b1}*l3.infcord+{b2}*f3.infcord+{b3}*bcord)), instruments(l(2/7).bcord l(1/6).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid 
estimates store cord2
predict cord

*Cundinamarca
gmm (Phillips:infbogo-({b0}+{b1}*l3.infbogo+{b2}*f3.infbogo+{b3}*bcund)), instruments(l(2/7).bcund l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid 
estimates store cund2
predict cund

*Choco
gmm (Phillips:infchoc-({b0}+{b1}*l3.infchoc+{b2}*f3.infchoc+{b3}*bchoc)), instruments(l(2/7).bchoc l(1/6).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid 
estimates store choc2
predict choc

*Huila
gmm (Phillips:infhuil-({b0}+{b1}*l3.infhuil+{b2}*f3.infhuil+{b3}*bhuil)), instruments(l(2/7).bhuil l(1/6).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid 
estimates store huil2
predict huil

*La Guajira
gmm (Phillips:inflagua-({b0}+{b1}*l3.inflagua+{b2}*f3.inflagua+{b3}*blagua)), instruments(l(2/7).blagua l(1/6).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid 
estimates store lagua2
predict lagua

*Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l3.infmagd+{b2}*f3.infmagd+{b3}*bmagd)), instruments(l(2/7).bmagd l(1/6).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid 
estimates store magd2
predict magd

*Meta
gmm (Phillips:infmeta-({b0}+{b1}*l3.infmeta+{b2}*f3.infmeta+{b3}*bmeta)), instruments(l(2/7).bmeta l(1/6).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid 
estimates store meta2
predict meta

*Nariño
gmm (Phillips:infnari-({b0}+{b1}*l3.infnari+{b2}*f3.infnari+{b3}*bnari)), instruments(l(2/7).bnari l(1/6).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid 
estimates store nari2
predict nari

*Norte de santander
gmm (Phillips:infnort-({b0}+{b1}*l3.infnort+{b2}*f3.infnort+{b3}*bnort)), instruments(l(2/7).bnort l(1/6).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid 
estimates store nort2
predict nort

*Quindio
gmm (Phillips:infquin-({b0}+{b1}*l3.infquin+{b2}*f3.infquin+{b3}*bquin)), instruments(l(2/7).bquin l(1/6).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid 
estimates store quin2
predict quin

*Risaralda
gmm (Phillips:infrisa-({b0}+{b1}*l3.infrisa+{b2}*f3.infrisa+{b3}*brisa)), instruments(l(2/7).brisa l(1/6).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid 
estimates store risa2
predict risa

*Santander
gmm (Phillips:infsant-({b0}+{b1}*l3.infsant+{b2}*f3.infsant+{b3}*bsant)), instruments(l(2/7).bsant l(1/6).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid 
estimates store sant2
predict sant

*Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l3.infsucr+{b2}*f3.infsucr+{b3}*bsucr)), instruments(l(2/7).bsucr l(1/6).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid 
estimates store sucr2
predict sucr

*Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l3.inftoli+{b2}*f3.inftoli+{b3}*btoli)), instruments(l(2/7).btoli l(1/6).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid 
estimates store toli2
predict toli

*Valle del cauca
gmm (Phillips:infvall-({b0}+{b1}*l3.infvall+{b2}*f3.infvall+{b3}*bvall)), instruments(l(2/7).bvall l(1/6).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid 
estimates store vall2
predict vall

*Colombia
gmm (Phillips:inftotal-({b0}+{b1}*l3.inftotal+{b2}*f3.inftotal+{b3}*bcolombia)), instruments(l(2/7).bcolombia l(1/6).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overid 
estimates store colo2
predict colo



/*************Correlación de los residuos**************************/
pwcorr ant atla bogo boli boya cald caqu cauc cesa cord cund choc huil lagua magd meta nari nort quin risa sant sucr toli vall

/********Recopilación de las estimaciones**************************/
estimates table ant2 atla2 bogo2 boli2 boya2 cald2 caqu2 cauc2 cesa2 cord2 cund2 choc2 huil2 lagua2 magd2 meta2 nari2 nort2 quin2 risa2 sant2 sucr2 toli2 vall2 colo2, star





*******************************************
****Estimación NKPC panel dinámico********
*******************************************
clear

import excel "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/panelmeses.xlsx", sheet("Hoja1") firstrow  // PC Personal Recuerde cambiar el directorio

egen dep=group(depar)

list depar dep in 1/120, sepby(depar)
*tsset dep tiempo, monthly

gen t= tiempo+599 
format t %tm
tsset dep t, monthly


eststo: xtabond2 infla l3.infla f3.infla l18.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep

eststo: xtabond2 infla l6.infla f6.infla l18.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep

esttab,label se stats(N j ar1 ar1p ar2 ar2p hansen hansenp sargan sarganp, labels("Observarsions" "No. of instruments" "AR1" "AR1 (p-value)" "AR2" "AR2 (p-value)" "Hansen" "Hansen-J (p-value)" "Sargan" "Sargan(p-value)"))
eststo clear











*******************************************
****3.2. Rigidez de precios y variables*** 
************por departamentos**************
***********Do:Probit MCO************************
*Probit
clear
import delimited "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/probitt33.csv", encoding(ISO-8859-1) // PC Personal Recuerde cambiar el directorio

probit indep parexp ocupados prim crecimiento
estimates store probit1

probit indep parexp ocupados prim parpibcorr
estimates store probit2

probit indep parexp ocupados prim pper
estimates store probit3

estimates table probit1 probit2 probit3, star stat(N r2)


*MCO
reg theta comp   pper terc crecimiento
estimates store reg1

reg theta comp   pper terc ocupados 
estimates store reg2


reg theta comp   pper terc parexp
estimates store reg3

estimates table reg1 reg2 reg3, star stat(N r2)

vif
imtest, white
predict residual
sktest residual
qnorm residual





/******************************************************************************/
/******************************** Apendice Tablas *****************************/
/******************************************************************************/


*******************************************
****NKPC por departamentos(Alternativa)********
***********Do:Residuos************************

clear

use "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/MesesCorridoCol2.dta" // PC Personal Recuerde cambiar el directorio


*2010
gen n=_n+599 
format n %tm
tsset n, monthly


tsfilter hp banti= anticm, trend(l1)
tsfilter hp batla= atlacm, trend(l2)
tsfilter hp bbogo= bogocm, trend(l3)
tsfilter hp bboli= bolicm, trend(l4)
tsfilter hp bboya= boyacm, trend(l5)
tsfilter hp bcald= caldcm, trend(l6)
tsfilter hp bcaqu= caqucm, trend(l7)
tsfilter hp bcauc= cauccm, trend(l8)
tsfilter hp bcesa= cesacm, trend(l9)
tsfilter hp bcord= cordcm, trend(l10)
tsfilter hp bcund= cundcm, trend(l11)
tsfilter hp bchoc= choccm, trend(l12)
tsfilter hp bhuil= huilcm, trend(l13)
tsfilter hp blagua= laguacm, trend(l14)
tsfilter hp bmagd= magdcm, trend(l15)
tsfilter hp bmeta= metacm, trend(l16)
tsfilter hp bnari= naricm, trend(l17)
tsfilter hp bnort= nortcm, trend(l18)
tsfilter hp bquin= quincm, trend(l19)
tsfilter hp brisa= risacm, trend(l20)
tsfilter hp bsant= santcm, trend(l21)
tsfilter hp bsucr= sucrcm, trend(l22)
tsfilter hp btoli= tolicm, trend(l23)
tsfilter hp bvall= vallcm, trend(l24)
tsfilter hp bcolombia= colombiacm, trend(l25)

*Antioquia
gmm (Phillips:infanti-({b0}+{b1}*l6.infanti+{b2}*f6.infanti+{b3}*banti)), instruments(l(2/7).banti l(1/6).infanti) wmatrix(hac bartlett opt) variables(banti infanti) vce(hac bartlett opt)
estat overid
estimates store ant2

*Atlantico
gmm (Phillips:infatla-({b0}+{b1}*l6.infatla+{b2}*f6.infatla+{b3}*batla)), instruments(l(2/7).batla l(1/6).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid 
estimates store atla2

*Bogota
gmm (Phillips:infbogo-({b0}+{b1}*l6.infbogo+{b2}*f6.infbogo+{b3}*bbogo)), instruments(l(2/7).bbogo l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt)
estat overid 
estimates store bogo2

*Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l12.infboli+{b2}*f12.infboli+{b3}*bboli)), instruments(l(2/7).bboli l(1/6).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid 
estimates store boli2

*Boyaca
gmm (Phillips:infboya-({b0}+{b1}*l6.infboya+{b2}*f6.infboya+{b3}*bboya)), instruments(l(2/7).bboya l(1/6).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid 
estimates store boya2

*Caldas
gmm (Phillips:infcald-({b0}+{b1}*l6.infcald+{b2}*f6.infcald+{b3}*bcald)), instruments(l(2/7).bcald l(1/6).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid 
estimates store cald2

*Caqueta
gmm (Phillips:infcaqu-({b0}+{b1}*l6.infcaqu+{b2}*f6.infcaqu+{b3}*bcaqu)), instruments(l(2/7).bcaqu l(1/6).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid 
estimates store caqu2

*Cauca
gmm (Phillips:infcauc-({b0}+{b1}*l6.infcauc+{b2}*f6.infcauc+{b3}*bcauc)), instruments(l(2/7).bcauc l(1/6).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid 
estimates store cauc2

*Cesar
gmm (Phillips:infcesa-({b0}+{b1}*l6.infcesa+{b2}*f6.infcesa+{b3}*bcesa)), instruments(l(2/7).bcesa l(1/6).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid 
estimates store cesa2

*Cordoba
gmm (Phillips:infcord-({b0}+{b1}*l6.infcord+{b2}*f6.infcord+{b3}*bcord)), instruments(l(2/7).bcord l(1/6).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid 
estimates store cord2

*Cundinamarca
gmm (Phillips:infbogo-({b0}+{b1}*l6.infbogo+{b2}*f6.infbogo+{b3}*bcund)), instruments(l(2/7).bcund l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid 
estimates store cund2

*Choco
gmm (Phillips:infchoc-({b0}+{b1}*l6.infchoc+{b2}*f6.infchoc+{b3}*bchoc)), instruments(l(2/7).bchoc l(1/6).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid 
estimates store choc2

*Huila
gmm (Phillips:infhuil-({b0}+{b1}*l6.infhuil+{b2}*f6.infhuil+{b3}*bhuil)), instruments(l(2/7).bhuil l(1/6).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid 
estimates store huil2

*La Guajira
gmm (Phillips:inflagua-({b0}+{b1}*l6.inflagua+{b2}*f6.inflagua+{b3}*blagua)), instruments(l(2/7).blagua l(1/6).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid 
estimates store lagua2

*Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l3.infmagd+{b2}*f3.infmagd+{b3}*bmagd)), instruments(l(8/13).bmagd l(6/12).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid 
estimates store magd2

*Meta
gmm (Phillips:infmeta-({b0}+{b1}*l12.infmeta+{b2}*f12.infmeta+{b3}*bmeta)), instruments(l(2/7).bmeta l(1/6).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid 
estimates store meta2

*Nariño
gmm (Phillips:infnari-({b0}+{b1}*l.infnari+{b2}*f.infnari+{b3}*bnari)), instruments(l(8/13).bnari l(6/12).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid 
estimates store nari2

*Norte de santander
gmm (Phillips:infnort-({b0}+{b1}*l6.infnort+{b2}*f6.infnort+{b3}*bnort)), instruments(l(2/7).bnort l(1/6).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid 
estimates store nort2

*Quindio
gmm (Phillips:infquin-({b0}+{b1}*l6.infquin+{b2}*f6.infquin+{b3}*bquin)), instruments(l(2/7).bquin l(1/6).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid 
estimates store quin2

*Risaralda
gmm (Phillips:infrisa-({b0}+{b1}*l6.infrisa+{b2}*f6.infrisa+{b3}*brisa)), instruments(l(2/7).brisa l(1/6).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid 
estimates store risa2

*Santander
gmm (Phillips:infsant-({b0}+{b1}*l6.infsant+{b2}*f6.infsant+{b3}*bsant)), instruments(l(2/7).bsant l(1/6).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid 
estimates store sant2

*Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l.infsucr+{b2}*f.infsucr+{b3}*bsucr)), instruments(l(8/13).bsucr l(6/12).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid 
estimates store sucr2

*Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l6.inftoli+{b2}*f6.inftoli+{b3}*btoli)), instruments(l(8/13).btoli l(6/12).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid 
estimates store toli2

*Valle del cauca
gmm (Phillips:infvall-({b0}+{b1}*l6.infvall+{b2}*f6.infvall+{b3}*bvall)), instruments(l(2/7).bvall l(1/6).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid 
estimates store vall2

*Colombia
gmm (Phillips:inftotal-({b0}+{b1}*l6.inftotal+{b2}*f6.inftotal+{b3}*bcolombia)), instruments(l(2/7).bcolombia l(1/6).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overid 
estimates store colo2


/********Recopilación de las estimaciones**************************/
  estimates table ant2 atla2 bogo2 boli2 boya2 cald2 caqu2 cauc2 cesa2 cord2 cund2 choc2 huil2 lagua2 magd2 meta2 nari2 nort2 quin2 risa2 sant2 sucr2 toli2 vall2 colo2, star

  
  
  
  
  
  
*******************************************
****Estimación NKPC panel dinámico (Alternativa)********
*******************************************
clear
eststo clear

import excel "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/panelmeses.xlsx", sheet("Hoja1") firstrow // PC Personal Recuerde cambiar el directorio

egen dep=group(depar)

list depar dep in 1/120, sepby(depar)
*tsset dep tiempo, monthly

gen t= tiempo+599 
format t %tm
tsset dep t, monthly

eststo: xtabond2 infla l3.infla f3.infla cmar, gmm(infla,collapse) iv(l12.infla)  noleveleq nodiffsargan robust twostep

eststo: xtabond2 infla l3.infla f3.infla l3.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep

eststo: xtabond2 infla l3.infla f3.infla l6.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep

eststo: xtabond2 infla l3.infla f3.infla l12.cmar, gmm(infla,collapse) iv(l12.infla) noleveleq nodiffsargan robust twostep

esttab,label se stats(N j ar1 ar1p ar2 ar2p hansen hansenp sargan sarganp, labels("Observarsions" "No. of instruments" "AR1" "AR1 (p-value)" "AR2" "AR2 (p-value)" "Hansen" "Hansen-J (p-value)" "Sargan" "Sargan(p-value)"))


