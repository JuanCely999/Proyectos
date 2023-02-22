************************************************************/
/**************** Econometría Básica***********************/
/**************Autor: Juan Pablo Cely***************/
/**********************30-04-2020****************************/
/************************************************************/

/******************************************************************************/
/******************************** OPCION 1 MESES AÑO CORRIDO******************/
/******************************************************************************/

use "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/MesesCorridoCol2.dta"
*use "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/Mesesnormales.dta"


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
predict ant

*Atlantico
gmm (Phillips:infatla-({b0}+{b1}*l3.infatla+{b2}*f3.infatla+{b3}*batla)), instruments(l(2/7).batla l(1/6).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
predict atla

*Bogota
gmm (Phillips:infbogo-({b0}+{b1}*l3.infbogo+{b2}*f3.infbogo+{b3}*bbogo)), instruments(l(2/7).bbogo l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt)
predict bogo

*Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l3.infboli+{b2}*f3.infboli+{b3}*bboli)), instruments(l(2/7).bboli l(1/6).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
predict boli

*Boyaca
gmm (Phillips:infboya-({b0}+{b1}*l3.infboya+{b2}*f3.infboya+{b3}*bboya)), instruments(l(2/7).bboya l(1/6).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
predict boya

*Caldas
gmm (Phillips:infcald-({b0}+{b1}*l3.infcald+{b2}*f3.infcald+{b3}*bcald)), instruments(l(2/7).bcald l(1/6).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
predict cald

*Caqueta
gmm (Phillips:infcaqu-({b0}+{b1}*l3.infcaqu+{b2}*f3.infcaqu+{b3}*bcaqu)), instruments(l(2/7).bcaqu l(1/6).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
predict caqu

*Cauca
gmm (Phillips:infcauc-({b0}+{b1}*l3.infcauc+{b2}*f3.infcauc+{b3}*bcauc)), instruments(l(2/7).bcauc l(1/6).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
predict cauc

*Cesar
gmm (Phillips:infcesa-({b0}+{b1}*l3.infcesa+{b2}*f3.infcesa+{b3}*bcesa)), instruments(l(2/7).bcesa l(1/6).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
predict cesa

*Cordoba
gmm (Phillips:infcord-({b0}+{b1}*l3.infcord+{b2}*f3.infcord+{b3}*bcord)), instruments(l(2/7).bcord l(1/6).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
predict cord

*Cundinamarca
gmm (Phillips:infbogo-({b0}+{b1}*l3.infbogo+{b2}*f3.infbogo+{b3}*bcund)), instruments(l(2/7).bcund l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
predict cund

*Choco
gmm (Phillips:infchoc-({b0}+{b1}*l3.infchoc+{b2}*f3.infchoc+{b3}*bchoc)), instruments(l(2/7).bchoc l(1/6).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
predict choc

*Huila
gmm (Phillips:infhuil-({b0}+{b1}*l3.infhuil+{b2}*f3.infhuil+{b3}*bhuil)), instruments(l(2/7).bhuil l(1/6).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
predict huil

*La Guajira
gmm (Phillips:inflagua-({b0}+{b1}*l3.inflagua+{b2}*f3.inflagua+{b3}*blagua)), instruments(l(2/7).blagua l(1/6).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
predict lagua

*Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l3.infmagd+{b2}*f3.infmagd+{b3}*bmagd)), instruments(l(2/7).bmagd l(1/6).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
predict magd

*Meta
gmm (Phillips:infmeta-({b0}+{b1}*l3.infmeta+{b2}*f3.infmeta+{b3}*bmeta)), instruments(l(2/7).bmeta l(1/6).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
predict meta

*Nariño
gmm (Phillips:infnari-({b0}+{b1}*l3.infnari+{b2}*f3.infnari+{b3}*bnari)), instruments(l(2/7).bnari l(1/6).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
predict nari

*Norte de santander
gmm (Phillips:infnort-({b0}+{b1}*l3.infnort+{b2}*f3.infnort+{b3}*bnort)), instruments(l(2/7).bnort l(1/6).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
predict nort

*Quindio
gmm (Phillips:infquin-({b0}+{b1}*l3.infquin+{b2}*f3.infquin+{b3}*bquin)), instruments(l(2/7).bquin l(1/6).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
predict quin

*Risaralda
gmm (Phillips:infrisa-({b0}+{b1}*l3.infrisa+{b2}*f3.infrisa+{b3}*brisa)), instruments(l(2/7).brisa l(1/6).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
predict risa

*Santander
gmm (Phillips:infsant-({b0}+{b1}*l3.infsant+{b2}*f3.infsant+{b3}*bsant)), instruments(l(2/7).bsant l(1/6).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
predict sant

*Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l3.infsucr+{b2}*f3.infsucr+{b3}*bsucr)), instruments(l(2/7).bsucr l(1/6).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
predict sucr

*Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l3.inftoli+{b2}*f3.inftoli+{b3}*btoli)), instruments(l(2/7).btoli l(1/6).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
predict toli

*Valle del cauca
gmm (Phillips:infvall-({b0}+{b1}*l3.infvall+{b2}*f3.infvall+{b3}*bvall)), instruments(l(2/7).bvall l(1/6).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
predict vall

*Colombia
gmm (Phillips:inftotal-({b0}+{b1}*l3.inftotal+{b2}*f3.inftotal+{b3}*bcolombia)), instruments(l(2/7).bcolombia l(1/6).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
predict colo

pwcorr ant atla bogo boli boya cald caqu cauc cesa cord cund choc huil lagua magd meta nari nort quin risa sant sucr toli vall
outreg2 using corre, append excel dec(3) ctitle(probit3) 





