/************************************************************/
/**************** Econometría Básica***********************/
/**************Autor: Juan Pablo Cely***************/
/**********************30-04-2020****************************/
/************************************************************/


/******************************************************************************/
/******************************** OPCION 1 MESES AÑO CORRIDO******************/
/******************************************************************************/

use "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/MesesCorridoCol2.dta"

// Cambiar el directorio
// Subir Trimestres

*DESCRIPCION VARIABLES

*anticm= mes corrido de los costos marginales de antioquia
*infanti= inflacion de antioquia



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

*vce(hac bartlett opt)
*Antioquia
gmm (Phillips:infanti-({b0}+{b1}*l24.infanti+{b2}*f24.infanti+{b3}*banti)), instruments(l(14/19).banti l(12/18).infanti) wmatrix(hac bartlett opt) variables(banti infanti) vce(hac bartlett opt)
estat overid
estimates store anti

*Atlantico
gmm (Phillips:infatla-({b0}+{b1}*l24.infatla+{b2}*f24.infatla+{b3}*batla)), instruments(l(14/19).batla l(12/18).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid
estimates store atla

*Bogota
gmm (Phillips:infbogo-({b0}+{b1}*l24.infbogo+{b2}*f24.infbogo+{b3}*bbogo)), instruments(l(14/19).bbogo l(12/18).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt) 
estat overid
estimates store bogo

*Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l24.infboli+{b2}*f24.infboli+{b3}*bboli)), instruments(l(14/19).bboli l(12/18).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid
estimates store boli

*Boyaca
gmm (Phillips:infboya-({b0}+{b1}*l24.infboya+{b2}*f24.infboya+{b3}*bboya)), instruments(l(14/19).bboya l(12/18).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid
estimates store boya

*Caldas
gmm (Phillips:infcald-({b0}+{b1}*l24.infcald+{b2}*f24.infcald+{b3}*bcald)), instruments(l(14/19).bcald l(12/18).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid
estimates store cald

*Caqueta
gmm (Phillips:infcaqu-({b0}+{b1}*l24.infcaqu+{b2}*f24.infcaqu+{b3}*bcaqu)), instruments(l(14/19).bcaqu l(12/18).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid
estimates store caqu

*Cauca
gmm (Phillips:infcauc-({b0}+{b1}*l24.infcauc+{b2}*f24.infcauc+{b3}*bcauc)), instruments(l(14/19).bcauc l(12/18).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid
estimates store cauc

*Cesar
gmm (Phillips:infcesa-({b0}+{b1}*l24.infcesa+{b2}*f24.infcesa+{b3}*bcesa)), instruments(l(14/19).bcesa l(12/18).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid
estimates store cesa

*Cordoba
gmm (Phillips:infcord-({b0}+{b1}*l24.infcord+{b2}*f24.infcord+{b3}*bcord)), instruments(l(14/19).bcord l(12/18).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid
estimates store cord

*Cundinamarca
gmm (Phillips:infbogo-({b0}+{b1}*l24.infbogo+{b2}*f24.infbogo+{b3}*bcund)), instruments(l(14/19).bcund l(12/18).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid
estimates store cund

*Choco
gmm (Phillips:infchoc-({b0}+{b1}*l24.infchoc+{b2}*f24.infchoc+{b3}*bchoc)), instruments(l(14/19).bchoc l(12/18).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid
estimates store choc

*Huila
gmm (Phillips:infhuil-({b0}+{b1}*l24.infhuil+{b2}*f24.infhuil+{b3}*bhuil)), instruments(l(14/19).bhuil l(12/18).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid
estimates store huil

*La Guajira
gmm (Phillips:inflagua-({b0}+{b1}*l24.inflagua+{b2}*f24.inflagua+{b3}*blagua)), instruments(l(14/19).blagua l(12/18).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid
estimates store lagua

*Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l24.infmagd+{b2}*f24.infmagd+{b3}*bmagd)), instruments(l(14/19).bmagd l(12/18).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid
estimates store magd

*Meta
gmm (Phillips:infmeta-({b0}+{b1}*l24.infmeta+{b2}*f24.infmeta+{b3}*bmeta)), instruments(l(14/19).bmeta l(12/18).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid
estimates store meta

*Nariño
gmm (Phillips:infnari-({b0}+{b1}*l24.infnari+{b2}*f24.infnari+{b3}*bnari)), instruments(l(14/19).bnari l(12/18).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid
estimates store nari

*Norte de santander
gmm (Phillips:infnort-({b0}+{b1}*l24.infnort+{b2}*f24.infnort+{b3}*bnort)), instruments(l(14/19).bnort l(12/18).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid
estimates store nort

*Quindio
gmm (Phillips:infquin-({b0}+{b1}*l24.infquin+{b2}*f24.infquin+{b3}*bquin)), instruments(l(14/19).bquin l(12/18).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid
estimates store quin

*Risaralda
gmm (Phillips:infrisa-({b0}+{b1}*l24.infrisa+{b2}*f24.infrisa+{b3}*brisa)), instruments(l(14/19).brisa l(12/18).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid
estimates store risa

*Santander
gmm (Phillips:infsant-({b0}+{b1}*l24.infsant+{b2}*f24.infsant+{b3}*bsant)), instruments(l(14/19).bsant l(12/18).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid
estimates store sant

*Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l24.infsucr+{b2}*f24.infsucr+{b3}*bsucr)), instruments(l(14/19).bsucr l(12/18).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid
estimates store sucr

*Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l24.inftoli+{b2}*f24.inftoli+{b3}*btoli)), instruments(l(14/19).btoli l(12/18).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid
estimates store toli

*Valle del cauca
gmm (Phillips:infvall-({b0}+{b1}*l24.infvall+{b2}*f24.infvall+{b3}*bvall)), instruments(l(14/19).bvall l(12/18).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid
estimates store vall

*Colombia
gmm (Phillips:inftotal-({b0}+{b1}*l24.inftotal+{b2}*f24.inftotal+{b3}*bcolombia)), instruments(l(14/19).bcolombia l(12/18).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overid
estimates store colombia

estimates table anti atla bogo boli boya cald caqu cauc cesa cord cund choc huil lagua magd meta nari nort quin risa sant sucr toli vall colombia, star
