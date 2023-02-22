/************************************************************/
/**************** Econometría Básica***********************/
/**************Autor: Juan Pablo Cely***************/
/**********************30-04-2020****************************/
/************************************************************/



*Linea 37: Ejercicio por mes corrido (2010-2019)
*Linea 197: Ejercicio por trimestre corrido (2010-2019)
*Linea 357: Ejercicio por trimestre suma del mismo año (2009-2019)

 
/******************************************************************************/
/******************************** OPCION 1 MESES AÑO CORRIDO******************/
/******************************************************************************/


*ESTOS SERAN LOS DATOS SON LOS QUE SE VAN A TRABAJAR (MESES CORRIDO), tener encuenta la correlacion entre rezagos (1/6)(4/6) y (1/12)(11/13) y los demas para encontrar mas correlaciones
*SEGUIR LOS LINKS DE STATA EN "PAGINAS IMPORTANTES"


use "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/MesesCorridoCol2.dta"
*use "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/Mesesnormales.dta"

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




*Antioquia
 gmm (Phillips:infanti-({b0}+{b1}*l.infanti+{b2}*f.infanti+{b3}*banti)), instruments(l(1/3).banti l(2/4).infanti) wmatrix(hac bartlett opt) variables(banti infanti) vce(hac bartlett opt)
estat overid

*Atlantico
gmm (Phillips:infatla-({b0}+{b1}*l.infatla+{b2}*f.infatla+{b3}*batla)), instruments(l(1/3).batla l(2/4).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid

*Bogota
gmm (Phillips:infbogo-({b0}+{b1}*l.infbogo+{b2}*f.infbogo+{b3}*bbogo)), instruments(l(1/3).bbogo l(2/4).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt)
estat overid

*Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l.infboli+{b2}*f.infboli+{b3}*bboli)), instruments(l(1/3).bboli l(2/4).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid

*Boyaca
gmm (Phillips:infboya-({b0}+{b1}*l.infboya+{b2}*f.infboya+{b3}*bboya)), instruments(l(1/3).bboya l(2/4).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid

*Caldas
gmm (Phillips:infcald-({b0}+{b1}*l.infcald+{b2}*f.infcald+{b3}*bcald)), instruments(l(1/3).bcald l(2/4).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid

*Caqueta
gmm (Phillips:infcaqu-({b0}+{b1}*l.infcaqu+{b2}*f.infcaqu+{b3}*bcaqu)), instruments(l(1/3).bcaqu l(2/4).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid

*Cauca
gmm (Phillips:infcauc-({b0}+{b1}*l.infcauc+{b2}*f.infcauc+{b3}*bcauc)), instruments(l(1/3).bcauc l(2/4).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid

*Cesar
gmm (Phillips:infcesa-({b0}+{b1}*l.infcesa+{b2}*f.infcesa+{b3}*bcesa)), instruments(l(1/3).bcesa l(2/4).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid

*Cordoba
gmm (Phillips:infcord-({b0}+{b1}*l.infcord+{b2}*f.infcord+{b3}*bcord)), instruments(l(1/3).bcord l(2/4).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid

*Cundinamarca
gmm (Phillips:infbogo-({b0}+{b1}*l.infbogo+{b2}*f.infbogo+{b3}*bcund)), instruments(l(1/3).bcund l(2/4).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid

*Choco
gmm (Phillips:infchoc-({b0}+{b1}*l.infchoc+{b2}*f.infchoc+{b3}*bchoc)), instruments(l(1/3).bchoc l(2/4).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid

*Huila
gmm (Phillips:infhuil-({b0}+{b1}*l.infhuil+{b2}*f.infhuil+{b3}*bhuil)), instruments(l(1/3).bhuil l(2/4).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid

*La Guajira
gmm (Phillips:inflagua-({b0}+{b1}*l.inflagua+{b2}*f.inflagua+{b3}*blagua)), instruments(l(1/3).blagua l(2/4).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid

*Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l.infmagd+{b2}*f.infmagd+{b3}*bmagd)), instruments(l(1/3).bmagd l(2/4).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid

*Meta
gmm (Phillips:infmeta-({b0}+{b1}*l.infmeta+{b2}*f.infmeta+{b3}*bmeta)), instruments(l(1/3).bmeta l(2/4).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid

*Nariño
gmm (Phillips:infnari-({b0}+{b1}*l.infnari+{b2}*f.infnari+{b3}*bnari)), instruments(l(1/3).bnari l(2/4).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid

*Norte de santander
gmm (Phillips:infnort-({b0}+{b1}*l.infnort+{b2}*f.infnort+{b3}*bnort)), instruments(l(1/3).bnort l(2/4).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid

*Quindio
gmm (Phillips:infquin-({b0}+{b1}*l.infquin+{b2}*f.infquin+{b3}*bquin)), instruments(l(1/3).bquin l(2/4).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid

*Risaralda
gmm (Phillips:infrisa-({b0}+{b1}*l.infrisa+{b2}*f.infrisa+{b3}*brisa)), instruments(l(1/3).brisa l(2/4).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid

*Santander
gmm (Phillips:infsant-({b0}+{b1}*l.infsant+{b2}*f.infsant+{b3}*bsant)), instruments(l(1/3).bsant l(2/4).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid

*Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l.infsucr+{b2}*f.infsucr+{b3}*bsucr)), instruments(l(1/3).bsucr l(2/4).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid

*Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l.inftoli+{b2}*f.inftoli+{b3}*btoli)), instruments(l(1/3).btoli l(2/4).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid

*Valle del cauca
gmm (Phillips:infvall-({b0}+{b1}*l.infvall+{b2}*f.infvall+{b3}*bvall)), instruments(l(1/3).bvall l(2/4).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid

*Colombia
gmm (Phillips:inftotal-({b0}+{b1}*l.inftotal+{b2}*f.inftotal+{b3}*bcolombia)), instruments(l(1/3).bcolombia l(2/4).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overidd

















clear


/******************************************************************************/
/******************************** OPCION 2 TRIMESTRE AÑO CORRIDO*******************************/
/******************************************************************************/

 use "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/TrimestreCorrido.dta"

// Cambiar el directorio
// Subir Trimestres

*DESCRIPCION VARIABLES

*anticm= mes corrido de los costos marginales de antioquia
*infanti= inflacion de antioquia





*Generar serie de tiempo para 2010
gen n=_n+199
*formato serie de tiempo
format n %tq
*declarar la serie de tiempo
tsset n, quarterly


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
gmm (Phillips:infanti-({b0}+{b1}*l.infanti+{b2}*f.infanti+{b3}*banti)), instruments(l(1/3).banti l(2/4).infanti) wmatrix(hac bartlett opt) variables(banti infanti) vce(hac bartlett opt)
estat overid

*Atlantico
gmm (Phillips:infatla-({b0}+{b1}*l.infatla+{b2}*f.infatla+{b3}*batla)), instruments(l(1/3).batla l(2/4).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid

*Bogota
gmm (Phillips:infbogo-({b0}+{b1}*l.infbogo+{b2}*f.infbogo+{b3}*bbogo)), instruments(l(1/3).bbogo l(2/4).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt)
estat overid

*Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l.infboli+{b2}*f.infboli+{b3}*bboli)), instruments(l(1/3).bboli l(2/4).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid

*Boyaca
gmm (Phillips:infboya-({b0}+{b1}*l.infboya+{b2}*f.infboya+{b3}*bboya)), instruments(l(1/3).bboya l(2/4).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid

*Caldas
gmm (Phillips:infcald-({b0}+{b1}*l.infcald+{b2}*f.infcald+{b3}*bcald)), instruments(l(1/3).bcald l(2/4).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid

*Caqueta
gmm (Phillips:infcaqu-({b0}+{b1}*l.infcaqu+{b2}*f.infcaqu+{b3}*bcaqu)), instruments(l(1/3).bcaqu l(2/4).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid

*Cauca
gmm (Phillips:infcauc-({b0}+{b1}*l.infcauc+{b2}*f.infcauc+{b3}*bcauc)), instruments(l(1/3).bcauc l(2/4).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid

*Cesar
gmm (Phillips:infcesa-({b0}+{b1}*l.infcesa+{b2}*f.infcesa+{b3}*bcesa)), instruments(l(1/3).bcesa l(2/4).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid

*Cordoba
gmm (Phillips:infcord-({b0}+{b1}*l.infcord+{b2}*f.infcord+{b3}*bcord)), instruments(l(1/3).bcord l(2/4).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid

*Cundinamarca
gmm (Phillips:infbogo-({b0}+{b1}*l.infbogo+{b2}*f.infbogo+{b3}*bcund)), instruments(l(1/3).bcund l(2/4).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid

*Choco
gmm (Phillips:infchoc-({b0}+{b1}*l.infchoc+{b2}*f.infchoc+{b3}*bchoc)), instruments(l(1/3).bchoc l(2/4).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid

*Huila
gmm (Phillips:infhuil-({b0}+{b1}*l.infhuil+{b2}*f.infhuil+{b3}*bhuil)), instruments(l(1/3).bhuil l(2/4).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid

*La Guajira
gmm (Phillips:inflagua-({b0}+{b1}*l.inflagua+{b2}*f.inflagua+{b3}*blagua)), instruments(l(1/3).blagua l(2/4).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid

*Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l.infmagd+{b2}*f.infmagd+{b3}*bmagd)), instruments(l(1/3).bmagd l(2/4).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid

*Meta
gmm (Phillips:infmeta-({b0}+{b1}*l.infmeta+{b2}*f.infmeta+{b3}*bmeta)), instruments(l(1/3).bmeta l(2/4).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid

*Nariño
gmm (Phillips:infnari-({b0}+{b1}*l.infnari+{b2}*f.infnari+{b3}*bnari)), instruments(l(1/3).bnari l(2/4).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid

*Norte de santander
gmm (Phillips:infnort-({b0}+{b1}*l.infnort+{b2}*f.infnort+{b3}*bnort)), instruments(l(1/3).bnort l(2/4).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid

*Quindio
gmm (Phillips:infquin-({b0}+{b1}*l.infquin+{b2}*f.infquin+{b3}*bquin)), instruments(l(1/3).bquin l(2/4).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid

*Risaralda
gmm (Phillips:infrisa-({b0}+{b1}*l.infrisa+{b2}*f.infrisa+{b3}*brisa)), instruments(l(1/3).brisa l(2/4).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid

*Santander
gmm (Phillips:infsant-({b0}+{b1}*l.infsant+{b2}*f.infsant+{b3}*bsant)), instruments(l(1/3).bsant l(2/4).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid

*Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l.infsucr+{b2}*f.infsucr+{b3}*bsucr)), instruments(l(1/3).bsucr l(2/4).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid

*Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l.inftoli+{b2}*f.inftoli+{b3}*btoli)), instruments(l(1/3).btoli l(2/4).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid

*Valle del cauca
gmm (Phillips:infvall-({b0}+{b1}*l.infvall+{b2}*f.infvall+{b3}*bvall)), instruments(l(1/3).bvall l(2/4).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid

*Colombia
gmm (Phillips:inftotal-({b0}+{b1}*l.inftotal+{b2}*f.inftotal+{b3}*bcolombia)), instruments(l(1/3).bcolombia l(2/4).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overid


clear


/******************************************************************************/
/******************OPCION 2 TRIMESTRE SUMA EXCLUSIVAMENTE DE LOS MESES*********/
/******************************************************************************/



// Cambiar el directorio
use "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/Actualizacion/Bases de datos/TrimestreInicial.dta"

// Subir Trimestres

*DESCRIPCION VARIABLES

*anticm= mes corrido de los costos marginales de antioquia
*infanti= inflacion de antioquia





*Generar serie de tiempo para 2009
gen n=_n+195
*formato serie de tiempo
format n %tq
*declarar la serie de tiempo
tsset n, quarterly


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
gmm (Phillips:infanti-({b0}+{b1}*l.infanti+{b2}*f.infanti+{b3}*banti)), instruments(l(1/3).banti l(2/4).infanti) wmatrix(hac bartlett opt) variables(banti infanti) vce(hac bartlett opt)
estat overid

*Atlantico
gmm (Phillips:infatla-({b0}+{b1}*l.infatla+{b2}*f.infatla+{b3}*batla)), instruments(l(1/3).batla l(2/4).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid

*Bogota
gmm (Phillips:infbogo-({b0}+{b1}*l.infbogo+{b2}*f.infbogo+{b3}*bbogo)), instruments(l(1/3).bbogo l(2/4).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt)
estat overid

*Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l.infboli+{b2}*f.infboli+{b3}*bboli)), instruments(l(1/3).bboli l(2/4).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid

*Boyaca
gmm (Phillips:infboya-({b0}+{b1}*l.infboya+{b2}*f.infboya+{b3}*bboya)), instruments(l(1/3).bboya l(2/4).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid

*Caldas
gmm (Phillips:infcald-({b0}+{b1}*l.infcald+{b2}*f.infcald+{b3}*bcald)), instruments(l(1/3).bcald l(2/4).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid

*Caqueta
gmm (Phillips:infcaqu-({b0}+{b1}*l.infcaqu+{b2}*f.infcaqu+{b3}*bcaqu)), instruments(l(1/3).bcaqu l(2/4).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid

*Cauca
gmm (Phillips:infcauc-({b0}+{b1}*l.infcauc+{b2}*f.infcauc+{b3}*bcauc)), instruments(l(1/3).bcauc l(2/4).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid

*Cesar
gmm (Phillips:infcesa-({b0}+{b1}*l.infcesa+{b2}*f.infcesa+{b3}*bcesa)), instruments(l(1/3).bcesa l(2/4).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid

*Cordoba
gmm (Phillips:infcord-({b0}+{b1}*l.infcord+{b2}*f.infcord+{b3}*bcord)), instruments(l(1/3).bcord l(2/4).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid

*Cundinamarca
gmm (Phillips:infbogo-({b0}+{b1}*l.infbogo+{b2}*f.infbogo+{b3}*bcund)), instruments(l(1/3).bcund l(2/4).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid

*Choco
gmm (Phillips:infchoc-({b0}+{b1}*l.infchoc+{b2}*f.infchoc+{b3}*bchoc)), instruments(l(1/3).bchoc l(2/4).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid

*Huila
gmm (Phillips:infhuil-({b0}+{b1}*l.infhuil+{b2}*f.infhuil+{b3}*bhuil)), instruments(l(1/3).bhuil l(2/4).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid

*La Guajira
gmm (Phillips:inflagua-({b0}+{b1}*l.inflagua+{b2}*f.inflagua+{b3}*blagua)), instruments(l(1/3).blagua l(2/4).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid

*Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l.infmagd+{b2}*f.infmagd+{b3}*bmagd)), instruments(l(1/3).bmagd l(2/4).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid

*Meta
gmm (Phillips:infmeta-({b0}+{b1}*l.infmeta+{b2}*f.infmeta+{b3}*bmeta)), instruments(l(1/3).bmeta l(2/4).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid

*Nariño
gmm (Phillips:infnari-({b0}+{b1}*l.infnari+{b2}*f.infnari+{b3}*bnari)), instruments(l(1/3).bnari l(2/4).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid

*Norte de santander
gmm (Phillips:infnort-({b0}+{b1}*l.infnort+{b2}*f.infnort+{b3}*bnort)), instruments(l(1/3).bnort l(2/4).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid

*Quindio
gmm (Phillips:infquin-({b0}+{b1}*l.infquin+{b2}*f.infquin+{b3}*bquin)), instruments(l(1/3).bquin l(2/4).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid

*Risaralda
gmm (Phillips:infrisa-({b0}+{b1}*l.infrisa+{b2}*f.infrisa+{b3}*brisa)), instruments(l(1/3).brisa l(2/4).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid

*Santander
gmm (Phillips:infsant-({b0}+{b1}*l.infsant+{b2}*f.infsant+{b3}*bsant)), instruments(l(1/3).bsant l(2/4).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid

*Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l.infsucr+{b2}*f.infsucr+{b3}*bsucr)), instruments(l(1/3).bsucr l(2/4).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid

*Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l.inftoli+{b2}*f.inftoli+{b3}*btoli)), instruments(l(1/3).btoli l(2/4).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid

*Valle del cauca
gmm (Phillips:infvall-({b0}+{b1}*l.infvall+{b2}*f.infvall+{b3}*bvall)), instruments(l(1/3).bvall l(2/4).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid

*Colombia
gmm (Phillips:inftotal-({b0}+{b1}*l.inftotal+{b2}*f.inftotal+{b3}*bcolombia)), instruments(l(1/3).bcolombia l(2/4).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overid


clear













