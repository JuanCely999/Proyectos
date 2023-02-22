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

*https://www.statalist.org/forums/forum/general-stata-discussion/general/1509271-adding-postestimation-and-p-values-as-scalars-with-esttab
*vce(hac bartlett opt)
*Codigos para reciclar relacionados a compilación en grafica
*quietly
*quietly estadd matrix Shea=r(J_p)
*esttab, se scalars(J_p)
*eststo: 
*esttab, se
*esttab using ejemplo.xlsx, se
*esttab using ejemplo.tex, se
*replace

*Antioquia
gmm (Phillips:infanti-({b0}+{b1}*l3.infanti+{b2}*f3.infanti+{b3}*banti)), instruments(l(2/7).banti l(1/6).infanti) wmatrix(hac bartlett opt) variables(banti infanti) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(anti) addtext(Hansen, 8.156)  addstat(Hansep, 0.518) 

gmm (Phillips:infanti-({b0}+{b1}*l6.infanti+{b2}*f6.infanti+{b3}*banti)), instruments(l(2/7).banti l(1/6).infanti) wmatrix(hac bartlett opt) variables(banti infanti) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3)ctitle(anti2) addtext(Hansen,  6.026)  addstat(Hansep, 0.737) 

//-----------------------------------------------------------------------------
  *Atlantico
gmm (Phillips:infatla-({b0}+{b1}*l3.infatla+{b2}*f3.infatla+{b3}*batla)), instruments(l(2/7).batla l(1/6).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(atla) addtext(Hansen, 5.071)  addstat(Hansep, 0.828) 


gmm (Phillips:infatla-({b0}+{b1}*l6.infatla+{b2}*f6.infatla+{b3}*batla)), instruments(l(2/7).batla l(1/6).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(atla2) addtext(Hansen, 4.818)  addstat(Hansep, 0.849) 
//-----------------------------------------------------------------------------
  
  *Bogota
gmm (Phillips:infbogo-({b0}+{b1}*l3.infbogo+{b2}*f3.infbogo+{b3}*bbogo)), instruments(l(2/7).bbogo l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt) 
estat overid
outreg2 using regress22, append excel dec(3) ctitle(bogo) addtext(Hansen, 7.320)  addstat(Hansep, 0.603) 

gmm (Phillips:infbogo-({b0}+{b1}*l6.infbogo+{b2}*f6.infbogo+{b3}*bbogo)), instruments(l(2/7).bbogo l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt) 
estat overid
outreg2 using regress22, append excel dec(3) ctitle(bogo2) addtext(Hansen, 6.863)  addstat(Hansep, 0.651) 
//-----------------------------------------------------------------------------
  
  *Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l12.infboli+{b2}*f12.infboli+{b3}*bboli)), instruments(l(2/7).bboli l(1/6).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(boli) addtext(Hansen, 4.891)  addstat(Hansep, 0.843) 

gmm (Phillips:infboli-({b0}+{b1}*l18.infboli+{b2}*f18.infboli+{b3}*bboli)), instruments(l(2/7).bboli l(1/6).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(boli2) addtext(Hansen, 5.298)  addstat(Hansep, 0.807) 

//-----------------------------------------------------------------------------
  
  *Boyaca
gmm (Phillips:infboya-({b0}+{b1}*l3.infboya+{b2}*f3.infboya+{b3}*bboya)), instruments(l(2/7).bboya l(1/6).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(boya) addtext(Hansen, 4.349)  addstat(Hansep, 0.886) 

gmm (Phillips:infboya-({b0}+{b1}*l6.infboya+{b2}*f6.infboya+{b3}*bboya)), instruments(l(2/7).bboya l(1/6).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(boya2) addtext(Hansen,  4.032)  addstat(Hansep, 0.909) 
//-----------------------------------------------------------------------------
  
  
  *Caldas
gmm (Phillips:infcald-({b0}+{b1}*l3.infcald+{b2}*f3.infcald+{b3}*bcald)), instruments(l(2/7).bcald l(1/6).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cald) addtext(Hansen, 7.9204)  addstat(Hansep, 0.542) 

gmm (Phillips:infcald-({b0}+{b1}*l6.infcald+{b2}*f6.infcald+{b3}*bcald)), instruments(l(2/7).bcald l(1/6).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cald2) addtext(Hansen,  3.604)  addstat(Hansep, 0.9354) 
//-----------------------------------------------------------------------------
  
  *Caqueta
gmm (Phillips:infcaqu-({b0}+{b1}*l3.infcaqu+{b2}*f3.infcaqu+{b3}*bcaqu)), instruments(l(2/7).bcaqu l(1/6).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(caqu) addtext(Hansen, 4.2663)  addstat(Hansep, 0.893) 

gmm (Phillips:infcaqu-({b0}+{b1}*l6.infcaqu+{b2}*f6.infcaqu+{b3}*bcaqu)), instruments(l(2/7).bcaqu l(1/6).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(caqu2) addtext(Hansen, 3.993)  addstat(Hansep, 0.911) 
//-----------------------------------------------------------------------------
  
  *Cauca
gmm (Phillips:infcauc-({b0}+{b1}*l3.infcauc+{b2}*f3.infcauc+{b3}*bcauc)), instruments(l(2/7).bcauc l(1/6).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cauc) addtext(Hansen, 5.998)  addstat(Hansep, 0.740) 

gmm (Phillips:infcauc-({b0}+{b1}*l6.infcauc+{b2}*f6.infcauc+{b3}*bcauc)), instruments(l(2/7).bcauc l(1/6).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cauc2) addtext(Hansen, 3.392)  addstat(Hansep, 0.946) 


//-----------------------------------------------------------------------------
  *Cesar
gmm (Phillips:infcesa-({b0}+{b1}*l3.infcesa+{b2}*f3.infcesa+{b3}*bcesa)), instruments(l(2/7).bcesa l(1/6).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cesa) addtext(Hansen, 15.304)  addstat(Hansep, 0.082) 

gmm (Phillips:infcesa-({b0}+{b1}*l6.infcesa+{b2}*f6.infcesa+{b3}*bcesa)), instruments(l(2/7).bcesa l(1/6).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cesa2) addtext(Hansen, 7.239 )  addstat(Hansep, 0.612) 


//-----------------------------------------------------------------------------
  *Cordoba
gmm (Phillips:infcord-({b0}+{b1}*l3.infcord+{b2}*f3.infcord+{b3}*bcord)), instruments(l(2/7).bcord l(1/6).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cord) addtext(Hansen, 9.606)  addstat(Hansep, 0.383) 

gmm (Phillips:infcord-({b0}+{b1}*l6.infcord+{b2}*f6.infcord+{b3}*bcord)), instruments(l(2/7).bcord l(1/6).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cord2) addtext(Hansen, 9.036)  addstat(Hansep, 0.433) 

//-----------------------------------------------------------------------------
  *Cundinamarca
gmm (Phillips:infbogo-({b0}+{b1}*l3.infbogo+{b2}*f3.infbogo+{b3}*bcund)), instruments(l(2/7).bcund l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cund) addtext(Hansen, 4.311)  addstat(Hansep, 0.889) 

gmm (Phillips:infbogo-({b0}+{b1}*l6.infbogo+{b2}*f6.infbogo+{b3}*bcund)), instruments(l(2/7).bcund l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(cund2) addtext(Hansen, 3.752)  addstat(Hansep, 0.927) 

//-----------------------------------------------------------------------------
  *Choco
gmm (Phillips:infchoc-({b0}+{b1}*l3.infchoc+{b2}*f3.infchoc+{b3}*bchoc)), instruments(l(2/7).bchoc l(1/6).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(choc) addtext(Hansen, 8.3287)  addstat(Hansep, 0.501) 

gmm (Phillips:infchoc-({b0}+{b1}*l6.infchoc+{b2}*f6.infchoc+{b3}*bchoc)), instruments(l(2/7).bchoc l(1/6).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(choc2) addtext(Hansen, 3.475)  addstat(Hansep, 0.942) 

//-----------------------------------------------------------------------------
  *Huila
gmm (Phillips:infhuil-({b0}+{b1}*l3.infhuil+{b2}*f3.infhuil+{b3}*bhuil)), instruments(l(2/7).bhuil l(1/6).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(huil) addtext(Hansen, 4.202)  addstat(Hansep, 0.897) 

gmm (Phillips:infhuil-({b0}+{b1}*l6.infhuil+{b2}*f6.infhuil+{b3}*bhuil)), instruments(l(2/7).bhuil l(1/6).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(huil2) addtext(Hansen, 4.635)  addstat(Hansep, 0.864) 

//-----------------------------------------------------------------------------
  *La Guajira
gmm (Phillips:inflagua-({b0}+{b1}*l.inflagua+{b2}*f.inflagua+{b3}*blagua)), instruments(l(2/7).blagua l(1/6).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(lagua) addtext(Hansen, 4.619)  addstat(Hansep, 0.866) 

gmm (Phillips:inflagua-({b0}+{b1}*l3.inflagua+{b2}*f3.inflagua+{b3}*blagua)), instruments(l(2/7).blagua l(1/6).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(lagua2) addtext(Hansen, 4.524)  addstat(Hansep, 0.873) 

//-----------------------------------------------------------------------------
  *Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l3.infmagd+{b2}*f3.infmagd+{b3}*bmagd)), instruments(l(8/13).bmagd l(6/12).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(magd) addtext(Hansen, 4.954)  addstat(Hansep, 0.894) 

gmm (Phillips:infmagd-({b0}+{b1}*l6.infmagd+{b2}*f6.infmagd+{b3}*bmagd)), instruments(l(8/13).bmagd l(6/12).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(magd2) addtext(Hansen, 3.875)  addstat(Hansep, 0.952) 

//-----------------------------------------------------------------------------
  *Meta
gmm (Phillips:infmeta-({b0}+{b1}*l12.infmeta+{b2}*f12.infmeta+{b3}*bmeta)), instruments(l(2/7).bmeta l(1/6).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(meta) addtext(Hansen, 2.970)  addstat(Hansep, 0.965) 

gmm (Phillips:infmeta-({b0}+{b1}*l18.infmeta+{b2}*f18.infmeta+{b3}*bmeta)), instruments(l(2/7).bmeta l(1/6).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(meta2) addtext(Hansen,  5.287)  addstat(Hansep, 0.808) 

//-----------------------------------------------------------------------------
  *Nariño
gmm (Phillips:infnari-({b0}+{b1}*l.infnari+{b2}*f.infnari+{b3}*bnari)), instruments(l(8/13).bnari l(6/12).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(nari) addtext(Hansen, 6.625)  addstat(Hansep, 0.760) 

gmm (Phillips:infnari-({b0}+{b1}*l3.infnari+{b2}*f3.infnari+{b3}*bnari)), instruments(l(8/13).bnari l(6/12).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(nari2) addtext(Hansen, 6.217)  addstat(Hansep, 0.796) 

//-----------------------------------------------------------------------------
  *Norte de santander
gmm (Phillips:infnort-({b0}+{b1}*l3.infnort+{b2}*f3.infnort+{b3}*bnort)), instruments(l(2/7).bnort l(1/6).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(nort) addtext(Hansen,  3.959)  addstat(Hansep, 0.914) 

gmm (Phillips:infnort-({b0}+{b1}*l6.infnort+{b2}*f6.infnort+{b3}*bnort)), instruments(l(2/7).bnort l(1/6).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(nort2) addtext(Hansen, 6.818)  addstat(Hansep, 0.656) 

//-----------------------------------------------------------------------------
  *Quindio
gmm (Phillips:infquin-({b0}+{b1}*l3.infquin+{b2}*f3.infquin+{b3}*bquin)), instruments(l(2/7).bquin l(1/6).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(quin) addtext(Hansen, 10.330)  addstat(Hansep, 0.324) 

gmm (Phillips:infquin-({b0}+{b1}*l6.infquin+{b2}*f6.infquin+{b3}*bquin)), instruments(l(2/7).bquin l(1/6).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(quin2) addtext(Hansen, 4.518)  addstat(Hansep, 0.874) 

//-----------------------------------------------------------------------------
  *Risaralda
gmm (Phillips:infrisa-({b0}+{b1}*l3.infrisa+{b2}*f3.infrisa+{b3}*brisa)), instruments(l(2/7).brisa l(1/6).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(risa) addtext(Hansen, 4.911)  addstat(Hansep, 0.842) 

gmm (Phillips:infrisa-({b0}+{b1}*l6.infrisa+{b2}*f6.infrisa+{b3}*brisa)), instruments(l(2/7).brisa l(1/6).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(risa2) addtext(Hansen, 5.305)  addstat(Hansep, 0.806) 

//-----------------------------------------------------------------------------
  *Santander
gmm (Phillips:infsant-({b0}+{b1}*l3.infsant+{b2}*f3.infsant+{b3}*bsant)), instruments(l(2/7).bsant l(1/6).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(sant) addtext(Hansen, 8.392)  addstat(Hansep, 0.495) 

gmm (Phillips:infsant-({b0}+{b1}*l6.infsant+{b2}*f6.infsant+{b3}*bsant)), instruments(l(2/7).bsant l(1/6).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(sant2) addtext(Hansen, 4.769)  addstat(Hansep, 0.854) 

//-----------------------------------------------------------------------------
  *Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l.infsucr+{b2}*f.infsucr+{b3}*bsucr)), instruments(l(8/13).bsucr l(6/12).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(sucr) addtext(Hansen, 11.671)  addstat(Hansep, 0.307) 

gmm (Phillips:infsucr-({b0}+{b1}*l3.infsucr+{b2}*f3.infsucr+{b3}*bsucr)), instruments(l(8/13).bsucr l(6/12).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(sucr2) addtext(Hansen, 6.453)  addstat(Hansep, 0.775) 

//-----------------------------------------------------------------------------
  *Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l3.inftoli+{b2}*f3.inftoli+{b3}*btoli)), instruments(l(8/13).btoli l(6/12).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(toli) addtext(Hansen, 5.812)  addstat(Hansep, 0.830) 

gmm (Phillips:inftoli-({b0}+{b1}*l6.inftoli+{b2}*f6.inftoli+{b3}*btoli)), instruments(l(8/13).btoli l(6/12).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(toli2) addtext(Hansen, 4.731)  addstat(Hansep, 0.908) 

//-----------------------------------------------------------------------------
  *Valle del cauca
gmm (Phillips:infvall-({b0}+{b1}*l3.infvall+{b2}*f3.infvall+{b3}*bvall)), instruments(l(2/7).bvall l(1/6).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(vall) addtext(Hansen,  6.493)  addstat(Hansep, 0.689) 

gmm (Phillips:infvall-({b0}+{b1}*l6.infvall+{b2}*f6.infvall+{b3}*bvall)), instruments(l(2/7).bvall l(1/6).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(vall2) addtext(Hansen, 6.881)  addstat(Hansep, 0.649) 

//-----------------------------------------------------------------------------
  *Colombia
gmm (Phillips:inftotal-({b0}+{b1}*l3.inftotal+{b2}*f3.inftotal+{b3}*bcolombia)), instruments(l(2/7).bcolombia l(1/6).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(colombia) addtext(Hansen, 7.539)  addstat(Hansep, 0.581) 

gmm (Phillips:inftotal-({b0}+{b1}*l6.inftotal+{b2}*f6.inftotal+{b3}*bcolombia)), instruments(l(2/7).bcolombia l(1/6).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overid
outreg2 using regress22, append excel dec(3) ctitle(colombia2) addtext(Hansen, 6.0150)  addstat(Hansep, 0.738) 


//-----------------------------------------------------------------------------
*******Complementarias-b)
*Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l3.infboli+{b2}*f3.infboli+{b3}*bboli)), instruments(l(2/7).bboli l(1/6).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid
outreg2 using complemento, append excel dec(3) ctitle(boli3) addtext(Hansen, 5.464)  addstat(Hansep, 0.792) 

 *Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l3.infmagd+{b2}*f3.infmagd+{b3}*bmagd)), instruments(l(2/7).bmagd l(1/6).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid
outreg2 using complemento, append excel dec(3) ctitle(magd3) addtext(Hansen, 5.578)  addstat(Hansep, 0.781) 

  *Meta
gmm (Phillips:infmeta-({b0}+{b1}*l3.infmeta+{b2}*f3.infmeta+{b3}*bmeta)), instruments(l(2/7).bmeta l(1/6).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid
outreg2 using complemento, append excel dec(3) ctitle(meta3) addtext(Hansen, 3.325)  addstat(Hansep, 0.950)

 *Nariño
gmm (Phillips:infnari-({b0}+{b1}*l3.infnari+{b2}*f3.infnari+{b3}*bnari)), instruments(l(2/7).bnari l(1/6).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid
outreg2 using complemento, append excel dec(3) ctitle(nari3) addtext(Hansen, 5.189)  addstat(Hansep, 0.817) 
 
 *Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l3.infsucr+{b2}*f3.infsucr+{b3}*bsucr)), instruments(l(2/7).bsucr l(1/6).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid
outreg2 using complemento, append excel dec(3) ctitle(sucr3) addtext(Hansen, 5.873)  addstat(Hansep, 0.752) 

 *Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l3.inftoli+{b2}*f3.inftoli+{b3}*btoli)), instruments(l(2/7).btoli l(1/6).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid
outreg2 using complemento, append excel dec(3) ctitle(toli3) addtext(Hansen, 4.591)  addstat(Hansep, 0.868) 
