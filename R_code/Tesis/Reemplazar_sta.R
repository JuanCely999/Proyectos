//-----------------------------------------------------------------------------
*Atlantico
gmm (Phillips:infatla-({b0}+{b1}*l3.infatla+{b2}*f3.infatla+{b3}*batla)), instruments(l(2/7).batla l(1/6).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(atla) addtext(Hansen, )  addstat(Hansep, ) 


gmm (Phillips:infatla-({b0}+{b1}*l6.infatla+{b2}*f6.infatla+{b3}*batla)), instruments(l(2/7).batla l(1/6).infatla) wmatrix(hac bartlett opt) variables(batla infatla) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(atla2) addtext(Hansen, )  addstat(Hansep, ) 
//-----------------------------------------------------------------------------
  
*Bogota
gmm (Phillips:infbogo-({b0}+{b1}*l3.infbogo+{b2}*f3.infbogo+{b3}*bbogo)), instruments(l(2/7).bbogo l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt) 
estat overid
outreg2 using regress, append excel dec(3) ctitle(bogo) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infbogo-({b0}+{b1}*l6.infbogo+{b2}*f6.infbogo+{b3}*bbogo)), instruments(l(2/7).bbogo l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bbogo infbogo) vce(hac bartlett opt) 
estat overid
outreg2 using regress, append excel dec(3) ctitle(bogo2) addtext(Hansen, )  addstat(Hansep, ) 
//-----------------------------------------------------------------------------
  
*Bolivar
gmm (Phillips:infboli-({b0}+{b1}*l12.infboli+{b2}*f12.infboli+{b3}*bboli)), instruments(l(2/7).bboli l(1/6).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(boli) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infboli-({b0}+{b1}*l18.infboli+{b2}*f18.infboli+{b3}*bboli)), instruments(l(2/7).bboli l(1/6).infboli) wmatrix(hac bartlett opt) variables(bboli infboli) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(boli2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------

*Boyaca
gmm (Phillips:infboya-({b0}+{b1}*l3.infboya+{b2}*f3.infboya+{b3}*bboya)), instruments(l(2/7).bboya l(1/6).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(boya) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infboya-({b0}+{b1}*l6.infboya+{b2}*f6.infboya+{b3}*bboya)), instruments(l(2/7).bboya l(1/6).infboya) wmatrix(hac bartlett opt) variables(bboya infboya) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(boya2) addtext(Hansen, )  addstat(Hansep, ) 
//-----------------------------------------------------------------------------
  

*Caldas
gmm (Phillips:infcald-({b0}+{b1}*l3.infcald+{b2}*f3.infcald+{b3}*bcald)), instruments(l(2/7).bcald l(1/6).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cald) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infcald-({b0}+{b1}*l6.infcald+{b2}*f6.infcald+{b3}*bcald)), instruments(l(2/7).bcald l(1/6).infcald) wmatrix(hac bartlett opt) variables(bcald infcald) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cald2) addtext(Hansen, )  addstat(Hansep, ) 
//-----------------------------------------------------------------------------

*Caqueta
gmm (Phillips:infcaqu-({b0}+{b1}*l3.infcaqu+{b2}*f3.infcaqu+{b3}*bcaqu)), instruments(l(2/7).bcaqu l(1/6).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(caqu) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infcaqu-({b0}+{b1}*l6.infcaqu+{b2}*f6.infcaqu+{b3}*bcaqu)), instruments(l(2/7).bcaqu l(1/6).infcaqu) wmatrix(hac bartlett opt) variables(bcaqu infcaqu) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(caqu2) addtext(Hansen, )  addstat(Hansep, ) 
//-----------------------------------------------------------------------------
  
*Cauca
gmm (Phillips:infcauc-({b0}+{b1}*l3.infcauc+{b2}*f3.infcauc+{b3}*bcauc)), instruments(l(2/7).bcauc l(1/6).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cauc) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infcauc-({b0}+{b1}*l6.infcauc+{b2}*f6.infcauc+{b3}*bcauc)), instruments(l(2/7).bcauc l(1/6).infcauc) wmatrix(hac bartlett opt) variables(bcauc infcauc) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cauc2) addtext(Hansen, )  addstat(Hansep, ) 


//-----------------------------------------------------------------------------
  *Cesar
gmm (Phillips:infcesa-({b0}+{b1}*l3.infcesa+{b2}*f3.infcesa+{b3}*bcesa)), instruments(l(2/7).bcesa l(1/6).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cesa) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infcesa-({b0}+{b1}*l6.infcesa+{b2}*f6.infcesa+{b3}*bcesa)), instruments(l(2/7).bcesa l(1/6).infcesa) wmatrix(hac bartlett opt) variables(bcesa infcesa) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cesa2) addtext(Hansen, )  addstat(Hansep, ) 


//-----------------------------------------------------------------------------
  *Cordoba
gmm (Phillips:infcord-({b0}+{b1}*l3.infcord+{b2}*f3.infcord+{b3}*bcord)), instruments(l(2/7).bcord l(1/6).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cord) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infcord-({b0}+{b1}*l6.infcord+{b2}*f6.infcord+{b3}*bcord)), instruments(l(2/7).bcord l(1/6).infcord) wmatrix(hac bartlett opt) variables(bcord infcord) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cord2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Cundinamarca
gmm (Phillips:infbogo-({b0}+{b1}*l3.infbogo+{b2}*f3.infbogo+{b3}*bcund)), instruments(l(2/7).bcund l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cund) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infbogo-({b0}+{b1}*l6.infbogo+{b2}*f6.infbogo+{b3}*bcund)), instruments(l(2/7).bcund l(1/6).infbogo) wmatrix(hac bartlett opt) variables(bcund infbogo) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(cund2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Choco
gmm (Phillips:infchoc-({b0}+{b1}*l3.infchoc+{b2}*f3.infchoc+{b3}*bchoc)), instruments(l(2/7).bchoc l(1/6).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(choc) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infchoc-({b0}+{b1}*l6.infchoc+{b2}*f6.infchoc+{b3}*bchoc)), instruments(l(2/7).bchoc l(1/6).infchoc) wmatrix(hac bartlett opt) variables(bchoc infchoc) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(choc2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Huila
gmm (Phillips:infhuil-({b0}+{b1}*l3.infhuil+{b2}*f3.infhuil+{b3}*bhuil)), instruments(l(2/7).bhuil l(1/6).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(huil) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infhuil-({b0}+{b1}*l6.infhuil+{b2}*f6.infhuil+{b3}*bhuil)), instruments(l(2/7).bhuil l(1/6).infhuil) wmatrix(hac bartlett opt) variables(bhuil infhuil) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(huil2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *La Guajira
gmm (Phillips:inflagua-({b0}+{b1}*l.inflagua+{b2}*f.inflagua+{b3}*blagua)), instruments(l(2/7).blagua l(1/6).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(lagua) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:inflagua-({b0}+{b1}*l3.inflagua+{b2}*f3.inflagua+{b3}*blagua)), instruments(l(2/7).blagua l(1/6).inflagua) wmatrix(hac bartlett opt) variables(blagua inflagua) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(lagua2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Magdalena
gmm (Phillips:infmagd-({b0}+{b1}*l3.infmagd+{b2}*f3.infmagd+{b3}*bmagd)), instruments(l(8/13).bmagd l(6/12).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(magd) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infmagd-({b0}+{b1}*l6.infmagd+{b2}*f6.infmagd+{b3}*bmagd)), instruments(l(8/13).bmagd l(6/12).infmagd) wmatrix(hac bartlett opt) variables(bmagd infmagd) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(magd2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Meta
gmm (Phillips:infmeta-({b0}+{b1}*l12.infmeta+{b2}*f12.infmeta+{b3}*bmeta)), instruments(l(2/7).bmeta l(1/6).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(meta) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infmeta-({b0}+{b1}*l18.infmeta+{b2}*f18.infmeta+{b3}*bmeta)), instruments(l(2/7).bmeta l(1/6).infmeta) wmatrix(hac bartlett opt) variables(bmeta infmeta) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(meta2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Nariño
gmm (Phillips:infnari-({b0}+{b1}*l.infnari+{b2}*f.infnari+{b3}*bnari)), instruments(l(8/13).bnari l(6/12).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(nari) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infnari-({b0}+{b1}*l3.infnari+{b2}*f3.infnari+{b3}*bnari)), instruments(l(8/13).bnari l(6/12).infnari) wmatrix(hac bartlett opt) variables(bnari infnari) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(nari2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Norte de santander
gmm (Phillips:infnort-({b0}+{b1}*l3.infnort+{b2}*f3.infnort+{b3}*bnort)), instruments(l(2/7).bnort l(1/6).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(nort) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infnort-({b0}+{b1}*l6.infnort+{b2}*f6.infnort+{b3}*bnort)), instruments(l(2/7).bnort l(1/6).infnort) wmatrix(hac bartlett opt) variables(bnort infnort) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(nort2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Quindio
gmm (Phillips:infquin-({b0}+{b1}*l3.infquin+{b2}*f3.infquin+{b3}*bquin)), instruments(l(2/7).bquin l(1/6).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(quin) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infquin-({b0}+{b1}*l6.infquin+{b2}*f6.infquin+{b3}*bquin)), instruments(l(2/7).bquin l(1/6).infquin) wmatrix(hac bartlett opt) variables(bquin infquin) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(quin2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Risaralda
gmm (Phillips:infrisa-({b0}+{b1}*l3.infrisa+{b2}*f3.infrisa+{b3}*brisa)), instruments(l(2/7).brisa l(1/6).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(risa) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infrisa-({b0}+{b1}*l6.infrisa+{b2}*f6.infrisa+{b3}*brisa)), instruments(l(2/7).brisa l(1/6).infrisa) wmatrix(hac bartlett opt) variables(brisa infrisa) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(risa2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Santander
gmm (Phillips:infsant-({b0}+{b1}*l3.infsant+{b2}*f3.infsant+{b3}*bsant)), instruments(l(2/7).bsant l(1/6).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(sant) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infsant-({b0}+{b1}*l6.infsant+{b2}*f6.infsant+{b3}*bsant)), instruments(l(2/7).bsant l(1/6).infsant) wmatrix(hac bartlett opt) variables(bsant infsant) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(sant2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Sucre
gmm (Phillips:infsucr-({b0}+{b1}*l.infsucr+{b2}*f.infsucr+{b3}*bsucr)), instruments(l(8/13).bsucr l(6/12).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(sucr) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infsucr-({b0}+{b1}*l3.infsucr+{b2}*f3.infsucr+{b3}*bsucr)), instruments(l(8/13).bsucr l(6/12).infsucr) wmatrix(hac bartlett opt) variables(bsucr infsucr) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(sucr2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Tolima
gmm (Phillips:inftoli-({b0}+{b1}*l3.inftoli+{b2}*f3.inftoli+{b3}*btoli)), instruments(l(8/13).btoli l(6/12).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(toli) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:inftoli-({b0}+{b1}*l6.inftoli+{b2}*f6.inftoli+{b3}*btoli)), instruments(l(8/13).btoli l(6/12).inftoli) wmatrix(hac bartlett opt) variables(btoli inftoli) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(toli2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Valle del cauca
gmm (Phillips:infvall-({b0}+{b1}*l3.infvall+{b2}*f3.infvall+{b3}*bvall)), instruments(l(2/7).bvall l(1/6).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(vall) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:infvall-({b0}+{b1}*l6.infvall+{b2}*f6.infvall+{b3}*bvall)), instruments(l(2/7).bvall l(1/6).infvall) wmatrix(hac bartlett opt) variables(bvall infvall) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(vall2) addtext(Hansen, )  addstat(Hansep, ) 

//-----------------------------------------------------------------------------
  *Colombia
gmm (Phillips:inftotal-({b0}+{b1}*l3.inftotal+{b2}*f3.inftotal+{b3}*bcolombia)), instruments(l(2/7).bcolombia l(1/6).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(colombia) addtext(Hansen, )  addstat(Hansep, ) 

gmm (Phillips:inftotal-({b0}+{b1}*l6.inftotal+{b2}*f6.inftotal+{b3}*bcolombia)), instruments(l(2/7).bcolombia l(1/6).inftotal) wmatrix(hac bartlett opt) variables(bcolombia inftotal) vce(hac bartlett opt)
estat overid
outreg2 using regress, append excel dec(3) ctitle(colombia2) addtext(Hansen, )  addstat(Hansep, ) 




