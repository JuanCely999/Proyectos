*Anualizar inflacion_Establecer variable serie de tiempo año-1960 *4(12) +1
 import excel "/Users/juanpablo/Documents/Tesis/Descomposicion PIB y GMM/GMM/anualizaripc.xlsx", sheet("Hoja1") firstrow clear
*2009-1960 *12 -1= 587.  Correr un renglon despues para tomar el anterior
gen n=_n+587
format n %tm
tsset n, monthly

gen infanti =(Medelln -l12.Medelln)/l12.Medelln
gen infatla =(Barranquilla -l12.Barranquilla)/l12.Barranquilla
gen infbogo =(BogotDC -l12.BogotDC)/l12.BogotDC
gen infboli =(Cartagena -l12.Cartagena)/l12.Cartagena
gen infboya =(Tunja -l12.Tunja)/l12.Tunja
gen infcald =(Manizales -l12.Manizales)/l12.Manizales
gen infcaqu =(Florencia -l12.Florencia)/l12.Florencia
gen infcauc =(Popayn -l12.Popayn)/l12.Popayn
gen infcesa =(Valledupar -l12.Valledupar)/l12.Valledupar
gen infcord =(Montera -l12.Montera)/l12.Montera
gen infchoc =(Quibdo -l12.Quibdo)/l12.Quibdo
gen infhuil =(Neiva -l12.Neiva)/l12.Neiva
gen inflagua =(Riohacha -l12.Riohacha)/l12.Riohacha
gen infmagd =(SantaMarta -l12.SantaMarta)/l12.SantaMarta
gen infmeta =(Villavicencio -l12.Villavicencio)/l12.Villavicencio
gen infnari =(Pasto -l12.Pasto)/l12.Pasto
gen infnort =(Ccuta -l12.Ccuta)/l12.Ccuta
gen infquin =(Armenia -l12.Armenia)/l12.Armenia
gen infrisa =(Pereira -l12.Pereira)/l12.Pereira
gen infsant =(Bucaramanga -l12.Bucaramanga)/l12.Bucaramanga
gen infsucr =(Sincelejo -l12.Sincelejo)/l12.Sincelejo
gen inftoli =(Ibagu -l12.Ibagu)/l12.Ibagu
gen infvall =(Cali -l12.Cali)/l12.Cali
gen inftotal =(TOTAL -l12.TOTAL)/l12.TOTAL





















*******************************************************************
*******************************************************************
********************BORRADORES:CONTINUAR CON EJERCICIO PRELIMINAR**
*BogotDC Cali Medelln Barranquilla Armenia


*Generar serie de tiempo para 2010
gen n=_n+195
*formato serie de tiempo
format n %tq
*declarar la serie de tiempo
tsset n, quarterly

*Inflacion- YA ESTA
*gen inflacion =(ipc -l4.ipc)/l4.ipc
gen logpib =ln(Pib)
tsfilter hp brecha= logpib, trend(logpibp)

****resumen GMM
gmm (Phillips:inflacion-({b0}+{b1}*l.inflacion+{b2}*f.inflacion+{b3}*brecha)), instruments(l(1/3).brecha l(2/4).inflacion) wmatrix(hac bartlett opt) variables(brecha inflacion) vce(hac bartlett opt)
*gmm (Phillips:inflacion-({b0}+{b1}*l.inflacion+{b2}*f.inflacion+{b3}*brecha)), igmm instruments(l(1/3).brecha l(2/4).inflacion) wmatrix(hac bartlett opt) variables(brecha inflacion) vce(hac bartlett opt)
*gmm (Phillips:inflacion-({b0}+{b1}*l.inflacion+{b2}*f.inflacion+{b3}*brecha)), instruments(l(1/3).brecha l(2/4).inflacion) wmatrix(hac bartlett opt) variables(brecha inflacion) vce(hac bartlett opt)
*gmm (Phillips:inflacion-({b0}+{b1}*l.inflacion+{b2}*f.inflacion+{b3}*brecha)), instruments(l(1/3).brecha l(2/4).inflacion) wmatrix(hac bartlett opt) variables(brecha inflacion) vce(hac bartlett opt)
*gmm (Phillips:inflacion-({b1}*l.inflacion+{b2}*f.inflacion+{b3}*brecha)), instruments(l(1/3).brecha l(2/4).inflacion) wmatrix(hac bartlett opt) variables(brecha inflacion) vce(hac bartlett opt)
*gmm (Phillips:inflacion-({b1}*l.inflacion+{b2}*f.inflacion+{b3}*brecha)), instruments(l(1/3).brecha l(2/4).inflacion) wmatrix(hac bartlett opt) variables(brecha inflacion) vce(hac bartlett)



*******************************************************
********************Departametos***********************
*Generar serie de tiempo para 201
gen n=_n+195
*formato serie de tiempo
format n %tq
*declarar la serie de tiempo
tsset n, quarterly
*********************************INFLACION************
gen infbog =(ibog -l4.ibog)/l4.ibog
gen infcal =(ical -l4.ical)/l4.ical
gen infmed =(imed -l4.imed)/l4.imed
gen infbar =(ibar -l4.ibar)/l4.ibar
gen infarm =(iarm -l4.iarm)/l4.iarm
gen inftun =(itun -l4.itun)/l4.itun
gen infbuc =(ibuc -l4.ibuc)/l4.ibuc
gen infflo =(iflo -l4.iflo)/l4.iflo
**********************************Brechas*************


gen lbog =ln(bog2)
gen lcal =ln(cal2)
gen lmed =ln(med2)
gen lbar =ln(bar2)
gen larm =ln(arm2)
gen ltun =ln(tun2)
gen lbuc =ln(buc2)
gen lflo =ln(flor2)

tsfilter hp bbog= lbog, trend(l1)
tsfilter hp bcali= lcal, trend(l2)
tsfilter hp bmed= lmed, trend(l3)
tsfilter hp bbar= lbar, trend(l4)
tsfilter hp barm= larm, trend(l5)
tsfilter hp btun= ltun, trend(l6)
tsfilter hp bbuc= lbuc, trend(l7)
tsfilter hp bflo= lflo, trend(l8)

****resumen GMM
gmm (Phillips:infbog-({b0}+{b1}*l.infbog+{b2}*f.infbog+{b3}*bbog)), instruments(l(1/3).bbog l(2/4).infbog) wmatrix(hac bartlett opt) variables(bbog infbog) vce(hac bartlett opt)
estat overid
gmm (Phillips:infcal-({b0}+{b1}*l.infcal+{b2}*f.infcal+{b3}*bcali)), instruments(l(1/3).bcali l(2/4).infcal) wmatrix(hac bartlett opt) variables(bcali infcal) vce(hac bartlett opt)
estat overid
gmm (Phillips:infmed-({b0}+{b1}*l.infmed+{b2}*f.infmed+{b3}*bmed)), instruments(l(1/3).bmed l(2/4).infmed) wmatrix(hac bartlett opt) variables(bmed infmed) vce(hac bartlett opt)
estat overid
gmm (Phillips:infbar-({b0}+{b1}*l.infbar+{b2}*f.infbar+{b3}*bbar)), instruments(l(1/3).bbar l(2/4).infbar) wmatrix(hac bartlett opt) variables(bbar infbar) vce(hac bartlett opt)
estat overid
gmm (Phillips:infarm-({b0}+{b1}*l.infarm+{b2}*f.infarm+{b3}*barm)), instruments(l(1/3).barm l(2/4).infarm) wmatrix(hac bartlett opt) variables(barm infarm) vce(hac bartlett opt)
estat overid
gmm (Phillips:inftun-({b0}+{b1}*l.inftun+{b2}*f.inftun+{b3}*btun)), instruments(l(1/3).btun l(2/4).inftun) wmatrix(hac bartlett opt) variables(btun inftun) vce(hac bartlett opt)
estat overid

gmm (Phillips:infbuc-({b0}+{b1}*l.infbuc+{b2}*f.infbuc+{b3}*bbuc)), instruments(l(1/3).bbuc l(2/4).infbuc) wmatrix(hac bartlett opt) variables(bbuc infbuc) vce(hac bartlett opt)
estat overid
gmm (Phillips:infflo-({b0}+{b1}*l.infflo+{b2}*f.infflo+{b3}*bflo)), instruments(l(1/3).bflo l(2/4).infflo) wmatrix(hac bartlett opt) variables(bflo infflo) vce(hac bartlett opt)
estat overid

*Codigo 
gen nombre1=INGLABO*fex_c_2011
gen nombre2=nombre1/1000000
tabstat nombre2, statistics( sum) by(DPTO) columns(variables)
