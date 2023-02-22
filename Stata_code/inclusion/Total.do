label define genero 0 "Femenino" 1 "Masculino"
label values genero genero

label define A11 1 "Totalmente de acuerdo" 2 "De acuerdo" 3 "Un poco de acuerdo" 4 "En desacuerdo"
label values A11 A12 A13 A14 A15 A16 A17 A21 A22 A23 A24 A25 A26 B11 B12 B13 B14 B15 B16 B21 B22 B23 B24 B25 B26 B27 B28 C11 C12 C13 C14 C15 C16 C17 C18 C19 C110 C111 C21 C22 C23 C24 C25 A11

label define ciudadorigen 0 "Tunja" 1 "Chiquinquira" 2 "Toca" 3 "San Andres" 4 "Yopal" 5 "Bogota" 6 "Soata" 7 "Cienaga" 8 "Villa de Leiva" 9 "Sogamoso" 10 "Ventaquemada" 11 "Belen" 12 "Arcabuco" 13 "Mani" 14 "Barbosa" 15 "Samaca" 16 "Duitama" 17 "Tibasosa" 18 "Tibana"19 "Villanueva" 20 "Chita" 22 "Zipaquira" 23 "Combita" 24 "Yopal" 25 "Paipa" 26 "San Jose de Pare" 27 "Aquitania" 28 "Santa Sofia" 30"San Martin- Meta" 31 "San Mateo" 32 "Santa Rosa de Viterbo" 33 "Socha" 34 "Nobsa" 35 "Puente Nacional-Santander" 36 "Villapinzon-Santander" 37 "Tenza" 38 "Cucuta" 39 "Sachica" 40 "Guayata" 41 "Siachoque" 42 "Moniquira" 43 "Paez" 44 "Somondoco" 45 "Miraflores" 46 "Muzo" 48 "Santana-Boyaca" 49 "Boavita" 50 "Arauca" 51 "Velez" 52 "Popayan" 53 "Cocuy" 54 "Chiscas" 55 "Granada- Meta" 56 "Paz de Ariporo" 57 "Sotaquira" 58 "San Eduardo " 59 "Junin-Cundinamarca" 60 "Corozal-Sucre", replace
label values ciudadorigen ciudadorigen




label define colegios 0 "Rural del Sur" 1 "Coopleboy" 2 "Inem: Carlos Arturo Torres" 3 "Juan de la Salle" 4 "Silvino Rodriguez"
 label values nombredelcolegio colegios
 
 label define carreras 0 "Educacion fisica" 1 "Ingenieria Electronica" 2 "Administracion de empresas" 3 "Contaduria publica" 4 "Derecho" 5 "Ingenieria mecanica" 6 "Ingenieria industrial" 7 "Ingenieria civil" 8 "Arquitectura" 9 "Negocios internacionales" 10 "Ingenieria ambiental" 11 "Ingenieria mecanica" 12 "Ingenieria de sistemas"
label values carreraquevaestudiar carreras


label define origen 0 "Tunja" 1 "Resto de Boyaca" 2 "Resto del Pais"
label values ciudadorigen2 origen

tabulate carreraquevaestudiar
tabulate genero
tabulate A11
tabulate A12
tabulate A13
tabulate A14
tabulate A15
tabulate A16
tabulate A17
tabulate A21
tabulate A22
tabulate A23
tabulate A24
tabulate A25
tabulate A26
tabulate B11
tabulate B12
tabulate B13
tabulate B14
tabulate B15
tabulate B16
tabulate B21
tabulate B22
tabulate B23
tabulate B24
tabulate B25
tabulate B26
tabulate B27
tabulate B28
tabulate C11
tabulate C12
tabulate C13
tabulate C14
tabulate C15
tabulate C16
tabulate C17
tabulate C18
tabulate C19
tabulate C110
tabulate C111
tabulate C21
tabulate C22
tabulate C23
tabulate C24
tabulate C25
graph pie, over (B11) plabel(_all percent, size (*2.0) color(white))
graph hbar, over(genero) over(C111) ytitle(Porcentaje) blabel(total)

graph hbar, over(A21) over(carreraquevaestudiar)  ytitle(Porcentaje) blabel(total)

 graph hbar, over(C25) over(ciudadorigen2) ytitle(Porcentaje) blabel(total)
 


 generate numero2=numero
 recode numero2 (1/144=0) (145/367=1)
 
 label values numero2 poblacion

label define poblacion 0 "Institución Media" 1 "Institucion Superior"

 
 
 
 generate ciudadorigen2=ciudadorigen 
recode ciudadorigen2 2 6 7 8 9 10 11 12 15 16 17 18 19 20 23 25 26 27 28 29 31 32 33 34 39 40 41 42 43 44 45 46 48 49 53 54 56 57 58=1
 
recode ciudadorigen2 3 4 5 13 14 22 24 30 35 36 37 38 50 51 52 55 59 60=2
 
 
 
 
 
 generate ZA11=A11
 recode ZA11 1 2=0 3 4=1
 
 generate ZA12=A12
 recode ZA12 1 2=0 3 4=1
 
 generate ZA13=A13
 recode ZA13 1 2=0 3 4=1
 
 generate ZA14=A14
 recode ZA14 1 2=0 3 4=1
 
 generate ZA15=A15
 recode ZA15 1 2=0 3 4=1
 
 generate ZA16=A16
 recode ZA16 1 2=0 3 4=1
 
 generate ZA17=A17
 recode ZA17 1 2=0 3 4=1
 
 generate ZA21=A21
 recode ZA21 1 2=0 3 4=1
 
 generate ZA22=A22
 recode ZA22 1 2=0 3 4=1
 
 generate ZA23=A23
 recode ZA23 1 2=0 3 4=1
 
 generate ZA24=A24
 recode ZA24 1 2=0 3 4=1
 
 generate ZA25=A25
 recode ZA25 1 2=0 3 4=1
 
 generate ZA26=A26
 recode ZA26 1 2=0 3 4=1
 
 generate ZB11=B11
 recode ZB11 1 2=0 3 4=1
 
  generate ZB12=B12
 recode ZB12 1 2=0 3 4=1
 
  generate ZB13=B13
 recode ZB13 1 2=0 3 4=1
 
  generate ZB14=B14
 recode ZB14 1 2=0 3 4=1
 
  generate ZB15=B15
 recode ZB15 1 2=0 3 4=1
 
  generate ZB16=B16
 recode ZB16 1 2=0 3 4=1
 
  generate ZB21=B21
 recode ZB21 1 2=0 3 4=1
 
  generate ZB22=B22
 recode ZB22 1 2=0 3 4=1
 
  generate ZB23=B23
 recode ZB23 1 2=0 3 4=1
 
  generate ZB24=B24
 recode ZB24 1 2=0 3 4=1
 
  generate ZB25=B25
 recode ZB25 1 2=0 3 4=1
 
  generate ZB26=B26
 recode ZB26 1 2=0 3 4=1
 
  generate ZB27=B27
 recode ZB27 1 2=0 3 4=1
 
  generate ZB28=B28
 recode ZB28 1 2=0 3 4=1
 
  generate ZC11=C11
 recode ZC11 1 2=0 3 4=1
 
  generate ZC12=C12
 recode ZC12 1 2=0 3 4=1
 
  generate ZC13=B13
 recode ZC13 1 2=0 3 4=1
 
  generate ZC14=C14
 recode ZC14 1 2=0 3 4=1
 
  generate ZC15=C15
 recode ZC15 1 2=0 3 4=1
 
  generate ZC16=C16
 recode ZC16 1 2=0 3 4=1
 
  generate ZC17=C17
 recode ZC17 1 2=0 3 4=1
 
  generate ZC18=C18
 recode ZC18 1 2=0 3 4=1
 
  generate ZC19=C19
 recode ZC19 1 2=0 3 4=1
 
  generate ZC110=C110
 recode ZC110 1 2=0 3 4=1
 
  generate ZC111=C111
 recode ZC111 1 2=0 3 4=1
 
 generate ZC21=C21
 recode ZC21 1 2=0 3 4=1
 
 generate ZC22=C22
 recode ZC22 1 2=0 3 4=1
 
 generate ZC23=C23
 recode ZC23 1 2=0 3 4=1
 
 generate ZC24=C24
 recode ZC24 1 2=0 3 4=1
 
 generate ZC25=C25
 recode ZC25 1 2=0 3 4=1
 

 
  pwcorr Total ZA12 ZA13 ZA15 ZA17 ZA21 ZA22 ZB14 ZB16 ZB25 ZB27 ZC11 ZC12 ZC111 ZC21 ZC24, sig
 reg Total ZA12 ZA13 ZA15 ZA17 ZA21 ZA22 ZB14 ZB16 ZB25 ZB27 ZC11 ZC12 ZC111 ZC21 ZC24
 predict rstu, rstu
  histogram rstu, normal
  predict yhat
  vif
  
  
  entrar desde datos, crear variable
  egen float hombre = std(Hombresengeneral), mean(0) std(1)
egen float mujer = std( Mujeresengeneral ), mean(0) std(1)

egen float sdtunja = std( Tunja ), mean(0) std(1)
 egen float sdregion = std( region ), mean(0) std(1)
egen float sdpais = std( pais ), mean(0) std(1)

 egen float sdmedia = std( media ), mean(0) std(1)
egen float sdsuperior = std( superior ), mean(0) std(1)
egen float sddocentes = std( docentes ), mean(0) std(1)



egen float sdrsur = std( rsur ), mean(0) std(1)
egen float sdcoople = std( coople ), mean(0) std(1)
egen float sdinem = std( inem ), mean(0) std(1)
egen float sdjsalle = std( jsalle ), mean(0) std(1)
egen float sdsilvino = std( silvino ), mean(0) std(1)
 
  
 egen float sdest = std( Total ), mean(0) std(1)
 
 
 
 
 
 
 
 
 graph hbar (count), over(genero) over(A11) showyvars blabel(bar)

 graph hbar [fweight = A11], over(genero) over(A11) showyvars blabel(bar) ytitle(A11)

 
 
 
 vif
 CALCULA LA COLINEALIDAD  MENORES DE 10 NO HAY PROBLEMA DE COLINEALIDAD
 colinealidad, mayor de 10. Mayor de 30 bastante colinealidad
 pwcorr genero A11 A12, sig
reg genero i.A11
 REVISAR VALORES SIGNIFICATIVOS
 i. para anteponer las variables categoricas
 corr genero A11
 eliminar en correlacion con bajos valores con beta menores

 para calcular el modelo mdl
 se guarda
 estimates store mpl
 logit ... variables
 estimates store logit
 probit ... variables
 estimates store probit
 traer los tres 
 estimates table mpl logit ptobit, star stat(N r2)

 *procedimientos finales. ANALISIS DE PROBABILIDAD. https://www.youtube.com/watch?v=xQ0uySZblxA
 

 
 
 summ Total,det 
 generate total2=Total
 recode total2 (1/91=0) (92/148=1)
 
 
 
 
 
 
 
 
 *Total de estudiantes
 
*EXITO
logit total2 ZA11 ZA13 ZA17 ZA25 ZB12 ZB14 ZB21  ZB25 ZC14 ZC21 ZC24
 estat class
 
 *si el modelo esta bien ajustado fiden count numer de observaciones correctas y el numero de observaciones del modelo  cuanto mas alto los valores encontrados mas alto es el grado de ajustamiento el modelo
 fitstat
 
 mfx
 
 mfx, at (ZA11=0 ZA13=0 ZA17=0 ZA25=0 ZB12=0 ZB14=0 ZB21=0  ZB25=0 ZC14=1 ZC21=1 ZC24=1)
logistic
predict prob, p
 list prob *probabilidad de cada uno de los individuos de que un evento ocurra
 *IMPORTANTE
predict total2hat
gen total2_pre=1 if total2hat>0.5
recode total2_pre.=0
tab total2 total2_pre
tab total2
generate Total=total2
