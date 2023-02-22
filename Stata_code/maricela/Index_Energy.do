* Import
clear all
cd "D:\Google Drive\Thesis\DOCTORADOS\Maricela Index\Index_Energy"
import excel "Energy.xlsx", firstrow
set more off
sum
tab Pais
tab Empresa

* Agrupar variables string
egen empresa = group(Empresa), label
egen pais = group(Pais), label
drop Empresa Pais
order pais empresa year

* Set panel
xtset empresa year
format gob1-fin6 %1.0f


// ----------------------------------------------------------------------
* Índices dimensionales
egen gobernanza = rowmean(gob*)
egen estrategia = rowmean(estr*)
egen riesgo = rowmean(rgo*)
egen finanzas = rowmean(fin*)
global dims "gobernanza estrategia riesgo finanzas"
foreach var of varlist $dims{
	replace `var' = 100*`var' 
}
sum $dims
order pais empresa year $dims
sort pais empresa year

* Graficar datos Colapsados
collapse (mean) $dims, by(pais year)
sum $dims
*xtline gobernanza, i(pais) t(year) overlay
*xtline estrategia, i(pais) t(year) overlay
*xtline riesgo, i(pais) t(year) overlay
*xtline finanzas, i(pais) t(year) overlay

* CRONBACH
alpha $dims, item


// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
* Índice global
egen index = rowmean(gob1 - fin6)
replace index = 100*index
drop gob1 - fin6
order pais empresa year index
sum index

* Full Index
tab index pais
hist index, frequency kdensity
graph box index

* Filtered Index
gen Index = .
replace Index = index if index!=0
sum Index
hist Index, frequency kdensity
graph box Index

* Graficar datos Colapsados por país
collapse (mean) Index, by(pais year)
xtline Index, i(pais) t(year)

* Graficar datos Colapsados por empresa
*collapse (mean) Index, by(empresa year)
*xtline Index, i(empresa) t(year)


