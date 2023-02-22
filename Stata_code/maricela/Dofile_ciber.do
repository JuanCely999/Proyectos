* Agrupar variables string
egen empresa = group(Empresa), label
egen pais = group(Pais), label
egen sector = group(Sector), label
drop Empresa Pais Sector
order pais sector empresa year

* Set panel
xtset empresa year
format gob1-fin6 %1.0f

tab pais
tab empresa
