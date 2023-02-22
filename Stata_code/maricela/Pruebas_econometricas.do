****Regresión
reg index p_indep p_hombres WGI dPIB Tamaodelajunta

***Evaluación de normalidad
predict residuales, r
sktest residuales
*se recomienda utilizar
swilk residuales

*Prueba de multicolinealidad
estat vif 


***Correlación entre las variables explicativas
corr p_indep p_hombres WGI dPIB Tamaodelajunta

*Prueba de homocedasticidad
imtest, white
