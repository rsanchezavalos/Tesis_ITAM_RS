clear
*Cargar base de datos;
cd "C:\Users\rsanchez\Dropbox\Tesis_ITAM_RS"
use "tesisDB_anexo2"

*Establecer temporalidad
keep if ao>=2008 & ao<=2013
tsset year, format(%tmCY)

*Creación de variables
gen lnhom=ln(homicidio)
gen lnrob=ln(robo)
gen Dhomicidio=D.homicidio
gen Dlnhom=D.lnhom

*Pre-análisis gráfico
	twoway (tsline homicidio) if ao>=2007 
	qui tsline homicidio, name(f, replace)
	qui tsline D.homicidio, name(df, replace) yline(0)
	qui tsline robo, name(b, replace)
	qui tsline D.robo, name(db, replace) yline(0)
	qui tsline DD.homicidio, name(df, replace) yline(0)
	qui tsline DD.robo, name(db, replace) yline(0)
	graph combine f df b db, cols(2) 
	graph combine f b, cols(1) 

*i) Pruebas de raiz unitaria (I. Verificar que las variables son I(d) (Integrada de orden d) con d>0)
	dfuller homicidio
	dfuller robo
*ii) no podemos rechazar la hipótesis de raíz unitaria. Verificamos que su orden de integración sea el mismo: 
	dfuller D.homicidio
	dfuller D.robo
		*La prueba Dickey-Fuller nos indica que ambas series son integradas de orden uno 

* iii) Modelo Engle Granger de cointegración
	regress robo homicidio 
	outreg2 using cointegracion, replace tex(fragment) 
	predict Rhat, residual
*iv)  prueba Dickey-Fuller en residuales 
		*Para asegurarnos que las variables estén co-integradas y que el modelo es apropiado se debe probar que los residuales son I(1) 
	dfuller Rhat

	regress robo L.homicidio
	predict ehat, residual
	dfuller ehat

*v) ECM – Modelo de Corrección de Errore
	regress D.robo L.Rhat D.homicidio
	predict Recm, residual
	dfuller Recm
	twoway (tsline Recm) 
