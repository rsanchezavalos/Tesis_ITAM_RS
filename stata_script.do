*insheet using "C:\Users\rsanchez\Dropbox\Tesis_ITAM_RS\Base_Tesis.csv", comma case
*save "C:\Users\rsanchez\Dropbox\tesis\Cuerpo\FINAL\BDTesis.dta", replace
*Clear and set directory;
	clear
	cd "C:\Users\rsanchez\Dropbox\tesis\Cuerpo\FINAL\"
*database query;
	use "BDTesis.dta"
	xtset id year
*Clean code

	*keep id code MunName km2 dens_mar year rivalry_homicide hom_rate_riv lnPIB poverty ind_marginacion niv_pobreza  mean_edu niv_rezago_edu prop_sinprim prop_desempleo coef_gini PrP15PRIM lnUnemployment tomasC Laboratorios lab tomas  Border1 Border2 Border3 Border4 route port PortGolfo PortPacifico coca mar opi e_mar e_pop erad_mar erad_pop Imar Icoca Iopi  erad_marijuana co_count conflicto_12 alianza_sinconf violentconflict estructura 	tratamientosequiamar ttratamientosequiamar 	PR_Secuestro S_Extorsion PR_Extorsion  S_Robo  PR_Robo  S_Allanamiento  PR_Allanamiento  S_Secuestro  eficiencia  hom_rate_riv SPATIALLAG WEFICI LWhom_rate_riv Lhom_rate_riv Leficiencia  LWeficiencia agresion_a_militares 
	 

*Variable construction;
	gen coef_gini_2 = coef_gini*coef_gini
	gen lnPIB_2 = lnPIB*lnPIB


	*Muerte de Capo por Ubicación de Cártel
	drop capo
	gen capo=0
	replace capo=1 if year==2007 & (beltran==1)
	replace capo=1 if year==2008 & (sinaloa==1 | zetas==1)
	replace capo=1 if year==2009 & (golfo==1 | zetas==1 | sinaloa==1 | beltran==1)
	replace capo=1 if year==2010 & (golfo==1 | sinaloa==1 | beltran==1 | familia==1 | tijuana==1)
	replace capo=0 if capo==.

	
*fe not used because of time invariant variables	
*re
*The random effects assumption (made in a random effects model) is that the individual specific effects are uncorrelated with the independent variables. The fixed effect assumption is that the individual specific effect is correlated with the independent variables. If the random effects assumption holds, the random effects model is more efficient than the fixed effects model. However, if this assumption does not hold (i.e., if the Durbin–Watson test fails), the random effects model is not consistent.

*Tabla 1) Parametrización de la violencia (Nivel Municipal)
	*Modelo 1
		xtpoisson hom_rate_riv lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim, pa corr(unstructured)
		outreg2 using tabla1, replace title(Parametrización de violencia - Poisson) ctitle(Tasa Homicidios Relacionados)  tex(fragment) 
	*Modelo 2
		xtpoisson hom_rate_riv tomasC Laboratorios erad_mar erad_pop Imar Icoca Iopi Border1 Border2 Border3 Border4 route PortGolfo PortPacifico, pa corr(unstructured)
		outreg2 using tabla1, append ctitle(tasa Homicidios Relacionados)  tex(fragment) 
	*Modelo 3
		xtpoisson hom_rate_riv  L.eficiencia L.WEFICI L.hom_rate_riv L.SPATIALLAG, pa corr(unstructured)
		outreg2 using tabla1, append ctitle(tasa Homicidios Relacionados)  tex(fragment) 
	*Modelo 4
		xtpoisson hom_rate_riv lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim tomasC Laboratorios erad_mar erad_pop  Imar Icoca Iopi Border1 Border2 Border3 Border4 route PortGolfo PortPacifico, pa corr(unstructured)
		outreg2 using tabla1, append ctitle(tasa Homicidios Relacionados)  tex(fragment) 
	*Modelo 5	
		xtpoisson hom_rate_riv lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim tomasC Laboratorios erad_mar erad_pop  Imar Icoca Iopi Border1 Border2 Border3 Border4 route PortGolfo PortPacifico L.eficiencia L.WEFICI L.hom_rate_riv L.SPATIALLAG, pa corr(unstructured) 
		outreg2 using tabla1, append ctitle(tasa Homicidios Relacionados)  tex(fragment) 
	*Modelo 6	
		xtpoisson hom_rate_riv lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim tomasC Laboratorios erad_mar erad_pop  Imar Icoca Iopi Border1 Border2 Border3 Border4 route PortGolfo PortPacifico L.eficiencia L.WEFICI L.hom_rate_riv L.SPATIALLAG capo, pa corr(unstructured) 
		outreg2 using tabla1, append ctitle(tasa Homicidios Relacionados Control Capos)  tex(fragment) 


		
*Elegir si mantenemos pa o no
*Tabla 2) Análisis de la competencia - *estructura control cluster municipal socioeconómico ¿usarlo?
	*co_count 
		xtpoisson co_count Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop i.estructura
		outreg2 using tabla2, replace title(competencia) ctitle(Número_Cárteles) tex(fragment) 

		xtlogit violentconflict Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop i.estructura 
		outreg2 using tabla2, append ctitle(contrarios) eq(auto) tex(fragment) 

		xtpoisson Ejecucionconflicto Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop i.estructura 
		outreg2 using tabla2, append ctitle(contrarios) eq(auto) tex(fragment) 

		xtpoisson agresion_a_militares Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop i.estructura, pa corr(unstructured)
		outreg2 using tabla2, append ctitle(militares) tex(fragment) 


		*****************************
		*****************************
		*****************************
		
***********************************;
********Análisis de cluster********;
	clear
	cd "C:\Users\rsanchez\Dropbox\tesis\Cuerpo\FINAL\"
*database query;
	use "BDTesis.dta"
	xtset id year
	keep if year==2008 

*1) Clusterización Socioeconómica -Wardslinkage-;
	
	egen std_mean_edu = std(mean_edu) 
	*sutex2 lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim std_mean_edu LWhom_rate_riv Lhom_rate_riv Leficiencia LWeficiencia , minmax

	sum lnPIB coef_gini prop_desempleo std_mean_edu prop_sinprim	
	cluster wardslinkage lnPIB coef_gini prop_desempleo std_mean_edu prop_sinprim, measure(L2) name(cluster1)

	cluster dendrogram cluster1, cutnumber(5) showcount 
	cluster generate estructura_= groups(3), name(cluster1) ties(error) 
	drop cluster1_id cluster1_ord cluster1_hgt 

*2) Clusterización sobre violencia e impunidad -Wardslinkage-;Leficiencia 
	cluster  wardslinkage  LWhom_rate_riv Lhom_rate_riv LWeficiencia , measure(L2) name(cluster2)
	cluster dendrogram cluster2, cutnumber(5)
	cluster generate categoriaviolencia2= groups(3), name(cluster2) ties(error)
	table categoriaviolencia2 


*3) Categoría industria
	gen industria=0
	replace industria=3 if (lab==1 |Border1==1 | Border2==1 | Border3==1 | Border4==1 | PortPacifico==1)
	replace industria=2 if (PortGolfo==1 | e_mar==1 | e_pop==1 | mar==1 | coca==1 | opi==1)
	replace industria=1 if (route==1 | tomas==1)
tabstat hom_rate_riv , statistics( mean var median ) by(industria) columns(variables)

*COSTO DE OPORTUNIDAD -> re-ordenamiento por nivel de violencia;
tabstat hom_rate_riv , statistics( mean var median ) by(estructura_) columns(variables)
	gen costo_oportunidad = 0
	replace costo_oportunidad=3 if estructura_==1
	replace costo_oportunidad=2 if estructura_==2
	replace costo_oportunidad=1 if estructura_==3

tabstat hom_rate_riv , statistics( mean var median ) by(costo_oportunidad) columns(variables)
tabstat PIB ind_marginacion prop_desempleo  mean_edu if hom_rate_riv>0, statistics( mean var median ) by(costo_oportunidad) columns(variables)
tabstat hom_rate_riv , statistics( mean median var count) by(costo_oportunidad) columns(variables) 

*CATEGORIA VIOLENCIA -> re-ordenamiento por nivel de violencia;
tabstat hom_rate_riv , statistics( mean var median ) by(categoriaviolencia2) columns(variables)
	gen categoriaviolencia = 0
	replace categoriaviolencia=1 if categoriaviolencia2==1
	replace categoriaviolencia=2 if categoriaviolencia2==2
	replace categoriaviolencia=3 if categoriaviolencia2==3
	tab categoriaviolencia
	
tabstat hom_rate_riv , statistics( mean var median count) by(categoriaviolencia) columns(variables)
tabstat LWhom_rate_riv Lhom_rate_riv Leficiencia LWeficiencia, statistics( mean var median ) by(categoriaviolencia) columns(variables)

sum hom_rate_riv, detail
centile (hom_rate_riv) , centile(75, 90,100)

gen riesgo_violencia = 0
	replace riesgo_violencia = 1 if  hom_rate_riv <=   .9479672
	replace riesgo_violencia = 2 if  hom_rate_riv <=   10.16147 & riesgo_violencia  !=1
	replace riesgo_violencia = 3 if  hom_rate_riv > 10.16147 & riesgo_violencia  !=1 & riesgo_violencia  !=2

tabstat hom_rate_riv , statistics( mean var median count) by(riesgo_violencia) columns(variables)

drop categoriaviolencia2
drop estructura_

save "C:\Users\rsanchez\Dropbox\tesis\Cuerpo\FINAL\shape_bd_full.dta", replace

keep costo_oportunidad categoriaviolencia  industria riesgo_violencia hom_rate_riv 
outsheet using "C:\Users\rsanchez\Dropbox\tesis\decisiontree_test.csv", comma replace

***********************************;
***************Anexo 1*************;		
clear
cd "C:\Users\rsanchez\Dropbox\tesis\Cuerpo\FINAL\"
use "BDTesis.dta"
xtset id year

	*Sequia (Quasi Experimental Method)
	******************
		*Análsis de Diferencia en Diferencias (grupo tratamiento: sequia)-> con kernel Maching
		*Relies on the panel structure of the data (usually two periods: based line and follow up).
		*Control for unobservable and time invariant characteristics. Control for observable characteristics if available.
		*psm
		*Stata module
		ssc install diff
	******************
		global breps 100
		*global xlist estructura
		*global xlist lnPIB PrP15PRIM lnUnemployment  coef_gini  mean_edu  Border1 Border2 Border3 route PortPacifico
		global xlist lnPIB PrP15PRIM lnUnemployment ind_marginacion prop_sinprim niv_pobreza niv_rezago_edu coef_gini
		global xlist lnPIB PrP15PRIM mean_edu lnUnemployment coef_gini

	*Efecto de la sequia en la erradicación de mariguana	
		*Quantile diff in diff
		diff erad_marijuana, t(tratamientosequiamar) p(ttratamientosequiamar) cov($xlist) kernel rcs qdid(.5) 
		diff erad_marijuana, t(tratamientosequiamar) p(ttratamientosequiamar) cov($xlist) id(code) kernel rcs 
		diff erad_marijuana, t(tratamientosequiamar) p(ttratamientosequiamar) 
			*Balancing - t test
			diff erad_marijuana, t(tratamientosequiamar) p(ttratamientosequiamar) cov($xlist)  test

		*Efecto de la sequia en la violencia
		diff hom_rate_riv, t(tratamientosequiamar) p(ttratamientosequiamar) cov($xlist) id(code) kernel rcs qdid(.5) 
		diff hom_rate_riv, t(tratamientosequiamar) p(ttratamientosequiamar) cov($xlist) id(code) kernel rcs 
		diff hom_rate_riv, t(tratamientosequiamar) p(ttratamientosequiamar) 
			*Balancing - t test
			diff hom_rate_riv, t(tratamientosequiamar) p(ttratamientosequiamar) cov($xlist)  test



***********************************;
***************Anexo 3*************;		
	*Efecto de la violencia en otros crímenes  (Método de Variables instrumentales)
	******************
		*Se instrumentaliza utilizando las principales variables de posición geográfica del municipio 
		*Para obtener un estimador consistente mediante el método de variables instrumentales se deben cumplir dos condiciones. 
		*1) Relevancia del instrumento con nuestra variable de interés (Cov(Homicidios Relacionados, Zi)).  
		*2) Exogeneidad o restricción de exclusión: que el instrumento no esté correlacionado con el error (Cov(Ui,Zi)=0). 
	******************
	xtivreg S_Secuestro lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim (rivalry_homicide = Border1 Border2 Border3 route port), re
	outreg2 using instrumento.doc, replace title(Instrumentos) ctitle(S_Secuestro) tex(fragment) 
	xtivreg PR_Secuestro lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim (rivalry_homicide = Border1 Border2 Border3 route port), re
	outreg2 using instrumento.doc, append ctitle(PR_Secuestro)  tex(fragment) 
	xtivreg S_Extorsion  lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim (rivalry_homicide = Border1 Border2 Border3 route port), re
	outreg2 using instrumento.doc, append  ctitle(S_Extorsion) tex(fragment) 
	xtivreg PR_Extorsion  lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim (rivalry_homicide = Border1 Border2 Border3 route port), re
	outreg2 using instrumento.doc, append ctitle(PR_Extorsion) tex(fragment) 
	xtivreg S_Robo lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim (rivalry_homicide = Border1 Border2 Border3 route port), re
	outreg2 using instrumento.doc, append ctitle(S_Robo) tex(fragment) 
	xtivreg PR_Robo lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim (rivalry_homicide = Border1 Border2 Border3 route port), re
	outreg2 using instrumento.doc, append ctitle(PR_Robo) tex(fragment) 
	xtivreg S_Allanamiento lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim (rivalry_homicide = Border1 Border2 Border3 route port), re
	outreg2 using instrumento.doc, append ctitle(S_Allanamiento) tex(fragment) 
	xtivreg PR_Allanamiento lnPIB  ind_marginacion niv_pobreza  coef_gini prop_desempleo mean_edu prop_sinprim (rivalry_homicide = Border1 Border2 Border3 route port), re
	outreg2 using instrumento.doc, append ctitle(PR_Allanamiento) tex(fragment) 
