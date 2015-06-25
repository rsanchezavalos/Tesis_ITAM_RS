*Clear and set directory;
	clear
	cd "C:\Users\rsanchez\Dropbox\Tesis_ITAM_RS"

*database query;
	use "tesisDB.dta"
	xtset id year

*Clean code
	keep id year hom_rate_riv conflicto_12 alianza_sinconf violentconflict agresion_a_militares 
	lnPIB ind_marginacion niv_pobreza mean_edu niv_sinprim niv_desempleo coef_gini tomasC 
	Laboratorios erad_mar erad_pop Imar Icoca Iopi Border1 Border2 Border3 Border4 route PortGolfo 
	PortPacifico eficiencia WEFICI hom_rate_riv SPATIALLAG co_count coca mar opi lab tomas e_mar e_pop estructura
	PrP15PRIM lnUnemployment 

*Variable construction;
	tabulate estructura, gen(e)



*Tabla 1) Parametrización de la violencia (Nivel Municipal)
	*Modelo 1
		xtpoisson hom_rate_riv lnPIB ind_marginacion niv_pobreza mean_edu niv_sinprim niv_desempleo coef_gini, pa corr(unstructured) 
		*outreg2 using "***", replace title(Parametrización de violencia - Poisson) ctitle(Tasa Homicidios Relacionados)
	*Modelo 2
		xtpoisson hom_rate_riv tomasC Laboratorios erad_mar erad_pop coca mar opi Border1 Border2 Border3 Border4 route PortGolfo PortPacifico, pa corr(unstructured)
		*outreg2 using "***", append ctitle(tasa Homicidios Relacionados)
	*Modelo 3
		*Con rivalidad
		xtpoisson hom_rate_riv  lnPIB ind_marginacion niv_pobreza mean_edu niv_sinprim niv_desempleo coef_gini tomasC Laboratorios erad_mar erad_pop  Imar Icoca Iopi Border1 Border2 Border3 Border4 route PortGolfo PortPacifico, pa corr(unstructured)
		*outreg2 using "***", append ctitle(tasa Homicidios Relacionados)
	*Modelo 4
		*Con Lag
		xtpoisson hom_rate_riv  lnPIB ind_marginacion niv_pobreza mean_edu niv_sinprim niv_desempleo coef_gini tomasC Laboratorios erad_mar erad_pop  Imar Icoca Iopi Border1 Border2 Border3 Border4 route PortGolfo PortPacifico L.eficiencia L.WEFICI L.hom_rate_riv L.SPATIALLAG, pa corr(unstructured)
		*outreg2 using "***", append ctitle(tasa Homicidios Relacionados)


*Tabla 2) Análisis de la competencia - *estructura control cluster municipal socioeconómico ¿usarlo?
	*Modelo 5  - **con corr unstructured no se logra convergencia
		xtpoisson co_count  Border1 Border2 Border3 Border4 route PortGolfo PortPacifico Icoca Imar Iopi Laboratorios tomasC erad_mar erad_pop  e1 e2 e3, re 
		*outreg2 using 2.doc, replace title(Causas de competencia) 
	*Modelo 6  - **con corr unstructured no se logra convergencia
		xtpoisson co_count Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop e1 e2 e3, re
		*outreg2 using 2.doc, append ctitle(Competencia con Dummy)
	*Modelo 7
		xtlogit conflicto_12 Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop e1 e2 e3, pa corr(unstructured)
		*outreg2 using 2.doc, append ctitle(Conflicto)
	*Modelo 8	
		xtlogit alianza_sinconf Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop e1 e2 e3, pa corr(unstructured)
		*outreg2 using 2.doc, append ctitle(Alianza_NC)
	*Modelo 9
		xtlogit  violentconflict Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop e1 e2 e3 if alianza_sinconf==0 , pa corr(unstructured)
		*outreg2 using 2.doc, append ctitle(Conflicto Violento)
	*Modelo 10
		xtpoisson agresion_a_militares Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop e1 e2 e3, pa corr(unstructured)
		*outreg2 using 2.doc, append ctitle(agresion_a_militares)
	*Modelo 11	*homicidios con control de municipios.
		xtpoisson hom_rate_riv Border1 Border2 Border3 Border4 route PortGolfo PortPacifico coca mar opi lab tomas e_mar e_pop e1 e2 e3, pa corr(unstructured)



************************************************************************************************************************************************		
		
********Análisis de cluster********;
***********************************;
clear
use "tesisDB.dta"
xtset code2 year

*Clusterización -se realizó con año base 2010-
	keep if year==2010


******************************************
*Clusterización Socioeconómica -Wardslinkage-;
	cluster wardslinkage lnPIB PrP15PRIM mean_edu lnUnemployment Coeficiente_de_Gini, measure(L2) name(cluster1)
	cluster dendrogram cluster1, cutnumber(10)
	cluster generate estructura_= groups(3), name(cluster1) ties(error)
	drop cluster1_id cluster1_ord cluster1_hgt 
	*tabstat PIB Unemployment mean_edu inequality if year==2008, statistics( mean var median ) by(estructura) columns(variables)

************************************************
*Clusterización sobre violencia e impunidad -Wardslinkage-;
	cluster  wardslinkage  LWhom_rate_riv Lhom_rate_riv Leficiencia LWeficiencia, measure(L2) name(cluster6)
	cluster dendrogram cluster6, cutnumber(5)
	cluster generate categoriaviolencia2= groups(3), name(cluster6) ties(error)
	table categoriaviolencia 
		*re-ordenamiento por numero de homicidios;
		replace categoriaviolencia=4 if categoriaviolencia==1
		replace categoriaviolencia=1 if categoriaviolencia==2
		replace categoriaviolencia=2 if categoriaviolencia==4
		table categoriaviolencia
		drop cluster5_id cluster5_ord cluster5_hgt 


		
******************
*Sequia
******************
	*Diferencia en Diferencias de la sequia-> con kernel Maching
	*Homicidios
		global xlist  km2 lnPIB lnUnemployment ind_marginacion niv_sinprim niv_pobreza niv_rezago_edu coef_gini
		global Border1 Border2 Border3 route PortPacifico
		global estructura
	*diferencia en diferencias
	diff erad_marijuana, t(tratamientosequiamar) p(ttratamientosequiamar) cov($xlist) id(code) kernel rcs  qdid(.5) 

	
	
	
