cd "C:\Users\51943\Videos\final"


use "resultado1.dta",clear

**Procesamiento de la información: 

drop if missing(Pobj)
drop if missing(distij)
gen ln_distij     = ln(distij)
gen ln_taoPBIi    = ln(taoPBI)
gen ln_areai      = ln(areai) 
gen ln_areaj      = ln(areaj) 
gen ln_Pobj       = ln(Pobj) 
gen ln_Pobi       = ln(Pobi) 
gen front_ln_distij  = front * ln_distij
gen front_ln_Pobi    = front * ln_Pobi
gen front_ln_Pobj    = front * ln_Pobj
gen front_ln_areai   = front * ln_areai
gen front_ln_areaj   = front * ln_areaj
gen front_sinsalidaalmar    = front * sinsalidaalmar


gen ln_pbi_percap_i=ln(pbi_percap_i)


**** Se procede a hacer 3 operaciones:

*Correr el modelo como si fueran dos regresiones, es decir el sistema no reconoce que una viene despues de la otra 


**** primer reg, se obtieene el t_hat o t estimado 
reg ln_taoPBIi ln_distij ln_areai ln_areaj ln_Pobi ln_Pobj sinsalidaalmar front front_ln_distij front_ln_Pobi front_ln_Pobj front_ln_areai front_ln_areaj front_sinsalidaalmar i.year, vce(robust)



predict ln_fitted
gen fitted = exp(ln_fitted)
collapse (sum) T_hat = fitted, by(paisi year ln_pbi_percap_i ln_Pobi ln_areai )

** segundo reg  se procede a regresionar  la segunda ecuación
reg ln_pbi_percap_i T_hat ln_Pobi ln_areai i.year , vce(robust)

outreg2 using resultados_nrml.doc, replace ///
	keep(T_hat ln_Pobi ln_areai ) ///
    ctitle("2 regresiones ") ///
    se bdec(3) tdec(3) stats(coef se tstat pval) ///
    addnote("Errores estándar robustos entre paréntesis") 
	

**¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨
*Segunda operación se corre el modelo como si fuera un modelo de variable instrumental y se quiere endogenisar a taoPBI , aca se pueden evaluar que tan bien es el instrumento.



ivregress 2sls ln_pbi_percap_i ///
    (ln_taoPBIi = ln_distij ln_areai ln_areaj ln_Pobi ln_Pobj sinsalidaalmar ///
                 front front_ln_distij front_ln_Pobi front_ln_Pobj ///
                 front_ln_areai front_ln_areaj front_sinsalidaalmar) ///
    ln_Pobi ln_areai i.year, vce(robust)
	

* Test de endogeneidad de ln_taoPBIi (Hausman)
estat endogenous

* Test de relevancia de los instrumentos (primera etapa)
estat firststage

* Test de sobreidentificación (validez de instrumentos) 
estat overid
	
outreg2 using resultados_iv.doc, replace ///
	keep(ln_pbi_percap_i ln_taoPBIi  ln_Pobi ln_areai ln_distij ln_areai ln_areaj ln_Pobi ln_Pobj sinsalidaalmar  front front_ln_distij front_ln_Pobi front_ln_Pobj  front_ln_areai front_ln_areaj front_sinsalidaalmar ) ///
    ctitle("2SLS - ln_pbi_percap_i") ///
    se bdec(3) tdec(3) stats(coef se tstat pval) ///
    addnote("Errores estándar robustos entre paréntesis") 
	

*** ultimo modo  , el del paper 

**primero generaremos el estimador 
*** primer reg, se obtieene el t_hat o t estimado 
reg ln_taoPBIi ln_distij ln_areai ln_areaj ln_Pobi ln_Pobj sinsalidaalmar front front_ln_distij front_ln_Pobi front_ln_Pobj front_ln_areai front_ln_areaj front_sinsalidaalmar i.year, vce(robust)



predict ln_fitted
gen fitted = exp(ln_fitted)
collapse (sum) T_hat = fitted, by(paisi year ln_pbi_percap_i ln_Pobi ln_areai ln_taoPBIi )


*luego dado que el t hat va funcionar como la variable que va limpiar el sesgo procedemos a correr como si fuera una regresion de dos etapas. 


** generar variable al cuadrado para ver efecto no lineal

gen t_2= (ln_taoPBIi)^2

ivregress 2sls ln_pbi_percap_i ///
    (ln_taoPBIi = T_hat) ///
    ln_Pobi ln_areai i.year, vce(robust)

* Test de endogeneidad de ln_taoPBIi (Hausman)
estat endogenous

* Test de relevancia de los instrumentos (primera etapa)
estat firststage

	

outreg2 using paper.doc, replace ///
	keep(ln_taoPBIi T_hat ln_Pobi ln_areai t_2 ) ///
    ctitle("1lz") ///
    se bdec(3) tdec(3) stats(coef se tstat pval) ///
    addnote("Errores estándar robustos entre paréntesis") 




