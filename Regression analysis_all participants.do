******************************************************************************** 
**------------------------File: HB Regression analysis------------------------**
**--------------------------Author: William Rudgard---------------------------**
******************************************************************************** 

/*
EXPOSURE: 
Food security: fod_i
Motherhood: hbfilt_i

MATERNAL OUTCOMES: 
Transactional sex: tsx6m_i
Multiple sexual partners: sxprt2_i
Age disparate sex (5 Years older): sxdisp_i
Condomless sex: sxunpr_i 
Sex on substances: sxsubs_i
Alcohol use: alchl_i
School enrollment: scdrop_i
*/ 

*------------------------------------------------------------------------------*
/// Correlation across multiple outcomes
*------------------------------------------------------------------------------*

bysort hbfilt_i : spearman ///
/*Sexual risk*/ sxprt2_i tsx6m_i sxdisp_i sxunpr_i ///
/*Antisocial behaviour*/ sxsubs_i alchl_i ///
/*Education*/ neet_i, pw

bysort hbfilt_i : pwcorr ///
/*Sexual risk*/ sxprt2_i tsx6m_i sxdisp_i sxunpr_i ///
/*Antisocial behaviour*/ sxsubs_i alchl_i ///
/*Education*/ neet_i

* Follow-ups
tab sxprt2_i tsx6m_i, row chi

* Factor analysis
forvalues i = 0/1{
factor ///
/*Sexual risk*/ sxprt2_i tsx6m_i sxdisp_i sxunpr_i ///
/*Antisocial behaviour*/ sxsubs_i alchl_i ///
/*Education*/ neet_i if hbfilt_i == `i' , ipf factor(2)
scree
rotate, promax horst blanks(.3)
}

*------------------------------------------------------------------------------*
/// Run univariable regression for motherhood
*------------------------------------------------------------------------------*

*---Binary outcomes
foreach var in ////
/*Sexual risk*/ sxprt2_i tsx6m_i sxdisp_i sxunpr_i ///
/*Antisocial behaviour*/ sxsubs_i alchl_i ///
/*Education*/ neet_i{
logistic `var' hbfilt_i
quietly lincom  _b[hbfilt_i], or 
matrix UNIM`var' = (r(estimate), r(lb), r(ub), r(p))
}

matrix UNIMAll = (UNIMsxprt2_i, UNIMtsx6m_i, UNIMsxdisp_i, UNIMsxunpr_i, UNIMsxsubs_i, UNIMalchl_i, UNIMneet_i)
matrix list UNIMAll

*------------------------------------------------------------------------------*
/// Run univariable regression for food security
*------------------------------------------------------------------------------*

*---Binary outcomes
foreach var in ////
/*Sexual risk*/ sxprt2_i tsx6m_i sxdisp_i sxunpr_i ///
/*Antisocial behaviour*/ sxsubs_i alchl_i ///
/*Education*/ neet_i{
logistic `var' fod_i
quietly lincom  _b[fod_i], or 
matrix UNIF`var' = (r(estimate), r(lb), r(ub), r(p))
}

matrix UNIFAll = (UNIFsxprt2_i, UNIFtsx6m_i, UNIFsxdisp_i, UNIFsxunpr_i, UNIFsxsubs_i, UNIFalchl_i, UNIFneet_i)
matrix UNIALL = (UNIMAll \ UNIFAll)
matrix list UNIALL

*------------------------------------------------------------------------------*
/// Run multivariable regression - GEE Approach
*------------------------------------------------------------------------------*

* Rename outcome variables for reshaping data from wide to long.
local varlist "sxprt2_i" "tsx6m_i" "sxdisp_i" "sxunpr_i" "sxsubs_i" "alchl_i" "neet_i"
forvalues n = 1/7{
local var : word `n' of "`varlist'"
di "`var'"
gen sxrisk`n' = `var'
tab sxrisk`n', m
}

* Reshape data from wide to long so that each individual i has k rows 
* corresponding to outcome j1 to jk.
reshape long sxrisk, i(ID) j(outcome)

* Generate binary indicators of outcomes j1 to jk in long data, and interaction
* with key predictor of interest, adolescent motherhood.
local varlist "sxprt2_i" "tsx6m_i" "sxdisp_i" "sxunpr_i" "sxsubs_i" "alchl_i" "neet_i"
forvalues n = 1/7{
local var : word `n' of "`varlist'"
replace `var' = 1 if outcome == `n'
replace `var' = 0 if outcome != `n' & `var' != .
tab `var'
gen hbfilt_ix`var' = hbfilt_i * `var'
gen fod_ix`var' = fod_i * `var'
gen age_cx`var' = age_c * `var'
gen hiv_ix`var' = hiv_i * `var'
gen bfgf_ix`var' = bfgf_i * `var'
gen monp_cx`var' = monp_c * `var'
gen rural_ix`var' = rural_i * `var'
gen house_ix`var' = house_i * `var'
gen hhsiz_cx`var' = hhsiz_c * `var'
gen morph_ix`var' = morph_i * `var'
gen porph_ix`var' = porph_i * `var'
}

* xtset data
xtset ID outcome

* MARGINAL MODEL (GEE) WITH LOGIT LINK & UNSTRUCTURED CORRELATION (MULTIPLE OUTCOME APPROACH WITH NO FOOD INTERACTION)
quietly xtgee sxrisk ///
i.hbfilt_ixsxprt2_i i.hiv_ixsxprt2_i i.fod_ixsxprt2_i c.age_cxsxprt2_i i.bfgf_ixsxprt2_i c.monp_cxsxprt2_i i.rural_ixsxprt2_i i.house_ixsxprt2_i c.hhsiz_cxsxprt2_i i.morph_ixsxprt2_i i.porph_ixsxprt2_i ///
i.hbfilt_ixtsx6m_i i.hiv_ixtsx6m_i i.fod_ixtsx6m_i c.age_cxtsx6m_i i.bfgf_ixtsx6m_i c.monp_cxtsx6m_i i.rural_ixtsx6m_i i.house_ixtsx6m_i c.hhsiz_cxtsx6m_i i.morph_ixtsx6m_i i.porph_ixtsx6m_i ///
i.hbfilt_ixsxdisp_i i.hiv_ixsxdisp_i i.fod_ixsxdisp_i c.age_cxsxdisp_i i.bfgf_ixsxdisp_i c.monp_cxsxdisp_i i.rural_ixsxdisp_i i.house_ixsxdisp_i c.hhsiz_cxsxdisp_i i.morph_ixsxdisp_i i.porph_ixsxdisp_i ///
i.hbfilt_ixsxunpr_i i.hiv_ixsxunpr_i i.fod_ixsxunpr_i c.age_cxsxunpr_i i.bfgf_ixsxunpr_i c.monp_cxsxunpr_i i.rural_ixsxunpr_i i.house_ixsxunpr_i c.hhsiz_cxsxunpr_i i.morph_ixsxunpr_i i.porph_ixsxunpr_i ///
i.hbfilt_ixsxsubs_i i.hiv_ixsxsubs_i i.fod_ixsxsubs_i c.age_cxsxsubs_i i.bfgf_ixsxsubs_i c.monp_cxsxsubs_i i.rural_ixsxsubs_i i.house_ixsxsubs_i c.hhsiz_cxsxsubs_i i.morph_ixsxsubs_i i.porph_ixsxsubs_i ///
i.hbfilt_ixalchl_i i.hiv_ixalchl_i i.fod_ixalchl_i c.age_cxalchl_i i.bfgf_ixalchl_i c.monp_cxalchl_i i.rural_ixalchl_i i.house_ixalchl_i c.hhsiz_cxalchl_i i.morph_ixalchl_i i.porph_ixalchl_i ///
i.hbfilt_ixneet_i i.hiv_ixneet_i i.fod_ixneet_i c.age_cxneet_i i.bfgf_ixneet_i c.monp_cxneet_i i.rural_ixneet_i i.house_ixneet_i c.hhsiz_cxneet_i i.morph_ixneet_i i.porph_ixneet_i ///
i.outcome, /*nocons*/ family(binomial) link(logit) robust corr(unstructured) eform
est store model1
* CHECK FOR INTERACTION WITH HIV status
xtgee sxrisk ///
i.hbfilt_ixsxprt2_i##i.hiv_ixsxprt2_i i.fod_ixsxprt2_i c.age_cxsxprt2_i i.bfgf_ixsxprt2_i c.monp_cxsxprt2_i i.rural_ixsxprt2_i i.house_ixsxprt2_i c.hhsiz_cxsxprt2_i i.morph_ixsxprt2_i i.porph_ixsxprt2_i ///
i.hbfilt_ixtsx6m_i##i.hiv_ixtsx6m_i i.fod_ixtsx6m_i c.age_cxtsx6m_i i.bfgf_ixtsx6m_i c.monp_cxtsx6m_i i.rural_ixtsx6m_i i.house_ixtsx6m_i c.hhsiz_cxtsx6m_i i.morph_ixtsx6m_i i.porph_ixtsx6m_i ///
i.hbfilt_ixsxdisp_i##i.hiv_ixsxdisp_i i.fod_ixsxdisp_i c.age_cxsxdisp_i i.bfgf_ixsxdisp_i c.monp_cxsxdisp_i i.rural_ixsxdisp_i i.house_ixsxdisp_i c.hhsiz_cxsxdisp_i i.morph_ixsxdisp_i i.porph_ixsxdisp_i ///
i.hbfilt_ixsxunpr_i##i.hiv_ixsxunpr_i i.fod_ixsxunpr_i c.age_cxsxunpr_i i.bfgf_ixsxunpr_i c.monp_cxsxunpr_i i.rural_ixsxunpr_i i.house_ixsxunpr_i c.hhsiz_cxsxunpr_i i.morph_ixsxunpr_i i.porph_ixsxunpr_i ///
i.hbfilt_ixsxsubs_i##i.hiv_ixsxsubs_i i.fod_ixsxsubs_i c.age_cxsxsubs_i i.bfgf_ixsxsubs_i c.monp_cxsxsubs_i i.rural_ixsxsubs_i i.house_ixsxsubs_i c.hhsiz_cxsxsubs_i i.morph_ixsxsubs_i i.porph_ixsxsubs_i ///
i.hbfilt_ixalchl_i##i.hiv_ixalchl_i i.fod_ixalchl_i c.age_cxalchl_i i.bfgf_ixalchl_i c.monp_cxalchl_i i.rural_ixalchl_i i.house_ixalchl_i c.hhsiz_cxalchl_i i.morph_ixalchl_i i.porph_ixalchl_i ///
i.hbfilt_ixneet_i##i.hiv_ixneet_i i.fod_ixneet_i c.age_cxneet_i i.bfgf_ixneet_i c.monp_cxneet_i i.rural_ixneet_i i.house_ixneet_i c.hhsiz_cxneet_i i.morph_ixneet_i i.porph_ixneet_i ///
i.outcome, /*nocons*/ family(binomial) link(logit) robust corr(unstructured) eform
est store model2
* TABULATE RESULTS
local outcomes "sxprt2_i" "tsx6m_i" "sxdisp_i" "sxunpr_i" "sxsubs_i" "alchl_i" "neet_i"
forvalues i = 1/7 {
est restore model1
local var : word `i' of "`outcomes'"
quietly lincom  _b[1.hbfilt_ix`var'], or 
matrix GEEM`var' = (r(estimate), r(lb), r(ub), r(p))
quietly lincom  _b[1.hiv_ix`var'], or 
matrix GEEH`var' = (r(estimate), r(lb), r(ub), r(p))
est restore model2
quietly lincom _b[1.hbfilt_ix`var'], or 
matrix GEEMNHIV`var' = (r(estimate), r(lb), r(ub), r(p))
quietly lincom _b[1.hbfilt_ix`var'] + _b[1.hbfilt_ix`var'#1.hiv_ix`var'], or 
matrix GEEMHIV`var' = (r(estimate), r(lb), r(ub), r(p))
testnl _b[1.hbfilt_ix`var'#1.hiv_ix`var']=0
matrix GEEMHIVWALD`var' =  r(p)
* For details see: https://www.stata.com/statalist/archive/2010-01/msg00427.html
}

* Generate table of odds ratios
* Motherhood
matrix GEEMOAll = (GEEMsxprt2_i, GEEMtsx6m_i, GEEMsxdisp_i, GEEMsxunpr_i, GEEMsxsubs_i, GEEMalchl_i, GEEMneet_i)
matrix GEEMNHIVAll = (GEEMNHIVsxprt2_i, GEEMNHIVtsx6m_i, GEEMNHIVsxdisp_i, GEEMNHIVsxunpr_i, GEEMNHIVsxsubs_i, GEEMNHIValchl_i, GEEMNHIVneet_i)
matrix GEEMHIVAll = (GEEMHIVsxprt2_i, GEEMHIVtsx6m_i, GEEMHIVsxdisp_i, GEEMHIVsxunpr_i, GEEMHIVsxsubs_i, GEEMHIValchl_i, GEEMHIVneet_i)
matrix GEEMAll = (GEEMOAll \ GEEMNHIVAll \ GEEMHIVAll)
matrix list GEEMAll
matrix GEEMHIVWALD = (GEEMHIVWALDsxprt2_i, GEEMHIVWALDtsx6m_i, GEEMHIVWALDsxdisp_i, GEEMHIVWALDsxunpr_i, GEEMHIVWALDsxsubs_i, GEEMHIVWALDalchl_i, GEEMHIVWALDneet_i)
matrix list GEEMHIVWALD

*------------------------------------------------------------------------------*
/// Run multivariable regression - GEE Approach with Interaction
*------------------------------------------------------------------------------*

* MARGINAL MODEL (GEE) WITH LOGIT LINK & UNSTRUCTURED CORRELATION (MULTIPLE OUTCOME APPROACH WITH FOOD INTERACTION)
xtgee sxrisk ///
i.fod_ixsxprt2_i i.hbfilt_ixsxprt2 c.age_cxsxprt2_i i.hiv_ixsxprt2_i i.bfgf_ixsxprt2_i c.monp_cxsxprt2_i i.rural_ixsxprt2_i i.house_ixsxprt2_i c.hhsiz_cxsxprt2_i i.morph_ixsxprt2_i i.porph_ixsxprt2_i ///
i.fod_ixtsx6m_i i.hbfilt_ixtsx6m_i c.age_cxtsx6m_i i.hiv_ixtsx6m_i i.bfgf_ixtsx6m_i c.monp_cxtsx6m_i i.rural_ixtsx6m_i i.house_ixtsx6m_i c.hhsiz_cxtsx6m_i i.morph_ixtsx6m_i i.porph_ixtsx6m_i ///
i.fod_ixsxdisp_i i.hbfilt_ixsxdisp_i c.age_cxsxdisp_i i.hbfilt_ixsxdisp_i##i.hiv_ixsxdisp_i i.bfgf_ixsxdisp_i c.monp_cxsxdisp_i i.rural_ixsxdisp_i i.house_ixsxdisp_i c.hhsiz_cxsxdisp_i i.morph_ixsxdisp_i i.porph_ixsxdisp_i ///
i.fod_ixsxunpr_i i.hbfilt_ixsxunpr_i c.age_cxsxunpr_i i.hiv_ixsxunpr_i i.bfgf_ixsxunpr_i c.monp_cxsxunpr_i i.rural_ixsxunpr_i i.house_ixsxunpr_i c.hhsiz_cxsxunpr_i i.morph_ixsxunpr_i i.porph_ixsxunpr_i ///
i.fod_ixsxsubs_i i.hbfilt_ixsxsubs_i c.age_cxsxsubs_i i.hiv_ixsxsubs_i i.bfgf_ixsxsubs_i c.monp_cxsxsubs_i i.rural_ixsxsubs_i i.house_ixsxsubs_i c.hhsiz_cxsxsubs_i i.morph_ixsxsubs_i i.porph_ixsxsubs_i ///
i.fod_ixalchl_i i.hbfilt_ixalchl_i c.age_cxalchl_i i.hiv_ixalchl_i i.bfgf_ixalchl_i c.monp_cxalchl_i i.rural_ixalchl_i i.house_ixalchl_i c.hhsiz_cxalchl_i i.morph_ixalchl_i i.porph_ixalchl_i ///
i.fod_ixneet_i i.hbfilt_ixneet_i c.age_cxneet_i i.hbfilt_ixneet_i##i.hiv_ixneet_i i.bfgf_ixneet_i c.monp_cxneet_i i.rural_ixneet_i i.house_ixneet_i c.hhsiz_cxneet_i i.morph_ixneet_i i.porph_ixneet_i ///
i.outcome, /*nocons*/ family(binomial) link(logit) robust corr(unstructured) eform
display e(N_g)
est store model3
* CHECK FOR INTERACTION WITH MOTHERHOOD
xtgee sxrisk ///
i.fod_ixsxprt2_i##i.hbfilt_ixsxprt2 c.age_cxsxprt2_i i.hiv_ixsxprt2_i i.bfgf_ixsxprt2_i c.monp_cxsxprt2_i i.rural_ixsxprt2_i i.house_ixsxprt2_i c.hhsiz_cxsxprt2_i i.morph_ixsxprt2_i i.porph_ixsxprt2_i ///
i.fod_ixtsx6m_i##i.hbfilt_ixtsx6m_i c.age_cxtsx_i i.hiv_ixtsx6m_i i.bfgf_ixtsx6m_i c.monp_cxtsx6m_i i.rural_ixtsx6m_i i.house_ixtsx6m_i c.hhsiz_cxtsx6m_i i.morph_ixtsx6m_i i.porph_ixtsx6m_i ///
i.fod_ixsxdisp_i##i.hbfilt_ixsxdisp_i c.age_cxsxdisp_i i.hbfilt_ixsxdisp_i##i.hiv_ixsxdisp_i i.bfgf_ixsxdisp_i c.monp_cxsxdisp_i i.rural_ixsxdisp_i i.house_ixsxdisp_i c.hhsiz_cxsxdisp_i i.morph_ixsxdisp_i i.porph_ixsxdisp_i ///
i.fod_ixsxunpr_i##i.hbfilt_ixsxunpr_i c.age_cxsxunpr_i i.hiv_ixsxunpr_i i.bfgf_ixsxunpr_i c.monp_cxsxunpr_i i.rural_ixsxunpr_i i.house_ixsxunpr_i c.hhsiz_cxsxunpr_i i.morph_ixsxunpr_i i.porph_ixsxunpr_i ///
i.fod_ixsxsubs_i##i.hbfilt_ixsxsubs_i c.age_cxsxsubs_i i.hiv_ixsxsubs_i i.bfgf_ixsxsubs_i c.monp_cxsxsubs_i i.rural_ixsxsubs_i i.house_ixsxsubs_i c.hhsiz_cxsxsubs_i i.morph_ixsxsubs_i i.porph_ixsxsubs_i ///
i.fod_ixalchl_i##i.hbfilt_ixalchl_i c.age_cxalchl_i i.hiv_ixalchl_i i.bfgf_ixalchl_i c.monp_cxalchl_i i.rural_ixalchl_i i.house_ixalchl_i c.hhsiz_cxalchl_i i.morph_ixalchl_i i.porph_ixalchl_i ///
i.fod_ixneet_i##i.hbfilt_ixneet_i c.age_cxneet_i i.hbfilt_ixneet_i##i.hiv_ixneet_i i.bfgf_ixneet_i c.monp_cxneet_i i.rural_ixneet_i i.house_ixneet_i c.hhsiz_cxneet_i i.morph_ixneet_i i.porph_ixneet_i ///
i.outcome, /*nocons*/ family(binomial) link(logit) robust corr(unstructured) eform
display e(N_g)
est store model4
* CHECK FOR INTERACTION WITH MOTHERHOOD & HIV STATUS
xtgee sxrisk ///
i.fod_ixsxprt2_i##i.hbfilt_ixsxprt2##i.hiv_ixsxprt2_i c.age_cxsxprt2_i i.bfgf_ixsxprt2_i c.monp_cxsxprt2_i i.rural_ixsxprt2_i i.house_ixsxprt2_i c.hhsiz_cxsxprt2_i i.morph_ixsxprt2_i i.porph_ixsxprt2_i ///
i.fod_ixtsx6m_i##i.hbfilt_ixtsx6m_i##i.hiv_ixtsx6m_i c.age_cxtsx6m_i i.bfgf_ixtsx6m_i c.monp_cxtsx6m_i i.rural_ixtsx6m_i i.house_ixtsx6m_i c.hhsiz_cxtsx6m_i i.morph_ixtsx6m_i i.porph_ixtsx6m_i ///
i.fod_ixsxdisp_i##i.hbfilt_ixsxdisp_i##i.hiv_ixsxdisp_i c.age_cxsxdisp_i i.bfgf_ixsxdisp_i c.monp_cxsxdisp_i i.rural_ixsxdisp_i i.house_ixsxdisp_i c.hhsiz_cxsxdisp_i i.morph_ixsxdisp_i i.porph_ixsxdisp_i ///
i.fod_ixsxunpr_i##i.hbfilt_ixsxunpr_i##i.hiv_ixsxunpr_i c.age_cxsxunpr_i i.bfgf_ixsxunpr_i c.monp_cxsxunpr_i i.rural_ixsxunpr_i i.house_ixsxunpr_i c.hhsiz_cxsxunpr_i i.morph_ixsxunpr_i i.porph_ixsxunpr_i ///
i.fod_ixsxsubs_i##i.hbfilt_ixsxsubs_i##i.hiv_ixsxsubs_i c.age_cxsxsubs_i i.bfgf_ixsxsubs_i c.monp_cxsxsubs_i i.rural_ixsxsubs_i i.house_ixsxsubs_i c.hhsiz_cxsxsubs_i i.morph_ixsxsubs_i i.porph_ixsxsubs_i ///
i.fod_ixalchl_i##i.hbfilt_ixalchl_i##i.hiv_ixalchl_i c.age_cxalchl_i i.bfgf_ixalchl_i c.monp_cxalchl_i i.rural_ixalchl_i i.house_ixalchl_i c.hhsiz_cxalchl_i i.morph_ixalchl_i i.porph_ixalchl_i ///
i.fod_ixneet_i##i.hbfilt_ixneet_i##i.hiv_ixneet_i c.age_cxneet_i i.bfgf_ixneet_i c.monp_cxneet_i i.rural_ixneet_i i.house_ixneet_i c.hhsiz_cxneet_i i.morph_ixneet_i i.porph_ixneet_i ///
i.outcome, /*nocons*/ family(binomial) link(logit) robust corr(unstructured) eform coeflegend
est store model5
* TABULATE RESULTS
local outcomes "sxprt2_i" "tsx6m_i" "sxdisp_i" "sxunpr_i" "sxsubs_i" "alchl_i" "neet_i" 
forvalues i = 1/7 {
local var : word `i' of "`outcomes'"
est restore model3
quietly lincom  _b[1.fod_ix`var'], or 
matrix GEEF`var' = (r(estimate), r(lb), r(ub), r(p))
est restore model4
quietly lincom  _b[1.fod_ix`var'], or 
matrix GEEFNM`var' = (r(estimate), r(lb), r(ub), r(p))
quietly lincom  _b[1.fod_ix`var'] + _b[1.fod_ix`var'#1.hbfilt_ix`var'], or 
matrix GEEFAM`var' = (r(estimate), r(lb), r(ub), r(p))
testnl _b[1.fod_ix`var'#1.hbfilt_ix`var']=0
matrix GEEFMWALD`var' =  r(p)
quietly margins if outcome == `i', at(hbfilt_ix`var' = (0 1) fod_ix`var' = (0 1)) post
* No food in non-mothers
quietly lincom _b[1bn._at]
matrix NMGEE`var'1 = (r(estimate), r(lb), r(ub), r(p))
* No food in mothers
quietly lincom _b[2._at]
matrix MMGEE`var'1 = (r(estimate), r(lb), r(ub), r(p))
* Food in non-mothers
quietly lincom _b[3._at]
matrix NMGEE`var'2 = (r(estimate), r(lb), r(ub), r(p))
* Difference in non-mothers
quietly lincom _b[3._at] - _b[1bn._at]
matrix NMGEE`var'Dif = (r(estimate), r(lb), r(ub), r(p))
matrix NMGEE`var'All = (NMGEE`var'1 \ NMGEE`var'2 \ NMGEE`var'Dif)
* Food in mothers
quietly lincom _b[4._at]
matrix MMGEE`var'2 = (r(estimate), r(lb), r(ub), r(p))
* Difference in mothers
quietly lincom _b[4._at] - _b[2._at]
matrix MMGEE`var'Dif = (r(estimate), r(lb), r(ub), r(p))
matrix MMGEE`var'All = (MMGEE`var'1 \ MMGEE`var'2 \ MMGEE`var'Dif)
est restore model5
* Food in HIV-ve non-mothers
quietly lincom  _b[1.fod_ix`var'], or 
matrix GEEFNMNHIV`var' = (r(estimate), r(lb), r(ub), r(p))
* Food in HIV+ve non-mothers
quietly lincom  _b[1.fod_ix`var'] + _b[1.fod_ix`var'#1.hiv_ix`var'], or 
matrix GEEFNMHIV`var' = (r(estimate), r(lb), r(ub), r(p))
* Food in HIV-ve mothers
quietly lincom  _b[1.fod_ix`var'] + _b[1.fod_ix`var'#1.hbfilt_ix`var'], or 
matrix GEEFMMNHIV`var' = (r(estimate), r(lb), r(ub), r(p))
* Food in HIV+ve mothers
quietly lincom  _b[1.fod_ix`var'] + _b[1.fod_ix`var'#1.hiv_ix`var'] + _b[1.fod_ix`var'#1.hbfilt_ix`var'] + _b[1.fod_ix`var'#1.hbfilt_ix`var'#1.hiv_ix`var'], or 
matrix GEEFMMHIV`var' = (r(estimate), r(lb), r(ub), r(p))
testnl _b[1.fod_ix`var'#1.hbfilt_ix`var'#1.hiv_ix`var']=0
matrix GEEFMHIVWALD`var' =  r(p)
}

* Generate table of odds ratios
matrix GEEFAll = (GEEFsxprt2_i, GEEFtsx6m_i, GEEFsxdisp_i, GEEFsxunpr_i, GEEFsxsubs_i, GEEFalchl_i, GEEFneet_i)
matrix GEEFNMAll = (GEEFNMsxprt2_i, GEEFNMtsx6m_i, GEEFNMsxdisp_i, GEEFNMsxunpr_i, GEEFNMsxsubs_i, GEEFNMalchl_i, GEEFNMneet_i)
matrix GEEFMMAll = (GEEFAMsxprt2_i, GEEFAMtsx6m_i, GEEFAMsxdisp_i, GEEFAMsxunpr_i, GEEFAMsxsubs_i, GEEFAMalchl_i, GEEFAMneet_i)
matrix GEEFNMNHIVAll = (GEEFNMNHIVsxprt2_i, GEEFNMNHIVtsx6m_i, GEEFNMNHIVsxdisp_i, GEEFNMNHIVsxunpr_i, GEEFNMNHIVsxsubs_i, GEEFNMNHIValchl_i, GEEFNMNHIVneet_i)
matrix GEEFNMHIVAll = (GEEFNMHIVsxprt2_i, GEEFNMHIVtsx6m_i, GEEFNMHIVsxdisp_i, GEEFNMHIVsxunpr_i, GEEFNMHIVsxsubs_i, GEEFNMHIValchl_i, GEEFNMHIVneet_i)
matrix GEEFMMNHIVAll = (GEEFMMNHIVsxprt2_i, GEEFMMNHIVtsx6m_i, GEEFMMNHIVsxdisp_i, GEEFMMNHIVsxunpr_i, GEEFMMNHIVsxsubs_i, GEEFMMNHIValchl_i, GEEFMMNHIVneet_i)
matrix GEEFMMHIVAll = (GEEFMMHIVsxprt2_i, GEEFMMHIVtsx6m_i, GEEFMMHIVsxdisp_i, GEEFMMHIVsxunpr_i, GEEFMMHIVsxsubs_i, GEEFMMHIValchl_i, GEEFMMHIVneet_i)
matrix GEEMAll = (GEEFAll \ GEEFNMAll \ GEEFMMAll \ GEEFNMNHIVAll \ GEEFNMHIVAll \ GEEFMMNHIVAll \ GEEFMMHIVAll)
matrix list GEEMAll

matrix GEEFMWALDAll = (GEEFMWALDsxprt2_i, GEEFMWALDtsx6m_i, GEEFMWALDsxdisp_i, GEEFMWALDsxunpr_i, GEEFMWALDsxsubs_i, GEEFMWALDalchl_i, GEEFMWALDneet_i)
matrix list GEEFMWALDAll

matrix GEEFMHIVWALDAll = (GEEFMHIVWALDsxprt2_i, GEEFMHIVWALDtsx6m_i, GEEFMHIVWALDsxdisp_i, GEEFMHIVWALDsxunpr_i, GEEFMHIVWALDsxsubs_i, GEEFMHIVWALDalchl_i, GEEFMHIVWALDneet_i)
matrix list GEEFMHIVWALDAll 

* Generate table of predicted probabilities
matrix NMGEEAll = (NMGEEsxprt2_iAll, NMGEEtsx6m_iAll, NMGEEsxdisp_iAll, NMGEEsxunpr_iAll, NMGEEsxsubs_iAll, NMGEEalchl_iAll, NMGEEneet_iAll)
matrix MMGEEAll = (MMGEEsxprt2_iAll, MMGEEtsx6m_iAll, MMGEEsxdisp_iAll, MMGEEsxunpr_iAll, MMGEEsxsubs_iAll, MMGEEalchl_iAll, MMGEEneet_iAll)
matrix FINAL = (NMGEEAll \ MMGEEAll)
matrix list FINAL

*--------------------------------------End-------------------------------------*
