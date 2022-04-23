ADH 2019 Replication
================
Michael KellerBenedikt Marxer
25 4 2022

-   [About ADH 2019](#about-adh-2019)
-   [Setup](#setup)
-   [IV Approach](#iv-approach)
    -   [Effect on Employment Status](#effect-on-employment-status)
    -   [Earnings](#earnings)
    -   [Idleness](#idleness)
    -   [Mortality](#mortality)
    -   [Marriage](#marriage)
-   [Bartik.weight Package](#bartikweight-package)

# About ADH 2019

Autor, David, David Dorn, and Gordon Hanson. 2019. “When Work
Disappears: Manufacturing Decline and the Falling Marriage Market Value
of Young Men.” *American Economic Review: Insights*, 1 (2): 161-78. DOI:
<https://doi.org/10.1257/aeri.20180010>

# Setup

Before running, install the bartik.weight package (Source:
<https://github.com/paulgp/bartik-weight/tree/master/R-code>):

``` r
# install.packages("devtools")
devtools::install_github("paulgp/bartik-weight/R-code/pkg")
```

Load all relevant packages:

``` r
library(tidyverse)
library(haven)
library(bartik.weight)
library(AER)
library(stargazer)
```

Load all the data files that are needed for the replication. The data
frames are named following the Bartik package nomenclature.

``` r
## Main data set
# Source: https://www.ddorn.net/data
temp <- tempfile()
download.file("https://www.ddorn.net/data/ADH-WWD-FileArchive.zip", temp, mode = "wb")
master <- read_dta(unz(temp, "ADH-WWD-AERi-Data/dta/workfile9014wwd.dta"))
unlink(temp)

## ADH China Shock: "Global" Industry-level Growth of Imports (year-industry level)
# Source: https://github.com/paulgp/bartik-weight
ADH_global <- bartik.weight::ADH_global

## ADH China Shock: Local Industry Shares (community-year-industry level)
# Source: https://github.com/paulgp/bartik-weight
ADH_local <- bartik.weight::ADH_local
```

# IV Approach

1st stage: *x* = *a*<sub>0</sub> + *a*<sub>1</sub> \* *B* + *n*, with
*B* = *z* \* *g*  
2nd stage: *y* = *b*<sub>0</sub> + *b*<sub>1</sub> \* *x* + *e*

*x* = Change in import exposure -> `master$d_impusch_p9` (endogen)  
*y* = change manufacturing employment -> `master$d_sh_empl_mfg_age1839`
(outcome)  
*z* = Chinese export growth -> `master$d_impotch_p9_lag` (instrument for
endogenous x)

## Effect on Employment Status

Estimated Impact of Manufacturing Trade Shock on Manufacturing
Employment by Gender and Gender Differential in Employment Status,
Earnings, and Idleness, 1990–2014

*I. Overall trade shock*

**M+F**

``` r
mainshock_mf <- ivreg(formula = d_sh_empl_mfg_age1839 ~ d_impusch_p9 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotch_p9_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)
```

**M**

``` r
mainshock_m <- ivreg(formula = d_sh_empl_mfg_age1839m ~ d_impusch_p9 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotch_p9_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)
```

**F**

``` r
mainshock_f <- ivreg(formula = d_sh_empl_mfg_age1839f ~ d_impusch_p9 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotch_p9_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)
```

***Summary Mainshock***

``` r
stargazer(mainshock_mf, mainshock_m, mainshock_f, title = "I. Overall trade shock", dep.var.labels = c("M+F","M","F"), keep = c("d_impusch_p9"), style = "default", type = "text", dep.var.caption = "Manufacturing employment as a share of population, age 18–39", covariate.labels = "Change in Import Penetration")
```

    ## 
    ## I. Overall trade shock
    ## ==============================================================================================
    ##                                  Manufacturing employment as a share of population, age 18–39 
    ##                                 --------------------------------------------------------------
    ##                                         M+F                   M                    F          
    ##                                         (1)                  (2)                  (3)         
    ## ----------------------------------------------------------------------------------------------
    ## Change in Import Penetration         -1.058***            -0.993***            -1.091***      
    ##                                       (0.117)              (0.155)              (0.107)       
    ##                                                                                               
    ## ----------------------------------------------------------------------------------------------
    ## Observations                           1,444                1,444                1,444        
    ## R2                                     0.406                0.321                0.443        
    ## Adjusted R2                            0.398                0.312                0.435        
    ## Residual Std. Error (df = 1423)        0.054                0.072                0.050        
    ## ==============================================================================================
    ## Note:                                                              *p<0.1; **p<0.05; ***p<0.01

*II. Male industry versus female industry shock*

**M+F**

``` r
gendershock_mf <- ivreg(formula = d_sh_empl_mfg_age1839 ~ d_impuschm_p9cen + d_impuschf_p9cen + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotchm_p9cen_lag + d_impotchf_p9cen_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)
```

**M**

``` r
gendershock_m <- ivreg(formula = d_sh_empl_mfg_age1839m ~ d_impuschm_p9cen + d_impuschf_p9cen + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotchm_p9cen_lag + d_impotchf_p9cen_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)
```

**F**

``` r
gendershock_f <- ivreg(formula = d_sh_empl_mfg_age1839f ~ d_impuschm_p9cen + d_impuschf_p9cen + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotchm_p9cen_lag + d_impotchf_p9cen_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)
```

***Summary Gendershock***

``` r
stargazer(gendershock_mf, gendershock_m, gendershock_f, title = "II. Male industry versus female industry shock", dep.var.labels = c("M+F","M","F"), keep = c("d_impuschm_p9cen", "d_impuschf_p9cen"), style = "default", type = "text", dep.var.caption = "Manufacturing employment as a share of population, age 18–39", covariate.labels = c("IP x Male Ind Share", "IP x Female Ind Share"))
```

    ## 
    ## II. Male industry versus female industry shock
    ## ==============================================================================================
    ##                                  Manufacturing employment as a share of population, age 18–39 
    ##                                 --------------------------------------------------------------
    ##                                         M+F                   M                    F          
    ##                                         (1)                  (2)                  (3)         
    ## ----------------------------------------------------------------------------------------------
    ## IP x Male Ind Share                  -1.214***            -2.588***              0.198        
    ##                                       (0.328)              (0.441)              (0.307)       
    ##                                                                                               
    ## IP x Female Ind Share                 -0.881**              0.823              -2.559***      
    ##                                       (0.386)              (0.520)              (0.361)       
    ##                                                                                               
    ## ----------------------------------------------------------------------------------------------
    ## Observations                           1,444                1,444                1,444        
    ## R2                                     0.406                0.300                0.421        
    ## Adjusted R2                            0.397                0.290                0.412        
    ## Residual Std. Error (df = 1422)        0.054                0.073                0.051        
    ## ==============================================================================================
    ## Note:                                                              *p<0.1; **p<0.05; ***p<0.01

*III. Male-female differential by employment status, age 18–39*

**Employed**

``` r
mainshock_employment <- ivreg(formula = d_gender_gap_emp_1839 ~ d_impusch_p9 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotch_p9_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)
```

## Earnings

Estimated Impact of Manufacturing Trade Shock on Gender Differential in
Earnings, 1990-2014

**Median Earnings**

``` r
mainshock_meanearnings <- ivreg(formula = d_gender_gap_inc1839p50 ~ d_impusch_p9 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotch_p9_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)

gendershock_meanearnings <- ivreg(formula = d_gender_gap_inc1839p50 ~ d_impuschm_p9cen + d_impuschf_p9cen + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotchm_p9cen_lag + d_impotchf_p9cen_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)

stargazer(mainshock_meanearnings, gendershock_meanearnings, title = "Estimated Impact of Manufacturing Trade Shock on Gender Differential in Earnings, 1990-2014", dep.var.labels = c("Median Earnings","Median Earnings"), keep = c("d_impusch_p9", "d_impuschm_p9cen", "d_impuschf_p9cen"), style = "default", type = "text", dep.var.caption = "Male-female differential in annual earnings (USD), age 18–39", covariate.labels = c("Change in Import Penetration", "IP x Male Ind Share", "IP x Female Ind Share"), digits = 2)
```

    ## 
    ## Estimated Impact of Manufacturing Trade Shock on Gender Differential in Earnings, 1990-2014
    ## ==========================================================================================
    ##                              Male-female differential in annual earnings (USD), age 18–39 
    ##                              -------------------------------------------------------------
    ##                                                     Median Earnings                       
    ##                                           (1)                            (2)              
    ## ------------------------------------------------------------------------------------------
    ## Change in Import Penetration           -445.07***                                         
    ##                                         (141.60)                                          
    ##                                                                                           
    ## IP x Male Ind Share                                                  -2,945.00***         
    ##                                                                        (412.49)           
    ##                                                                                           
    ## IP x Female Ind Share                                                2,399.53***          
    ##                                                                        (486.07)           
    ##                                                                                           
    ## ------------------------------------------------------------------------------------------
    ## Observations                             1,444                          1,444             
    ## R2                                        0.50                           0.46             
    ## Adjusted R2                               0.49                           0.45             
    ## Residual Std. Error                65.57 (df = 1423)              68.15 (df = 1422)       
    ## ==========================================================================================
    ## Note:                                                          *p<0.1; **p<0.05; ***p<0.01

## Idleness

Estimated Impact of Manufacturing Trade Shock on Gender Differential in
Idleness, 1990–2014

**Employed**

``` r
mainshock_idleness_emp <- ivreg(formula = d_gender_gap_emp_1825 ~ d_impusch_p9 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotch_p9_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)

gendershock_idleness_emp <- ivreg(formula = d_gender_gap_emp_1825 ~ d_impuschm_p9cen + d_impuschf_p9cen + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotchm_p9cen_lag + d_impotchf_p9cen_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)
```

**Not employed, not in school**

``` r
mainshock_idleness_noempnoschool <- ivreg(formula = d_gender_gap_noedunoemp_1825 ~ d_impusch_p9 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotch_p9_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)

gendershock_idleness_noempnoschool <- ivreg(formula = d_gender_gap_noedunoemp_1825 ~ d_impuschm_p9cen + d_impuschf_p9cen + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotchm_p9cen_lag + d_impotchf_p9cen_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)

stargazer(mainshock_idleness_noempnoschool, gendershock_idleness_noempnoschool, title = "Estimated Impact of Manufacturing Trade Shock on Gender Differential in Idleness, 1990–2014", dep.var.labels = c("Not employed, not in school"), keep = c("d_impusch_p9", "d_impuschm_p9cen", "d_impuschf_p9cen"), style = "default", type = "text", dep.var.caption = "Male-female differential in idleness, age 18–25", covariate.labels = c("Change in Import Penetration", "IP x Male Ind Share", "IP x Female Ind Share"), digits = 2)
```

    ## 
    ## Estimated Impact of Manufacturing Trade Shock on Gender Differential in Idleness, 1990–2014
    ## =============================================================================
    ##                              Male-female differential in idleness, age 18–25 
    ##                              ------------------------------------------------
    ##                                        Not employed, not in school           
    ##                                        (1)                      (2)          
    ## -----------------------------------------------------------------------------
    ## Change in Import Penetration         0.66***                                 
    ##                                       (0.19)                                 
    ##                                                                              
    ## IP x Male Ind Share                                           2.60***        
    ##                                                               (0.53)         
    ##                                                                              
    ## IP x Female Ind Share                                         -1.55**        
    ##                                                               (0.63)         
    ##                                                                              
    ## -----------------------------------------------------------------------------
    ## Observations                          1,444                    1,444         
    ## R2                                     0.13                    0.10          
    ## Adjusted R2                            0.12                    0.09          
    ## Residual Std. Error              0.09 (df = 1423)        0.09 (df = 1422)    
    ## =============================================================================
    ## Note:                                             *p<0.1; **p<0.05; ***p<0.01

## Mortality

Estimated Impact of Manufacturing Trade Shock on Gender Differentials in
Death Rates, 1990–2015

**Mortality**

``` r
mainshock_mortality <- ivreg(formula = cum_mortmfgap_total ~ d_impusch_p9 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotch_p9_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)

gendershock_mortality <- ivreg(formula = cum_mortmfgap_total ~ d_impuschm_p9cen + d_impuschf_p9cen + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotchm_p9cen_lag + d_impotchf_p9cen_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)

stargazer(mainshock_mortality, gendershock_mortality, title = "Estimated Impact of Manufacturing Trade Shock on Gender Differentials in Death Rates, 1990–2015", dep.var.labels = c("Total Mortality"), keep = c("d_impusch_p9", "d_impuschm_p9cen", "d_impuschf_p9cen"), style = "default", type = "text", dep.var.caption = "Male-female death rate differential ages 18–39", covariate.labels = c("Change in Import Penetration", "IP x Male Ind Share", "IP x Female Ind Share"), digits = 2)
```

    ## 
    ## Estimated Impact of Manufacturing Trade Shock on Gender Differentials in Death Rates, 1990–2015
    ## ============================================================================
    ##                              Male-female death rate differential ages 18–39 
    ##                              -----------------------------------------------
    ##                                              Total Mortality                
    ##                                        (1)                     (2)          
    ## ----------------------------------------------------------------------------
    ## Change in Import Penetration         42.03**                                
    ##                                      (18.67)                                
    ##                                                                             
    ## IP x Male Ind Share                                           49.47         
    ##                                                              (52.35)        
    ##                                                                             
    ## IP x Female Ind Share                                         33.56         
    ##                                                              (61.69)        
    ##                                                                             
    ## ----------------------------------------------------------------------------
    ## Observations                          1,444                   1,444         
    ## R2                                    0.64                    0.64          
    ## Adjusted R2                           0.64                    0.64          
    ## Residual Std. Error             8.65 (df = 1423)        8.65 (df = 1422)    
    ## ============================================================================
    ## Note:                                            *p<0.1; **p<0.05; ***p<0.01

## Marriage

Estimated Impact of Manufacturing Trade Shock on Marriage, 1990–2014

**Marriage**

``` r
mainshock_marriage <- ivreg(formula = d_sh_fem1839_marrexsep ~ d_impusch_p9 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotch_p9_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)

gendershock_marriage <- ivreg(formula = d_sh_fem1839_marrexsep ~ d_impuschm_p9cen + d_impuschf_p9cen + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic | d_impotchm_p9cen_lag + d_impotchf_p9cen_lag + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + t2 + reg_midatl + reg_encen + reg_wncen + reg_satl + reg_escen + reg_wscen + reg_mount + reg_pacif + l_sh_pop_black + l_sh_pop_asian + l_sh_pop_oth + l_sh_pop_hispanic, weights = timepwt24, data=master)

stargazer(mainshock_marriage, gendershock_marriage, title = "Estimated Impact of Manufacturing Trade Shock on Marriage, 1990–2014", dep.var.labels = c("Married"), keep = c("d_impusch_p9", "d_impuschm_p9cen", "d_impuschf_p9cen"), style = "default", type = "text", dep.var.caption = "Women’s marital status", covariate.labels = c("Change in Import Penetration", "IP x Male Ind Share", "IP x Female Ind Share"), digits = 2)
```

    ## 
    ## Estimated Impact of Manufacturing Trade Shock on Marriage, 1990–2014
    ## ==============================================================
    ##                                   Women’s marital status      
    ##                              ---------------------------------
    ##                                           Married             
    ##                                    (1)              (2)       
    ## --------------------------------------------------------------
    ## Change in Import Penetration     -0.95***                     
    ##                                   (0.17)                      
    ##                                                               
    ## IP x Male Ind Share                               -3.57***    
    ##                                                    (0.50)     
    ##                                                               
    ## IP x Female Ind Share                             2.03***     
    ##                                                    (0.59)     
    ##                                                               
    ## --------------------------------------------------------------
    ## Observations                      1,444            1,444      
    ## R2                                 0.73             0.71      
    ## Adjusted R2                        0.72             0.70      
    ## Residual Std. Error          0.08 (df = 1423) 0.08 (df = 1422)
    ## ==============================================================
    ## Note:                              *p<0.1; **p<0.05; ***p<0.01

# Bartik.weight Package

We keep the structure as in the example provided for the bartik weights
package (<https://rdrr.io/github/jjchern/bartik.weight/f/README.md>)

`bw(master, y, x, controls = NULL, weight = NULL, local, Z, global, G)`

*master*: The master data frame -> `master`  
*y*: Outcome variable. Change manufacturing employment ->
`master*d_sh_empl_mfg_age1839`  
*x*: Endogenous variable. Change in import exposure ->
`master*d_impusch_p9`  
*controls*: Control variables (in `master`)  
*weight*: Weighted variable. Each CZ’s share in the start-of-period
population -> `timepwt24`  
*local*: The local data frame -> `bartik.weight::ADH_local` (in wide
format!)  
*Z*: Local industry shares -> `bartik.weight::ADH_local*sh_ind_`  
*global*: The global data frame -> `bartik.weight::ADH_global`  
*G*: A string for the the overall industry growth rates ->
`ADH_global*trade_`

``` r
# Convert industry shares to wide format as suggested by authors in package description.
ADH_local %>%
  mutate(ind = str_glue("t{year}_sh_ind_{ind}")) %>%
  spread(ind, sh_ind_, fill = 0) %>%
  print() -> ADH_local_wide
```

    ## # A tibble: 1,444 x 782
    ##    czone  year t1990_sh_ind_2011 t1990_sh_ind_2015 t1990_sh_ind_2022
    ##    <dbl> <dbl>             <dbl>             <dbl>             <dbl>
    ##  1   100  1990          0.0101             0                0.000433
    ##  2   100  2000          0                  0                0       
    ##  3   200  1990          0.00165            0.00714          0       
    ##  4   200  2000          0                  0                0       
    ##  5   301  1990          0.000581           0                0       
    ##  6   301  2000          0                  0                0       
    ##  7   302  1990          0.0215             0.00341          0       
    ##  8   302  2000          0                  0                0       
    ##  9   401  1990          0.000242           0.00274          0       
    ## 10   401  2000          0                  0                0       
    ## # ... with 1,434 more rows, and 777 more variables: t1990_sh_ind_2023 <dbl>,
    ## #   t1990_sh_ind_2024 <dbl>, t1990_sh_ind_2026 <dbl>, t1990_sh_ind_2032 <dbl>,
    ## #   t1990_sh_ind_2033 <dbl>, t1990_sh_ind_2034 <dbl>, t1990_sh_ind_2035 <dbl>,
    ## #   t1990_sh_ind_2037 <dbl>, t1990_sh_ind_2041 <dbl>, t1990_sh_ind_2043 <dbl>,
    ## #   t1990_sh_ind_2044 <dbl>, t1990_sh_ind_2045 <dbl>, t1990_sh_ind_2046 <dbl>,
    ## #   t1990_sh_ind_2047 <dbl>, t1990_sh_ind_2048 <dbl>, t1990_sh_ind_2051 <dbl>,
    ## #   t1990_sh_ind_2062 <dbl>, t1990_sh_ind_2064 <dbl>, ...

Run estimation

``` r
# Prepare variables in the master tibble
y <- "d_sh_empl_mfg_age1839" # change manufacturing employment rate
x <- "d_impusch_p9" # Growth imports from China to US
controls <- c("l_shind_manuf_cbp", "l_sh_popedu_c", "l_sh_popfborn", "l_sh_empl_f", "l_sh_routine33", "l_task_outsource", "t2", "reg_midatl", "reg_encen", "reg_wncen", "reg_satl", "reg_escen", "reg_wscen", "reg_mount", "reg_pacif", "l_sh_pop_black", "l_sh_pop_asian", "l_sh_pop_oth", "l_sh_pop_hispanic")
weight <- "timepwt24" # Weighted by population

# Prepare variables in the local tibble
# This function operates row-wise on dataframes, and element-wise among the outcomes of ps objects. The elements of setdiff(x,y) are those elements in x but not in y. The definition is taken to match the version in the base package.
Z <- setdiff(names(ADH_local_wide), c("czone", "year")) # Funktion, damit Z nur Industry share von 2000

# Prepare variables in the global tibble
G <- "trade_" # Globale Wachstumsrate: Growth imports from China to other rich countries

# Estimate the weight (alpha) and the IV estimates (beta)
bw <- bw(master, y, x, controls, weight, ADH_local_wide, Z, ADH_global, G) 
bw 
```

    ## # A tibble: 780 x 5
    ##     year ind     trade_        alpha    beta
    ##    <dbl> <chr>    <dbl>        <dbl>   <dbl>
    ##  1  1990 2011   1.21    -0.00172      -4.77 
    ##  2  1990 2015   7.46    -0.0134       -1.68 
    ##  3  1990 2022   0.00698 -0.000000516 -18.1  
    ##  4  1990 2023   3.16     0.000186      5.40 
    ##  5  1990 2024   0        0           -19.7  
    ##  6  1990 2026   0.0333   0.00000388   -0.523
    ##  7  1990 2032   0.176   -0.00000234  -71.5  
    ##  8  1990 2033   3.78    -0.00119      -0.342
    ##  9  1990 2034  12.2     -0.00283       0.264
    ## 10  1990 2035   5.11    -0.000261      5.18 
    ## # ... with 770 more rows

``` r
# Top five Rotemberg weight industries
bw %>%
  top_n(5, alpha) %>%
  arrange(desc(alpha)) %>%
  mutate(ind = case_when(
    ind == "3571" ~ "Electronic Computers",
    ind == "3663" ~ "Radio and Television Broadcasting and Communications Equipment",
    ind == "3674" ~ "Semiconductors and Related Devices",
    ind == "3661" ~ "Telephone Apparatus",
    ind == "3679" ~ "Electronic Components, Not Elsewhere Classified"
  )) %>%
  rename(g = trade_) %>%
  knitr::kable(digits = 3, caption = "Top five Rotemberg weight industries")
```

| year | ind                                                            |       g | alpha |   beta |
|-----:|:---------------------------------------------------------------|--------:|------:|-------:|
| 2000 | Electronic Computers                                           | 189.117 | 0.298 | -0.494 |
| 2000 | Radio and Television Broadcasting and Communications Equipment |  92.011 | 0.109 | -0.158 |
| 2000 | Semiconductors and Related Devices                             |  41.881 | 0.092 | -0.744 |
| 2000 | Telephone Apparatus                                            |  94.577 | 0.045 | -0.632 |
| 2000 | Electronic Components, Not Elsewhere Classified                |  34.676 | 0.028 | -1.218 |

Top five Rotemberg weight industries

``` r
# Weight of top five industries
bw %>%
  top_n(5, alpha) %>%
  transmute(sum = sum(alpha)) %>%
  slice_head(n = 1)
```

    ## # A tibble: 1 x 1
    ##     sum
    ##   <dbl>
    ## 1 0.572

``` r
# Beta estimate from the bartik.weight function
bw %>% 
  mutate(b = alpha * beta) %>%
  transmute(sum = sum(b)) %>%
  slice_head(n = 1) # similar, but not identical to our previous estimate
```

    ## # A tibble: 1 x 1
    ##     sum
    ##   <dbl>
    ## 1 -1.16

sessionInfo {utils}: Collect Information About the Current R Session

``` r
sessioninfo::session_info()
```

    ## - Session info ---------------------------------------------------------------
    ##  setting  value
    ##  version  R version 4.1.2 (2021-11-01)
    ##  os       Windows 10 x64 (build 19042)
    ##  system   x86_64, mingw32
    ##  ui       RTerm
    ##  language (EN)
    ##  collate  German_Switzerland.1252
    ##  ctype    German_Switzerland.1252
    ##  tz       Europe/Berlin
    ##  date     2022-04-23
    ##  pandoc   2.14.0.3 @ C:/Program Files/RStudio/bin/pandoc/ (via rmarkdown)
    ## 
    ## - Packages -------------------------------------------------------------------
    ##  package       * version date (UTC) lib source
    ##  abind           1.4-5   2016-07-21 [2] CRAN (R 4.1.1)
    ##  AER           * 1.2-9   2020-02-06 [2] CRAN (R 4.1.1)
    ##  assertthat      0.2.1   2019-03-21 [2] CRAN (R 4.1.1)
    ##  backports       1.3.0   2021-10-27 [2] CRAN (R 4.1.1)
    ##  bartik.weight * 0.1.0   2022-03-07 [1] Github (paulgp/bartik-weight@722ceb8)
    ##  broom           0.7.10  2021-10-31 [2] CRAN (R 4.1.1)
    ##  car           * 3.0-11  2021-06-27 [2] CRAN (R 4.1.1)
    ##  carData       * 3.0-4   2020-05-22 [2] CRAN (R 4.1.1)
    ##  cellranger      1.1.0   2016-07-27 [2] CRAN (R 4.1.1)
    ##  cli             3.2.0   2022-02-14 [1] CRAN (R 4.1.2)
    ##  colorspace      2.0-2   2021-06-24 [2] CRAN (R 4.1.1)
    ##  crayon          1.5.0   2022-02-14 [1] CRAN (R 4.1.2)
    ##  curl            4.3.2   2021-06-23 [2] CRAN (R 4.1.1)
    ##  data.table      1.14.2  2021-09-27 [2] CRAN (R 4.1.1)
    ##  DBI             1.1.1   2021-01-15 [2] CRAN (R 4.1.1)
    ##  dbplyr          2.1.1   2021-04-06 [2] CRAN (R 4.1.1)
    ##  digest          0.6.28  2021-09-23 [2] CRAN (R 4.1.1)
    ##  dplyr         * 1.0.7   2021-06-18 [2] CRAN (R 4.1.1)
    ##  ellipsis        0.3.2   2021-04-29 [2] CRAN (R 4.1.1)
    ##  evaluate        0.14    2019-05-28 [2] CRAN (R 4.1.1)
    ##  fansi           1.0.2   2022-01-14 [1] CRAN (R 4.1.2)
    ##  fastmap         1.1.0   2021-01-25 [2] CRAN (R 4.1.1)
    ##  forcats       * 0.5.1   2021-01-27 [2] CRAN (R 4.1.1)
    ##  foreign         0.8-81  2020-12-22 [2] CRAN (R 4.1.1)
    ##  Formula         1.2-4   2020-10-16 [2] CRAN (R 4.1.1)
    ##  fs              1.5.0   2020-07-31 [2] CRAN (R 4.1.1)
    ##  generics        0.1.1   2021-10-25 [2] CRAN (R 4.1.1)
    ##  ggplot2       * 3.3.5   2021-06-25 [2] CRAN (R 4.1.1)
    ##  glue            1.6.1   2022-01-22 [1] CRAN (R 4.1.2)
    ##  gtable          0.3.0   2019-03-25 [2] CRAN (R 4.1.1)
    ##  haven         * 2.4.3   2021-08-04 [2] CRAN (R 4.1.1)
    ##  highr           0.9     2021-04-16 [2] CRAN (R 4.1.1)
    ##  hms             1.1.1   2021-09-26 [2] CRAN (R 4.1.1)
    ##  htmltools       0.5.2   2021-08-25 [2] CRAN (R 4.1.1)
    ##  httr            1.4.2   2020-07-20 [2] CRAN (R 4.1.1)
    ##  jsonlite        1.7.2   2020-12-09 [2] CRAN (R 4.1.1)
    ##  knitr           1.36    2021-09-29 [2] CRAN (R 4.1.1)
    ##  lattice         0.20-45 2021-09-22 [2] CRAN (R 4.1.1)
    ##  lifecycle       1.0.1   2021-09-24 [2] CRAN (R 4.1.1)
    ##  lmtest        * 0.9-38  2020-09-09 [2] CRAN (R 4.1.1)
    ##  lubridate       1.8.0   2021-10-07 [2] CRAN (R 4.1.1)
    ##  magrittr        2.0.2   2022-01-26 [1] CRAN (R 4.1.2)
    ##  Matrix          1.3-4   2021-06-01 [2] CRAN (R 4.1.1)
    ##  modelr          0.1.8   2020-05-19 [2] CRAN (R 4.1.1)
    ##  munsell         0.5.0   2018-06-12 [2] CRAN (R 4.1.1)
    ##  openxlsx        4.2.4   2021-06-16 [2] CRAN (R 4.1.1)
    ##  pillar          1.7.0   2022-02-01 [1] CRAN (R 4.1.2)
    ##  pkgconfig       2.0.3   2019-09-22 [2] CRAN (R 4.1.1)
    ##  purrr         * 0.3.4   2020-04-17 [2] CRAN (R 4.1.1)
    ##  R6              2.5.1   2021-08-19 [2] CRAN (R 4.1.1)
    ##  Rcpp            1.0.8   2022-01-13 [1] CRAN (R 4.1.2)
    ##  readr         * 2.1.2   2022-01-30 [1] CRAN (R 4.1.2)
    ##  readxl          1.3.1   2019-03-13 [2] CRAN (R 4.1.1)
    ##  reprex          2.0.1   2021-08-05 [2] CRAN (R 4.1.1)
    ##  rio             0.5.27  2021-06-21 [2] CRAN (R 4.1.1)
    ##  rlang           1.0.1   2022-02-03 [1] CRAN (R 4.1.2)
    ##  rmarkdown       2.11    2021-09-14 [2] CRAN (R 4.1.1)
    ##  rstudioapi      0.13    2020-11-12 [2] CRAN (R 4.1.1)
    ##  rvest           1.0.2   2021-10-16 [2] CRAN (R 4.1.1)
    ##  sandwich      * 3.0-1   2021-05-18 [1] CRAN (R 4.1.2)
    ##  scales          1.1.1   2020-05-11 [2] CRAN (R 4.1.1)
    ##  sessioninfo     1.2.2   2021-12-06 [1] CRAN (R 4.1.2)
    ##  stargazer     * 5.2.2   2018-05-30 [2] CRAN (R 4.1.1)
    ##  stringi         1.7.5   2021-10-04 [2] CRAN (R 4.1.1)
    ##  stringr       * 1.4.0   2019-02-10 [2] CRAN (R 4.1.1)
    ##  survival      * 3.2-13  2021-08-24 [2] CRAN (R 4.1.1)
    ##  tibble        * 3.1.6   2021-11-07 [1] CRAN (R 4.1.2)
    ##  tidyr         * 1.1.4   2021-09-27 [2] CRAN (R 4.1.1)
    ##  tidyselect      1.1.1   2021-04-30 [2] CRAN (R 4.1.1)
    ##  tidyverse     * 1.3.1   2021-04-15 [2] CRAN (R 4.1.1)
    ##  tzdb            0.2.0   2021-10-27 [2] CRAN (R 4.1.1)
    ##  utf8            1.2.2   2021-07-24 [2] CRAN (R 4.1.1)
    ##  vctrs           0.3.8   2021-04-29 [2] CRAN (R 4.1.1)
    ##  withr           2.4.2   2021-04-18 [2] CRAN (R 4.1.1)
    ##  xfun            0.27    2021-10-18 [2] CRAN (R 4.1.1)
    ##  xml2            1.3.2   2020-04-23 [2] CRAN (R 4.1.1)
    ##  yaml            2.2.1   2020-02-01 [2] CRAN (R 4.1.1)
    ##  zip             2.2.0   2021-05-31 [2] CRAN (R 4.1.1)
    ##  zoo           * 1.8-9   2021-03-09 [2] CRAN (R 4.1.1)
    ## 
    ##  [1] \\unetna01/MarxerB$/Daten/R/win-library/4.1
    ##  [2] C:/Program Files/R/library
    ## 
    ## ------------------------------------------------------------------------------
