# Master's Thesis Repository 

**(NOTE: THIS PAPER IS STILL IN PROGRESS, THE CODE  MAY NOT EXACTLY REPLICATE THE FINAL VERSION)**

***A default Bayes Factor for Null Hypothesis Bayesian Testing in the context of Two-level Models***
Department of Methodology and Statistics, Utrecht University, the Netherlands.


## Introduction 

This paper proposes a default Bayes Factor with clear operating characteristics that can be used for testing whether the fixed effects of two-level models are equal to zero. This is achieved by generalizing an already existing approach for linear regression, presented in Hoijtink (2021). Since the specification of the scaling parameter of the prior distribution for this Bayes Factor includes the value for the sample size, a new estimator for the effective sample size in two-level models containing random slopes is proposed and subsequently used throughout the paper.

## Content:

### `Data` contains all `csv` files produced by the code given in the `scripts` folder:

   - `effectve-sample-size.csv` contains the results presented in the Table 1 of the paper, regarding the effetvie sample size
   - `sensitivity.csv` contains the results from the sensitivity analysis presented in Figure 1 and in Appendix B;
   - `combined_BF.csv` contains the results from the simulation study presented in Figures 2 and 3 (using FMLE);
   - `simulation-FMLE.scv` containts the summary statistics from the simulation using models with FMLE (Appendix C1);
   - `simulation-RMLE.scv` containts the summary statistics from the simulation using models with RMLE (Appendix C2);
   - `popular.sav` contains a data set used to test the `wrapper function` available from Hox, Moerbeek, & Van de Schoot, (2017)
   
Everything marked with '*' does not need to be run:

### Scripts 
the scripts within each separate folder should be run in the order specified here, the ones marked as "Additional" are either functions that need to be sourced within the main scripts or additional explanations. 

 - effective_sample_size:
   
    - `effective_sample_size.R`
    
    Additional*:
    
    - `mi-Neff_functions(L1).R` - functions that calculate the MI based effective sample size

 - sensitivity-analysis:
    
    - `sensitivity-analysis.R`
    
    Additional*:
    
    - `mi-Neff_functions(L1).R` - functions that calculate the MI based effective sample size
    - `wrapper_function.R` - a copy oof the wrapper function
    
 - j-ref_frac-ref:
 
    - `jref-example.R` 
    
 - simulation:
   
    - `simulation-data.R`
    - `simulation-mi-neff.R`
    - `simulation-BFs-FMLE.R`
    - `simulation-BFs-RMLE.R`
    - `simulation-summaries.R`
  
    Additional*:
    
    - `mi-Neff_functions(L1).R` - functions that calculate the MI based effective sample size
    - `wrapper_function.R` - a copy oof the wrapper function
    - `simulation-motivation-for-data.R` - explanation of how and why the data sets are simulated.
    
  - r-squared contains code that explains how the value for the marginal R squared is calculated (NO need to run the scripts in this folder)*
  

### Output (TBA)

Contains the output given in the code.
 
### `Wrapper_function`* 
this folder contains the programmed wrapper function around `bain`, for testing hypotheses about the (fixed) parameters of two-level `lmer` models using `bain`, which includes the possibility of calculating and using jref directly within the function, with no need to calculate it separately. The folder also contains tests for this function (see, `tests_wrapper.R`).


## Ethics:

All the data used is simulated. The research protocol has been approved by the Ethics Committee of the Faculty of Social and Behavioural Sciences at Utrecht University



## Notes: 

If you wish to use the Wrapper function, please use **only** the `wrapper_function.R` script, which contains the tested and ready-to use version of the function. Currently this version can apply overall data standardization, and uses the sample size of either the level-1 observations, level-2 observations or the effective sample size, calculated based on the ICC approach, or a sample size provided by the user. The wrapper can also implement the value fro jref, based on this paper. In the future the novel method for calculating the effective sample size, might be included.

## References:

 - Hoijtink, H. (2021). Prior sensitivity of null hypothesis bayesian testing. Psychological Methods.
https://doi.org/10.1037/met0000292
 - Hox, J. J., Moerbeek, M., & Van de Schoot, R. (2017). Multilevel analysis: Techniques and applications.
Routledge. https://doi.org/https://doi-org.proxy.library.uu.nl/10.4324/9781315650982
 
