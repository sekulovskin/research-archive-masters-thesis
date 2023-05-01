# Master's Thesis Research Archive


***A Default Bayes Factor for testing Null Hypotheses about the Fixed Effects of Linear Two-level Models***

The paper is published in [Psychological Methods](https://www.apa.org/pubs/journals/met) and can be cited as: 

Sekulovski, N., & Hoijtink, H. (2023). A default Bayes factor for testing null hypotheses about the fixed effects of linear two-level models. *Psychological Methods*. Advance online publication. [https://doi.org/10.1037/met0000573](https://doi.org/10.1037/met0000573)


## Introduction 

The paper proposes a default Bayes Factor with clear operating characteristics that can be used for testing whether the fixed effects of linear two-level models are equal to zero. This is achieved by generalising an already existing approach for linear regression, presented in Hoijtink (2021). Since the specification of the scaling parameter of the prior distribution for this Bayes Factor includes the value for the sample size (Gu et al., 2018), a new estimator for the effective sample size in two-level models containing random slopes is proposed and subsequently used throughout the paper. 

For detailed examples on how to use the `wrapper function` for the `R` package [`bain`](https://cran.r-project.org/web/packages/bain/vignettes/Introduction_to_bain.html) to compute and interpret the proposed default Bayes Factor, as well as a step-by-step calculation of the new effective sample size, please see [this](https://www.nikolasekulovski.com/tutorial/) tutorial.

## Content:

All the data used in the paper is either simulated within the respective `R` scripts or openly available from `R` packages.

### *Scripts*

The `R` scripts within each separate folder should be run in the specified order given here. Note, only run the scripts that are explicitly mentioned in this file, in order to obtain the same results like the ones stored in the `Output` folder. In other words, the additional scripts are usually `R` functions that are sourced within the main scripts. The ordering of the subfolders within this folder corresponds to the ordering of the sections within the paper. Please note that it takes around 10-30 seconds to calculate the effective sample size based on the newly proposed approach (depending on the complexity of the model and the number of level-1 observations).

 - **1.data**
 
    - `data.R` - illustrates all the different simulated data sets used throughout the paper.

 - **2.effective_sample_size:**
   
    - `effective_sample_size.R`
    
       Produces:
    
    - `effectve-sample-size.csv`, and the parameter estimates presented in Table 5 of the paper (not saved as a `.csv`)
    
     - `additional_explanation.R`
    
       Produces:
    
    - `Table_2.txt` 

 - **3.sensitivity-analysis:**
    
    - `sensitivity-analysis.R`
    
        Produces:
    
    - `sensitivity.csv` , and the parameter estimates presented in Table 3 of the paper (not saved as a `.csv`)
    
 - **4.j-ref_frac-ref:**
 
    - `jref-example.R` 
    
    (there is no output saved from this script in the `Output` folder, you can see the results directly from the console)
       
 - **5.simulation:**
   
    - 1. `simulation-data.R`
    
    - 2. `simulation-mi-neff.R`
  
    - 3. `simulation-BFs-FML.R`
    
    - 4. `simulation-summaries-FML.R`
      
      Produces:
      
      `combined_BF.scv`; `simulation-FML.csv` 
    
    - 5. `simulation-BFs-different-sample-sizes.R`
    
    - 6. `simulation-summaries-different-sample-sizes.R`
    
      Produces:
      
      `combined_nlvl1.csv`; `combined_nlvl2.csv`; `combined_n-icc.csv`; `simulation_nlvl1.csv`, `simulation_nlvl2.csv`, `simulation_n-icc.csv`
      
    
    - 7. `simulation-BFs-REML.R`
    
    - 8. `simulation-summaries-RMLE.R`
    
       Produces: 
      
      `combined_BF_REML.csv`; `simulation-REML.csv` 
      
    - 10. `simulation-BF_99.R`
    
    - 11. `simulation-summaries-BF_99.R`
    
      Produces:
      
      `combined_BF_99.csv`; `simulation_BF_99.csv`
      
      
  - **6.examples**
    
    - `examples.R` prints the results given in the example section including the estimates in Table 4 (not saved as a `.csv`).
  
    
    
    Additional scripts:
    
    - `mi-Neff_functions(L1).R` - functions that calculate the MI based effective sample size
    - `wrapper_function.R` - a copy of the wrapper function
    - `wrapper_function_BF_99.R` - a copy of the wrapper function that calculates BFs with the condition (BF = 99, when effect size is zero)
  

### *Output* contains all `csv` files produced by the code given in the `scripts` folder:

* Note the files stating `simulation_...` represent additional Tables that give nice summaries of the results presented in the Figures of the paper.

   - `effectve-sample-size.csv` contains the results presented in Table 1;
   - `Table_2.txt` containts the results presented in Table ;
   - `sensitivity.csv` contains the results from the sensitivity analysis presented in Figure 3;
   - `combined_BF.csv` contains the results from the simulation study plotted in Figures 4 and 5 (using FML est. and MI-based N_eff);
   - `simulation-FML.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using FML and N = MI-based N_eff (not presented in the thesis, but included such that researchers can easily check the results);
   -  `combined_nlvl1.csv` contains the results from the second part of the simulation study (using FML est. and N = number of level 1 observations);
   -  `combined_nlvl2.csv` contains the results from the second part of the simulation study (using FML est. and N = number of level 2 observations);
   -  `combined_n-icc.csv` contains the results from the second part of the simulation study (using FML est. and N = ICC-based N_eff);
    -  `combined_BF_99.csv` contains the results from the third part of the simulation study where it is required for the BF = 99 when the effect size is zero;
   
   - `simulation_nlvl1.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using FML and N = number of level 1 observations (not presented in the thesis, but included such that researchers can easily check the results);
   - `simulation_nlvl2.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using FML and N = number of level 2 observations (not presented in the thesis, but included such that researchers can easily check the results);
   - `simulation_n-icc.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using FML and N = ICC-based N_eff (not presented in the thesis, but included such that researchers can easily check the results);
   -  `combined_BF_REML.csv` contains the results from the simulation study plotted in Figures 6 and 7 (using REML est. and MI-based N_eff);
   -  `simulation-RMLE.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using REML and N = MI-based N_eff (not presented in the thesis, but included such that researchers can easily check the results);
   - `simulation_BF_99.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated by requiring BF = 99 when the effect size is zero and using REML and N = MI-based N_eff (not presented in the thesis, but included such that researchers can easily check the results);
   
 
### *Wrapper_function*

This folder contains the programmed wrapper function around `bain`, for testing hypotheses about the (fixed) parameters of two-level `lmer` models using `bain`, which includes the possibility of calculating and using `jref` directly within the function, with no need to calculate it separately. The folder also contains tests for this function (see, `tests_wrapper.R`).

### *Postprocessing*

Contains the script that is necessary to obtain the plots presented in the paper.

### *Manuscript*

Contains the manuscript as a `pdf`(most up-to-date version)

## Ethics:

All the data is simulated or openly available. The research protocol has been approved by the Ethical Review Board of the Faculty of Social and Behavioural Sciences of
Utrecht University.


## Notes: 

If you wish to use the Wrapper function, please use **only** the `wrapper_function.R` script, which contains the tested and ready-to-use version of the function. Currently, this version can apply overall data standardization and uses the sample size of either the level-1 observations, level-2 observations or the effective sample size, calculated based on the ICC approach, or a sample size provided by the user. The wrapper can also implement the value for `jref`, based on this paper. 


## References:

 - Gu, X., Mulder, J., & Hoijtink, H. (2018). Approximated adjusted fractional bayes factors: A general
method for testing informative hypotheses. British Journal of Mathematical and Statistical
Psychology, 71 (2), 229-261. https://doi.org/10.1111/bmsp.12110
 - Hoijtink, H. (2021). Prior sensitivity of null hypothesis bayesian testing. Psychological Methods.
https://doi.org/10.1037/met0000292


 
