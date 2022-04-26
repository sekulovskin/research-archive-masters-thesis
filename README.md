# Master's Thesis Research Archive

***A Default Bayes Factor for testing Null Hypotheses about the Fixed Effects of Linear Two-level Models***

*Author:* Nikola Sekulovski 

Department of Methodology and Statistics, Utrecht University, The Netherlands.


## Introduction 

This paper proposes a default Bayes Factor with clear operating characteristics that can be used for testing whether the fixed effects of two-level models are equal to zero. This is achieved by generalizing an already existing approach for linear regression, presented in Hoijtink (2021). Since the specification of the scaling parameter of the prior distribution for this Bayes Factor includes the value for the sample size (Gu et al., 2018), a new estimator for the effective sample size in two-level models containing random slopes is proposed and subsequently used throughout the paper. For detailed examples on how to use and interpret this BF as well as a step-by-step calculation of the new effective sample size, please see [this](https://www.nikolasekulovski.com/tutorials/) page.

## Content:

Note, this repository does not contain a separate Data folder, since all the data used within the paper is either simulated from the `R` scripts or openly available within `R` packages.

### **Scripts** 

The `R` scripts within each separate folder should be run in the order specified here. Please
note only run the scripts that are explicitly mentioned in order to obtain the same results as the ones stored in the `Output` folder. In other words, the additional scripts are usually `R` functions that are sourced within the main scripts. The ordering of the scripts correspond to the ordering of the sections within the paper.

 - effective_sample_size:
   
    - `effective_sample_size.R`
    
       Produces:
    
    - `effectve-sample-size.csv`, and the parameter estimates presented in Table 3 of the paper (not saved in the csv)

 - sensitivity-analysis:
    
    - `sensitivity-analysis.R`
    
        Produces:
    
    - `sensitivity.csv` , and the model fit presented in Table 2 of the paper (not saved in the csv)
    
 - j-ref_frac-ref:
 
    - `jref-example.R` 
    
    (there is no output saved from this script in the `Output` folder, you can see the results directly in the console)
       
 - simulation:
   
    - `simulation-data.R`
    
    - `simulation-mi-neff.R`
  
    - `simulation-BFs-FML.R`
    
    - `simulation-summaries-FML.R`
      
      Produces:
      
      `combined_BF.scv`; `simulation-FML.csv` 
    
    - `simulation-BFs-different-sample-sizes.R`
    
    - `simulation-summaries-different-samle-sizes.R`
    
      Produces:
      
      `combined_nlvl1.csv`; `combined_nlvl2.csv`; `combined_n-icc.csv`; `simulation_nlvl1.csv`, `simulation_nlvl2.csv`, `simulation_n-icc.csv`
      
    
    - `simulation-BFs-REML.R`
    
    - `simulation-summaries-RMLE.R`
    
     Produces: 
      
      `combined_BF_REML.csv`; `simulation-REML.csv` 
      
  - example
    
    - `example.R`
  
    
    
    Additional scripts:
    
    - `mi-Neff_functions(L1).R` - functions that calculate the MI based effective sample size
    - `wrapper_function.R` - a copy of the wrapper function
    - `simulation-motivation-for-data.R` - explanation of how and why the data sets are simulated.
    
  

### **Output** contains all `csv` files produced by the code given in the `scripts` folder:

   - `effectve-sample-size.csv` contains the results presented in the Table 1;
   - `sensitivity.csv` contains the results from the sensitivity analysis presented in Figure 1;
   - `combined_BF.csv` contains the results from the simulation study plotted in Figures 4 and 5 (using FML est. and MI-based N_eff);
   - `simulation-FML.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using FML and N = MI-based N_eff (not presented in the thesis, but included such that researchers can easily check the results);
   -  `combined_nlvl1.csv` contains the results from the second part of the simulation study (using FML est. and N = number of level 1 observations);
   -  `combined_nlvl2.csv` contains the results from the second part of the simulation study (using FML est. and N = number of level 2 observations);
   -  `combined_n-icc.csv` contains the results from the second part of the simulation study (using FML est. and N = ICC-based N_eff);
   - `simulation_nlvl1.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using FML and N = number of level 1 observations (not presented in the thesis, but included such that researchers can easily check the results);
   - `simulation_nlvl2.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using FML and N = number of level 2 observations (not presented in the thesis, but included such that researchers can easily check the results);
   - `simulation_n-icc.csv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using FML and N = ICC-based N_eff (not presented in the thesis, but included such that researchers can easily check the results);
   -  `combined_BF_REML.csv` contains the results from the simulation study plotted in Figures 6 and 7 (using REML est. and MI-based N_eff);
   - `simulation-RMLE.ccv` contains the average BF, fit and complexity along with a 95% interval for each of the 16 categories calculated using REML and N = MI-based N_eff (not presented in the thesis, but included such that researchers can easily check the results);
   
 
### **Wrapper_function**

This folder contains the programmed wrapper function around `bain`, for testing hypotheses about the (fixed) parameters of two-level `lmer` models using `bain`, which includes the possibility of calculating and using `jref` directly within the function, with no need to calculate it separately. The folder also contains tests for this function (see, `tests_wrapper.R`).

### **Postprocessing** 

Contains the script that are necessary to obtain the plots that are presented in the paper.

### **Thesis**

Contains the paper in a pdf format (TBA).

## Ethics:

All the data is simulated. The research protocol has been approved by the Ethics Committee of the Faculty of Social and Behavioural Sciences at Utrecht University


## Notes: 

If you wish to use the Wrapper function, please use **only** the `wrapper_function.R` script, which contains the tested and ready-to use version of the function. Currently this version can apply overall data standardization, and uses the sample size of either the level-1 observations, level-2 observations or the effective sample size, calculated based on the ICC approach, or a sample size provided by the user. The wrapper can also implement the value for jref, based on this paper. In the future the novel method for calculating the effective sample size, might be included.

## References:

 - Gu, X., Mulder, J., & Hoijtink, H. (2018). Approximated adjusted fractional bayes factors: A general
method for testing informative hypotheses. British Journal of Mathematical and Statistical
Psychology, 71 (2), 229-261. https://doi.org/10.1111/bmsp.12110
 - Hoijtink, H. (2021). Prior sensitivity of null hypothesis bayesian testing. Psychological Methods.
https://doi.org/10.1037/met0000292


 
