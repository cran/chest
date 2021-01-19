# chest 0.3.5
Fix the issue between 'broom::tidy' 0.7.3 and the new version of 'speedglm' 0.3-3. 'chest_speedglm' no longer depends on 'broom'.

# chest 0.3.4
Fix a issue that 'broom:tidy' could not pick up p values from 'speedglm' models. 

# chest 0.3.3
Fix a dependency issue with new version 'broom'.

# chest 0.3.2
Fix a dependency issue with new version 'broom'.

# chest 0.3.1
* replaced width with height for chest_plot function

# chest 0.3.0

## Major changes:
* Separated functions for plots and estimates 
* added plot two functions: chest_plot, chest_forest 
* added chest_nb for negative binomial regression

## chest 0.2.1
* chest_speedglm occasionally gives incorrect p-values due to the way of speedglm handling scientific notations: fixed   
* Incorrect observation numbers for chest_glm: fixed 
* Changed the default 'na_omit = TRUE'.  
* Added 'weights' to all 'chest' functions. 

## chest 0.2.0
* First package version for CRAN.
