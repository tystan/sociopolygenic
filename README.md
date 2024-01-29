# sociopolygenic

R code to produce the bootstrapped GLMM (generalised linear mixed models) confidence intervals of longitudinal adiposity outcomes based on socioeconomic (dis)advantage as seen in the article:

* ["Socioeconomic disadvantage amplifies polygenic risk of ..." (Kerr et al.)](https://linktocome.com)

The repository structure is summarised further [down](https://github.com/tystan/sociopolygenic/blob/main/README.md#model-and-cohort-summaries) after an overview of the [models](https://github.com/tystan/sociopolygenic/blob/main/README.md#repository-structure) first.

## Model and cohort summaries

More detail about the cohorts can be found in the paper itself and [*Growing Up in Australia*: The Longitudinal Study of Australian Children (LSAC)](https://growingupinaustralia.gov.au/data-and-documentation).

The generalized linear mixed models (GLMMs) fit in Aim 1 are summarised below:


| Cohort  |  Outcome | SES variable   |  Response (link fn)  |`glmmTMB` model formula  |
|----|-----|------|------|------|
| Childhood  |  BMI  |  SEIFA  | Gaussian (identity)  |  `bmi ~ sex + (age + prs + sei)^2 + (1 + wave\|pid)`   |
| Childhood  |  BMI  |  SEP  | Gaussian (identity)  |  `bmi ~ sex + (age + prs + sep)^2 + (1 + wave\|pid)`   |
| Childhood  |  Overweight/obese  |  SEIFA  | Binomial (log-odds)  |  `ovo ~ sex + (age + prs + sei)^2 + (1 + wave\|pid)`   |
| Childhood  |  Overweight/obese  |  SEP  | Binomial (log-odds)  |  `ovo ~ sex + (age + prs + sep)^2 + (1 + wave\|pid)`   |
| Adult  |  BMI  |  SEIFA  | Gaussian (identity)  |  `bmi ~ sex + wave + (age + prs + sei)^2 + (1 + wave\|fam_id) + (1\|pid)`   |
| Adult  |  BMI  |  SEP  | Gaussian (identity)  |  `bmi ~ sex + wave + (age + prs + sep)^2 + (1 + wave\|fam_id) + (1\|pid)`   |
| Adult  |  Overweight/obese  |  SEIFA  | Binomial (log-odds)  |  `ovo ~ sex + wave + (age + prs + sei)^2 + (1 + wave\|fam_id) + (1\|pid)`   |
| Adult  |  Overweight/obese  |  SEP  | Binomial (log-odds)  |  `ovo ~ sex + wave + (age + prs + sep)^2 + (1 + wave\|fam_id) + (1\|pid)`   |

### Model selection details

AIC and BIC were used to assess model fit to choose a common model for the fixed effects across models, as a compromise for model comparability across Cohort, Outcome, and SES variable combinations. Only the adult cohort included a time fixed effect in addition to the age, sex, PRS, and SES variables because it was not completely colinear with the age variable (not the case with the child cohort). 
 
Many group variance-covariance structures were considered for model fit as well (AIC and BIC assessed). Random intercepts and slopes over time were included in all models with unstructured covariance. Simplification to random intercepts only was not justified when assessing the model fit and was expected as population level slope parameters, at least conceptually, would not adequately account for the variability of time-course paths that are present in the data. 
 
Differing random effect structures were selected for the adult and child cohorts because of nuance in the data structure. Random (correlated) intercept and slope over waves (time) for each participant was determined most appropriate for the child cohort. However, for the adult cohort, the final models used random (correlated) intercept and slope over waves (time) within family with an additional random intercept for participant that allowed multi-participant families to have different time-course paths while allowing these observations to be more correlated within family than participants from different families. A more general structure of random (correlated) intercept and slopes for both family and participant provided an inferior model fit. Similarly, random (correlated) intercept and slope over time within participant (instead of family) with an additional random intercept for family also was inferior in model fit (the interpretation interestingly being that time-course paths are more related to the family level as this provided a superior model fit). 


## Repository Structure

* Code (fitting models, bootstrapping, producing output): [r/](https://github.com/tystan/sociopolygenic/blob/main/r/) directory
* Bootstrapping results: [res/](https://github.com/tystan/sociopolygenic/blob/main/res/) directory
* Figures: [fig/](https://github.com/tystan/sociopolygenic/blob/main/fig/) directory
* Full supplementary file (model details incl.): [supp_material.pdf](https://github.com/tystan/sociopolygenic/blob/main/supp_material.pdf) document
* Figure and table only supplementary file: [supp_material_figtab_only.docx](https://github.com/tystan/sociopolygenic/blob/main/supp_material_figtab_only.docx) document


## Supplementary figures

Code to produce the below figures are here: [r/2_compile_results.R](https://github.com/tystan/sociopolygenic/blob/main/r/2_compile_results.R)

![](https://github.com/tystan/sociopolygenic/blob/main/fig/supp_fig_chi_bmi.png)
Supp Fig: *Association of SEIFA neighbourhood disadvantage (Panel A) and SEP family disadvantage (Panel B) with BMI across childhood. In all cases quintile 1 represents the most disadvantage.*


![](https://github.com/tystan/sociopolygenic/blob/main/fig/supp_fig_chi_ovo.png)
Supp Fig: *Association of SEIFA neighbourhood disadvantage (Panel A) and SEP family disadvantage (Panel B) with the probabiltity of overweight/obese across childhood. In all cases quintile 1 represents the most disadvantage.*


![](https://github.com/tystan/sociopolygenic/blob/main/fig/supp_fig_adu_bmi.png)
Supp Fig: *Association of SEIFA neighbourhood disadvantage (Panel A) and SEP family disadvantage (Panel B) with BMI across adulthood. In all cases quintile 1 represents the most disadvantage.*


![](https://github.com/tystan/sociopolygenic/blob/main/fig/supp_fig_adu_ovo.png)
Supp Fig: *Association of SEIFA neighbourhood disadvantage (Panel A) and SEP family disadvantage (Panel B) with the probabiltity of overweight/obese across adulthood. In all cases quintile 1 represents the most disadvantage.*




