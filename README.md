# sociopolygenic

R code to produce the bootstrapped GLMM (generalised linear mixed models) confidence intervals of longitudinal adiposity outcomes based on socioeconomic (dis)advantage as seen in the article:

* ["Socioeconomic disadvantage amplifies polygenic risk of ..." (Kerr et al.)](https://linktocome.com)


## Repository Structure

* Code (fitting models, bootstrapping, producing output): [r/](https://github.com/tystan/sociopolygenic/blob/main/r/) directory
* Bootstrapping results: [res/](https://github.com/tystan/sociopolygenic/blob/main/res/) directory
* Figures: [fig/](https://github.com/tystan/sociopolygenic/blob/main/fig/) directory
* Full supplementary file (model details incl.): [supp_material.pdf](https://github.com/tystan/sociopolygenic/blob/main/supp_material.pdf) document
* Figure and table only supplementary file: [supp_material_figtab_only.docx](https://github.com/tystan/sociopolygenic/blob/main/supp_material_figtab_only.docx) document

Here is a summary of the generalized linear mixed models (GLMMs) fit:


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




