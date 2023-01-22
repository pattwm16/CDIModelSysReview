# CDI Models: Systematic Review of Methodology and Reporting
This repository contains the analytic code used to generate all analyses and figures in the associated manuscript, "Inconsistent methodology limits the performance & generalizability of healthcare facility-onset Clostridioides difficile diagnostic models: a systematic review of methodology & reporting".

Presented in this GitHub are the data analysis and commands used to generate both the figures and tables. Figures were primarily generated through use of the Tidyverse and ggplot2 packages (see [Tidyverse documentation](https://tidyverse.tidyverse.org/index.html)). The source code used to create figures 2, 3, 4, and all of the tables can be found in the corresponding chunks (e.g. `fig2`, `fig3`, `tbl1`) in the file, `tbls_and_figures.Rmd`.

**Manuscript figures:**
1. PRISMA diagram. This figure was generated from an online Shiny Application released by the PRISMA group. More information on generating these figures is included [here](https://estech.shinyapps.io/prisma_flowdiagram/).
2. Count of predictors in 10 models of C. difficile infection.
3. Number of events (CDI infection) vs. number of model coefficients. 

**Manuscript tables:**
1. Table 1 – Counts of coefficients, sample size, and events by study.
2. Table 2– Issues in model development. 
3. Table 3 – Evaluating performance of risk prediction models.
4. Table 4 – Performance by area under the receiver operator characteristic curve (derivation and validation sets) by study.


**References:**
-Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” Journal of Open Source Software, 4(43), 1686. doi:10.21105/joss.01686.
- Zhu, Hao. 2021. kableExtra: Construct Complex Table with Kable and Pipe Syntax.