

# Early Growth and Adult Epigenetic Age Acceleration

<br>

Exploring the relationship between prenatal, infant, childhood weight gain, an early-adult epigenetic age acceleration: 
a phenomenon associated with late life disease and mortality risk. 


<br>

### Project Overview

Low birth weight (BW), postnatal growth, and the timing of growth acceleration have been linked to risk of cardiovascular diseases and related conditions of aging. Evidence suggests that rapid infant growth is protective, while slow prenatal growth, followed by accelerated childhood growth predicts elevated long-term risk. 

We used conditional weight models to explore infant and childhood growth as predictors of DNA methylation-based epigenetic aging (DNAm age) for 840 boys and 694 girls. 
Because epigenetic clocks predict changes in functional capacity that occur independently of chronological age, they can be used to explore the role of growth in health and aging, prior to the onset of age-related clinical or functional decline.

<br>

### CLHNS Data

Anthropometric and methylation (DNAm) data  come from the the [Cebu Longitudinal Health and Nutrition Survey](https://cebu.cpc.unc.edu/) (CLHNS), a long-running birth cohort study following women and their singleton offspring since 1983-4 in the Philippines.
Weight was measured at birth (prenatal), 2 years (infant), 8.5 (childhood), and 21.5 years (adolescent-adult). 

#### Conditional Weight

The relationship between the rate of weight gain and AA was analyzed using conditional weight (CW) measures. 
CW is calculated as the residual from a regression of that weight measure on all prior weights, reflecting the relative rate of weight gain within a specified period of development.
Modeling CW handles autocorrelation and predicts adult outcomes independently of previous size.

#### DNAmAge

Analysis was conducted at age 21 with six principal component DNAm based clocks– PCHorvath1, PCHannum, PCPhenoAge, PCGrimAge, PCDNAmTL– and one non-principal component DNAm based clock, DunedinPoAm.
These clocks predict chronological age (Horvath; Hannum), biomarkers of late-life chronic disease (PhenoAge), all-cause mortality (GrimAge), telomere length (DNAmTL), and the pace-of-aging as reflected in the longitudinal pace of change in chronic disease biomarkers (DunedinPoAm). 
<br>


<br>


### File Structure


**01_cw_models.R** : Calculate CW models

**02_clock_lm.R** : Linear regression models for each clock

**03_compare_lm.R**: Compare full model to reduced model 

- Full model: all CW, age, BMI, pregnancy, smoking, household income, and education

- Reduced model: all CW, age

**04_organize_results.R** : Organize outputs from models in script 2 for visualization

**05_figures.R** : Build figure comparing beta coefficients for each clock during each developmental period

<br>



