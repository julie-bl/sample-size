# SampleSize


<div style="text-align: justify">

This page is devoted to the calculation of the number of patient required for a clinical trial.This repositorie list a certain number of different sample size calculation methods and their associated R scripts. Each section are written and discussed by the community of methodologists and biostatisticians of the CHU of Poitiers.


## REMINDERS

**Superiority :** Use to show that the experimental treatment is more effective than standard therapy.

**Non-inferiority :** Use to show that the experimental treatment is as effective as standard therapy. WARNING : the results of the study can not be interpreted to show a superiority of the experimental treatment !

## COMPARING MEANS

Evaluation of the effect within a given treatment, the null hypothesis of interest is to test whether there is a significant difference in mean change from baseline to endpoint.


<details>
<summary>SUPERIORITY</summary>
<br>

*Sample size for a randomised controlled superiority trial in two parallel groups (experimental treatment A versus control treatment B) with balanced randomisation (ratio 1 :1) for a binary endpoint. The average quality of life was 66 points with treatment B compared to 72 points with treatment A. In order to highlight this absolute difference of 6 points, with a standard deviation of 23, with a two-sided alpha risk of 5% and a power of 80%, the sample size is related to the result of the script bellow :*

	
```r
library(epiR)
		
epi.sscompc(N = NA, treat = 66, control = 72, 
			sigma = 23, n = NA, power = 0.8, 
			r = 1, design = 1, sided.test = 2, conf.level = 0.95)
```

**Parameters :**

* treat : mean expected in the experimental group
* control : mean expected in the control group
* sigma : standard deviation (commun for both group)
* n : number of subjects to include (experimental + control), define as NA
* power : power of the trial
* r : randomization ratio, number of patients of the experimental group divided by the number of patients of the control group
* design : estimated design effect
* sided.test : One-side test (=1), two-side test (=2) 
*	conf.level : Confidence level (1-α)

</summary>	
</details>

<details>
<summary>NON-INFERIORITY</summary>
<br>

*Sample size for a randomised controlled non-inferiority trial in two parallel groups (experimental treatment A versus control treatment B) with balanced randomisation (ratio 1 :1) for a binary endpoint. The average quality of life was 66 points with treatment B. Assuming an absolute non-inferiority margin of 7 points, with a standard deviation of 23, with a one-sided alpha risk of 5% and a power of 80%, the sample size is related to the result of the script bellow :*
	
```r
library(epiR)
	
epi.ssninfc(treat = 66, control = 66, sigma = 23, 
			delta = 7, n = NA, power = 0.8, alpha = 0.05, r = 1)
```
	
**Parameters :**

* treat : mean expected in the experimental group
* control : mean expected in the control group
* sigma : standard deviation (commun for both group)
* delta : equivalence limit, which represents the clinically significant difference (>0)
* n : number of subjects to include (experimental + control), define as NA
* power : power of the trial
* alpha : type I error
* r : randomization ratio, number of patients of the experimental group divided by the number of patients of the control group
</summary>
</details>	

## COMPARING PROPORTIONS

Evaluation of treatment effect based on discrete clinical endpoint, the proportions of events that have occurred between treatment groups are compared.

<details>
<summary>SUPERIORITY</summary>
<br>

	
*Sample size for a randomised controlled superiority trial in two parallel groups (experimental treatment A versus control treatment B) with balanced randomisation (ratio 1 :1) for a binary endpoint. The proportion of patients with an episode of hypertension was 35% with the B treatment compared to 28% with treatment A. In order to highlight this absolute difference of 7%, with a two-sided alpha risk of 5% and a power of 80%, the sample size is related to the result of the script bellow :*
	
	
```r
library(epiR)

epi.sscohortc(N = NA, irexp1 = 0.35, irexp0 = 0.28, pexp = NA, n = NA, 
			power = 0.80, r = 1, design = 1, sided.test = 2, 
			finite.correction = FALSE, nfractional = FALSE, conf.level = 0.95)

```
	
**Parameters :**

*	irexp1 : Proportion expected within the experimental group
*	irexp0 : Proportion expected within the control group
* n : number of subjects to include (experimental + control), define as NA
*	power : Power of the trial
* r : randomization ratio, number of patients of the experimental group divided by the number of patients of the control group
* design : estimated design effect
*	sided.test : One-side test (=1), two-side test (=2) 
*	conf.level : Confidence level (1-α)
</summary>
</details>


<details>
<summary>NON-INFERIORITY</summary>
<br>	

	
*Sample size for a randomised controlled non-inferiority trial in two parallel groups (experimental treatment A versus control treatment B) with balanced randomisation (ratio 1 :1) for a binary endpoint. The proportion of patients with an episode of hypertension was 35% with the B treatment. Assuming an absolute non-inferiority margin of 5%, with a one-sided alpha risk of 5% and a power of 80%, the sample size is related to the result of the script bellow :*
	
	
```r
epi.ssninfb(treat = 0.35, control = 0.35, delta = 0.05, 
			n = NA, r = 1, power = 0.8, alpha = 0.05)
```
	
**Parameters :**

* treat : proportion expected in the experimental group
* control : proportion expected in the control group
* delta : equivalence limit, which represents the clinically significant difference (>0)
* n : number of subjects to include (experimental + control), define as NA
* r : randomization ratio, number of patients of the experimental group divided by the number of patients of the control group
* power : power of the trial
* alpha : type I error

</details>

## BINARY EVENT PREDICTION

*Sample size for developing a logistic regression model based on up to  candidate 34 predictors, with an anticipated R2 of at least 0.25, and to target an expected shrinkage of 0.9(equation 11 in Riley et al. Statistics in Medicine. 2019;38:1276–1296).") :*

```r
ceiling(34/((0.9-1)*log(1-0.25/0.9)))
```

**Parameters :**

*34 : number of potential predictors
*0.9  : expected shrinkage
*0.25 : expected predictive capacities

