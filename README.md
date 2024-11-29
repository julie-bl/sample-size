# RECQUIRED SAMPLE SIZES


<div style="text-align: justify">

This page is devoted to the calculation of the number of patients required for several studies such as an randomized clinical trial (RCT) for causal inference or a cohort for constructing or validating a predictive tool. R codes are proposed. We can click [here](https://poitiers-health-data.shinyapps.io/SampleSize/) to access to the related user-friendly calculatos.

## REMINDERS

**Superiority RCT:** Use to demonstrate that the experimental treatment is more effective than standard therapy.

**Non-inferiority RCT:** Use to demonstrate that the experimental treatment is as effective as standard therapy.

## COMPARING TWO MEANS

### &nbsp;&nbsp;&nbsp;&nbsp;SUPERIORITY

<details>
<summary>Normal design</summary>
<br>

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio. The expected mean is 66 units in patients in the experimental arm versus 72 units in the control arm. In order to demonstrate such a difference of 6 units, with a standard deviation of 23, a 5% two-sided type-I error rate and a power of 80%, the minimum sample size per arm equals 231 (i.e., a total of 462 patients).*

	
```r
library(epiR)
		
epi.sscompc(treat = 66, control = 72,	sigma = 23, n = NA, power = 0.8, 
		      	r = 1, sided.test = 2, conf.level = 1-0.05)

#> $n.total
#> [1] 462

#> $n.treat
#> [1] 231

#> $n.control
#> [1] 231

#> $power
#> [1] 0.8

#> $delta
#> [1] 6
```

**Input parameters:**

* treat: mean expected in the experimental arm
* control: mean expected in the control arm
* sigma: expected standard deviation in the two arms
* power : recquired power (1 minus type-II error rate)
* r : randomization ratio (experimental:control)
* sided.test : one-sided test (1) or two-sided test (2) 
* conf.level : confidence level (1-type-I error rate)

</summary>
</details>	

<details>
<summary>Sequential design</summary>
<br>

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio and 2 planned intermediate analyses for efficacy by using the O'Brien-Fleming method for considering the inflation of the type-I error rate). The expected mean is 66 units in patients in the experimental arm versus 72 units in the control arm. In order to demonstrate such a difference of 6 units, with a standard deviation of 23, a 5% two-sided type-I error rate and a power of 80%,  the final analysis should be carried out on 472 patients (236 patients per group). The first and second intermediate analyses would be performed on 158 and 316 patients respectively, i.e. 33% and 66% of the maximum number of included patients if their is no decision of stopping the study.*

```r
library("rpact")
		
design <- getDesignGroupSequential(typeOfDesign = "OF", informationRates = c(1/3, 2/3, 1),
                                   alpha = 0.05, beta = 1-0.8, sided = 2)

designPlan <- getSampleSizeMeans(design, alternative = 6, stDev = 23, allocationRatioPlanned = 1)

summary(designPlan)

#> Stage                                          1       2       3 
#> Planned information rate                   33.3%   66.7%    100% 
#> Cumulative alpha spent                    0.0005  0.0143  0.0500 
#> Stage levels (two-sided)                  0.0005  0.0141  0.0451 
#> Efficacy boundary (z-value scale)          3.471   2.454   2.004 
#> Lower efficacy boundary (t)              -13.012  -6.405  -4.258 
#> Upper efficacy boundary (t)               13.012   6.405   4.258 
#> Cumulative power                          0.0329  0.4424  0.8000 
#> Number of subjects                         157.1   314.2   471.3 
#> Expected number of subjects under H1                       396.7 
#> Exit probability for efficacy (under H0)  0.0005  0.0138 
#> Exit probability for efficacy (under H1)  0.0329  0.4095 
```

**Parameters :**

* typeOfDesign : type of design
* informationRates : information rates
* alpha : type I error rate
* beta : type II error rate
* sided : one-side test (=1), two-side test (=2)
* riskRatio : one-side test (=TRUE), two-side test (=FALSE)
* thetaH0 : non-inferiority bound when ≠ 0
* normalApproximation : one treatment group is calculated exactly using the binomial distribution (=FALSE), else (=FALSE)
* pi1 : assumed probability in the experimental treatment group
* pi2 : assumed probability in the control treatment group
* groups: the number of treatment groups
* allocationRatioPlanned : planned allocation ratio (n1/n2)

</summary>	
</details>

### &nbsp;&nbsp;&nbsp;&nbsp;NON-INFERIORITY

<details>
<summary>Normal design</summary>
<br>

*Sample size for a randomised controlled non-inferiority trial in two parallel groups (experimental treatment A versus control treatment B) with balanced randomisation (ratio 1 :1) for a binary endpoint. 
The average quality of life was 66 points with treatment B. Assuming an absolute non-inferiority margin of 7 points, with a standard deviation of 23, with a one-sided alpha risk of 5% and a power of 80%, 
the sample size is related to the result of the script bellow :*
	
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
* alpha : type I error rate
* r : randomization ratio, number of patients of the experimental group divided by the number of patients of the control group

</summary>
</details>


## COMPARING PROPORTIONS

Evaluation of treatment effect based on discrete clinical endpoint, the proportions of events that have occurred between treatment groups are compared.

### &nbsp;&nbsp;&nbsp;&nbsp;SUPERIORITY

<details>
<summary>Normal design</summary>
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

</summary>	
</details>

<details>
<summary>Sequential design</summary>
<br>

*The prevalence of infections at 30 days is assumed to be 15% in the population and a relative reduction of at least 25% in the experimental population (prevalence of 11.25%). By planning 2 intermediate efficacy analyses and using the O'Brien-Fleming method to take into account the repetition of the tests (inflation of the risk of the first kind), the final analysis should be carried out on 2,588 patients (1,294 patients per group) in order to respect an overall risk of the first kind equal to 5% (two-sided) and a power of 80%. The first and second intermediate analyses would be performed on 864 and 1726 patients respectively, i.e. 33 and 66% of the maximum number of patients, the sample size is related to the result of the script bellow :*

```r
library("rpact")
		
design <- getDesignGroupSequential(typeOfDesign = "OF", 
                informationRates = c(1/3, 2/3, 1), alpha = 0.05, beta = 1-0.8, sided = 2)

designPlan <- getSampleSizeRates(design, riskRatio = FALSE, thetaH0 = 0,
                   normalApproximation = TRUE, pi1 = 0.15*0.75, pi2 = 0.15, groups = 2,
                   allocationRatioPlanned = 1)

summary(designPlan)
```

**Parameters :**

* typeOfDesign : type of design
* informationRates : information rates
* alpha : type I error rate
* beta : type II error rate
* sided : one-side test (=1), two-side test (=2)
* riskRatio : one-side test (=TRUE), two-side test (=FALSE)
* thetaH0 : non-inferiority bound when ≠ 0
* normalApproximation : one treatment group is calculated exactly using the binomial distribution (=FALSE), else (=FALSE)
* pi1 : assumed probability in the experimental treatment group
* pi2 : assumed probability in the control treatment group
* groups: the number of treatment groups
* allocationRatioPlanned : planned allocation ratio (n1/n2)

</summary>	
</details>

### &nbsp;&nbsp;&nbsp;&nbsp;NON-INFERIORITY

<details>
<summary>Normal design</summary>
<br>	

	
*Sample size for a randomised controlled non-inferiority trial in two parallel groups (experimental treatment A versus control treatment B) with balanced randomisation (ratio 1 :1) for a binary endpoint. 
The proportion of patients with an episode of hypertension was 35% with the B treatment. Assuming an absolute non-inferiority margin of 5%, with a one-sided alpha risk of 5% and a power of 80%, 
the sample size is related to the result of the script bellow :*
	
	
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
* alpha : type I error rate

</details>

## BINARY EVENT PREDICTION

### &nbsp;&nbsp;&nbsp;&nbsp;CONSTRUCTION

Creation of a predictive tool that return the probability of a future event based on factors in order to inform clinical diagnosis and prognosis in healthcare.

<details>
<summary>Example</summary>
<br>	

*Sample size for developing a logistic regression model based on up to  candidate 34 predictors, with an anticipated R2 of at least 0.25, and to target an expected shrinkage of 0.9(equation 11 in Riley et al. Statistics in Medicine. 2019;38:1276–1296)."), the sample size is related to the result of the script bellow:*

```r
ceiling(34/((0.9-1)*log(1-0.25/0.9)))
```

**Parameters :**

* 34 : number of potential predictors
* 0.9  : expected shrinkage
* 0.25 : expected predictive capacities

</details>

### &nbsp;&nbsp;&nbsp;&nbsp;EXTERNAL VALIDATION

External validation for a predictive tool that return the probability of a future event based on factors in order to inform clinical diagnosis and prognosis in healthcare.

<details>
<summary>Example</summary>
<br>	

*Sample size for external validation of a logistic regression model based with an expected outcome event proportions of 50%, with a alpha risk at 5% and with a target confidence interval width of 20% (Riley et al.  Statistics in Medicine. 2021;19:4230-4251).*

```r
se <- function(width, alpha) # The standard error associated with the 1-alpha confidence interval
{
  fun <- function(x) { exp( qnorm(1-alpha/2, mean=0, sd=1) * x ) - exp(-1* qnorm(1-alpha/2, mean=0, sd=1) * x ) - width } 
  return(uniroot(fun, lower = 0.001, upper = 100)$root)
} 

size.calib <- function(p0, width, alpha) # the minimum sample size to achieve this precision
{   
  (1-p0) / ((p0 * se(width=width, alpha=alpha)**2 ))
}

size.calib(p0=0.5, width=0.2, alpha=0.05)
```

**Parameters :**

* p0 : expected event outcome proportion
* width  : size of the target confidence interval
* alpha : type I error rate

</details>