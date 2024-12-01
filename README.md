# RECQUIRED SAMPLE SIZES

<div style="text-align: justify">

This page is devoted to the calculation of the number of patients required for several studies such as an randomized clinical trial (RCT) for causal inference or a cohort for constructing or validating a predictive tool. R codes are proposed. We can click [here](https://poitiers-health-data.shinyapps.io/SampleSize/) to access to the related user-friendly calculators. Some reminders:

* **Superiority RCT:** Use to demonstrate that the experimental treatment is more effective than standard therapy.

* **Non-inferiority RCT:** Use to demonstrate that the experimental treatment is as effective as standard therapy.

* **Sequential RCT:** Intermediate analyses for early stopping the study.

## COMPARING TWO MEANS

### &nbsp;&nbsp;&nbsp;&nbsp;SUPERIORITY TRIALS

<details>
<summary>No intermediate anlysis</summary>
<br>

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio. The expected mean is 66 units in patients in the experimental arm versus 72 units in the control arm. In order to demonstrate such a difference of 6 units, with a standard deviation of 23, a 5% two-sided type I error rate and a power of 80%, the minimum sample size per arm equals 231 (i.e., a total of 462 patients).*

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
* treat: expected mean in the experimental arm
* control: expected mean in the control arm
* sigma: expected standard deviation in the two arms
* power: recquired power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* sided.test: one-sided test (1) or two-sided test (2) 
* conf.level: recquired confidence level (1 minus type I error rate)

</summary>
</details>	

<details>
<summary>Sequential design</summary>
<br>

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio and 2 planned intermediate analyses for efficacy by using the O'Brien-Fleming method for considering the inflation of the type I error rate). The expected mean is 66 units in patients in the experimental arm versus 72 units in the control arm. In order to demonstrate such a difference of 6 units, with a standard deviation of 23, a 5% two-sided type I error rate and a power of 80%,  the final analysis should be carried out on 472 patients (236 patients per group). The first and second intermediate analyses would be performed on 158 and 316 patients respectively, i.e. 33% and 66% of the maximum number of included patients if their is no decision of stopping the study.*

```r
library("rpact")
		
design <- getDesignGroupSequential(
               typeOfDesign = "OF", informationRates = c(1/3, 2/3, 1),
               alpha = 0.05, beta = 1-0.8, sided = 2)

designPlan <- getSampleSizeMeans(design, alternative = 6, stDev = 23,
                                 allocationRatioPlanned = 1)

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

**Input parameters:**
* typeOfDesign: type of design ("OF" for the O'Brien-Fleming method)
* informationRates: planned analyses defined as proportions of the maximum sample size
* alpha: recquired type I error rate
* beta: recquired type II error rate (1 minus power)
* sided: one-sided test (1), two-sided test (2)
* alternative: expected difference between the two arms
* stDev: expected standard deviation in the two arms
* allocationRatioPlanned: randomization ratio

</summary>	
</details>

### &nbsp;&nbsp;&nbsp;&nbsp;NON-INFERIORITY TRIALS

<details>
<summary>No intermediate anlysis</summary>
<br>

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio. The expected mean is 66 units in patients in the control arm and no difference compared to the experimental arm. Assuming an absolute non-inferiority margin of 7 points, a standard deviation of 23, the minimum sample size per arm equals 134 (i.e., a total of 268 patients) to achieve a 5% one-sided type I error rate and a power of 80%*

```r
library(epiR)
	
epi.ssninfc(treat = 66, control = 66, sd= 23, delta = 7,
            power = 0.8, alpha = 0.05, r = 1, n = NA)

#> $n.total
#> [1] 268

#> $n.treat
#> [1] 134

#> $n.control
#> [1] 134

#> $delta
#> [1] 7

#> $power
#> [1] 0.8
```
	
**Input parameters:**
* treat: expected mean in the experimental arm
* control: expected mean in the control arm
* sd: expected standard deviation in the two arms
* delta: equivalence limit
* alpha: recquired type I error rate
* power: required power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* n: number of subjects to include (experimental + control) define as NA

</summary>
</details>

## COMPARING TWO PROPORTIONS

### &nbsp;&nbsp;&nbsp;&nbsp;SUPERIORITY

<details>
<summary>No intermediate anlysis</summary>
<br>

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio. The expected proportion of events is 35% in the experimental arm compared to 28% in the control arm. In order to demonstrate such a difference of 7%, with a two-sided type I error rate of 5% and a power of 80%, the minimum sample size per arm equals 691 (i.e., a total of 1382 patients).*

```r
library(epiR)

epi.sscohortc(irexp1 = 0.35, irexp0 = 0.28, power = 0.80, r = 1,
              sided.test = 2, conf.level = 1-0.05)

#> $n.total
#> [1] 1382

#> $n.exp1
#> [1] 691

#> $n.exp0
#> [1] 691

#> $power
#> [1] 0.8

#> $irr
#> [1] 1.25

#> $or
#> [1] 1.384615
```
	
**Input parameters:**
*	irexp1: expected proportion in the experimental group
*	irexp0: expected proportion  in the control group
*	power: required power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* sided: one-sided test (1), two-sided test (2)
* conf.level: recquired confidence level (1 minus type I error rate)

</summary>
</details>

<details>
<summary>Sequential design</summary>
<br>

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio and 2 planned intermediate analyses for efficacy by using the O'Brien-Fleming method for considering the inflation of the type I error rate. The expected proportion of event is 11% in patients in the experimental arm versus 15% units in the control arm. In order to demonstrate such a difference of 4%, with a 5% two-sided type I error rate and a power of 80%, the final analysis should be carried out on 2,256 patients (1,128 patients per group). The first and second intermediate analyses would be performed on 752 and 1,504 patients respectively, i.e. 33% and 66% of the maximum number of included patients if their is no decision of stopping the study.*

```r
library("rpact")
		
design <- getDesignGroupSequential(typeOfDesign = "OF", 
                informationRates = c(1/3, 2/3, 1), alpha = 0.05,
                beta = 1-0.8, sided = 2)

designPlan <- getSampleSizeRates(design,  pi1 = 0.11, pi2 = 0.15,
                   allocationRatioPlanned = 1)

summary(designPlan)

#> Stage                                         1      2      3 
#> Planned information rate                  33.3%  66.7%   100% 
#> Cumulative alpha spent                   0.0005 0.0143 0.0500 
#> Stage levels (two-sided)                 0.0005 0.0141 0.0451 
#> Efficacy boundary (z-value scale)         3.471  2.454  2.004 
#> Lower efficacy boundary (t)              -0.079 -0.042 -0.029 
#> Upper efficacy boundary (t)               0.101  0.048  0.031 
#> Cumulative power                         0.0329 0.4424 0.8000 
#> Number of subjects                        751.8 1503.7 2255.5 
#> Expected number of subjects under H1                   1898.1 
#> Exit probability for efficacy (under H0) 0.0005 0.0138 
#> Exit probability for efficacy (under H1) 0.0329 0.4095 
```

**Input parameters:**
* typeOfDesign: type of design ("OF" for the O'Brien-Fleming method)
* informationRates: planned analyses defined as proportions of the maximum sample size
* alpha: recquired type I error rate
* beta: recquired type II error rate (1 minus power)
* sided: one-sided test (1), two-sided test (2)
* pi1: expected probability in the experimental group
* pi2: expected probability in the control group
* allocationRatioPlanned: randomization ratio (experimental/control)

</summary>	
</details>

### &nbsp;&nbsp;&nbsp;&nbsp;NON-INFERIORITY

<details>
<summary>No interim analysis</summary>
<br>	

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio. The expected percentage of events is 35% in patients in the control arm and no difference compared to the experimental arm. Assuming an absolute non-inferiority margin of 5%,  the minimum sample size per arm equals 1,126 (i.e., a total of 2,252 patients) to achieve a 5% one-sided type I error rate and a power of 80%.*

```r
epi.ssninfb(treat = 0.35, control = 0.35, delta = 0.05, 
			n = NA, r = 1, power = 0.8, alpha = 0.05)

#> $n.total
#> [1] 2252

#> $n.treat
#> [1] 1126

#> $n.control
#> [1] 1126

#> $delta
#> [1] 0.05

#> $power
#> [1] 0.8
```
	
**Parameters :**
* treat: expected proportion in the experimental arm
* control: expected proportion in the control arm
* delta: equivalence limit
* alpha: recquired type I error rate
* power: required power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* n: number of subjects to include (experimental + control) define as NA

</details>

## PREDICTING A PROPORTION

<details>
<summary>Construction of predictive model</summary>
<br>	

*For developing a model/alghorithm based on 34 predictors as candidates with an expected R2 of at least 0.25 and an expected shrinkage of 0.9 (equation 11 in Riley et al. Statistics in Medicine. 2019;38:1276–1296), the minimal sample size is 1045.*

```r
34/((0.9-1)*log(1-0.25/0.9))

#> [1] 1044.796
```

</summary>
</details>

<details>
<summary>External validation</summary>
<br>	

*Consider O/E the ratio between the number of observed events versus expected ones. To achieve a precision defined as a length of the (1-α)% confidence interval of this ratio equals to 0.2, if the expected proportions is 50%, the required sample size is 386 (Riley et al. Minimum sample size for external validation of a clinical prediction model with a binary outcome. Statistics in Medicine. 2021;19:4230-4251).*

```r
se <- function(width, alpha) # The standard error associated with the 1-alpha confidence interval
{
  fun <- function(x) { exp( qnorm(1-alpha/2, mean=0, sd=1) * x ) - exp(-1* qnorm(1-alpha/2, mean=0, sd=1) * x ) - width } 
  return(uniroot(fun, lower = 0.001, upper = 100)$root)
} 

size.calib <- function(p, width, alpha) # the minimum sample size to achieve this precision
{   
  (1-p) / ((p * se(width=width, alpha=alpha)**2 ))
}

size.calib(p=0.5, width=0.2, alpha=0.05)

#> [1] 385.4265
```
**Input parameters:**
* p: expected proportion of events
* width: size of the (1-α)% confidence interval
* alpha: type I error rate (α)

</details>