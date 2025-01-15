# RECQUIRED SAMPLE SIZES

<div style="text-align: justify">

This page is devoted to the calculation of the number of patients required for several studies such as an randomized clinical trial (RCT) for causal inference or a cohort for constructing or validating a predictive tool. R codes are proposed. We can click [here](https://poitiers-health-data.shinyapps.io/SampleSize/) to access to the related user-friendly calculators. Some reminders:

* **Superiority RCT:** Use to demonstrate that the experimental treatment is more effective than standard therapy.

* **Non-inferiority RCT:** Use to demonstrate that the experimental treatment is as effective as standard therapy.

* **Sequential RCT:** Intermediate analyses for early stopping the study.

* **Stepped wedge RCT:** Clusters are randomized sequentially to cross from control to experimental intervention <sup>1</sup>.

<sup>1</sup> Hemming K, Taljaard M. <a href=https://doi.org/10.1016/j.jclinepi.2015.08.015>Sample size calculations for stepped wedge and cluster randomised trials: a unified approach.</a> J Clin Epidemiol. 2016 Jan;69:137-46

## DESCRIBE A POPULATION

<details>
<summary>PROPORTION ENDPOINT</summary>
<br>

*Consider the following binary endpoint descriptive study. In order to demonstrate the expected proportion of event of 35% with a precision define by a 10% width confidence interval and a 5% two-sided type I error rate, the minimum sample size needed is 350 patients.**

```r
sampleSize <- function(p=0.35, alpha=0.05, width=0.1){
  Z <- qnorm(1-alpha/2)
  (((2*Z)**2)*(p*(1-p)))/(width**2)
}

sampleSize()

#> [1] 349.5728
```

**Input parameters:**
* p : expected proportion of event
* alpha : recquired type I error rate
* width : size of the (1-α)% confidence interval

</summary>
</details>	

## COMPARING TWO MEANS

### &nbsp;&nbsp;&nbsp;&nbsp;SUPERIORITY TRIALS

<details>
<summary>No intermediate analysis</summary>
<br>

<details>
<summary>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Individual randomization</summary>

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
* n: define as NA
* power: recquired power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* sided.test: one-sided test (1) or two-sided test (2) 
* conf.level: recquired confidence level (1 minus type I error rate)

</details>

<details>
<summary>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Stepped wedge randomization</summary>

*Consider the following stepped wedge RCT with 30 centers randomized in 30 sequences. The expected mean is 38 units in patients in the experimental arm versus 48 units in the control arm. In order to demonstrate such a difference of 10 units, with a standard deviation of 17, a 5% two-sided type I error rate and a power of 90%, the minimum sample size per arm equals 61 (i.e., a total of 122 patients) in case of individual randomization with a 1:1 ratio.* 
<br>
*According to our stepped wedge design and assuming an intraclass correlation coefficient of 0.05, we need to recruit 208 patients (104 in each arm).*

```r
library(epiR)


SampleSize_SW <- function(ni, center=30, sequence=30, icc=0.05) {

# Resolve the quadratic equation

aa <- -2*center*(sequence - 1/sequence)*rho*(1+sequence/2)
bb <- 3*ni*(1-icc)*icc*(1+sequence) - 2*center*(sequence -1/sequence)*(1-icc)
cc <- 3*ni*(1-icc)*(1-icc)

m1 <- (-bb + sqrt(bb^2 - 4*aa*cc)) / (2*aa)
m2 <- (-bb - sqrt(bb^2 - 4*aa*cc)) / (2*aa)
m_sol <- max(m1,m2) 

Npat_center <- m_sol*(sequence+1) 
N_tot_SW <- Npat_center*center 

# Results

2*ceiling(N_tot_SW /2)

}



SampSize_I <- epi.sscompc(treat = 38, control = 48, sigma = 17, n = NA, 
                          power = 0.9, r = 1, sided.test = 2, conf.level = 1-0.05)

SampleSize_SW(ni = SampSize_I$n.total, center = 30, sequence = 30, icc = 0.05)

# [1] 208
		
```

**Input parameters:**
* treat: expected mean in the experimental arm
* control: expected mean in the control arm
* sigma: expected standard deviation in the two arms
* n: define as NA
* power: recquired power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* sided.test: one-sided test (1) or two-sided test (2) 
* conf.level: recquired confidence level (1 minus type I error rate)
* ni: sample size in case of indivudal randomization
* center: number of centers in the stepped wedge design
* sequence: number of sequences in the stepped wedge design
* icc: intraclass correlation coefficient anticipated 


</summary>
</details>

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
<summary>No intermediate analysis</summary>
<br>

<details>
<summary>Individual randomization</summary>
<br>

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio. The expected mean is 66 units in patients in the control arm and no difference compared to the experimental arm. Assuming an absolute non-inferiority margin of 7 points, a standard deviation of 23, the minimum sample size per arm equals 134 (i.e., a total of 268 patients) to achieve a 5% one-sided type I error rate and a power of 80%*

```r
library(epiR)
	
epi.ssninfc(treat = 66, control = 66, sigma = 23, delta = 7,
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
* sigma: expected standard deviation in the two arms
* delta: equivalence limit
* alpha: recquired type I error rate
* power: required power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* n: number of subjects to include (experimental + control) define as NA

</summary>
</details>


<details>
<summary>Stepped wedge randomization</summary>
<br>

*Consider the following stepped wedge RCT with 30 centers randomized in 30 sequences. The expected mean is 48 units in patients in the control arm and no difference compared to the experimental arm. Assuming an absolute non-inferiority margin of 7 points, a standard deviation of 17, the minimum sample size per arm equals 102 (i.e., a total of 204 patients) to achieve a 5% one-sided type I error rate and a power of 90% in case of individual randomization with a 1:1 ratio.* 
<br>
*According to our stepped wedge design and assuming an intraclass correlation coefficient of 0.05, we need to recruit 372 patients (186 in each arm).*

```r
library(epiR)
		
SampSize_I <- epi.ssninfc(treat = 48, control = 48, sigma = 17, delta = 7,
                          power = 0.9, alpha = 0.05, r = 1, n = NA)

#> $n.total
#> [1] 204
 
#> $n.treat
#> [1] 102
 
#> $n.control
#> [1] 102

#> $delta
#> [1] 7
 
#> $power
#> [1] 0.9

Nindiv <- SampSize_I$n.total

```

**Input parameters:**
* treat: expected mean in the experimental arm
* control: expected mean in the control arm
* sigma: expected standard deviation in the two arms
* delta: equivalence limit
* n: number of subjects to include (experimental + control) define as NA
* power: recquired power (1 minus type II error rate)
* alpha: recquired confidence level (type I error rate)
* r: randomization ratio (experimental:control)


```r

# Define stepped wedge parameters

N_center = 30 
N_seq = 30  
rho = 0.05 

# Resolve the quadratic equation

N_ratio = N_center/N_seq 
aa <- -2*N_center*(N_seq - 1/N_seq)*rho*(1+N_seq/2)
bb <- 3*Nindiv*(1-rho)*rho*(1+N_seq) - 2*N_center*(N_seq -1/N_seq)*(1-rho)
cc <- 3*Nindiv*(1-rho)*(1-rho)

m1 <- (-bb + sqrt(bb^2 - 4*aa*cc)) / (2*aa)
m2 <- (-bb - sqrt(bb^2 - 4*aa*cc)) / (2*aa)
m_sol <- max(m1,m2) 

Npat_center <- m_sol*(N_seq+1) 
N_tot_SW <- Npat_center*N_center 

# Results

N_total_SW <- 2*ceiling(N_tot_SW /2)
N_total_SW

# [1] 372

```

**Input parameters:**
* N_center: number of centers in the stepped wedge design
* N_seq: number of sequences in the stepped wedge design
* rho: intraclass correlation coefficient anticipated 


</summary>
</details>

</details>


## COMPARING TWO PROPORTIONS

### &nbsp;&nbsp;&nbsp;&nbsp;SUPERIORITY

<details>
<summary>No intermediate analysis</summary>
<br>

<details>
<summary>Individual randomization</summary>
<br>

*Consider the following RCT with two parallel groups with a 1:1 randomization ratio. The expected proportion of events is 35% in the experimental arm compared to 28% in the control arm. In order to demonstrate such a difference of 7%, with a two-sided type I error rate of 5% and a power of 80%, the minimum sample size per arm equals 691 (i.e., a total of 1,382 patients).*

```r
library(epiR)

epi.sscohortc(irexp1 = 0.35, irexp0 = 0.28, n = NA, power = 0.80, 
              r = 1, sided.test = 2, conf.level = 1-0.05)

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
*	irexp0: expected proportion in the control group
*	n: define as NA
*	power: required power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* sided: one-sided test (1), two-sided test (2)
* conf.level: recquired confidence level (1 minus type I error rate)

</summary>
</details>


<details>
<summary>Stepped wedge randomization</summary>
<br>

*Consider the following stepped wedge RCT with 15 centers randomized in 5 sequences. The expected proportion of events is 72% in the experimental arm compared to 62% in the control arm. In order to demonstrate such a difference of 10%, with a two-sided type I error rate of 5% and a power of 80%, the minimum sample size per arm equals 346 (i.e., a total of 692 patients) in case of individual randomization with a 1:1 ratio.* 
<br>
*According to our stepped wedge design and assuming an intraclass correlation coefficient of 0.01, we need to recruit 1,646 patients (823 in each arm).*

```r
library(epiR)

SampSize_I <- epi.sscohortc(irexp1 = 0.72, irexp0 = 0.62, n = NA, 
                            power = 0.80, r = 1, sided.test = 2, conf.level = 1-0.05)

#> $n.total
#> [1] 692

#> $n.exp1
#> [1] 346

#> $n.exp0
#> [1] 346

#> $power
#> [1] 0.8

#> $irr
#> [1] 1.16129

#> $or
#> [1] 1.576037

Nindiv <- SampSize_I$n.total

```
	
**Input parameters:**
*	irexp1: expected proportion in the experimental group
*	irexp0: expected proportion in the control group
* n: number of subjects to include (experimental + control) define as NA
*	power: required power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* sided: one-sided test (1), two-sided test (2)
* conf.level: recquired confidence level (1 minus type I error rate)


```r

# Define stepped wedge parameters

N_center = 15 
N_seq = 5  
rho = 0.01 

# Resolve the quadratic equation

N_ratio = N_center/N_seq 
aa <- -2*N_center*(N_seq - 1/N_seq)*rho*(1+N_seq/2)
bb <- 3*Nindiv*(1-rho)*rho*(1+N_seq) - 2*N_center*(N_seq -1/N_seq)*(1-rho)
cc <- 3*Nindiv*(1-rho)*(1-rho)

m1 <- (-bb + sqrt(bb^2 - 4*aa*cc)) / (2*aa)
m2 <- (-bb - sqrt(bb^2 - 4*aa*cc)) / (2*aa)
m_sol <- max(m1,m2) 

Npat_center <- m_sol*(N_seq+1) 
N_tot_SW <- Npat_center*N_center 

# Results

N_total_SW <- 2*ceiling(N_tot_SW /2)
N_total_SW

# [1] 1646

```

**Input parameters:**
* N_center: number of centers in the stepped wedge design
* N_seq: number of sequences in the stepped wedge design
* rho: intraclass correlation coefficient anticipated 


</summary>
</details>

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

<details>
<summary>Individual randomization</summary>
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


<details>
<summary>Stepped wedge randomization</summary>
<br>

*Consider the following stepped wedge RCT with 15 centers randomized in 5 sequences. The expected proportion of events is 72% in patients in the control arm and no difference compared to the experimental arm. Assuming an absolute non-inferiority margin of 8%, the minimum sample size per arm equals 390 (i.e., a total of 780 patients) to achieve a one-sided type I error rate of 5% and a power of 80%, in case of individual randomization with a 1 :1 ratio.* 
<br>
*According to our stepped wedge design and assuming an intraclass correlation coefficient of 0.01, we need to recruit 1,890 patients (945 in each arm).*

```r
library(epiR)

SampSize_I <- epi.ssninfb(treat = 0.72, control = 0.72, delta = 0.08, 
			n = NA, r = 1, power = 0.8, alpha = 0.05)


#> $n.total
#> [1] 780

#> $n.treat
#> [1] 390

#> $n.control
#> [1] 390

#> $delta
#> [1] 0.08

#> $power
#> [1] 0.8

Nindiv <- SampSize_I$n.total

```
	
**Input parameters:**
* treat: expected proportion in the experimental arm
* control: expected proportion in the control arm
* delta: equivalence limit
* alpha: recquired type I error rate
* power: required power (1 minus type II error rate)
* r: randomization ratio (experimental:control)
* n: number of subjects to include (experimental + control) define as NA


```r

# Define stepped wedge parameters

N_center = 15 
N_seq = 5  
rho = 0.01 

# Resolve the quadratic equation

N_ratio = N_center/N_seq 
aa <- -2*N_center*(N_seq - 1/N_seq)*rho*(1+N_seq/2)
bb <- 3*Nindiv*(1-rho)*rho*(1+N_seq) - 2*N_center*(N_seq -1/N_seq)*(1-rho)
cc <- 3*Nindiv*(1-rho)*(1-rho)

m1 <- (-bb + sqrt(bb^2 - 4*aa*cc)) / (2*aa)
m2 <- (-bb - sqrt(bb^2 - 4*aa*cc)) / (2*aa)
m_sol <- max(m1,m2) 

Npat_center <- m_sol*(N_seq+1) 
N_tot_SW <- Npat_center*N_center 

# Results

N_total_SW <- 2*ceiling(N_tot_SW /2)
N_total_SW

# [1] 1890

```

**Input parameters:**
* N_center: number of centers in the stepped wedge design
* N_seq: number of sequences in the stepped wedge design
* rho: intraclass correlation coefficient anticipated 

</summary>
</details>

</details>

## PREDICTING A PROPORTION

<details>
<summary>Construction of predictive model</summary>
<br>	

*For developing a model/alghorithm based on 34 predictors as candidates with an expected R2 of at least 0.25 and an expected shrinkage of 0.9 (equation 11 in Riley et al. Statistics in Medicine. 2019;38:1276–1296), the minimal sample size is 1045.*

```r
sampleSize <- function(predictors=34, R2=0.25, shrink=0.9){
  predictors/((shrink-1)*log(1-R2/shrink))
}

sampleSize()

#> [1] 1044.796
```

**Input parameters:**
* predictors : number of predictors as candidates
* R2 : expected R2
* shrink : expected shrinkage

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