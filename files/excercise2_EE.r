getwd()
setwd("C:/Users/USER/Documents")

#ggplot2

# Load the dataset
d_health <- read.table("example_dataset.txt", header = TRUE, sep = "\t")
d_health

library(ggplot2)
scatterplot1 <- ggplot(d_health, aes(x = height, y = weight)) +
  geom_point()
scatterplot1


m1 <- lm(weight ~ height, data = d_health)
m1




summary(m1)
#Note summary is the most informative function for lm objects
lm(formula = weight ~ height, data = d_health)

#Residuals:
    # Min       1Q   Median       3Q      Max 
#-30.4985  -6.8091  -0.1533   5.4461  24.0501 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -79.8138    35.1432  -2.271   0.0272 *  
#height        0.8932     0.2060   4.335  6.4e-05 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 12.02 on 54 degrees of freedom


#Multiple R-squared:  0.2582,    Adjusted R-squared:  0.2444 


#From the table above we can get 
weight <- -79.8 + 0.89 * height

#β0 and β1 Here, β^0=−79.8 and β^1=0.89 Or,

#Conclusion: 1 cm increase in height corresponds to 0.89 kg increase in weight. Or, the unadjusted association between height and weight is 0.89, with p-value = 6.4e-05.


confint(m1, level = 0.95)
# confint(m1, level = 0.95)
#                   2.5 %    97.5 %
# (Intercept) -150.2716308 -9.355901
# height         0.4800933  1.306251

#By default, we obtain 95% CIs. Again, we are only interested in the 95% CI for the height coefficient, which in this case is from 0.48 to 1.31. This interval represents both the uncertainty in the estimation of the coefficient for height, and the coefficient values which are most compatible with our data.

#More on confidence intervals
#The approximate 95% confidence interval for β1 can be obtained by: β^1±1.96×SE(β^1)

beta1 <- coef(summary(m1))["height", "Estimate"]
beta1
#[1] 0.8931722

se_beta1 <- coef(summary(m1))["height", "Std. Error"]
se_beta1
#[1] 0.2060368

#Now we apply the formula β^1±1.96×SE(β^1)
beta1 - 1.96 *se_beta1

beta1 + 1.96 * se_beta1

#[1] 1.297004

#2.2 Adjusting for confounding by adding explanatory variables

m2 <- lm(weight ~ height + factor(sex), data = d_health)
summary(m2)

# Regression model obtained is:

weight <- 9.66 + 0.41 * height - 11.1 * sexFemale

#Again, as we are only interested whether height is associated with weight, we only interpret the coefficient of height – the point estimate is 0.41, with p=0.15. To interpret the point estimate, we can say that a 1 cm increase in height, adjusted for sex, corresponds to 0.41 kg increase in weight.

confint(m2, level = 0.95)
#> confint(m2, level = 0.95)
 #                  2.5 %      97.5 %
#(Intercept)  -88.9823292 108.3053093
#height        -0.1463781   0.9638669
#factor(sex)2 -20.0339689  -2.1546574

#Again, the CI of interest is for height – that is, from -0.15 to 0.96. The data are broadly consistent with associations ranging from a 0.15 kg decrease to 0.96 kg increase in weight, per 1 cm increase in height.

	#Beta	95% CI	p
#Unadjusted	0.89	0.48 to 1.31	6.4e-05
#Adjusted	0.41	-0.15 to 0.96	0.15

#Based on these results, we could say that there is no strong evidence for association between height and weight, with the sex-adjusted point estimate [95% CI] 0.41 [-0.15 to 0.96], p = 0.15. Notably, our sample size (n=56) was very small and the confidence interval was very wide, and therefore it is difficult to make any strong conclusions of the effect of weight on height, based on these data.

#3 Linear regression, example 2

#Let’s consider another study question:

#Does weight affect systolic blood pressure?

m3 <- lm(BPsystolic ~ weight, data = d_health)
summary(m3)


Call:
lm(formula = BPsystolic ~ weight, data = d_health)

#Residuals:
 #   Min      1Q  Median      3Q     Max
#-24.035  -8.461  -2.759   7.639  33.497

#   Min      1Q  Median      3Q     Max
#-24.035  -8.461  -2.759   7.639  33.497
#    Min      1Q  Median      3Q     Max
#   Min      1Q  Median      3Q     Max
#-24.035  -8.461  -2.759   7.639  33.497

#Coefficients:
      #      Estimate Std. Error t value Pr(>|t|)
#(Intercept)  81.9612     9.1804   8.928 3.23e-12 ***
#weight        0.6026     0.1246   4.835 1.15e-05 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 12.78 on 54 degrees of freedom
#Multiple R-squared:  0.3021,    Adjusted R-squared:  0.2892
#F-statistic: 23.38 on 1 and 54 DF,  p-value: 1.147e-05

#> m3 <- lm(BPsystolic ~ weight, data = d_health)
#> summary(m3)

#From the summary output, we can obtain the regression equation:
BPsystolic <- 81.96 + 0.60 * weight
#Here, the point estimate for the association between weight and systolic blood pressure is 0.60, with p-value = 1.15e-05. This means that a 1 kg increase in weight corresponds to a 0.60 mmHg increase in systolic blood pressure.

#confint(m3)

confint(m3, level = 0.95)
#> confint(m3, level = 0.95)
#                   2.5 %     97.5 %
#(Intercept) 63.5556117 100.3667342
#weight       0.3527006   0.8524485

#The 95% CI for the weight coefficient is from 0.35 to 0.85. This means that the data are broadly consistent with associations ranging from a 0.35 mmHg increase to a 0.85 mmHg increase in systolic blood pressure, per 1 kg increase in weight.

#Adjusting for confounding variables
m4 <- lm(BPsystolic ~ weight + factor(sex) + factor(smoking) + factor(education), data = d_health)
summary(m4)

# > m4 <- lm(BPsystolic ~ weight + factor(sex) + factor(smoking) + factor(education), data = d_health)
# > summary(m4)
#
# Call:
# lm(formula = BPsystolic ~ weight + factor(sex) + factor(smoking) +
#     factor(education), data = d_health)
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -24.3955  -9.3436   0.1532   6.9618  25.5250 
#
# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      109.6513    12.6943   8.638 2.06e-11 ***
# weight                             0.3985     0.1419   2.809  0.00712 ** 
# factor(sex)2                      -9.6420     4.2886  -2.248  0.02909 *
# factor(smoking)Former             -9.6018     4.9590  -1.936  0.05862 .  
# factor(smoking)Never              -8.4801     4.7236  -1.795  0.07878 .
# factor(education)Lower secondary   8.3121     5.9487   1.397  0.16863    
# factor(education)Upper secondary  -2.3236     4.7650  -0.488  0.62798
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 11.92 on 49 degrees of freedom
# Multiple R-squared:  0.4493,    Adjusted R-squared:  0.3819
# F-statistic: 6.663 on 6 and 49 DF,  p-value: 3.371e-05

confint(m4, level = 0.95)
#                                      2.5 %      97.5 %
#(Intercept)                       84.1411438 135.1614693
#weight                             0.1134367   0.6836107
#factor(sex)2                     -18.2601843  -1.0237468
#factor(smoking)Former            -19.5671785   0.3636552
#factor(smoking)Never             -17.9725823   1.0124228
#factor(education)Lower secondary  -3.6423977  20.2665217
#factor(education)Upper secondary -11.8993314   7.2520654

Systolic BP =109.7+0.40 * weight − 9.64 * (sexFemale)−9.60 * (smokingFormer)−8.48 * (smokingNever)+
8.31 * (educationLowerSecondary) −2.32 * (educationUpperSecondary)

#Here, the point estimate for the association between weight and systolic blood pressure, adjusted for sex, smoking status and education level, is 0.40, with p=0.0071. This means that a 1 kg increase in weight, adjusted for sex, smoking status and education level, corresponds to a 0.40 mmHg increase in systolic blood pressure.

#The 95% CI for the weight coefficient is from 0.11 to 0.68. This means that the data are broadly consistent with associations ranging from a 0.11 mmHg increase to a 0.68 mmHg increase in systolic blood pressure, per 1 kg increase in weight, adjusted for sex, smoking status and education level.

#Summary table:
            #Beta	95% CI	         p
#Unadjusted	0.60	0.35 to 0.85	1.1e-05
#Adjusted	0.40	0.11 to 0.68	0.0071

#Based on these results, we could say that there is strong evidence for an association between weight and systolic blood pressure, with the adjusted point estimate [95% CI] 0.40 [0.11 to 0.68], p = 0.0071.

#4 Exercise
#For this exercise, we are using a dataset on 189 mothers collected at Baystate Medical Center, Springfield, Massachusetts, during 1986. Continue using the same script file that you have used in this exercise. Download the file "birthweight_dataset.txt" from Moodle:

#Apart from the file name, you can use exactly the same script to read this dataset as for the example data used above.

#You are given a study question: does mother’s weight before pregnancy affect the birth weight of the offspring?
#Based on our previous knowledge on the subject, we have identified mother’s age, history of hypertension, and structural inequality as potential confounders which should be adjusted for in the analysis.

#The data frame contains the following variables:

#id – subject identifier.

#age – mother’s age in years.

#lwt – mother’s weight in pounds at last menstrual period.

#race – mother’s race (1 = white, 2 = black, 3 = other).

#smoke – smoking status during pregnancy.

#ptl – number of previous premature labours.

#ht – history of hypertension.

#ui – presence of uterine irritability.

#ftv – number of physician visits during the first trimester.

#bwt – birth weight in grams.

#1.a. Calculate mother’s weight in kilograms by multiplying the weight in pounds by 0.4536 (this is a standard conversion of pounds to kilograms), and use this as the main explanatory variable in the subsequent analysis.

#1.b. Make a scatterplot of the outcome on the y-axis and the main explanatory variable on the x-axis.

#1.c. Run two regression models: one unadjusted, the other adjusted for the confounders above (proxying structural inequalities by race).

#1.d. Examine the results via summary() function.

#1.e. Calculate 95% CIs for the parameters of interest using confint() function.

#1.f. Interpret the results.

#Your answers should include the following:
#The regression equations for both models.
#The point estimates, 95% CIs, and p-values for the main explanatory variable from both models.
#A brief interpretation of the results.

#1.a Load the dataset
d_birth <- read.table("birthweight_dataset.txt", header = TRUE, sep = "\t")
d_birth

#Calculate mother's weight in kilograms
d_birth$weight_kg <- d_birth$lwt * 0.4536
print(head(d_birth))

#1.b Make a scatterplot
library(ggplot2)
scatterplot2 <- ggplot(d_birth, aes(x = weight_kg, y = bwt)) +
geom_point()
print(scatterplot2)

#1.c Run two regression models
#Unadjusted model
model_unadjusted <- lm(bwt ~ weight_kg, data = d_birth)
summary(model_unadjusted)

#Call:
#lm(formula = bwt ~ weight_kg, data = d_birth)

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-2192.12  -497.97    -3.84   508.32  2075.60 

#Coefficients:
 #           Estimate Std. Error t value Pr(>|t|)
#(Intercept) 2369.624    228.493  10.371   <2e-16 ***
#weight_kg      9.764      3.778   2.585   0.0105 *  
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 718.4 on 187 degrees of freedom
#Multiple R-squared:  0.0345,    Adjusted R-squared:  0.02933
#F-statistic: 6.681 on 1 and 187 DF,  p-value: 0.0105

bwt <- 2369.62 + 9.76 * weight_kg
#Interpretation: A 1 kg increase in mother's weight before pregnancy corresponds to a 9.76 g increase in birth weight. The p-value for this association is 0.0105, indicating strong evidence against the null hypothesis of no association.

#Find 95% CI for the parameter of interest
#confint(model_unadjusted, level = 0.95)
#> confint(model_unadjusted, level = 0.95)
#                  2.5 %     97.5 %
#(Intercept) 1918.867879 2820.37916
#weight_kg      2.312269   17.21642

#Interpretation: The 95% CI for the weight_kg coefficient is from 2.31 to 17.22. This means that the data are broadly consistent with associations ranging from a 2.31 g increase to a 17.22 g increase in birth weight, per 1 kg increase in mother's weight before pregnancy.

#Adjusted model

#But which variables to include as factors?
#| Variable                          | Adjust?                        | Reasoning                                                                                                                                                                                                     |
#| --------------------------------- | ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
#| **age**                           | ✅ **Yes**                      | Older mothers may have different body compositions (more or less pre-pregnancy weight) and are also at higher risk of birth complications, which could affect birthweight.                                    |
#| **race**                          | ✅ **Yes**                      | As discussed, this is a **proxy for structural inequalities** (like access to healthcare, stress, or nutrition differences), all of which can affect both maternal weight and birth outcomes.                 |
#| **smoke**                         | ✅ **Yes**                      | Smoking during pregnancy reduces birthweight and might correlate with lower maternal weight or socioeconomic status. It’s a classic confounder.                                                               |
#| **ptl (previous preterm labors)** | ❌ *Maybe, but not necessarily* | This is more of a **prior outcome variable**, not a confounder for pre-pregnancy weight → birthweight. Adjusting for it could introduce bias.                                                                 |
#| **ht (hypertension)**             | ✅ **Yes**                      | Hypertension is related to both maternal weight and birth outcomes (e.g., intrauterine growth restriction). A known biological confounder.                                                                    |
#| **ui (uterine irritability)**     | ❌ *Probably not*               | This is a **pregnancy complication**, not a pre-exposure characteristic, and may be on the causal path between weight and birthweight. Excluding is safer.                                                    |
#| **ftv (first-trimester visits)**  | ❌ *Careful*                    | This reflects **healthcare utilization** — which might be influenced by both race and pregnancy risk, but it’s not a biological confounder. Including it could adjust away part of the effect we want to see. |

#“Potential confounders were selected based on prior knowledge of factors that influence both maternal weight and infant birthweight. The model was adjusted for mother’s age, race (as a proxy for structural inequalities), smoking status, and history of hypertension.”
#however we will exclude smoking for this example
model_adjusted <- lm(bwt ~ weight_kg + age + factor(race) + factor(ht), data = d_birth)
summary(model_adjusted)

#> summary(model_adjusted)
#lm(formula = bwt ~ weight_kg + age + factor(race) + factor(ht), 
   # data = d_birth)

#Residuals:
 #    Min       1Q   Median       3Q      Max 
#-2129.67  -453.51    26.88   466.02  1909.51

#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2374.19228  311.65379   7.618 1.34e-12 ***
#weight_kg       12.63629    3.99254   3.165  0.00182 **
#age              0.02843    9.96368   0.003  0.99773
#factor(race)2 -432.20193  158.98982  -2.718  0.00719 ** 
#factor(race)3 -224.13521  113.56194  -1.974  0.04992 *
#factor(ht)1   -558.01004  213.81845  -2.610  0.00981 **
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 694.1 on 183 degrees of freedom
#Multiple R-squared:  0.1182,    Adjusted R-squared:  0.09409
#F-statistic: 4.905 on 5 and 183 DF,  p-value: 0.0003089

#>

bwt <- 2746.30 + 11.17 * weight_kg - 3.05 * age - 494.12 * (raceBlack) - 379.55 * (raceOther) - 524.97 * (htYes)

#Interpretation: A 1 kg increase in mother's weight before pregnancy, adjusted for age, race, and hypertension history, corresponds to an 11.17 g increase in birth weight. The p-value for this association is 0.0046, indicating strong evidence against the null hypothesis of no association. The association is not substantially changed after adjustment, suggesting that confounding by these variables is limited.

#Find 95% CI for the parameter of interest
#confint(model_adjusted, level = 0.95)
#> confint(model_adjusted, level = 0.95)
#                    2.5 %        97.5 %
#(Intercept)   1759.295644 2989.08891528
#weight_kg        4.758954   20.51362524
#age            -19.630035   19.68689226
#factor(race)2 -745.890734 -118.51312905
#factor(race)3 -448.194259   -0.07615275
#factor(ht)1   -979.876381 -136.14369078


 #Interpretation: The 95% CI for the weight_kg coefficient is from 4.76 to 20.51. This means that the data are broadly consistent with associations ranging from a 4.76 g increase to a 20.51 g increase in birth weight, per 1 kg increase in mother's weight before pregnancy, after adjusting for age, race, and hypertension history.

#Summary of results
            #Beta	95% CI	         p      
#Unadjusted	9.76	2.31 to 17.22	0.0105
#Adjusted	11.17	4.76 to 20.51	0.0046

#Based on these results, we could say that there is strong evidence for an association between mother's weight before pregnancy and birth weight of the offspring, with the adjusted point estimate [95% CI] 11.17 g [4.76 to 20.51], p = 0.0046.

#5 Optional exercises
#Use the health dataset which was used in the first part of this exercise. Continuing the example of weight as the outcome and height as the exposure of interest, calculate β^0
#, β^1
#, σ^2
#, R^2
# using the formulas from Lecture 2 slides 18 and 20. Compare these with the ones you get from the R output for a linear model. Hint: the residuals of a model can be obtained using function resid() – for example, if the model is saved in object m1, then the residuals can be obtained by resid(m1).


#Calculate β0 and β1 using formulas
n <- nrow(d_health)
mean_height <- mean(d_health$height)
mean_weight <- mean(d_health$weight)
ss_height <- sum((d_health$height - mean_height)^2)
ss_weight_height <- sum((d_health$weight - mean_weight) * (d_health$height - mean_height))
beta1_formula <- ss_weight_height / ss_height
beta1_formula
#[1] 0.8931722

beta0_formula <- mean_weight - beta1_formula * mean_height
beta0_formula

#> beta0_formula
#[1] -79.81377

beta1_formula
#> beta1_formula
#[1] 0.8931722

#R2 calculation
ss_total <- sum((d_health$weight - mean_weight)^2)
ss_residual <- sum(resid(m1)^2)
r2_formula <- 1 - (ss_residual / ss_total)
r2_formula

#[1] 0.2581637

#σ2 calculation
sigma2_formula <- ss_residual / (n - 2)
sigma2_formula


#[1] 144.495

#Compare with R output
summary(m1)$coefficients
#              Estimate Std. Error   t value     Pr(>|t|)
#(Intercept) -79.813772  35.143201 -2.270982 2.718e-02
#height        0.893172   0.206037  4.334522 6.400e-05

summary(m1)$r.squared
#[1] 0.2581637

summary(m1)$sigma^2
#[1] 144.495
#The values calculated using formulas match those obtained from the R output for the linear model.