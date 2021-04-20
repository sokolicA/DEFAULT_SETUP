00_Data contains data files.

01_Code contains functions and code. Output is a clean data file used to build models.
	001_Packages: Installs general packages
	002_Custom_Functions: General user-defined functions
	010_Import: Used if data has to be imported manually from various sources	 		
	011_Wrangle: Contains data transformations. Used to clean/tidy data. 
	012_Explore: Explore and visualize data. Find patterns, problems, etc. Further cleaning can be done in this step if necessary.
	
02_Model contains model building.

03_Pipeline contains code that executes the whole workflow.

04_Output contains files and data created during the workflow.

05_Report contains Rmarkdown files and presentations.

06_Test contains files to test the methods used.

07_References contains references and methodology.


---
title: "Untitled"
author: "Andrej Sokolič"
date: "20 4 2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Trenutni rezultat je model logistične regresije na transformiranih spremenljivkah. V nadaljevanju vsaki skupini (atributu) vsakega kazalnika priredim določeno število točk $S$, seštevek točk po vseh kazalnikih pa bo predstavljal verjetnost neplačila posameznika.

Želimo najti parametra $C$ in $D$, da bo veljalo $S = C + D \log(obeti)$. Za to je potrebno določiti tri začetne parametre:

+ referenčno število točk (rs, angl. reference score) 600;
+ obete pri referenčnem številu točk (ro, angl. reference odds) 50:1; 
+ število točk za podvojitev obetov (pdo, angl. points to double the odds) 20.

Parametra $C$ in $D$ izračunamo na podlagi spodnjih formul:

$S = C + D \log(obeti)$ in $S + pdo = C + D \log(2\ obeti)$.

Rešitev zgornjega sistema enačb je:
$D = pdo / \log(2)$ in $C = T - D\log(obeti)$

Pri zgornjih začetnih parametrih je enačba za število točk sledeča:

$T = 487,123 + 28,84 \log(obeti)$

Opomba 1: Začetni parametri ne vplivajo na napovedno moč modelov.
Opomba 2: Zgornja formula se lahko uporabi pri vseh modelih, katerih rezultat so obeti ali verjetnosti.

V osnovi bi potem bilo smiselno, da se točke za vsako skupino $i$ kazalnika $j$ izračunajo po sledeči formuli:  $T = D (\beta_j\ WOE_i + \alpha/m) + C/m$, kjer je $\alpha$ presešišče modela in $m$ število kazalnikov.

Funkcija `scorecard` točke za vsako skupino $i$ kazalnika $j$ izračuna po nekoliko drugačni formuli:
$T = -D\ \beta_j\ WOE_i$


```{r}
library(scorecard)

data("germancredit")
str(germancredit)

dt_f <- var_filter(germancredit, y = "creditability")


dt_list <- split_df(dt_f, y = "creditability", ratios = c(0.6, 0.4), seed = 30)
label_list <- lapply(dt_list, function(x) x$creditability)


bins <- woebin(dt_f, y = "creditability")
# woebin_plot(bins)



breaks_adj <- list(
  age.in.years = c(26, 35, 40),
  other.debtors.or.guarantors = c("none", "co-applicant%,%guarantor")
)

bins_adj <- woebin(dt_f, y = "creditability", breaks_list = breaks_adj)

dt_woe_list <- lapply(dt_list, function(x) woebin_ply(x, bins_adj))


m1 <- glm( creditability ~ ., family = binomial(), data = dt_woe_list$train)

# vif(m1, merge_coef = TRUE) # summary(m1)

# Select a formula-based model by AIC (or by LASSO for large dataset)
m_step <- step(m1, direction = "both", trace = FALSE)
m2 <- eval(m_step$call)


# First, get probabalistic predictions
pred_list <- lapply(dt_woe_list, function(x) predict(m2, x, type = 'response'))
# Then evaluate model accuracy  
perf <- perf_eva(pred = pred_list, label = label_list)


# Build the card
card <- scorecard(bins_adj, m2, points0 = 600, odds0 = 1/19, pdo = 20, digits = 5)
# ROČNO : ta funkcija naredi drugače kot siddiqi: score = -factor * WOE * Beta 
card$credit.amount
coef_df <- data.frame(x = names(coef(m2)), y = coef(m2))
D <- 20/log(2)
C <- 600 - D * log(1/19)
m <- nrow(coef_df)-1
-D * card$credit.amount$woe * coef_df$y[coef_df$x == "credit.amount_woe"]
card$credit.amount$points

# po siddiqi bi moralo biti:
-D * (card$credit.amount$woe * coef_df$y[coef_df$x == "credit.amount_woe"] +  coef_df$y[coef_df$x == "(Intercept)"] / m) + C / m

# Obtain Credit Scores
score_list <- lapply(dt_list, function(x) scorecard_ply(x, card))
# Analyze the PSI
perf_psi(score = score_list, label = label_list)

```


```{r}
m1 <- glm( creditability ~ credit.amount_woe, family = binomial(), data = dt_woe_list$train)

# vif(m1, merge_coef = TRUE) # summary(m1)

# Select a formula-based model by AIC (or by LASSO for large dataset)
m_step <- step(m1, direction = "both", trace = FALSE)
m2 <- eval(m_step$call)


# First, get probabalistic predictions
pred_list <- lapply(dt_woe_list, function(x) predict(m2, x, type = 'response'))
# Then evaluate model accuracy  
perf <- perf_eva(pred = pred_list, label = label_list)


# Build the card
card <- scorecard(bins_adj, m2, points0 = 600, odds0 = 1/19, pdo = 20, digits = 5)
# ROČNO : ta funkcija naredi drugače kot siddiqi: score = -factor * WOE * Beta 
card$credit.amount
coef_df <- data.frame(x = names(coef(m2)), y = coef(m2))
factor <- 20/log(2)
-factor * card$credit.amount$woe * coef_df$y[coef_df$x == "credit.amount_woe"]
card$credit.amount$points



bins$status.of.existing.checking.account

card$credit.amount

offs <- 600 - 28 * log(19)
```


```{r}
scorecard3 <- function (bins, model, points0 = 600, odds0 = 1/19, pdo = 20, 
    basepoints_eq0 = FALSE, digits = 0) 
{
    variable = var_woe = Estimate = points = woe = NULL
    D = pdo / log(2)
    C = points0 - D * log(1/19)
    if (inherits(bins, "list") && all(sapply(bins, is.data.frame))) 
        bins = rbindlist(bins)
    bins = setDT(bins)
    coef_dt = data.table(var_woe = names(coef(model)), Estimate = coef(model))[, 
        `:=`(variable, sub("_woe$", "", var_woe))][]
    m = nrow(coef_dt) - 1
    basepoints = points0 - D * coef_dt[1, Estimate]
    card = list()
    if (basepoints_eq0) {
        card[["basepoints"]] = data.table(variable = "basepoints", 
            bin = NA, woe = NA, points = 0)
        for (i in coef_dt[-1, variable]) {
            card[[i]] = bins[variable == i][, `:=`(points, 
                round(-D * coef_dt[variable == i, Estimate] * 
                  woe + basepoints/coef_dt[, .N - 1], digits))]
        }
    }
    else {
        card[["basepoints"]] = data.table(variable = "basepoints", 
            bin = NA, woe = NA, points = round(basepoints, digits))
        for (i in coef_dt[-1, variable]) {
            card[[i]] = bins[variable == i][, `:=`(points, 
                round(-D * (coef_dt[variable == i, Estimate] * 
                  woe + coef_dt[1, Estimate]/m) + C/m, digits))]
        }
    }
    return(card)
}

c3 <- scorecard3(bins_adj, m2, points0 = 600, odds0 = 1/19, pdo = 20, digits = 5)
# Obtain Credit Scores
score_list <- lapply(dt_list, function(x) scorecard_ply(x, c3))
# Analyze the PSI
perf_psi(score = score_list, label = label_list)

```


