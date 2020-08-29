## Load and install the packages that we'll be using today
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mfx, tidyverse, hrbrthemes, estimatr, fixest, sandwich, lmtest, AER, lfe, margins, vtable)

## Use dev versions of these two packages for some extra features
pacman::p_install_gh("tidymodels/broom") 
pacman::p_install_gh("vincentarelbundock/modelsummary")

## My preferred ggplot2 plotting theme (optional)
theme_set(hrbrthemes::theme_ipsum())

# Simple regression 
ols1 = lm(mass ~ height, data = starwars)
ols1

#View(ols1)
#listviewer::jsonedit(ols1, mode="view") ## Better for R Markdown
summary(ols1)
summary(ols1)$coef

## Get "tidy" regression coef with the broom package
library(broom)
tidy(ols1, conf.int = TRUE)

glance(ols1)

# Regressing on subsetted data 
# 1) Creat a new data frame
starwars2 = 
  starwars %>% 
  #filter(name != "Jabba Desillijic Tiure")
  filter(!(grepl("Jabba", name)))
ols2 = lm(mass ~ height, data = starwars2)
summary(ols2)

# 2) Subset directly in the lm() call
ols2a = lm(mass ~ height, data = starwars %>% filter(!(grepl("Jabba", name))))
summary(ols2a)

# Robust standard errors 
library(estimatr)
ols1_robust = lm_robust(mass ~ height, data = starwars)
# tidy(ols1_robust, conf.int=TRUE)
summary(ols1_robust)

# HAC standard errors 
# recommended to convert the model with `lmtest::coeftest()`
library(sandwich)
NeweyWest(ols1)
sqrt(diag(NeweyWest(ols1)))

library(lmtest)
ols1_hac = lmtest::coeftest(ols1, vcov = NeweyWest)
ols1_hac
tidy(ols1_hac, conf.int = TRUE)

# Clustered standard errors
# issue that most commonly affects panel data
lm_robust(mass ~ height, data=starwars, clusters=homeworld)

# Dummy variables and interaction terms
humans = 
  starwars %>% 
  mutate(gender_factored = as.factor(gender)) %>% 
  select(contains("gender"), everything())
humans

# Dummy variables as factors 
summary(lm(mass~height + gender_factored, data=humans))

## Use the non-factored version of "gender" instead; R knows it must be ordered
## for it to be included as a regression variable 
summary(lm(mass ~ height + gender, data = humans))

# Interaction effects
ols_ie = lm(mass ~ gender*height, data=humans)
summary(ols_ie)

## Panel Models 
## Fixed effects with the fixest package

# Simple FE model
library(fixest)
ols_fe = feols(mass ~ height | species, data = starwars) ## Fixed effect(s) go after the "|"
ols_fe
summary(ols_fe, se = 'standard')
# coefs_fe = tidy(summary(ols_fe, se = 'standard'), coef.int = TRUE)
coefs_fe = tidy(ols_fe, se = 'standard', conf.int = TRUE)

# High dimensional FEs and multiway clustering 
# add "homeworld" as an additional fixed effect to the model 
# now have two fixed effects: specis and homeworld
ols_hdfe = feols(mass ~ height | species+homeworld, data = starwars)
ols_hdfe

## but the standard errors of the above model are automatically clustered by species 
 
## Cluster by both species and homeworld
# ols_hdfe = summary(ols_hdfe, se = 'twoway') ## Same effect as the next line
ols_hdfe = summary(ols_hdfe, cluster = c('species', 'homeworld'))
ols_hdfe

## Comparing our model coefficients 
library(ggplot2)

# First get tidied output of the ols_hdfe object
coefs_hdfe = tidy(ols_hdfe, conf.int = TRUE)

bind_rows(
  coefs_fe %>% mutate(reg = "Model 1\nFE and no clustering"),
  coefs_hdfe %>% mutate(reg = "Model 2\nHDFE and twoway clustering") 
) %>% 
  ggplot(aes(x = reg, y = estimate, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() +
  labs(title = "Marginal effect of height on mass") + 
  geom_hline(yintercept = 0, col = "orange") +
  ylim(-0.5, NA) + ## Added a bit more bottom space to emphasize the zero line
  labs(
    title = "'Effect' of height on mass",
    caption = "Data: Characters from the Star Wars universe"
  ) +
  theme(axis.title.x = element_blank())


## Instrumental Variables 
## Get the data
data("CigarettesSW", package = "AER")
## Create a new data frame with some modified variables
cigs =
  CigarettesSW %>%
  mutate(
    rprice = price/cpi,
    rincome = income/population/cpi,
    rtax = tax/cpi,
    tdiff = (taxs - tax)/cpi
  ) %>%
  as_tibble()
## Create a subset of the data limited to 1995
cigs95 = cigs %>% filter(year==1995)
cigs95

library(AER) ## Already loaded

## Run the IV regression 
iv_reg = 
  ivreg(
    log(packs) ~ log(rprice) + log(rincome) | ## The main regression. "rprice" is endogenous
      log(rincome) + tdiff + rtax, ## List all *exogenous* variables, including "rincome"
    data = cigs95
  )
summary(iv_reg, diagnostics = TRUE)

## For those of you that prefer Stata-esque ivreg syntax, where we we specify
## the instruments explicitly
ivreg(
  log(packs) ~ log(rprice) + log(rincome) | 
    . -log(rprice) + tdiff + rtax, ## Alternative way of specifying the first-stage.
  data = cigs95
)


## Run the IV regression with robust SEs
iv_reg_robust = 
  iv_robust( ## We only need to change the function call. Everything else stays the same.
    log(packs) ~ log(rprice) + log(rincome) | 
      log(rincome) + tdiff + rtax,
    data = cigs95
  )
summary(iv_reg_robust, diagnostics = TRUE)


library(lfe) ## Already loaded

# using subsetted data
iv_felm = 
  felm(
    log(packs) ~ log(rincome) |
      0 | ## No FEs
      (log(rprice) ~ tdiff + rtax), ## First-stage. Note the surrounding parentheses
    data = cigs95
  )
summary(iv_felm)

# year and state fixed effects 
iv_felm_all = 
  felm(
    log(packs) ~ log(rincome) |
      year + state | ## Now include FEs
      (log(rprice) ~ tdiff + rtax), 
    data = cigs ## Use whole panel data set
  )
summary(iv_felm_all)

### Other models 
glm_logit = glm(am ~ cyl + hp + wt, data = mtcars, family = binomial)
tidy(glm_logit, conf.int = TRUE)

library(mfx) ## Already loaded
## Be careful: mfx loads the MASS package, which produces a namespace conflict
## with dplyr for select(). You probably want to be explicit about which one you 
## want, e.g. `select = dplyr::select`

## Get marginal effects for the above logit model
glm_logitmfx = logitmfx(glm_logit, atmean = TRUE, data = mtcars)
## Could also plug in the original formula directly
# glm_logitmfx = logitmfx(am ~ cyl + hp + wt, atmean = TRUE, data = mtcars)
tidy(glm_logitmfx, conf.int = TRUE)


### Bayesian Regression
install.packages("rstanarm") ## Run this first if you want to try yourself
library(rstanarm)
bayes_reg = 
  stan_glm(
    mass ~ gender * height,
    data = humans, 
    family = gaussian(), prior = cauchy(), prior_intercept = cauchy()
  )
summary(bayes_reg)

### Marginal Effects 
library(margins)
ols_ie_marg = margins(ols_ie)
ols_ie_marg

# summary(ols_ie_marg) ## Same effect
tidy(margins(ols_ie), conf.int = TRUE)

ols_ie %>% 
  margins(
    variables = "height", ## The main variable we're interested in
    at = list(gender = c("masculine", "feminine")) ## How the main variable is         modulated by at specific values of a second variable
  ) %>% 
  tidy(conf.int = TRUE) ## Tidy it (optional)

cplot(ols_ie, x = "gender", dx = "height", what = "effect")

par(mfrow=c(1, 2)) ## Just to plot these next two (base) figures side-by-side
cplot(ols_ie, x = "gender", what = "prediction")
cplot(ols_ie, x = "height", what = "prediction")
par(mfrow=c(1, 1)) ## Reset plot defaults


#ols_ie = lm(mass ~ gender * height, data = humans) ## Original model
ols_ie_marg2 = lm(mass ~ gender / height, data = humans)
tidy(ols_ie_marg2, conf.int = TRUE)


### Presentation 
# Tables 
library(modelsummary) ## Already loaded

## Note: msummary() is an alias for modelsummary()
msummary(list(ols1, ols_ie, ols_fe, ols_hdfe))

# summary tables 
datasummary_balance(~ gender,
                    data = select(humans, gender, height, mass, birth_year, eye_color))

library(vtable) ## Already loaded
## st() is an alias for sumtable()
st(select(humans, gender, height, mass, birth_year, eye_color), 
   group = 'gender')
st(starwars)



mods = list('FE, no clustering' = summary(ols_fe, se = 'standard'),  # Don't cluster SEs 
            'HDFE, twoway clustering' = ols_hdfe)

modelplot(mods) +
  ## You can further modify with normal ggplot2 commands...
  coord_flip() + 
  labs(
    title = "'Effect' of height on mass",
    subtitle = "Comparing fixed effect models"
  )

ie_mods = list('Partial effect' = ols_ie, 'Marginal effect' = ols_ie_marg2)

modelplot(ie_mods, coef_map = c("gendermasculine:height" = "Masculine × Height")) +
  coord_flip() + 
  labs(
    title = "'Effect' of height on mass",
    subtitle = "Comparing partial vs marginal effects"
  )


ggplot(starwars2, aes(x = height, y = mass)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm") ## See ?geom_smooth for other methods/options


## Estimate a model on a training sample of the data (shortest 30 characters)
ols1_train = lm(mass ~ height, data = starwars %>% filter(rank(height) <=30))

## Use our model to predict the mass for all starwars characters (excl. Jabba).
## Note that I'm including a 95% prediction interval. See ?predict.lm for other
## intervals and options.
predict(ols1_train, newdata = starwars2, interval = "prediction") %>%
  head(5) ## Just print the first few rows


## Alternative to predict(): Use augment() to add .fitted and .resid, as well as 
## .conf.low and .conf.high prediction interval variables to the data.
starwars2 = augment(ols1_train, newdata = starwars2, interval = "prediction")

## Show the new variables (all have a "." prefix)
starwars2 %>% select(contains("."), everything()) %>% head()

starwars2 %>%
  ggplot(aes(x = height, y = mass, col = rank(height)<=30, fill = rank(height)<=30)) +
  geom_point(alpha = 0.7) +
  geom_line(aes(y = .fitted)) +
  geom_ribbon(aes(ymin = .conf.low, ymax = .conf.high), alpha = 0.3, col = NA) +
  scale_color_discrete(name = "Training sample?", aesthetics = c("colour", "fill")) +
  labs(
    title = "Predicting mass from height",
    caption = "Line of best fit, with shaded regions denoting 95% prediction interval."
  )
