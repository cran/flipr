## ----setup, message = FALSE, include = FALSE----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(flipr)
load("../R/sysdata.rda")

## -----------------------------------------------------------------------------
n1 <- 10
n2 <- 10
mu1 <- 1
mu2 <- 4
sd1 <- 1
sd2 <- 1
B <- 100000

## -----------------------------------------------------------------------------
set.seed(1234)
a1 <- rnorm(n1, mean = mu1, sd = sd1)
a2 <- rnorm(n2, mean = mu2, sd = sd2)

## -----------------------------------------------------------------------------
set.seed(1234)
b1 <- rgamma(n1, shape = 1, rate = 1)
b2 <- rgamma(n2, shape = 16, rate = 4)

## -----------------------------------------------------------------------------
set.seed(1234)
c1 <- rnorm(n1, mean = mu1, sd = sd1)
c2 <- rgamma(n2, shape = 16, rate = 4)

## -----------------------------------------------------------------------------
null_spec <- function(y, parameters) {
  purrr::map(y, ~ .x - parameters[1])
}

## -----------------------------------------------------------------------------
stat_functions <- list(stat_t)

## -----------------------------------------------------------------------------
stat_assignments <- list(delta = 1)

## ---- eval=FALSE--------------------------------------------------------------
#  pf <- PlausibilityFunction$new(
#    null_spec = null_spec,
#    stat_functions = stat_functions,
#    stat_assignments = stat_assignments,
#    x, y
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  pf$get_value(0)

## ---- eval=FALSE--------------------------------------------------------------
#  pfa <- PlausibilityFunction$new(
#    null_spec = null_spec,
#    stat_functions = stat_functions,
#    stat_assignments = stat_assignments,
#    a1, a2
#  )
#  pfa$set_nperms(B)

## ---- eval=FALSE--------------------------------------------------------------
#  pfa$set_point_estimate(mean(a2) - mean(a1))

## -----------------------------------------------------------------------------
pfa$point_estimate

## ---- eval=FALSE--------------------------------------------------------------
#  pfa$parameters

## ---- echo=FALSE--------------------------------------------------------------
p <- pfa$parameters
p[[1]]$range$lower <- dials::unknown()
p[[1]]$range$upper <- dials::unknown()
p

## ---- eval=FALSE--------------------------------------------------------------
#  pfa$set_parameter_bounds(
#    point_estimate = pfa$point_estimate,
#    conf_level = pfa$max_conf_level
#  )

## -----------------------------------------------------------------------------
pfa$parameters

## ---- eval=FALSE--------------------------------------------------------------
#  pfa$set_grid(
#    parameters = pfa$parameters,
#    npoints = 50L
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  pfa$grid

## ---- echo=FALSE--------------------------------------------------------------
select(pfa$grid, -pvalue)

## ---- eval=FALSE--------------------------------------------------------------
#  pfa$evaluate_grid(grid = pfa$grid)

## -----------------------------------------------------------------------------
pfa$grid

## -----------------------------------------------------------------------------
dfa <- pfa$grid %>%
  mutate(
    pvalue_alt = delta %>%
      map_dbl(~ {
        t.test(
          x = a2, 
          y = a1, 
          alternative = "two.sided", 
          mu = .x, 
          var.equal = TRUE
        )$p.value
      })
  ) %>% 
  select(
    delta, 
    `Parametric Approach`  = pvalue_alt, 
    `Permutation Approach` = pvalue
  ) %>% 
  pivot_longer(-delta)

## ---- fig.asp=0.8, fig.width=6, out.width="97%", dpi=300----------------------
dfa %>% 
  filter(value > 1e-3) %>% 
  ggplot(aes(delta, value, color = name)) + 
  geom_line() + 
  geom_hline(
    yintercept = 0.05, 
    color = "black", 
    linetype = "dashed"
  ) + 
  labs(
    title = "Scenario A: P-value function for the mean difference", 
    subtitle = "Using Student's t-statistic and two-tailed p-values", 
    x = expression(delta), 
    y = "p-value", 
    color = "Type of test"
  ) + 
  theme_bw() + 
  theme(legend.position = "top") + 
  scale_y_log10()

## ---- eval=FALSE--------------------------------------------------------------
#  pfb <- PlausibilityFunction$new(
#    null_spec = null_spec,
#    stat_functions = stat_functions,
#    stat_assignments = stat_assignments,
#    b1, b2
#  )
#  pfb$set_nperms(B)
#  pfb$set_point_estimate(mean(b2) - mean(b1))
#  pfb$set_parameter_bounds(
#    point_estimate = pfb$point_estimate,
#    conf_level = pfb$max_conf_level
#  )
#  pfb$set_grid(
#    parameters = pfb$parameters,
#    npoints = 50L
#  )
#  pfb$evaluate_grid(grid = pfb$grid)

## -----------------------------------------------------------------------------
dfb <- pfb$grid %>%
  mutate(
    pvalue_alt = delta %>%
      map_dbl(~ {
        t.test(
          x = b2, 
          y = b1, 
          alternative = "two.sided", 
          mu = .x, 
          var.equal = TRUE
        )$p.value
      })
  ) %>% 
  select(
    delta, 
    `Parametric Approach`  = pvalue_alt, 
    `Permutation Approach` = pvalue
  ) %>% 
  pivot_longer(-delta)

## ----fig.asp=0.8, fig.width=6, out.width="97%", dpi=300-----------------------
dfb %>% 
  filter(value > 1e-3) %>% 
  ggplot(aes(delta, value, color = name)) + 
  geom_line() + 
  geom_hline(
    yintercept = 0.05, 
    color = "black", 
    linetype = "dashed"
  ) + 
  labs(
    title = "Scenario B: P-value function for the mean difference", 
    subtitle = "Using Student's t-statistic and two-tailed p-values", 
    x = expression(delta), 
    y = "p-value", 
    color = "Type of test"
  ) + 
  theme_bw() + 
  theme(legend.position = "top") + 
  scale_y_log10()

## ---- eval=FALSE--------------------------------------------------------------
#  pfc <- PlausibilityFunction$new(
#    null_spec = null_spec,
#    stat_functions = stat_functions,
#    stat_assignments = stat_assignments,
#    c1, c2
#  )
#  pfc$set_nperms(B)
#  pfc$set_point_estimate(mean(c2) - mean(c1))
#  pfc$set_parameter_bounds(
#    point_estimate = pfc$point_estimate,
#    conf_level = pfc$max_conf_level
#  )
#  pfc$set_grid(
#    parameters = pfc$parameters,
#    npoints = 50L
#  )
#  pfc$evaluate_grid(grid = pfc$grid)

## -----------------------------------------------------------------------------
dfc <- pfc$grid %>%
  mutate(
    pvalue_alt = delta %>%
      map_dbl(~ {
        t.test(
          x = c2, 
          y = c1, 
          alternative = "two.sided", 
          mu = .x, 
          var.equal = TRUE
        )$p.value
      })
  ) %>% 
  select(
    delta, 
    `Parametric Approach`  = pvalue_alt, 
    `Permutation Approach` = pvalue
  ) %>% 
  pivot_longer(-delta)

## ----fig.asp=0.8, fig.width=6, out.width="97%", dpi=300-----------------------
dfc %>% 
  filter(value > 1e-3) %>% 
  ggplot(aes(delta, value, color = name)) + 
  geom_line() + 
  geom_hline(
    yintercept = 0.05, 
    color = "black", 
    linetype = "dashed"
  ) + 
  labs(
    title = "Scenario C: P-value function for the mean difference", 
    subtitle = "Using Student's t-statistic and two-tailed p-values", 
    x = expression(delta), 
    y = "p-value", 
    color = "Type of test"
  ) + 
  theme_bw() + 
  theme(legend.position = "top") + 
  scale_y_log10()

## ---- eval=FALSE--------------------------------------------------------------
#  pfa$set_point_estimate(overwrite = TRUE)

## -----------------------------------------------------------------------------
pfa$point_estimate

## ---- eval=FALSE--------------------------------------------------------------
#  pfa$set_parameter_bounds(
#    point_estimate = pfa$point_estimate,
#    conf_level = 0.95
#  )

## -----------------------------------------------------------------------------
pfa$parameters

## -----------------------------------------------------------------------------
pfa$set_nperms(1000)
pfa$get_value(3)

