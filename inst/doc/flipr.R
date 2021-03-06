## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
load("../R/sysdata.rda")

## ----setup, message = FALSE---------------------------------------------------
library(tidyverse)
library(flipr)

generate_grid <- function(center_value, min_value, max_value, n) {
  stopifnot(center_value > min_value && center_value < max_value)
  c(
    seq(min_value, center_value, len = n / 2 + 1)[1:(n / 2)], 
    center_value, 
    seq(center_value, max_value, len = n / 2 + 1)[-1]
  )
}

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
null_spec <- function(y, parameters) {y - parameters[1]}

## ---- eval=FALSE--------------------------------------------------------------
#  delta_pe <- mean(a2) - mean(a1)
#  dfa <- tibble(
#    delta = generate_grid(delta_pe, delta_pe - 2, delta_pe + 2, 20),
#    pvalue = delta %>%
#      two_sample_pf(
#        null_specification = null_spec,
#        x = a1,
#        y = a2,
#        statistic = stat_t,
#        B = B,
#        seed = 1234,
#        alternative = "two_tail"
#      )
#  ) %>%
#    mutate(
#      pvalue_alt = delta %>%
#        map_dbl(~ {
#          t.test(
#            x = a2,
#            y = a1,
#            alternative = "two.sided",
#            mu = .x,
#            var.equal = TRUE
#          )$p.value
#        })
#    ) %>%
#    select(
#      delta,
#      `Parametric Approach` = pvalue_alt,
#      `Permutation Approach` = pvalue
#    ) %>%
#    pivot_longer(-delta)

## ---- fig.asp=0.8, fig.width=6, out.width="97%", dpi=300----------------------
dfa %>% 
  subset(name == "Permutation Approach") %>% 
  ggplot(aes(delta, value)) + 
  geom_line() + 
  labs(
    title = "P-value function for the mean difference", 
    subtitle = "Using Student's t-statistic and two-tailed p-values", 
    x = expression(delta), 
    y = "p-value"
  ) + 
  theme_bw()

## ---- fig.asp=0.8, fig.width=6, out.width="97%", dpi=300----------------------
dfa %>% 
  ggplot(aes(delta, value, color = name)) + 
  geom_line() + 
  labs(
    title = "Scenario A: P-value function for the mean difference", 
    subtitle = "Using Student's t-statistic and two-tailed p-values", 
    x = expression(delta), 
    y = "p-value"
  ) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  facet_wrap(vars(name), nrow = 1)

## ---- eval=FALSE--------------------------------------------------------------
#  deltb_pe <- mean(b2) - mean(b1)
#  dfb <- tibble(
#    delta = generate_grid(deltb_pe, deltb_pe - 2, deltb_pe + 2, 20),
#    pvalue = delta %>%
#      two_sample_pf(
#        null_specification = null_spec,
#        x = b1,
#        y = b2,
#        statistic = stat_t,
#        B = B,
#        seed = 1234,
#        alternative = "two_tail"
#      )
#  ) %>%
#    mutate(
#      pvalue_alt = delta %>%
#        map_dbl(~ {
#          t.test(
#            x = b2,
#            y = b1,
#            alternative = "two.sided",
#            mu = .x,
#            var.equal = TRUE
#          )$p.value
#        })
#    ) %>%
#    select(
#      delta,
#      Parametric = pvalue_alt,
#      Permutation = pvalue
#    ) %>%
#    pivot_longer(-delta)

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
#  deltc_pe <- mean(c2) - mean(c1)
#  dfc <- tibble(
#    delta = generate_grid(deltc_pe, deltc_pe - 2, deltc_pe + 2, 20),
#    pvalue = delta %>%
#      two_sample_pf(
#        null_specification = null_spec,
#        x = c1,
#        y = c2,
#        statistic = stat_t,
#        B = B,
#        seed = 1234,
#        alternative = "two_tail"
#      )
#  ) %>%
#    mutate(
#      pvalue_alt = delta %>%
#        map_dbl(~ {
#          t.test(
#            x = c2,
#            y = c1,
#            alternative = "two.sided",
#            mu = .x,
#            var.equal = TRUE
#          )$p.value
#        })
#    ) %>%
#    select(
#      delta,
#      Parametric = pvalue_alt,
#      Permutation = pvalue
#    ) %>%
#    pivot_longer(-delta)

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

## -----------------------------------------------------------------------------
optimise(
  f = two_sample_pf, 
  interval = c(0, 6), 
  null_specification = null_spec, 
  x = a1, 
  y = a2, 
  statistic = stat_t, 
  B = 10000, 
  seed = 1234, 
  alternative = "two_tail", 
  maximum = TRUE
)

## -----------------------------------------------------------------------------
two_sample_ci(
  point_estimate = mean(a2) - mean(a1), 
  alpha = 0.05, 
  null_specification = null_spec, 
  x = a1, 
  y = a2, 
  statistic = stat_t, 
  B = 10000, 
  alternative = "two_tail"
)

## -----------------------------------------------------------------------------
two_sample_pf(
  parameters = 3, 
  null_specification = null_spec, 
  x = a1, 
  y = a2, 
  statistic = stat_t, 
  B = 10000, 
  seed = 1234, 
  alternative = "two_tail"
  )

