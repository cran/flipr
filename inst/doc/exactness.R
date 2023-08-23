## ----setup, include = FALSE---------------------------------------------------
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

## ----fig.width=6, out.width="100%"--------------------------------------------
alpha <- seq(0.01, 0.1, by = 0.01)
m <- c(10, 100, 1000)
p1 <- crossing(alpha, m) %>% 
  mutate(
    p = (floor(m * alpha) + 1) / (m + 1), 
    mf = paste("m =", m)
  ) %>% 
  ggplot(aes(alpha, p, color = mf)) + 
  geom_point() + 
  geom_abline(aes(intercept = 0, slope = 1)) + 
  labs(
    x = "Significance level",
    y = "Probability of wrongly rejecting H0"
  ) + 
  facet_wrap(vars(mf)) + 
  scale_color_viridis_d() + 
  scale_y_continuous(limits = c(0, 0.1)) + 
  coord_equal() + 
  theme_bw()
fig <- p1 %>% 
  plotly::ggplotly() %>% 
  plotly::hide_legend()
htmlwidgets::saveWidget(
  widget = fig, 
  file = "exactness-fig1.html", 
  selfcontained = rmarkdown::pandoc_available("1.12.3")
)
htmltools::tags$iframe(
  src = "exactness-fig1.html",
  scrolling = "no", 
  seamless = "seamless",
  frameBorder = "0",
  width = "100%", 
  height = 400
)

## ---- eval=FALSE--------------------------------------------------------------
#  # General setup
#  nreps <- 1e4
#  n1 <- 5
#  n2 <- 5
#  set.seed(12345)
#  sim <- map(sample.int(.Machine$integer.max, nreps, replace = TRUE), ~ {
#      list(
#        x = rnorm(n = n1, mean = 0, sd = 1),
#        y = rnorm(n = n2, mean = 0, sd = 1),
#        s = .x
#      )
#    })
#  
#  # Cluster setup
#  cl <- makeCluster(detectCores(logical = FALSE))
#  clusterEvalQ(cl, {
#    library(tidyverse)
#    library(flipr)
#    null_spec <- function(y, parameters) {
#      map(y, ~ .x - parameters)
#    }
#    stat_functions <- list(stat_t)
#    stat_assignments <- list(delta = 1)
#    nperms <- 20
#    alpha <- 0.05
#  })
#  
#  alpha_estimates <- pbapply::pblapply(sim, function(.l) {
#      pf <- PlausibilityFunction$new(
#        null_spec = null_spec,
#        stat_functions = stat_functions,
#        stat_assignments = stat_assignments,
#        .l$x, .l$y,
#        seed = .l$s
#      )
#      pf$set_nperms(nperms)
#      pf$set_alternative("left_tail")
#      pf$set_pvalue_formula("exact")
#      pv_exact <- pf$get_value(0)
#      pf$set_pvalue_formula("upper_bound")
#      pv_upper_bound <- pf$get_value(0)
#      pf$set_pvalue_formula("estimate")
#      pv_estimate <- pf$get_value(0)
#      c(
#        exact       = pv_exact       <= alpha,
#        upper_bound = pv_upper_bound <= alpha,
#        estimate    = pv_estimate    <= alpha
#      )
#    }, cl = cl) %>%
#    transpose() %>%
#    simplify_all() %>%
#    map(mean)
#  stopCluster(cl)

## -----------------------------------------------------------------------------
as_tibble(alpha_estimates)

