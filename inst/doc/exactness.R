params <-
list(R = 1000L, seed = 12345L)

## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(tidyverse)
library(flipr)

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
htmlwidgets::saveWidget(fig, "plotly-fig.html")
htmltools::tags$iframe(
  src = "plotly-fig.html",
  scrolling = "no", 
  seamless = "seamless",
  frameBorder = "0",
  width = "100%", 
  height = 400
)

## ----power-simulation---------------------------------------------------------
alpha <- 0.05
R <- params$R
set.seed(params$seed)
1:R %>% 
  map(~ {
    x <- rnorm(n = 10, mean = 0, sd = 1)
    y <- rnorm(n = 10, mean = 0, sd = 1)
    test_exact <- two_sample_test(
      x = x, 
      y = y, 
      statistic = stat_hotelling, 
      test = "exact", 
      B = 100
    )
    test_approx <- two_sample_test(
      x = x, 
      y = y, 
      statistic = stat_hotelling, 
      test = "approximate", 
      B = 100
    )
    c(
      approx = (test_approx$pvalue <= alpha), 
      exact = (test_exact$pvalue <= alpha)
    )
  }) %>% 
  transpose() %>% 
  simplify_all() %>% 
  map(mean)
