library(brms)
#devtools::install_github("bnicenboim/bcogsci")
library(bcogsci)

data("df_spacebar")
df_spacebar

fit_press <- brm(rt ~ 1,
                     data = df_spacebar,
                     family = gaussian(),
                     prior = c(
                         prior(uniform(0, 60000), class = Intercept),
                         prior(uniform(0, 2000), class = sigma)
                     ),
                     chains = 4,
                     iter = 2000,
                     warmup = 1000
)
fit_press


fit_press_unif <- brm(rt ~ 1,
                      data = df_spacebar,
                      family = gaussian(),
                      prior = c(
                          prior(uniform(-10^10, 10^10), class = Intercept),
                          prior(uniform(0, 10^10), class = sigma)
                      )
)

fit_press_inf <- brm(rt ~ 1,
                         data = df_spacebar,
                         family = gaussian(),
                         prior = c(
                             prior(normal(400, 10), class = Intercept),
                             # brms knows that SDs need to be bounded 
                             # to exclude values below zero:
                             prior(normal(100, 10), class = sigma)
                         )
)

fit_press_reg <- brm(rt ~ 1,
                     data = df_spacebar,
                     family = gaussian(),
                     prior = c(
                         prior(normal(200, 100), class = Intercept),
                         prior(normal(50, 50), class = sigma)
                     )
)

posterior_predict(fit_press)
pp_check(fit_press_reg, ndraws = 11, type = "hist")

N_obs <- nrow(df_spacebar)
mu_samples <- as_draws_df(fit_press)$b_Intercept
sigma_samples <- as_draws_df(fit_press)$sigma
normal_predictive_distribution(
    mu_samples = mu_samples,
    sigma_samples = sigma_samples,
    N_obs = N_obs
)

pp_check(fit_press, ndraws = 11, type = "hist")
pp_check(fit_press, ndraws = 100, type = "dens_overlay")
normal_predictive_distribution <- function(mu_samples, sigma_samples, N_obs) {
    # empty data frame with headers:
    df_pred <- tibble(
        trialn = numeric(0),
        rt_pred = numeric(0),
        iter = numeric(0)
    )
    # i iterates from 1 to the length of mu_samples,
    # which we assume is identical to
    # the length of the sigma_samples:
    for (i in seq_along(mu_samples)) {
        mu <- mu_samples[i]
        sigma <- sigma_samples[i]
        df_pred <- bind_rows(
            df_pred,
            tibble(
                trialn = seq_len(N_obs), # 1, 2,... N_obs
                rt_pred = rnorm(N_obs, mu, sigma),
                iter = i
            )
        )
    }
    df_pred
}



N_obs <- nrow(df_spacebar)
mu_samples <- as_draws_df(fit_press)$b_Intercept
sigma_samples <- as_draws_df(fit_press)$sigma
normal_predictive_distribution(
    mu_samples = mu_samples,
    sigma_samples = sigma_samples,
    N_obs = N_obs
)

prior_pred %>%
    filter(iter <= 18) %>%
    ggplot(aes(rt_pred)) +
    geom_histogram() +
    facet_wrap(~iter, ncol = 3)