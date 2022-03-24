set.seed(1234)
n_birds <- 25
# beta0
avg_nestlings_at_avg_mass <- log(4.2)
#beta1
effect_one_gram <- .2

mother_masses_g <- rnorm(n_birds, mean = 15, sd = 2)
hist(mother_masses_g)
avg_mother_mass <- mean(mother_masses_g)

hist(mother_masses_g - avg_mother_mass)

log_average_nestlings <- avg_nestlings_at_avg_mass + effect_one_gram * (mother_masses_g - avg_mother_mass)

plot(mother_masses_g, log_average_nestlings)

nestlings <- rpois(n = n_birds, lambda = exp(log_average_nestlings))

library(tidyverse)

imaginary_birds <- tibble(mother_masses_g,
                          nestlings)

ggplot(imaginary_birds, aes(x = mother_masses_g, y = nestlings)) +
  geom_point()


glm(nestlings ~ 1 + I(mother_masses_g - mean(mother_masses_g)),
    family = poisson())



library(brms)

imaginary_birds_centered <- imaginary_birds |>
  mutate(mother_mass_g_cen = mother_masses_g - mean(mother_masses_g))

bird_form <- bf(nestlings ~ 1 + mother_mass_g_cen, family = poisson(link = "log"))

get_prior(bird_form, data = imaginary_birds_centered)

bird_priors <- c(
  prior(normal(1, .5), class = "Intercept"),
  prior(normal(.1, .1), class = "b", coef = "mother_mass_g_cen" )
)

bird_priors

prior_predictions <- brm(formula = bird_form,
                         data = imaginary_birds_centered,
                         prior = bird_priors,
                         sample_prior = "only")

stancode(prior_predictions)

imaginary_birds_centered |>
  add_predicted_draws(prior_predictions, ndraws = 6) |>
  ggplot(aes(x = mother_masses_g, y = .prediction)) +
  geom_point() +
  facet_wrap(~.draw)

bird_posterior <- update(prior_predictions, sample_prior = "yes")

tidybayes::tidy_draws(bird_posterior) |>
  select(.draw, b_Intercept:prior_b_mother_mass_g_cen) |>
  pivot_longer(-.draw) |>
  ggplot(aes(x = value, y = name)) + geom_density_ridges()

