## Simulation
source("Code/error_variance.R")

## Seed it
set.seed(2019)

## Multivariate normal simulation on logit scale
simulations.logit <- rmvn(100000, mu = means, sigma = party_covariance)

## Do a series of conversions to calculate corresponding seat counts
simulations.vote <- invlogit(simulations.logit) 
simulations.vote_scaled <- t(simulations.vote) %>% scale(center = FALSE, scale = rowSums(simulations.vote)) %>% t()
simulations.threshold <- simulations.vote >= 0.0325
simulations.vote_thresholded <- t(simulations.vote_scaled*simulations.threshold) %>%
  scale(center = FALSE, scale = rowSums(simulations.vote_scaled*simulations.threshold)) %>% 
  t()

## Make it into a tibble
simulations.seats <- round(simulations.vote_thresholded*120) %>%
  as.data.frame() %>%
  as.tbl()

names(simulations.seats) <- poll_averages_logit$party

simulations.df <- simulations.seats %>%
  mutate(left = meretz + labor,
         center = blue_white,
         center_right = kulanu + gesher,
         right = likud + yisrael_beiteinu + new_right + urwp + zehut,
         arab = taal_hadash + balad_raam,
         ultraorthodox = shas + utj)

## By party
party_seats <- simulations.df %>%
  dplyr::select(-left, -center, -center_right, -right, -ultraorthodox, -arab)

# Largest party
party_seats %>%
  mutate(likud_largest = likud > blue_white,
         blue_white_largest = blue_white > likud,
         both_largest  = likud == blue_white) %>%
  dplyr::select(likud_largest, blue_white_largest, both_largest) %>%
  melt(id.vars = NULL) %>%
  group_by(party = variable) %>%
  summarise(prob = mean(value))

# Quantiles
party_seats %>%
  melt(variable.name = "party", value.name = "seats") %>%
  group_by(party) %>%
  dplyr::summarise(mean = mean(seats),
                   pct_5 = quantile(seats, 0.05),
                   pct_25 = quantile(seats, 0.25),
                   pct_50 = quantile(seats, 0.50),
                   pct_75 = quantile(seats, 0.75),
                   pct_95 = quantile(seats, 0.95))

party_seats %>%
  melt(variable.name = "party", value.name = "seats") %>%
  plyr::join(parties_df, by = "party") %>%
  ggplot(aes(x = seats, y = ..density.., fill = party)) +
  geom_histogram(binwidth = 1, col = "black", show.legend = FALSE) +
  facet_wrap(~party, labeller = as_labeller(party_facet_labels)) +
  scale_fill_manual(name = "Party", values = party_palette[parties2019], labels = party_labels[parties2019]) +
  labs(title = "Projected seat distributions by party", 
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       x = "Seats", y = "Probability")

## By bloc
ideology_seats <- simulations.df %>%
  dplyr::select(left, center, center_right, right, arab, ultraorthodox)

# Largest bloc
ideology_seats %>%
  mutate(right_largest = right > pmax(left, center, center_right, arab, ultraorthodox),
         center_largest = center > pmax(left, center_right, right, arab, ultraorthodox),
         both_largest = right == pmax(left, center, center_right, right, arab, ultraorthodox) &
           center == pmax(left, center, center_right, right, arab, ultraorthodox)) %>%
  dplyr::select(right_largest, center_largest, both_largest) %>%
  melt(id.vars = NULL) %>%
  group_by(party = variable) %>%
  summarise(prob = mean(value))

# Credible intervals
ideology_seats %>%
  melt(variable.name = "bloc", value.name = "seats") %>%
  group_by(bloc) %>%
  dplyr::summarise(mean = mean(seats),
                   pct_5 = quantile(seats, 0.05),
                   pct_25 = quantile(seats, 0.25),
                   pct_50 = quantile(seats, 0.50),
                   pct_75 = quantile(seats, 0.75),
                   pct_95 = quantile(seats, 0.95))

## Distributions by bloc
ideology_seats %>%
  melt(variable.name = "bloc", value.name = "seats") %>%
  ggplot(aes(x = seats, y = ..density.., fill = bloc)) +
  geom_histogram(binwidth = 1, col = "black", show.legend = FALSE) +
  facet_wrap(~bloc, labeller = as_labeller(bloc_labels), scales = "free_x") +
  scale_fill_manual(name = "Bloc", values = c("red", "yellow", "darkturquoise", "blue", "darkgreen", "black"),
                    labels = bloc_labels) +
  lims(x = c(0,75), y = c(0, 0.2)) +
  labs(title = "Israel 2019 forecast seat distributions", 
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       x = "Seats", y = "Probability")