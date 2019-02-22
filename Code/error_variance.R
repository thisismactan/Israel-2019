## Calculate error variances

source("Code/historical_accuracy.R")
source("Code/polling_average.R")

## Compute error variance
poll_errors <- bind_rows(error_2009, error_2013, error_2015) %>%
  as.tbl() %>%
  mutate(weight = 1/(2019 - year)) %>%
  group_by(party = variable) %>%
  summarise(variance = Hmisc::wtd.var(value, weight))

## Compute variances on logit scale
poll_averages_logit <- poll_average.logit %>%
  left_join(poll_errors, by = "party") %>%
  mutate(variance = case_when(!is.na(variance) ~ variance,
                              party == "taal_hadash" ~ mean(poll_errors$variance[c(1,4)]),
                              party == "balad_raam" ~ mean(poll_errors$variance[c(1,4)]),
                              party == "blue_white" ~ poll_errors$variance[14],
                              party == "gesher" ~ 0,
                              party == "new_right" ~ mean(poll_errors$variance[c(8,9,11)]),
                              party == "zehut" ~ mean(poll_errors$variance[c(11)]),
                              party == "urwp" ~ mean(poll_errors$variance[3])),
         poll_variance = sd^2,
         total_variance = variance + poll_variance)

## Mean and variance for simulation
means <- poll_averages_logit$vote_logit
variances <- diag(poll_averages_logit$variance) 

polls_2019.logit_wide <- polls_2019.logit %>%
  dplyr::select(party, date, pollster, vote_logit) %>%
  reshape2::dcast(...~party, value.var = "vote_logit") %>%
  na.omit()

party_covariance <- (polls_2019.logit_wide %>%
  dplyr::select(-date, -pollster) %>%
  as.matrix()) %>%
  cov() + variances