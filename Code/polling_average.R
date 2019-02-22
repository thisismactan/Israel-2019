source("library.R")

#### 2019 polling average ####
election_date <- as.Date("2019-04-09")

polls_2019 <- read_xlsx("Data/polls.xlsx", sheet = "2019", range = "A1:U100", col_names = TRUE) %>%
  mutate(age = as.numeric(today() - as.Date(date) + 2),
         weight = (age < 45)/exp(age^0.3)) %>%
  dplyr::select(date, age, pollster, weight, taal_hadash, balad_raam, meretz, labor, blue_white, kulanu, gesher, likud, 
                yisrael_beiteinu, shas, utj, zehut, new_right, urwp)

polls_2019.long <- polls_2019 %>%
  melt(id.vars = c("date", "pollster", "age", "weight")) %>%
  na.omit() %>%
  dplyr::select(date, pollster, weight, party = variable, seats = value) %>%
  as.tbl()

polls_2019.logit <- polls_2019.long %>% 
  mutate(vote_share = pmax(0.025, seats/120),
         vote_logit = logit(vote_share))

poll_average.logit <- polls_2019.logit %>%
  filter(date >= as.Date("2019-02-21")) %>%
  group_by(party) %>%
  mutate(mean_vote.logit = wtd.mean(vote_logit, weights = weight),
         dev_sq = (vote_logit - mean_vote.logit)^2) %>%
  summarise(vote_logit = mean(mean_vote.logit),
            var = sum(dev_sq)/((n()-1)*sum(weight))) %>%
  ungroup() %>%
  mutate(sd = sqrt(var),
         party = ordered(party, levels = c("taal_hadash", "balad_raam", "meretz", "labor", "blue_white", "kulanu", "gesher", 
                                           "likud", "yisrael_beiteinu", "shas", "utj", "zehut", "new_right", "urwp"))) %>%
  arrange(as.numeric(party)) %>%
  dplyr::select(party, vote_logit, sd)

projected_seats <- poll_average.logit %>%
  mutate(lower_logit = vote_logit - 1.645*sd,
         upper_logit = vote_logit + 1.645*sd) %>%
  mutate_at(vars(ends_with("logit")), invlogit) %>%
  mutate(avg_sum = sum(vote_logit),
         lower_sum = sum(lower_logit),
         upper_sum = sum(upper_logit)) %>%
  mutate_at(vars(ends_with("logit")), function(x) x - x*(x < 0.0325)) %>%
  mutate_at(vars(ends_with("logit")), function(x) x*120) %>%
  dplyr::select(party, seats = vote_logit, lower = lower_logit, upper = upper_logit)

  
## Plot
source("party_colors.R")

ggplot(projected_seats, aes(x = party, y = seats, fill = party, label = round(seats, 1))) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  scale_fill_manual(name = "Party/List", 
                    values = party_palette[parties2019],
                    labels = party_labels[parties2019]) +
  geom_text(nudge_y = 0.8, size = 3) +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Seat projections for the 2019 Israeli Knesset election",
       subtitle = format(lubridate::today(), "Pre-deadline period"), y = "Seats",
       caption = "Error bars indicate 90% confidence intervals")

## Over time
ggplot(polls_2019.long, aes(x = as.Date(date), y = seats, col = party)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.8) +
  geom_point(alpha = 0.5) +
  scale_colour_manual(name = "Party/List",
                      values = party_palette[parties2019],
                      labels = party_labels[parties2019]) +
  labs(title = "2019 Israeli Knesset election polls",
       subtitle = paste0("January 14, 2019 - ", month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       x = "Date", y = "Seats") +
  scale_x_date(date_breaks = "week", limits = c(as.Date("2019-01-16"), as.Date("2019-04-09")), date_labels = "%B %e") +
  theme(axis.text.x = element_text(angle = 90))
