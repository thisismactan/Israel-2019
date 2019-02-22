source("Code/library.R")

polls_2009 <- read_xlsx("Data/polls.xlsx", sheet = "2009", range = "A1:L48", col_names = TRUE) %>%
  mutate(date = as.Date(date))
polls_2013 <- read_xlsx("Data/polls.xlsx", sheet = "2013", range = "A1:K61", col_names = TRUE)  %>%
  mutate(date = as.Date(date))
polls_2015 <- read_xlsx("Data/polls.xlsx", sheet = "2015", range = "A1:L101", col_names = TRUE)  %>%
  mutate(date = as.Date(date))
results <- read_xlsx("Data/polls.xlsx", sheet = "Results", range = "A1:C34", col_names = TRUE) 
results_2009 <- results %>%
  filter(year(date) %in% 2008:2009)
results_2013 <- results %>%
  filter(year(date) %in% 2012:2013)
results_2015 <- results %>%
  filter(year(date) %in% 2014:2015)

polls_2009.vote <- polls_2009 %>%
  mutate_at(3:12, function(x) x/120)
polls_2013.vote <- polls_2013 %>%
  mutate_at(3:11, function(x) x/120)
polls_2015.vote <- polls_2015 %>%
  mutate_at(3:12, function(x) x/120)

polls_2009.logit <- polls_2009.vote %>%
  mutate_at(3:12, logit)
polls_2013.logit <- polls_2013.vote %>%
  mutate_at(3:11, logit)
polls_2015.logit <- polls_2015.vote %>%
  mutate_at(3:12, logit)

#### Error calculation ####
error_2009 <- polls_2009.logit %>%
  mutate(kadima = kadima - logit(28/120),
         labor = labor - logit(13/120),
         shas = shas - logit(11/120),
         likud = likud - logit(27/120),
         yisrael_beiteinu = yisrael_beiteinu - logit(15/120),
         arab_parties = arab_parties - logit(11/120),
         jewish_home = jewish_home - logit(3/120),
         national_union = national_union - logit(4/120),
         utj = utj - logit(5/120),
         meretz = meretz - logit(3/120)) %>%
  melt(id.vars = c("date", "pollster")) %>%
  mutate(year = 2009) %>%
  filter(as.numeric(as.Date(date) - as.Date("2009-02-10")) <= 30,
         value != -Inf)

error_2013 <- polls_2013.logit %>%
  mutate(likud_yb = likud_yb - logit(31/120),
         labor = labor - logit(15/120),
         shas = shas - logit(11/120),
         utj = utj - logit(7/120),
         jewish_home = jewish_home - logit(12/120),
         arab_parties = arab_parties - logit(11/120),
         meretz = meretz - logit(6/120),
         yesh_atid = yesh_atid - logit(19/120),
         hatnuah = hatnuah - logit(6/120)) %>%
  melt(id.vars = c("date", "pollster")) %>%
  mutate(year = 2013) %>%
  filter(as.numeric(as.Date(date) - as.Date("2013-01-22")) <= 30,
         value != -Inf)

error_2015 <- polls_2015.logit %>%
  mutate(likud = likud - logit(30/120),
         yisrael_beiteinu = yisrael_beiteinu - logit(6/120),
         yesh_atid = yesh_atid - logit(11/120),
         zionist_union = zionist_union - logit(24/120),
         jewish_home = jewish_home - logit(8/120),
         shas = shas - logit(7/120),
         utj = utj - logit(6/120),
         meretz = meretz - logit(5/120),
         joint_list = joint_list - logit(13/120),
         kulanu = kulanu - logit(10/120)) %>%
  melt(id.vars = c("date", "pollster")) %>%
  mutate(year = 2015) %>%
  filter(as.numeric(as.Date(date) - as.Date("2015-03-17")) <= 30,
         value != -Inf)

#### Plots ####
## Shape to long
polls_2009.long <- polls_2009 %>%
  melt(id.vars = c("date", "pollster")) %>%
  na.omit()

polls_2013.long <- polls_2013 %>%
  melt(id.vars = c("date", "pollster")) %>%
  na.omit()

polls_2015.long <- polls_2015 %>%
  melt(id.vars = c("date", "pollster")) %>%
  na.omit()

## Poll plots
ggplot() +
  geom_point(data = polls_2009.long, aes(x = date, y = value, col = variable), alpha = 0.5, size = 1) +
  geom_smooth(data = polls_2009.long, aes(x = date, y = value, col = variable), method = "loess", span = 0.5) +
  geom_point(data = results_2009, aes(x = as.Date(date), y = seats, fill = party), show.legend = FALSE, pch = 23) +
  scale_colour_manual(name = "Party/List", values = party_palette[parties2009], labels = party_labels[parties2009]) +
  scale_fill_manual(name = "Result",
                    values = c("darkgreen", "darkblue", "indianred1", "red", "dodgerblue4", "greenyellow", "blue", "deepskyblue", 
                               "black", "darkslateblue"),
                    labels = c("Arab parties", "Jewish Home", "Kadima", "Labor", "Likud", "Meretz", "National Union", "Shas", 
                               "United Torah Judaism", "Yisrael Beiteinu")) +
  scale_x_date(date_breaks = "week", limits = c(as.Date("2009-01-01"), as.Date("2009-02-10")), date_labels = "%B %e") +
  labs(title = "2009 Israeli general election polling", x = "Date", y = "Seats") +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_point(data = polls_2013.long, aes(x = date, y = value, col = variable), alpha = 0.5, size = 1) +
  geom_smooth(data = polls_2013.long, aes(x = date, y = value, col = variable), method = "loess", span = 0.5) +
  geom_point(data = results_2013, aes(x = as.Date(date), y = seats, fill = party), show.legend = FALSE, pch = 23) +
  scale_colour_manual(name = "Party/List", values = party_palette[parties2013], labels = party_labels[parties2013]) +
  scale_fill_manual(name = "Result",
                    values = c("darkgreen", "indianred1", "darkblue", "red", "dodgerblue4", "greenyellow", "deepskyblue", 
                               "black", "cyan3")) +
  scale_x_date(date_breaks = "week", limits = c(as.Date("2012-10-28"), as.Date("2013-01-22")), date_labels = "%b %e, %Y") +
  labs(title = "2013 Israeli general election polling", x = "Date", y = "Seats") +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_point(data = polls_2015.long, aes(x = date, y = value, col = variable), alpha = 0.5, size = 1) +
  geom_smooth(data = polls_2015.long, aes(x = date, y = value, col = variable), method = "loess", span = 0.5) +
  geom_point(data = results_2015, aes(x = as.Date(date), y = seats, fill = party), show.legend = FALSE, pch = 23) +
  scale_colour_manual(name = "Party/List",
                      values = c("dodgerblue4", "darkslateblue", "cyan3", "red", "darkblue", "deepskyblue", "black", 
                                 "greenyellow", "darkgreen", "darkturquoise"),
                      labels = c("Likud", "Yisrael Beiteinu", "Yesh Atid", "Zionist Union", "Jewish Home", "Shas", "United Torah Judaism", 
                                 "Meretz", "Joint List", "Kulanu")) +
  scale_fill_manual(name = "Result",
                    values = c("darkblue", "darkgreen", "darkturquoise", "dodgerblue4", "greenyellow", "deepskyblue", "black",
                               "cyan3", "darkslateblue", "red")) + 
  scale_x_date(date_breaks = "week", limits = c(as.Date("2014-12-16"), as.Date("2015-03-17")), date_labels = "%b %e, %Y") +
  labs(title = "2015 Israeli general election polling", x = "Date", y = "Seats") +
  theme(axis.text.x = element_text(angle = 90))

