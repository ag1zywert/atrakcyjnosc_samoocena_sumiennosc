library(tidyverse)
library(ez)
d <- read_csv("dane_wlasciwe.csv")

## który warunek jest który? 0 to odpowiedzialna, 1 to nieodpowiedzialna
d <- d %>% 
  mutate(reason = ifelse(reasonable == 0, "yes", "no"))


ggplot(d) +
  geom_freqpoly(mapping = aes(x = attract), binwidth = 1)

d %>% 
  ggplot() +
  geom_freqpoly(aes(x = attract, color = reason), binwidth = 1)

d %>% 
  ggplot() + geom_boxplot(aes(x = reason, y = attract))

d %>% 
  group_by(reason) %>% 
  summarise(m_attr = mean(attract), sd_attr = sd(attract))

median <- summarise(d, medi = median(s_esteem))

d <- d %>% 
  mutate(esteem_split = ifelse(s_esteem <= median$medi, "low", "high"))

d %>% group_by(esteem_split) %>%  summarise(count = n())

#d <- d %>% 
 # mutate(esteem_three = ifelse(s_esteem > ))

d %>% 
  ggplot() + 
  geom_freqpoly(aes(x = s_esteem), binwidth = 2)

t.test(d$attract ~ d$reason)

# nic z tego
attract_anova <- ezANOVA(data = d, wid = id, between = reason, 
                         between_covariates = s_esteem, dv = attract,
                         type = 2)
print(attract_anova)

# wywal dewiantów o niskiej samooc
d_outliers <- d %>% 
  filter(s_esteem >= 20)

## dalej nic
attract_anova <- ezANOVA(data = d_outliers, 
                         wid = id, 
                         between = .(reason, relationship), 
                         dv = attract,
                         type = 2)
print(attract_anova)


d %>% group_by(relationship) %>% summarise(count = n(), m = mean(attract))

d %>% 
  ggplot() + geom_freqpoly(aes(x = attract, color = relationship), binwidth = 1)

d %>% ggplot() + 
  geom_point(aes(x = age, y = attract), alpha = 4/10) + 
  geom_smooth(aes(x = age, y = attract))


d <- d %>% 
  mutate(age_split = ifelse(age <= median(d$age), "low", "high"))

attract_age_anova <- ezANOVA(data = d, 
                         wid = id, 
                         between = .(reason),
#                         between_covariates = age,
                         dv = attract,
                         type = 2)
print(attract_age_anova)

d %>% 
  ggplot() + 
  geom_point(aes(x = age, y = attract, color = reason), 
             position = position_jitter())

d_young <- d %>% filter(age < 30)
attract_age_anova <- ezANOVA(data = d_young, 
                             wid = id, 
                             between = .(reason, esteem_split),
                             #                         between_covariates = age,
                             dv = attract,
                             type = 2)
print(attract_age_anova)
cor.test(d_young$attract, d_young$s_esteem)


d_old <- d %>% filter(age >= 30)
attract_age_anova <- ezANOVA(data = d_old, 
                             wid = id, 
                             between = .(reason, esteem_split),
                             #                         between_covariates = age,
                             dv = attract,
                             type = 2)
print(attract_age_anova)
cor.test(d_old$attract, d_old$s_esteem)
