library(tidyverse)
str(dane_wlasciwe)

warunki <- group_by(dane_wlasciwe, Warunek)
summarise (warunki, srednia = mean(Ocena), odchylenie = mad(Ocena))

status_zwiazku <- group_by(dane_wlasciwe, status.zwiazku)
summarise (status_zwiazku, srednia = mean(Ocena), odchylenie = mad(Ocena))

ggplot(data = dane_wlasciwe) +
  geom_point(mapping = aes(x = Wiek, y = Ocena, alpha = 3/10)) +
  geom_smooth(mapping = aes(x = Wiek, y = Ocena))

