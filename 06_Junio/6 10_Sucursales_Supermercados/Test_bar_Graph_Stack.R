# Test

df <- read.table(text="County  Group   Plan1   Plan2   Plan3   Plan4   Plan5   Total
County1 Group1  2019    597 513 5342    3220    11691
                 County2 Group1  521 182 130 1771    731 3335
                 County3 Group1  592 180 126 2448    1044    4390
                 County4 Group1  630 266 284 2298    937 4415
                 County5 Group1  708 258 171 2640    1404    5181
                 County6 Group1  443 159 71  1580    528 2781
                 County7 Group1  492 187 157 1823    900 3559
                 County8 Group1  261 101 84  1418    357 2221", header = TRUE)

library(tidyverse)
df %>% 
  filter(Group == "Group1") %>% 
  select(-Total) %>% 
  gather(key = `Plan Type`, value = value, -County, -Group) %>% 
  group_by(County, Group) %>% 
  mutate(Percentage = value/sum(value)) %>% 
  ggplot(aes(x = County, y = Percentage, fill = `Plan Type`, label = paste0(round(Percentage*100), "%"))) +
  geom_col(position = position_stack(), color = "black") +
  geom_text(position = position_stack(vjust = .5)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format())
