rm(list = ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)



rain <- read_excel("rain.xlsx")
rain %<>% select(year:Fada_Ngourma)

mean_data <- rain %>% filter(between(year, 1991, 2020)) %>% 
  group_by(month, day) %>% 
  select(-year) %>% 
  summarise (mean_rain = mean(Fada_Ngourma)) %>% 
  ungroup() %>% 
  mutate(mean_rain = cumsum(mean_rain), mean_rain_75 = 0.75*mean_rain, mean_rain_125 = 1.25*mean_rain )

rain_1996  <- rain %>% filter(year == 1996) %>% 
  group_by(month, day) %>% 
  select(-year) %>% 
  summarise (rain_1996 = mean(Fada_Ngourma)) %>% 
  ungroup() %>% 
  mutate(rain_1996 = cumsum(rain_1996))


rain_2008  <- rain %>% filter(year == 2008) %>% 
  group_by(month, day) %>% 
  select(-year) %>% 
  summarise (rain_2008 = mean(Fada_Ngourma)) %>% 
  ungroup() %>% 
  mutate(rain_2008 = cumsum(rain_2008))

rain_2001  <- rain %>% filter(year == 2001) %>% 
  group_by(month, day) %>% 
  select(-year) %>% 
  summarise (rain_2001 = mean(Fada_Ngourma)) %>% 
  ungroup() %>% 
  mutate(rain_2001 = cumsum(rain_2001))


rain_2018  <- rain %>% filter(year == 2018) %>% 
  group_by(month, day) %>% 
  select(-year) %>% 
  summarise (rain_2018 = mean(Fada_Ngourma)) %>% 
  ungroup() %>% 
  mutate(rain_2018 = cumsum(rain_2018))

rain_2021  <- rain %>% filter(year == 2021) %>% 
  group_by(month, day) %>% 
  select(-year) %>% 
  summarise (rain_2021 = mean(Fada_Ngourma)) %>% 
  ungroup() %>% 
  mutate(rain_2021 = cumsum(rain_2021))



all_data <- left_join(mean_data, rain_1996, by = c("month", "day")) %>% 
  left_join(rain_1996,by = c("month", "day") ) %>% 
  left_join(rain_2001,by = c("month", "day") ) %>%
  left_join(rain_2008,by = c("month", "day") ) %>%
  left_join(rain_2018,by = c("month", "day") ) %>%
  left_join(rain_2021,by = c("month", "day") )


all_data %<>% mutate(date = str_c(day, month, rep(2000, 366),sep = "/" ) , Date = dmy(date), dmonth = format(Date, "%d-%b")) 
  

# ggplot(data = all_data, lwd = 2)+
#   geom_line(mapping = aes(x = as_factor(dmonth), y = mean_rain, group = 1 , color = "Normal 91-2020: 18-Mai"))+
#   geom_line(mapping = aes(x = as_factor(dmonth), y = mean_rain_75, group = 1,  color = "75% Normal :24-Mai"))+
#   geom_line(mapping = aes(x = as_factor(dmonth), y = mean_rain_125, group = 1 , color = "125% Normal: 13-Mai"))+
#   geom_line(mapping = aes(x = as_factor(dmonth), y = rain_1996.x, group = 1, color = "1996: 25-Mai"), linetype = "dashed")+
#   geom_line(mapping = aes(x = as_factor(dmonth), y = rain_2001, group = 1, color = "2001: 29-Mai"), linetype = "dashed")+
#   geom_line(mapping = aes(x = as_factor(dmonth), y = rain_2008, group = 1, color = "2008: 30-Mai"), linetype = "dashed")+
#   geom_line(mapping = aes(x = as_factor(dmonth), y = rain_2018, group = 1, color = "2018: 31-Mai"), linetype = "dashed")+
#   geom_line(mapping = aes(x = as_factor(dmonth), y = rain_2021, group = 1, color = "2021: 23-Mai"), linetype = "dashed")+
#   geom_hline(yintercept =  50, linetype = "dashed")+
#   scale_y_continuous(breaks = seq(0, 1000, 50))+
#   scale_color_manual(name = "Year & Onset Date", values = c("blue", "orange","green", "grey30", "red","magenta", "green","Black"))+
#   scale_x_discrete(breaks = seq(1, 366, 10), labels = all_data$dmonth[breaks])+
#   labs(x = "JOURS", y = "Cumul pluviométrique", title = "Burkina Faso: Evolution du cumul puviométrique pour Ouagadougou-aéro",
#        caption = "© Agence Nationale de la Météorologie/ANAM-BURKINA")+
#   theme_classic()+
#   theme(legend.position = c(.2, .7))




ggplot(data = all_data, lwd = 2)+
  geom_line(mapping = aes(x = Date, y = mean_rain, group = 1 , color = "Normal 91-2020: 18-Mai"))+
  geom_line(mapping = aes(x = Date, y = mean_rain_75, group = 1,  color = "75% Normal :24-Mai"))+
  geom_line(mapping = aes(x = Date, y = mean_rain_125, group = 1 , color = "125% Normal: 13-Mai"))+
  geom_line(mapping = aes(x = Date, y = rain_1996.x, group = 1, color = "1996: 25-Mai"), linetype = "dashed")+
  geom_line(mapping = aes(x = Date, y = rain_2001, group = 1, color = "2001: 29-Mai"), linetype = "dashed")+
  geom_line(mapping = aes(x = Date, y = rain_2008, group = 1, color = "2008: 30-Mai"), linetype = "dashed")+
  geom_line(mapping = aes(x = Date, y = rain_2018, group = 1, color = "2018: 31-Mai"), linetype = "dashed")+
  geom_line(mapping = aes(x = Date, y = rain_2021, group = 1, color = "2021: 23-Mai"), linetype = "dashed")+
  geom_hline(yintercept =  50, linetype = "dashed")+
  scale_y_continuous(breaks = seq(0, 1000, 50))+
  scale_color_manual(name = "Year & Onset Date", values = c("blue", "orange","green", "grey30", "red","magenta", "green","Black"))+
  scale_x_date(date_breaks = "10 day", date_labels = "%b-%d")+
  labs(x = "JOURS", y = "Cumul pluviométrique", title = "Burkina Faso: Evolution du cumul puviométrique pour Ouagadougou-aéro",
       caption = "© Agence Nationale de la Météorologie/ANAM-BURKINA")+
  theme_classic()+
  theme(legend.position = c(.2, .7), axis.text.x = element_text(face =  "bold", angle =  90))

  