library(tidyverse)
library(dplyr)
library("lubridate")
library("stringr")
library("forcats")
library("broom")
library("modelr")

#More corruption is supposed to bring about more governemnt censorship on Media
#in order to cover them up.  However, through data analysis, it surprisingly shows 
#the following result.  "More corruption leads to less media censorship".
#why does it happen?  



China <- read.csv(file = "/Users/yuya/reproject3/China")

plot_data <-
  China %>%
  dplyr::select(-X, -X.1, -v2excrptps) %>%
  gather(key = "VDEM", value = "score", - year)



#time series judicial corruption, legislature corrupt activities, 
#government cesorship effort on media from 1950 to 2016 in China

ggplot(plot_data, aes(x = year, y = score, color = VDEM)) +
  geom_line()


ggplot(China, aes(x = year)) +
  geom_line(aes(y = v2jucorrdc), color = "blue") +
  geom_line(aes(y = v2lgcrrpt), color = "red") + 
  geom_line(aes(y = v2mecenefm), color = "grey") + 
  ylab("value") +
  xlab("year") +
  scale_color_manual(lab = "judicial", values = "blue") +
  labs(subtitle = "Red means judicial, etc., ec.")



#time series judicial corruption decision from 1950 to 2016 in China
ggplot(China, aes(year, v2jucorrdc)) +
  geom_line() + 
  ylab("judicial corruption decision") +
  xlab("year")



#relations between "legislature corrupt activities" and "government cesorship effort on media"
ggplot(data = China, mapping = aes(x = v2lgcrrpt, y = v2mecenefm)) +
  geom_point(na.rm = TRUE)  +
  labs(x = "legislature corrupt activities",
       y = "government cesorship effort on media") +
  geom_smooth(method = "lm", se = FALSE)

#relations between "judicial corruption decision" and "government cesorship effort on media"
ggplot(data = China, mapping = aes(x = v2jucorrdc, y = v2mecenefm)) +
  geom_point(na.rm = TRUE)  +
  labs(x = "judicial corruption decision",
       y = "government cesorship effort on media") +
  geom_smooth(method = "lm", se = FALSE)

ggplot() +
  geom_line(data = China, aes(x = year, y = v2lgcrrpt, color = v2lgcrrpt)) +
  geom_line(data = China, aes(x = year, y = v2jucorrdc, color = v2jucorrdc))  +
  geom_line(data = China, aes(x = year, y = v2mecenefm, color = v2mecenefm))  +
  ylab('corruption and censorship')

#should do "diff() function to make sure the change in time series
legislature_corrupt <- China %>%
  select(v2lgcrrpt)


#time series analysis of legislature corrupt activities"
legislature_ts <- ts(legislature_corrupt, frequency = 1, start = c(1950,1))
plot.ts(legislature_ts)

#time series analysis of judicial corruption decision"
judicial_corruption <- China %>%
  select(v2jucorrdc)

judicial_ts <- ts(judicial_corruption, frequency = 1, start = c(1950,1))
plot.ts(judicial_ts)

#time series analysis of  government cesorship effort on media
media_censorship <- China %>%
  select(v2mecenefm)

media_censorship_ts <- ts(media_censorship, frequency = 1, start = c(1950,1))
plot.ts(media_censorship_ts)

#how to display three time series data on the same graph?
#how to get correlation on time series analysis between corruption and censorship?



#should do "diff() function to make sure the change in time series
diff_legislature<- diff(legislature_ts, differences = 1)
plot.ts(diff_legislature)



#legislature corruption activities decreases before 1960, 1970, after 2010
#juducial corruption decision decrease before 1970, 1980, after 1990
#media sensor ship increases before 1980
#when corruption decreases, the number of media sensorship increases before 1980, after 1990

#creating data "China"
China <- V_Dem_DS_CY_Others_v7_1 %>%
  filter(country_name == "China" & year %in% 1950:2016) %>%
  select(year, v2lgcrrpt, v2jucorrdc, v2excrptps, v2mecenefm)




fit <- lm(v2lgcrrpt ~ v2mecenefm, data = China)
fit
glance(fit)



ggplot(data = China, mapping = aes(x = v2jucorrdc, y = v2mecenefm)) +
  geom_point(na.rm = TRUE)  +
  labs(x = "judicial corruption decision",
       y = "government cesorship effort on media") +
  geom_smooth(method = "lm", se = FALSE)

fit2 <- lm(v2jucorrdc~ v2mecenefm, data = V_Dem_DS_CY_Others_v7_1)
fit2


write("VDem_data.csv")
write.csv(China, file = "/Users/yuya/reproject3/China")



frequency(China)
times_series <- ts.plot(cbind(v2lgcrrpt, v2jgcrrpt)), col = : , xlab = "Year" ylab = "legislature corrupt activities and judicial corruption decision"
                        main = "corruption and year", start = 1950, frequency = 1)



cov(v2lgcrrpt, v2mecenefm)
cov(v2jgcrrpt, v2mecenefm)

cor(v2lgcrrpt, v2mecenefm)
cor(v2jgcrrpt, v2mecenefm)



#explantion of variables
v2mecenef government cesorship effort on media
v2meslfcen media self-censorship

v2lgcrrpt legislature corrupt activities
v2jgcrrpt judicial corruption decision