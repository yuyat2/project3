library(tidyverse)

install.packages("countrycode")
library(countrycode)
?countrycode

countrycode()


glimpse(V_Dem_DS_CY_Others_v7_1)


ggplot(data = V_Dem_DS_CY_Others_v7_1, mapping = aes(x = v2lgcrrpt, y = v2mecenefm)) +
  geom_point(na.rm = TRUE)  +
  labs(x = "legislature corrupt activities",
       y = "government cesorship effort on media") +
  geom_smooth(method = "lm", se = FALSE)

fit <- lm(v2lgcrrpt ~ v2mecenefm, data = V_Dem_DS_CY_Others_v7_1)
fit
glance(fit)

VDem_data <- V_Dem_DS_CY_Others_v7_1 %>%
  filter(country_name == "China" & year %in% 1950:2016) %>%
  select(year, v2lgcrrpt, v2jucorrdc, v2excrptps, v2mecenefm)



 colnames(VDem_data)



library("tidyverse")
library("lubridate")
library("stringr")
library("forcats")
library("broom")
library("modelr")

ggplot(data = V_Dem_DS_CY_Others_v7_1, mapping = aes(x = v2jucorrdc, y = v2mecenefm)) +
  geom_point(na.rm = TRUE)  +
  labs(x = "judicial corruption decision",
       y = "government cesorship effort on media") +
  geom_smooth(method = "lm", se = FALSE)
fit2 <- lm(v2jucorrdc~ v2mecenefm, data = V_Dem_DS_CY_Others_v7_1)
fit2
glance(fit2)

ggplot(data = V_Dem_DS_CY_Others_v7_1, mapping = aes(x = v2excrptps, y = v2mecenefm)) +
  geom_point(na.rm = TRUE)  +
  labs(x = "public sector currupt exchanges",
       y = "government cesorship effort on media") +
  geom_smooth(method = "lm", se = FALSE)
fit3 <- lm(v2excrptps~ v2mecenefm, data = V_Dem_DS_CY_Others_v7_1)
fit3
glance(fit3)

ggplot(data = V_Dem_DS_CY_Others_v7_1, mapping = aes(x = v2exthftps, y = v2mecenefm)) +
  geom_point(na.rm = TRUE)  +
  labs(x = "public sector theft",
       y = "government cesorship effort on media") +
  geom_smooth(method = "lm", se = FALSE)
fit4 <- lm(v2exthftps~ v2mecenefm, data = V_Dem_DS_CY_Others_v7_1)
fit4
glance(fit4)






china <- readRDS("china")
read.csv(file = "/Users/yuya/reproject3/china")

frequency(China)
times_series <- ts.plot(china, col = : , xlab = "Year", ylab = "",
                        main = "", start = 1950, frequency = )
plot(times_series)

#explantion of variables
v2mecenefm government cesorship effort on media
v2meslfcen media self-censorship

v2lgcrrpt legislature corrupt activities
v2jgcrrpt judicial corruption decision
v2excrptps public sector currupt exchanges
v2exthftps public sector theft