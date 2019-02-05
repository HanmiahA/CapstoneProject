library("tidyverse")
library("maps")
library("mapdata")

States <- map_data("state")

# Plotting FBI Crime (use geom_map to show the rate per offense)

ggplot() + geom_map(aes(x = long, y = lat, map_id = region), data = States, map = States, fill = "#ffffff", color = "#ffffff", size = 0.15) + 
  geom_map(aes(fill = Rate, map_id = region), map = States, data = Assault, color = "#ffffff", size = 0.15) + 
  scale_fill_gradient2(name = "RateOfCrime", low = "grey50", mid = "cyan", high = "blue", midpoint = 0) +
  coord_map("albers", lat0 = 39, lat1 = 45) + labs(x = NULL, y = NULL, title = "Rate of Aggravated Assault by State, 2017") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank(), panel.border = element_blank())

ggplot() + geom_map(aes(x = long, y = lat, map_id = region), data = States, map = States, fill = "#ffffff", color = "#ffffff", size = 0.15) + 
  geom_map(aes(fill = Rate, map_id = region), map = States, data = Burglary, color = "#ffffff", size = 0.15) + 
  scale_fill_gradient2(name = "RateOfCrime", low = "grey50", mid = "cyan", high = "blue", midpoint = 0) +
  coord_map("albers", lat0 = 39, lat1 = 45) + labs(x = NULL, y = NULL, title = "Rate of Burglary by State, 2017") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank(), panel.border = element_blank())

ggplot() + geom_map(aes(x = long, y = lat, map_id = region), data = States, map = States, fill = "#ffffff", color = "#ffffff", size = 0.15) + 
  geom_map(aes(fill = Rate, map_id = region), map = States, data = Larceny, color = "#ffffff", size = 0.15) + 
  scale_fill_gradient2(name = "RateOfCrime", low = "grey50", mid = "cyan", high = "blue", midpoint = 0) +
  coord_map("albers", lat0 = 39, lat1 = 45) + labs(x = NULL, y = NULL, title = "Rate of Larceny by State, 2017") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank(), panel.border = element_blank())

ggplot() + geom_map(aes(x = long, y = lat, map_id = region), data = States, map = States, fill = "#ffffff", color = "#ffffff", size = 0.15) + 
  geom_map(aes(fill = Rate, map_id = region), map = States, data = Murder, color = "#ffffff", size = 0.15) + 
  scale_fill_gradient2(name = "RateOfCrime", low = "grey50", mid = "cyan", high = "blue", midpoint = 0) +
  coord_map("albers", lat0 = 39, lat1 = 45) + labs(x = NULL, y = NULL, title = "Rate of Murder by State, 2017") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank(), panel.border = element_blank())

ggplot() + geom_map(aes(x = long, y = lat, map_id = region), data = States, map = States, fill = "#ffffff", color = "#ffffff", size = 0.15) + 
  geom_map(aes(fill = Rate, map_id = region), map = States, data = Rape, color = "#ffffff", size = 0.15) + 
  scale_fill_gradient2(name = "RateOfCrime", low = "grey50", mid = "cyan", high = "blue", midpoint = 0) +
  coord_map("albers", lat0 = 39, lat1 = 45) + labs(x = NULL, y = NULL, title = "Rate of Rape by State, 2017") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank(), panel.border = element_blank())

ggplot() + geom_map(aes(x = long, y = lat, map_id = region), data = States, map = States, fill = "#ffffff", color = "#ffffff", size = 0.15) + 
  geom_map(aes(fill = Rate, map_id = region), map = States, data = Robbery, color = "#ffffff", size = 0.15) + 
  scale_fill_gradient2(name = "RateOfCrime", low = "grey50", mid = "cyan", high = "blue", midpoint = 0) +
  coord_map("albers", lat0 = 39, lat1 = 45) + labs(x = NULL, y = NULL, title = "Rate of Robbery by State, 2017") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank(), panel.border = element_blank())

ggplot() + geom_map(aes(x = long, y = lat, map_id = region), data = States, map = States, fill = "#ffffff", color = "#ffffff", size = 0.15) + 
  geom_map(aes(fill = Rate, map_id = region), map = States, data = Theft, color = "#ffffff", size = 0.15) + 
  scale_fill_gradient2(name = "RateOfCrime", low = "grey50", mid = "cyan", high = "blue", midpoint = 0) +
  coord_map("albers", lat0 = 39, lat1 = 45) + labs(x = NULL, y = NULL, title = "Rate of Theft by State, 2017") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank(), panel.border = element_blank())

#Plotting FBICrime2 (possibly geom map for violent versus property crime)
ggplot(FBICrime2, aes(x = State, y = Rate)) + geom_point() + facet_wrap(vars(Offense)) + coord_flip()

ggplot() + geom_map(aes(x = long, y = lat, map_id = region), data = States, map = States, fill = "#ffffff", color = "#ffffff", size = 0.15) + 
  geom_map(aes(fill = Rate, map_id = region), map = States, data = FBICrime2, color = "#ffffff", size = 0.15) + 
  facet_wrap(vars(Offense)) + scale_fill_gradient2(name = "RateOfCrime", low = "blue", mid = "purple", high = "red") +
  coord_map("albers", lat0 = 39, lat1 = 45) + labs(x = NULL, y = NULL, title = "Rate of Violent and Property Crime by State, 2017") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank(), panel.border = element_blank())

#Plotting CrimePoverty (Do an annotate with the regression equation and show prediction intervals)

dat <- data.frame(Poverty.Rate = CrimePoverty$Poverty.Rate, Average.Crime.Rate = CrimePoverty$Average.Crime.Rate)
CrimePov.lm <- lm(data = dat, formula = Average.Crime.Rate ~ Poverty.Rate)
cppi <- cbind(CrimePoverty, predict(CrimePov.lm, interval = "prediction"))


p <- ggplot(cppi, aes(x = Poverty.Rate)) + geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) + geom_point(aes(y = Average.Crime.Rate), size = 3) + geom_smooth(aes(y = Average.Crime.Rate), method = lm, se = FALSE, na.rm = TRUE)
p + annotate("text", label = "y = 20.42x + 133.30", x = 10.0, y = 725, size = 4, color = "blue") + labs(x = NULL, y = NULL, title = "Average Crime Rate versus Poverty Rate of the United States for 2017")

