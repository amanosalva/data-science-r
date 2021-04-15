library(gapminder)
library(dplyr)
library(ggplot2)

# Bases
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()


ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() + scale_x_log10()


ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() + scale_x_log10() + scale_y_log10()

# Faceting
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, size = pop)) +
  geom_point() + scale_x_log10() + scale_y_log10() + 
facet_wrap(~continent)


ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() + scale_x_log10() + 
facet_wrap(~year)
