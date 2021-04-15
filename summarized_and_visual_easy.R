library(gapminder)
library(dplyr)
library(ggplot2)

by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time
ggplot(by_year, aes(x = year, y = medianLifeExp))+ expand_limits(y = 0) + geom_point()


#### Visualizing median GDP per capita per continent over time

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent = gapminder %>% group_by(continent, year) %>% summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent))+ expand_limits(y = 0) + geom_point()

#### Comparing median life expectancy and median GDP per continent in 2007

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 = gapminder %>% filter(year == 2007) %>% group_by(continent) %>% summarize(medianGdpPercap = median(gdpPercap), medianLifeExp = median(lifeExp))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x = medianGdpPercap, y = medianLifeExp, color = continent))+ geom_point()

#### Visualizing median GDP per capita over time (LINE)

# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x = year, y = medianGdpPercap)) + expand_limits(y = 0) + geom_line()

#### Visualizing median GDP per capita by continent over time

# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) + expand_limits(y = 0) + geom_line()



##### Barras

### Visualizing median GDP per capita by continent

# Summarize the median gdpPercap by continent in 1952
by_continent <- gapminder %>%
  filter(year == 1952) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x = continent, y = medianGdpPercap)) + geom_col()


### Visualizing GDP per capita by country in Oceania

# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>%
  filter(year == 1952, continent == 'Oceania')  

# Create a bar plot of gdpPercap by country
ggplot(oceania_1952, aes(x = country, y = gdpPercap)) + geom_col()


#### Histograms

### Visualizing population

gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

# Create a histogram of population (pop_by_mil)
ggplot(gapminder_1952, aes(x = pop_by_mil)) + geom_histogram(bins = 50)


### Visualizing population with x-axis on a log scale

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop), with x on a log scale
ggplot(gapminder_1952, aes(x = pop)) + scale_x_log10() + geom_histogram(bins = 50)


#### BOXPLOTS

# Comparing GDP per capita across continents

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a boxplot comparing gdpPercap among continents
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) + scale_y_log10() + geom_boxplot()


#### Adding a title to your graph

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Add a title to this graph: "Comparing GDP per capita across continents"
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() + 
  ggtitle('Comparing GDP per capita across continents')
