# Visualization is a fundamentally human activity. A good visualization will show
# you things that you did not expect, or raise new questions about the data.
# A good visualization might also hint that you're asking the wrong question, 
# or you need to collect different data. Visualizations can surprise you, but
# don't scale particularly well because they require a human to interpret them.

# Libraries --------------------------------------------------------------

library(tidyverse)
library(hrbrthemes)
library(ggthemes)
#extrafont::font_import()
#hrbrthemes::import_roboto_condensed()

#library(scales)


# Read data ---------------------------------------------------------------

data("iris")
Data <-  iris

# Explore -----------------------------------------------------------------

Data %>% 
  ggplot(aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()

Data %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

Data %>% 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(col = Species)) +
  theme_ipsum()


# Export data -------------------------------------------------------------


