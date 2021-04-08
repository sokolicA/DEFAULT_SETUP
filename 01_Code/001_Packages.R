
# All wanted packages
All_Pkgs <- c("tidyverse", "magrittr", "lubridate", "zoo", "data.table", 
              "gganimate", "plotly", "hrbrthemes", "cowplot",
              "DescTools",
              "lme4", "survival", "caret", 
              "shiny", "rmarkdown", "markdown", "knitr", "kableExtra",
              "parallel", "future", "furrr",
              "naniar")

# Missing packages
Missing_Pkgs <- All_Pkgs[!(All_Pkgs %in% installed.packages()[, "Package"])]

# Install missing packages if any
if(length(Missing_Pkgs)) install.packages(Missing_Pkgs)

