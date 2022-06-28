local({
  # List of packages
  packages <- c(
    "sessioninfo",
    "here",
    "knitr",
    "markdown",
    "rmarkdown",
    "shiny",
    "htmltools",
    "data.table",
    "tidyverse", 
    "mlr3",
    "mlr3verse",
    "car",
    "Hmisc", 
    "lubridate", 
    "forcats",
    "crosstalk",
    "DT",
    "plotly",
    "gt",
    "gtsummary",
    "ISLR"
  )
  # Missing packages
  missing_pkgs <- setdiff(packages, rownames(installed.packages()))
  
  # Install missing packages if any
  if(length(missing_pkgs)) install.packages(missing_pkgs)
})
