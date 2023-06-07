#### SCRIPT SYNTHETIC CONTROL
# ESTIMATES THE CAUSAL EFFECT OF PRISON CAPACITY EXPANSION ON INCARCERATION
# RATES USING SYNTH. SCOTT CUNNINGHAM.

### library
## install required packages
install.packages("tidyverse")
install.packages("tidysynth")
install.packages("haven")
## load required packages
library(tidyverse)
library(tidysynth)
library(haven)

### set your working directory
# replace "YOURUSER" with your user folder name.
# replace "PATHTOYOURWORKINGDIRECTORY" with your working directory path
setwd("C:/Users/YOURUSER/PATHTOYOURWORKINGDIRECTORY")

### defining main database
# database github url
base_url <- "https://github.com/scunning1975/mixtape/raw/master/texas.dta"
# download database
download.file(base_url, "texas.dta", mode = "wb")
# assign to a dataframe
base <- read_dta("texas.dta")
# confirm that your data set is a data frame
is.data.frame(base)
class(base)
# transform in data frame
base <- as.data.frame(base)
# organize state index values
base$statefip <- group_indices(base, base$state)
# deal with scientific notation
options(scipen = 999)

### building synth control
base_out <- 
  base %>%
  # initial the synthetic control object
  synthetic_control(outcome = bmprison, # outcome
                    unit = state, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Texas", # unit where the intervention occurred
                    i_time = 1993, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%

  # Generate the aggregate predictors used to fit the weights using the chosen variables, 
  # in this case we're going to use:
  # bmprison (1988,1990:1992)
  generate_predictor(time_window = 1988, bmprison88 = bmprison) %>%
  generate_predictor(time_window = 1990, bmprison90 = bmprison) %>%
  generate_predictor(time_window = 1991, bmprison91 = bmprison) %>%
  generate_predictor(time_window = 1992, bmprison92 = bmprison) %>%
  # alcohol (1990)
  generate_predictor(time_window = 1990, alcohol = alcohol) %>%
  # aidscapta (1990, 1991) 
  generate_predictor(time_window = 1990, aidscapita90 = aidscapita) %>%
  generate_predictor(time_window = 1991, aidscapita91 = aidscapita) %>%
  # income
  generate_predictor(income = income) %>%
  # ur
  generate_predictor(ur = ur) %>%
  # poverty
  generate_predictor(poverty = poverty) %>%
  # black (1990:1992)
  generate_predictor(time_window = 1990, black90 = black) %>%
  generate_predictor(time_window = 1991, black91 = black) %>%
  generate_predictor(time_window = 1992, black92 = black) %>%
  # perc1519
  generate_predictor(time_window = 1990, perc1519 = perc1519) %>%
  
  # Generate the fitted weights for the synthetic control
  generate_weights() %>%
  
  # Generate the synthetic control
  generate_control()

### generate your synth control plots
# synth control plot over the time
base_out %>% plot_trends()
# differences plot, between observable variable and synthetic control
base_out %>% plot_differences()
# plot weights
base_out %>% plot_weights()
# plot your placebos
base_out %>% filter(.id %in% c("Texas", "Illinois")) %>% plot_placebos()
# plot your mspe
base_out %>% plot_mspe_ratio()
## plot placebos according to your criteria
base_out %>% filter(.id %in% c("Texas", "Illinois", "Ohio")) %>% plot_placebos()

### get some statistics
base_out %>% grab_balance_table()
base_out %>% grab_predictors(type = "treated")
base_out %>% grab_predictor_weights()
base_out %>% grab_unit_weights() %>% print(n=50)
base_out %>% grab_synthetic_control()

# ALTERNATIVE PACKAGE -----------------------------------------------------
## install package Synth
install.packages("Synth")
## load package
library(Synth)

## specify your data and model
base_out2 <-
  base %>%
  dataprep(special.predictors = list(
             list("bmprison",1988, "mean"),
             list("bmprison",1990, "mean"),
             list("bmprison",1991, "mean"),
             list("bmprison",1992, "mean"),
             list("alcohol", 1990, "mean"),
             list("aidscapita", 1990, "mean"),
             list("aidscapita", 1991, "mean"),
             list("income", 1985:1992, "mean"),
             list("ur", 1985:1992, "mean"),
             list("poverty", 1985:1992, "mean"),
             list("black", 1990, "mean"),
             list("black", 1991, "mean"),
             list("black", 1992, "mean"),
             list("perc1519", 1990, "mean")),
           dependent = "bmprison",
           unit.variable = "statefip", 
           time.variable = "year", 
           unit.names.variable = "state",
           treatment.identifier = 44,
           controls.identifier = c(1:43,45:51),
           time.predictors.prior = c(c(1985:1992)),
           time.optimize.ssr = c(1985:1992),
           time.plot = c(1985:2000)
           )

## process your synthetic control
synth_cont <- synth(base_out2)

## print some tables and plots
# table
print(synth.tables   <- synth.tab(
  dataprep.res = base_out2,
  synth.res    = synth_cont))
# synth control plot over the time
path.plot(synth.res    = synth_cont,
          dataprep.res = base_out2,
          Ylab         = c("Y"),
          Xlab         = c("Year"),
          Legend       = c("Texas","Synthetic Texas"),
          Legend.position = c("topleft")
)
# gaps plot
gaps.plot(synth.res    = synth_cont,
          dataprep.res = base_out2,
          Ylab         = c("Gap"),
          Xlab         = c("Year"),
          Main         = "",
          Ylim = c(-3000,30000)
)
abline(h=c(5000,10000,15000,20000,25000), lty=2)
