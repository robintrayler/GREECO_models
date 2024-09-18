# load required libraries -----------------------------------------------------
library(tidyverse)
library(modifiedBChron)
# see instructions here (https://github.com/robintrayler/modifiedBChron) to 
# install modified Bchron
# load the data ---------------------------------------------------------------
tuffs <- read.csv(file = './data/cleaned_data/tuffs.csv') |> 
  filter(bed != 'Scheggs')

beds  <- read.csv(file = './data/cleaned_data/marker_beds.csv')
fisher <- read.csv(file = './data/cleaned_data/fisher.csv')

# calculate age models --------------------------------------------------------
# preallocate list to hold the models 
model       <- list()
predictions <- list()

cores <- unique(fisher$core)
# model each core individually 
for(i in seq_along(cores)) {
  # extract dates for current core
  tmp <- tuffs |> 
    filter(core == cores[i]) |> 
    mutate(thickness_m = 0.1)
  
  # calculate max and min for extrapolation
  range <- fisher |> 
    filter(core == cores[i]) |> 
    pull(mid_m) |> 
    range()
  
  # extract fisher data for current c
  oilgpt <- fisher |> 
    filter(core == cores[i])
  
  model[[i]] <- ageModel(ages = tmp$age,
                         ageSds = tmp$age_sd,
                         ids = tmp$bed,
                         positions = -tmp$top_m,
                         positionThicknesses = tmp$thickness_m,
                         predictPositions = -seq(range[1], 
                                                 range[2], 
                                                 length = 1000),
                         MC = 10000,
                         burn = 1000)
  # predict fisher assay data ages -----------------------
  tmp_predict <- agePredict(model = model[[i]],
                                 newPositions = -oilgpt$mid_m)
  # extract the credible interval 
  predictions[[i]] <- tmp_predict$HDI |> 
    # set column names
    set_names(nm = c('position', 'CI_2.5' , 'median', 'CI_97.5', 'id')) |> 
    # add in oil records
    add_column(oilgpt = oilgpt$oilgpt) |> 
    # flip back to meters core depth
    mutate(position = position * -1) |> 
    add_column(core = cores[i])
}

predictions <- predictions |> 
  reduce(rbind)

# # write files -----------------------------------------------------------------
predictions |> 
  write_csv(file = './results/time_calibrated_fisher_assay.csv')

rm(list = ls())

