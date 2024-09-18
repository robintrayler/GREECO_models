# load required packages ------------------------------------------------------ 
library(tidyverse)
theme_set(theme_classic())
library(astrochron)
library(viridis)
# load custom function --------------------------------------------------------
source('./R/functions/tidy_eha.R')
# load the data ---------------------------------------------------------------
predictions <- read.csv(file = './results/time_calibrated_fisher_assay.csv')
cores <- unique(predictions$core)

target_frequency <- read.csv(file ='./data/target_frequency.csv')
# plot the time calibrated fisher assay data ----------------------------------
predictions |> 
  ggplot(mapping = aes(x = oilgpt,
                       y = median,
                       color = core)) + 
  geom_path() + 
  facet_wrap(~core)

# eha analysis of the fisher assay data ---------------------------------------
eha_results <- list()
for(i in 1:3) {
  eha_results[[i]] <- predictions |> 
    filter(core == cores[i]) |> 
    select(median, oilgpt)   |> 
    linterp(genplot = FALSE) |> 
    demean(genplot = FALSE)  |> 
    detrend(genplot = FALSE) |> 
    tidy_eha(win = 0.5,   # 0.5 Ma window size
             step = 0.01, # 0.01 Ma step size
             fmax = 60) |> 
    add_column(core = cores[i]) |> 
    rename(age = depth)
}

# collapse into a single data frame
eha_results <- eha_results |> 
  reduce(rbind)

# plot the results of each core -----------------------------------------------
eha_figure <- eha_results |> 
  ggplot(mapping = aes(x = freq,
                       y = age,
                       fill = amplitude)) + 
  geom_raster(show.legend = FALSE) + 
  facet_wrap(~core) + 
  scale_fill_viridis(option = 'turbo') + 
  geom_vline(data = target_frequency,
             mapping = aes(xintercept = frequency),
             linewidth = 0.75,
             color = 'white',
             linetype = 'dashed') + 
  scale_color_brewer(palette = 'Set2') +
  theme(legend.position = 'top') + 
  xlab('Frequency (cycles/Myr)') + 
  ylab('Age (Ma)')

pdf(file = './figures/eha.pdf',
  width = 12, 
    height = 4)
eha_figure
dev.off()
