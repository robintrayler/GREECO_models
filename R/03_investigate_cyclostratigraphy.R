# load required packages ------------------------------------------------------ 
library(tidyverse)
theme_set(theme_classic())
library(astrochron)
library(viridis)
# load custom function --------------------------------------------------------
source('./R/functions/tidy_eha.R')
# load the data ---------------------------------------------------------------
predictions <- read.csv(file = './results/time_calibrated_fisher_assay.csv')
beds <- read.csv(file = './results/time_calibrated_beds.csv')
target_frequency <- read.csv(file ='./data/target_frequency.csv')
laskar <- read.csv(file = './data/Laskar2010bEcc.csv') |> 
  mutate(t = t / 1000) |> 
  rename(age = t, 
         value = e)
# get list of core names
cores <- unique(predictions$core)
# plot the time calibrated fisher assay data ----------------------------------
beds <- beds |> 
  select(median,
         bed, 
         core,
         name) |> 
  pivot_wider(names_from = name, 
              values_from = median)

bed_plots <- predictions |> 
  ggplot(mapping = aes(y = oilgpt,
                       x = median)) + 
  geom_path() + 
  facet_wrap(~core,
             nrow = 3) + 
  geom_rect(data = beds,
            mapping = aes(xmin = top_m,
                          xmax = base_m,
                          ymin = -Inf,
                          ymax = Inf,
                          fill = bed),
            inherit.aes = FALSE,
            alpha = 0.25) + 
  scale_fill_viridis(option = 'turbo',
                     discrete = TRUE) + 
  xlab('Age (Ma)') + 
  ylab("Oil Yields")

predictions |>
  select(median,
         oilgpt) |>
  linterp(genplot = FALSE) |>
  demean(genplot = FALSE) |>
  detrend(genplot = FALSE) |>
  bandpass(flow = 1/0.11,
           fhigh = 1/0.09,
           genplot = FALSE) |>
  ggplot(mapping = aes(y = oilgpt,
                       x = median)) +
  geom_path() +
  facet_wrap(~core,
             nrow = 3) +
  geom_rect(data = beds,
            mapping = aes(xmin = top_m,
                          xmax = base_m,
                          ymin = -Inf,
                          ymax = Inf,
                          fill = bed),
            inherit.aes = FALSE,
            alpha = 0.25) +
  scale_fill_viridis(option = 'turbo',
                     discrete = TRUE) +
  xlab('Age (Ma)') +
  ylab("Oil Yields")


pdf(file = './figures/oil_time_calbrated.pdf',
    width = 10, 
    height = 5)
bed_plots
dev.off()
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
  ylab('Age (Ma)') + 
  scale_y_reverse()

pdf(file = './figures/eha.pdf',
  width = 12, 
    height = 4)
eha_figure
dev.off()
