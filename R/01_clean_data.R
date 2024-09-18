# load required libraries -----------------------------------------------------
library(tidyverse)
theme_set(theme_bw()) # set ggplot theme

# load the data ---------------------------------------------------------------
# marker bed and tuff positions as determined by Michael E. Smith
# note: stratigraphic positions are in meters above base
beds   <- read.csv(file = './data/raw_data/wilkins_peak_beds.csv') |> 
  filter(core != 'Solvay')

# fisher assay data provided by Alan Carroll
# note: stratigraphic positions are in m core depth
fisher <- read.csv(file = './data/raw_data/fisher_assay.csv')

# convert beds to meters depth ------------------------------------------------
# extract mike smiths wilkins peak member tops
WPM_tops <- beds |> 
  filter(bed == 'top WPM') |> 
  select(core = core, 
         smith_member_top = top_m)

# flip the beds around so the WPM top is at 0 meters depth
beds <- beds |> 
  full_join(WPM_tops, 
            by = 'core') |> 
  mutate(top_m = -1*(top_m - smith_member_top),
         base_m = -1*(base_m - smith_member_top)) |> 
  select(core,
         bed, 
         type,
         top_m,
         base_m,
         thickness_m,
         age,
         age_sd)

# extract marker beds and tuffs into seperate data frames
marker_beds <- beds |> filter(type == 'marker_bed')
tuffs       <- beds |> filter(type == 'tuff')

# clean up the fisher assay data ----------------------------------------------
# clean up the fisher data
fisher <- fisher |> 
  select(core = Well..Name,
         top_ft = TOPFT,
         bottom_ft = BOTFT,
         member_top = Member.Top,
         oilgpt = OILGPT) |> 
  # use the midpoint of each oil assay thickness 
  mutate(mid_ft = (top_ft + bottom_ft) / 2) |> 
  # convert to meters, dumbass 
  mutate(mid_m = mid_ft * 0.3048,
         member_top = member_top * 0.3048) |> 
  select(core, 
         mid_m,
         member_top,
         oilgpt)

# find the top of the wilkins peak member to adjust the fisher assay data
WPM_tops <- beds |> 
  filter(bed == 'top WPM') |> 
  select(core = core, 
         smith_member_top = top_m)

# adjust scaling of fisher data to match Mike Smiths stratigraphic placements
fisher <- fisher |> 
  left_join(WPM_tops, 
            by = 'core') |> 
  mutate(top_diff = smith_member_top - member_top) |> 
  mutate(mid_m = mid_m + top_diff) |> 
  select(core, 
         mid_m,
         member_top = smith_member_top,
         oilgpt)

# write cleaned data to csv files ---------------------------------------------
marker_beds |> 
  write_csv(file = './data/cleaned_data/marker_beds.csv')

tuffs |> 
  write_csv(file = './data/cleaned_data/tuffs.csv')

fisher |> 
  write_csv(file = './data/cleaned_data/fisher.csv')

rm(list = ls())