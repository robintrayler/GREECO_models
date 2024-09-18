# README

This repository contains the data and code to explore time-calibrated fisher assay data from various Green River Formation cores as part of the GREECO project. 

## File Structure 

```
├── data
    └── target_frequency.csv # Eocene milankovitch frequences from Andrew Walters
    └── raw_data # raw data directory
        └── fisher_assay.csv # fisher assay data as provided by Alan Carroll
        └── wilkins_peak_beds.csv # tuff and and marker bed positions from Mike Smith
    └── cleaned_data # processing directory with cleaned data
├── results # results file directory 
    └── time_calibrated_fisher_assay.csv # fisher assay data calibrated to median modifiedBchron age-depth model
├── R # R scripts to be run in numerical order 0 - 3
     └── 01_clean_data.R # fixes strat problems and reinterpolates fisher assay data
     └── 02_age_depth_model.R # modifiedBchron age-depth models for fisher assay data 
     └── 03_investigate_cyclostratigraphy.R # preliminary EHA 
        └── functions # data processing functions 
└── README.md
```

