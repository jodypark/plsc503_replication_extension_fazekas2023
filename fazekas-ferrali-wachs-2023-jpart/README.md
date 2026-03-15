# From the authors ---
# Replication files for "Agency independence, campaign contributions, and favouritism in US federal government contracting."


## File structure

- `./README.md`: the file you are currently reading
- `./replication.R`: replication script
- `./functions.R`: functions used by the replication script
- `./codebook.pdf`: data dictionary
- `./contracts.csv`: This dataset contains contract-level data from 2003 to 2014. Please refer to the manuscript for additional details about data construction. Sources: 
    - Procurement data: https://usaspending.gov
    - Tax haven data: https://cthi.taxjustice.net/en/
    - Disbarrment data: https://sam.gov/content/exclusions
    - Contributions data: https://data.stanford.edu/dime
- `./validation.csv`: This dataset compiles a series of state-level indicators of corruption. Sources: 
    - GALLUP perception of corruption surveys (2006-2014), aggregated at the state level
    - Corruption in American States Survey of reporters
    - Boylan & Long (2003) State House perception of corruption survey

## How to use this replication file

Run the script `./replication.R`. Please make sure to set the working directory to the folder that contains this script, and that the working directory contains folders named `./figures` and `./tables`. 

---

# Park ---

- `park_robustness_check_fazekas-etal-2023_503_260121.Rmd` : R markdown file for robustness checks using the replication codes & datasets 
- `park_robustness_check_fazekas-etal-2023_503_260121.html` : HTML output of the R markdown file

- Extra files for formatting:
  - `apanodoi.csl`
  - `styles.css`
  - `references.bib`

