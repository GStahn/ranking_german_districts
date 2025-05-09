# ranking_german_districts
Develops an application that allows users to rank German districts based on a customized, preference-weighted index.

This repository contains an R-based application that computes and visualizes a ranking of German districts (`Kreise`) based on a customizable, weighted index reflecting various dimensions of quality of life. The index allows users to assign individual weights to factors such as environmental quality, economic opportunity, infrastructure access, and social indicators.

## Features

- **Custom Index Calculation**: Combines over 40 variables into a single weighted index score for each district.
- **District-level Analysis**: Separate rankings available for all districts (`Alle Kreise`), urban districts (`SK`), and rural districts (`LK`).
- **Top 20 Visualizations**: Automatically generates bar plots of the top 20 ranked districts for each group.
- **Modular Structure**: Clean separation of data loading, index creation, and plotting.

## Variables Used

The index is based on indicators like:

- **Environmental Quality**: NO₂, PM₂.₅, PM₁₀, CO, SO₂, Pb levels  
- **Socio-Economic Indicators**: GDP per capita, median income, unemployment, migration balance  
- **Infrastructure Access**: Access to highways, public transport, broadband, medical services  
- **Housing and Urban Indicators**: Rent levels, housing construction, green space  
- **Social Services**: Daycare availability, staff-to-child ratio, poverty rates  
- **Demographics**: Population density, share of elderly, diversity  

Each variable can be positively or negatively weighted to reflect user preferences.

## File Structure

- `WO_Index_V[number]_git.R`: Main script for data processing, index computation, and visualization.
- `Data_prep_git.R`: Script for manipulating the raw data. 
- `Data/`: Folder containing cleaned and normalized `.rds` data inputs.
- `Graphs/`: Output directory for the top-20 district visualizations.
- `Work/`: Folder for index outputs as `.txt` files.

## How to Use

1. Open the `WO_Index_V[number]_git.R` script.
2. Ensure the required data paths (`path_data`, `path_graphs`, `path_work`) are set correctly.
3. Adjust the `weights` vector to reflect your own priorities.
4. Run the script to:
    - Compute index scores for each district.
    - Save results to `.rds` and `.txt` files.
    - Generate bar plots of the top 20 districts by index score.

## Roadmap
1. Create an R-Shiny app, which let's useres set their preferences easily without changing and executing R code.
2. Update data. 

## Dependencies

- R (≥ 4.0.0)
- `tidyverse`

To install required packages:

```r
install.packages("tidyverse")
```

## Author

Gerrit Stahn  
Email: [gerrit.stahn93.gs4@gmail.com](mailto:gerrit.stahn93.gs4@gmail.com)
