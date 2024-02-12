# TraXpert
> A particle track analysis and visualisation software mainly developed for TrackMate file formats but also supports Imaris and Chemotaxis Tool.

## Table of Contents
* [General Info](#general-information)
* [Technologies Used](#technologies-used)
* [Features](#features)
* [Screenshots](#screenshots)
* [Setup](#setup)
* [Usage](#usage)
* [Project Status](#project-status)
* [Room for Improvement](#room-for-improvement)
* [Acknowledgements](#acknowledgements)
* [Contact](#contact)
* [License](#license)

## General Information
- There are a number of particle tracking software for cell biology. However, there was a lack of specialized data analysis and visualisation tool for it.
- This project aims to fulfill this gap.
- I have realized an easy way of doing it, which then developed into this massive project.

## Technologies Used
- R
- R Shiny
- Docker for deployment

## Features
Traxpert...
- recognizes file naming pattern and the experimental groups in that pattern.
- automates data visualization and analysis and remains to be flexible as well.
- offers tracking specific data visualization, such as trajectory plots, directionality plots (aka radar plots or Nightingale plots).
- implements simple statistics and circular statistics for track directions.

## Setup
You have two alternatives to setup TraXpert.

### Recommended way with Docker and git

1. You will need `git` and `docker` first.
2. Clone the most recent release with e.g. `git clone --depth 1 -b release/0.4.0 https://github.com/SarenT/TraXpert.git` (depth option only downloads necessary files skipping the git history, Current release version may differ.)
3. Open your terminal in the repository folder (e.g. navigate to the TraXpert folder.)
4. Run `docker compose up` and wait until you see the following line: `[INFO] shiny-server - Starting listener on http://[::]:3838`. Note that first run can take some time, while all the required packages are installed.
5. Then in your browser (e.g. Firefox) visit or click this link [localhost:3838](localhost:3838)
<br>To stop TraXpert, you can stop the process (e.g. with `Ctrl + C`) within the terminal.

### Alternative way with own local installation and deployement with R & RStudio
You need to know about R and RStudio<br>

1. Install R and RStudio on your PC
2. Download this repository in a Zip file
3. Extract the contents of the Zip file
4. On Linux: Install all system dependencies: `libpq-dev` and `libudunits2-dev` (on Windows and Mac this may not be required)
5. Install all of the R packages: `xml2`, `ggplot2`, `magrittr`, `dplyr`, `ggpubr`, `RColorBrewer`, `udunits2`, `sticky`, `shiny`, `sortable`, `colourpicker`, `DT`, `svglite`, `readxl`, `colorspace`, `smoother`, `shinyBS`, `car`, `latex2exp`, `shinyjs`, `htmlwidgets`, `shinyWidgets`, `dplyr`, `tidyr`, `purrr`, `rlang`, `viridisLite`, `e1071`, `circular`, `stringr`, `ggiraphExtra`
6. Open `server.R` and `ui.R` in RStudio and use the `Run App` button.

## Usage
When using TraXpert *MAKE SURE* to use consistent physical units with your files, especially in Trackmate. TraXpert requires physical units. If you don't have any calibration, then feel free to use _1 μm_ or _1 mm_

## Project Status
Project is: _released 0.4.0_


## Room for Improvement
3D migration directionality requires more testing and perhaps correction. Currently, this feature shouldn't be used.

To do:
- More tests on Imaris format

## Acknowledgements
- Many thanks to Sixt Lab for their support.<br>
- Ste Tavano for constant testing and feedback on Imaris support.<br>
- Many thanks to Julian Stopp for initial tests.<br>
- Many thanks to Michael Sixt for the scientific supervision and project.<br>
- Many thanks to Christoph Sommer for the data science help.
- Many thanks to Institute of Science and Technology Austria for the amazing environment.

## Contact
Created by [@SarenT](sarentasciyan.eu) - feel free to contact me! Also via saren.tasciyan [at] ist.ac.at or via Github.

## License

[TraXpert](https://github.com/SarenT/TraXpert) © 2024 by [Saren Tasciyan](http://sarentasciyan.eu/) is licensed under [Attribution-NonCommercial-ShareAlike 4.0 International](http://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1)
