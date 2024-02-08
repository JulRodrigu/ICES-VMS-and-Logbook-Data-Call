## ICES VMS and Logbook Data Call
This repository has been developed incollaboration with the ICES working group WGSFD to aid technical personnel of countries in successfully submitting data to comply with the 2023 ICES Data call for VMS/Log book data for fishing activities in the North East Atlantic and Baltic Sea over the years 2009-2023.

Users please note: this R-script is offered as an aid for countries to use and is not mandatory. ICES cannot take responsibility for any mistakes, updates or corrections to the R-script. The script has been, and is being developed, as a community driven initiative to help guide data submitters in an iterative way to meet the ICES VMS/Log book data call. The responsibility lies with individual countries to meet the ICES Data call for VMS/Log book data.

The following will provide a method on how to complete the SFD datacall using R and Rstudio on a windows computer.
In Rstudio go to files -> New Project... -> Version Control -> Git (if you dont already have git, download and install it and add it to Rstudio in Tools -> Global Options -> Git/SVN)
- Browse to your desired folder and add this Reposity URL: [https://github.com/ices-eg/ICES-VMS-and-Logbook-Data-Call.git](https://github.com/jepol77/ICES-VMS-and-Logbook-Data-Call.git)
- You now have all the necessary scripts to complete the datacall.

0_global.R
- Change the path to where you will work with the data
- Change thresholds if these do not suit your data
- choose how you want to link your tacsat to your eflalo - default is by day and by trip
- Run the helper functions

0.5_Change_folder.R
- Run this script if your tacsat and eflalo is in .csv format - it will change it to the required .RData files

1_eflalo_tacsat_preprocessing.R
- This will clean the eflalo and tacsat. If already cleaned, skip this. This script removes data, please check if everything is appropiate for your data.

2.1_Link_eflalo_tacsat.R
-   links eflalo categories to tacsat by trip id. Sometimes there will be multiple gears/mesh sizes/ICES rectangles/metiers/gear widths in a single trip. The function trip_assign add these categories from eflalo into tacsat by landing date / haul information / highest value. Please check if applicable for your data.
-   Adds a fleet segment to the data. Each segment should have a similar fishing pattern. This segmentation is later used to assign fishing/not fishing to each fishing ping. Default is to use metier level 5 and add a few special level 6 metiers that have special fishing patterns.
-   Saves speed profiles

2.2_Automatic_speed_thresholds.R
- Use this for the gears you want automatic speed thresholds for some selected gears. 

2.3_Construct_tacsaEflalo
- Make speed histograms for all fleet segments. Check these to see if you are using appropiate speed filters.
- Make a new speedarr fil (fix(speedaar)), using appropiate speed values
- Run for all years to merge tacsat and eflalo. The merged tacsatEflalo contain all fishing pings with eflalo information. 

<<<<<<< HEAD
2.4_add_info_tacsatEflalo.R
- Download EUSeaMap_2023, either from emodnet and do some calculations or directly from dtu: https://figshare.com/s/46ed591ca29f87c53311 
- Add habitat and swepat area to each fishing ping. 

3.1_Collect_tables.R
- Add all years together and only select the necessary columns

3.2_data_submission.R
- Add categories and aggregate data
- Check if all records are valid
- Submit data to ICES database. 
=======





>>>>>>> 5b98d8982a979334590a01a676a1e9f25180363b


For questions or to provide feedback please contact ICES accessions: accessions@ices.dk
