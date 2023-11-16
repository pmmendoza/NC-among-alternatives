# NC-among-alternatives

This is the official repository for all analyses conducted in: 
Mendoza, P., Nai, A., Bos, L. (2023). The fleeting allure of dark campaigns. Backlash from negative and uncivil campaigning in the presence of (better) alternatives. _Political Communication_ (_forthcoming_).


## Replication instructions  
__0. Download this repository.__  
Download the entire repository, start an R project in the same directory.  
To assure the reproducibility of the current code, I have added a [renv](https://rstudio.github.io/renv/articles/renv.html) package control file. After having downloaded the repository, you can install all necessary packages and in the same versions used in this code by running `renv::restore()` within your R project.  

### A) Replication from scratch  
__1. Download the necessary datasets.__  
The analyses in this study are based on two other publicly available datasets cited in the paper:  
1. The voter component of the European Election Study (Schmitt et al. 2021). Dataset: https://search.gesis.org/research_data/ZA7581  
2. The EPEES_19 published as replication material (Nai et al. 2022). Dataset: https://osf.io/bzm48  

To be able to run this code you first need to manually download the EES datasets from the GESIS data archive.
After logging in to the GESIS data archive you can access the data here: https://search.gesis.org/research_data/ZA7581
Download the following two files and place them in the '/data' folder of your project directory.  
1) 'ZA7581_v2-0-1.dta' - The full dataset. You can find this under 'Downloads > Datasets  
2) 'ZA7581_cp.csv' - The party dataset. You can find this under 'Downloads > Other documents  

__2. Run the party matching script__  
The script '01_harmonisation-and-matching.R' links and matches parties in the Voter Component of the European Election Study (EES) to parties in the EPEES_2019, the 2019 European Parliament Election Expert Survey dataset.  

The resulting file will be:  
1. a .csv match file with a time stamp "00 2023-10-18_Linkage-matched-parties.csv" that includes i.a. the unique party id in the EES dataset and a newly created unique party id for the EPEES_19 dataset.
2. Harmonised versions of the EPEES_19 and EES datasets.

The matching is done in two steps.
1. matching of parties via partyfacts.
   The EPEES dataset has a CHES id. Via the partyfacts database (https://partyfacts.herokuapp.com/)
   We can link this CHES id to the parties' ids for the previous edition of the EES14
   Whenever a EES party's EES19 id is also contained in the EES14, we complete the match.
2. manual matching.
   The remaining, unmatched parties in the EPEES were matched manually with EES19
   party ids. These manual matches are available in the repository under 'data/00_manual-matches-EPEESxEES.csv'.


__3. Run the data wrangling script__  
The data wrangling script prepares all necessary variables for the analysis and already drops parties from the EPEES file if they can not be matched with the EES data or if they have less than 3 expert ratings. The output of this script is are pre-processed EPEES and EES files stored in the '/data' directory as 'CURRENTDATE_EPEES-file.csv', and 'CURRENTDATE_Analysisfile.csv', where current date represents the time stamp of when the file was created.  


### B) Replication from analysis file  
__1. Recreating the analyses__  
Run the analysis and the tables script. Note that the tables script only works if the objects from the analysis script are still in the gloval environment!


### References
Nai, A., Medeiros, M., Maier, M., & Maier, J. (2022). Euroscepticism and the use of negative, uncivil and emotional campaigns in the 2019 European Parliament election: A winning combination. European Union Politics, 23(1), 21–42. https://doi.org/10.1177/14651165211035675

Schmitt, H., Hobolt, S. B., Brug, W. V. D., & Popa, S. A. (2021). European Parliament Election Study 2019, Voter Study. Verion 2.0.1. GESIS Data Archive, Cologne. https://doi.org/10.4232/1.13846
