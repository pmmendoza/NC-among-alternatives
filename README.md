# NC-among-alternatives

This is the official repository for all analyses conducted in: 
Mendoza, P., Nai, A., Bos, L. (2023). The fleeting allure of dark campaigns. Backlash from negative and uncivil campaigning in the presence of (better) alternatives. _Political Communication_ (_forthcoming_).


## Replication instructions  
### Replication from scratch  
*0. Download this repository.*  _Download the entire repository, open an R session / in the same directory._
*1. Download the necessary datasets.*  
The analyses in this study are based on two other publicly available datasets cited in the paper:  
1. The voter component of the European Election Study (Schmitt et al. 2021). Dataset: https://search.gesis.org/research_data/ZA7581  
2. The EPEES_19 published as replication material (Nai et al. 2022). Dataset: https://osf.io/bzm48  

To be able to run this code you first need to manually download the EES datasets from the GESIS data archive.
After logging in to the GESIS data archive you can access the data here: https://search.gesis.org/research_data/ZA7581
Download the following two files and place them in the '/data' folder of your project directory.
1) 'ZA7581_v2-0-1.dta' - The full dataset. You can find this under 'Downloads > Datasets
2) 'ZA7581_cp.csv' - The party dataset. You can find this under 'Downloads > Other documents

*2. Run the data pre-processing file*


### Replication from analysis file  
To replicate the plots and tables presented in the paper, download the entire repository, open an R session in the same directory and run the analysis script and then the tables script.


### References
Nai, A., Medeiros, M., Maier, M., & Maier, J. (2022). Euroscepticism and the use of negative, uncivil and emotional campaigns in the 2019 European Parliament election: A winning combination. European Union Politics, 23(1), 21–42. https://doi.org/10.1177/14651165211035675

Schmitt, H., Hobolt, S. B., Brug, W. V. D., & Popa, S. A. (2021). European Parliament Election Study 2019, Voter Study. Verion 2.0.1. GESIS Data Archive, Cologne. https://doi.org/10.4232/1.13846
