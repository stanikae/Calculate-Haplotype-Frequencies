# Calculate-Haplotype-Frequencies
This script calculates allelic and genotypic frequencies, as well as perform Fisher's Test to compare representation of genotypes and alleles in different sample groups

## Instructions to set-up and run the script for calculating haplotype frequencies

### A. SET-UP
1. (On a Windows Computer) In your Documents folder, create folder were you will be working from.
````
Rename the folder to: Haplotype-Frequencies

````		
2. Open RStudio

3. Go to File --> New Project... --> Version control --> Git

4. In the tab under Repository URL: paste the following link
````
https://github.com/stanikae/Calculate-Haplotype-Frequencies.git
````
5. Check to see that in the tab under "Create project as subdirectory of:" there is
``
~/Documents/Haplotype-Frequencies
``		
6. Click "Create Project"
* This will begin copy the scripts from my GitHub
	
### B. Run the analysis
1. First run the install_packages.R script
* This script will install the relevant packages needed to perform the analysis
* In addition this script will create the "input-files" folder and the "sample-metadata" folder
``
source("install_packages.R")
``		
2. Place your input files (.txt) in the "input-files" folder for all your genes for which you want to calculate frequencies
* Make sure file format is the same as that used in the example folder (input-files)

3. Place your metadata file in the sample-metadata folder
* Make sure the format of your metadata file is of the following format and sample and group separated by tabs:
````
sample1	Group
sample2	Group
etc.
````
* Do not put headers (column names) on this file

4. Next, step is to run the actual analysis script
``
source("Calculating_frequencies.R")
``		
### C. Results
* When the script is done running you will find your results in the "Results" folder
* Results for all your genes will be saved in an excel WorkBook with each WorkSheet containing results for each gene
