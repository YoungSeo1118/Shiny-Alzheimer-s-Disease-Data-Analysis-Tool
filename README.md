# Shiny-Alzheimer's-Disease-Data-Analysis-Tool

## Requirements
This application uses the ADNIMERGE package from ADNI(adni.loni.usc.edu). The ADNIMERGE package must be downloaded for the execution of the application. In addition to the ADNIMERGE package, users should download the two files in the Datasets folder (MRI and FDG) that I have already preprocessed.

## Description
This is the first prototype of the Shiny Alzheimer's Disease (AD) Data Analysis Tool created by Younghoon Seo during his internship at Samsung Medical Center. 
The purpose of the app is to assist clinical neurologists in analyzing the relationship between various AD-related variables by generating linear mixed effect 
models between the variables of the user's choice from the ADNI cohort database.

The Functions code/file consists of different functions that would facilitate the data processing and analysis steps. It consists of find_dataset() that locates  
the dataset names based on the variables that the users inserted, data_merge() that automatically merges the identified datasets, Normalize() that performs  
automatic normalization based on the Pearson P test statistic for normality, and lmer_fun() that runs linear mixed effect models. These functions are grouped into 
an umbrella function called wrapper_function().

The code for the Shiny Application can be found in the folder Shiny Application, which includes the ui and server files. This is the first prototype, so the future
modifications would take measures to enhance the overall aesthetics and improve the user-friendliness of the app (e.g., simplify the variable name). As can be seen
in the pictures below, the app consists of the statistics section (shows estimate, confidence interval, standard error, random effects, and significance) and the
plot section (prediction graph using the ggPredict function).

### Statistics Section
![Alt text](https://github.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/blob/main/Shiny_Application/Screenshots/statistics_app_screenshot.png)
### Plot Section
![Alt text](https://github.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/blob/main/Shiny_Application/Screenshots/plot_app_screenshot.png)
