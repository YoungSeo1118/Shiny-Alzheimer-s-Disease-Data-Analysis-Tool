# Shiny-Alzheimer's-Disease-Data-Analysis-Tool

## Requirements
This application uses the ADNIMERGE package from ADNI(adni.loni.usc.edu). The ADNIMERGE package must be downloaded for the execution of the application. In addition to the ADNIMERGE package, users should download the two files in the Datasets folder (MRI and FDG) that I have already preprocessed.

## Description
This is the first prototype of the Shiny Alzheimer's Disease (AD) Data Analysis Tool created by Younghoon Seo during his internship at Samsung Medical Center. 
The purpose of the app is to assist clinical neurologists in analyzing the relationship between various AD-related variables by generating linear mixed effect 
models between the variables of the user's choice from the ADNI cohort database. Using these relationships, the app aims to prognosticate the biological and clinical features of Alzheimer's disease progression, which would facilitate patient care and disease monitoring process.

The code for the Shiny Application can be found in the folder Shiny Application, which includes the ui and server files. This is the first prototype, so the future
modifications would take measures to enhance the overall aesthetics and improve the user-friendliness of the app (e.g., simplify the variable name). As can be seen
in the pictures below, the app consists of the statistics section (shows estimate, confidence interval, standard error, random effects, and significance) and the
plot section (prediction graph using the ggPredict function).

### Statistics Section
![Alt text](https://github.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/blob/main/Shiny_Application/Screenshots/statistics_app_screenshot.png)
### Plot Section
![Alt text](https://github.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/blob/main/Shiny_Application/Screenshots/plot_app_screenshot.png)
