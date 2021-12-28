library(shiny)
library(tidyverse)
library(broom.mixed)
library(lmerTest)
library(bestNormalize)
library(XML)
library(Hmisc)
library(htmlTable)
library(shinythemes)
adnimerge<-read.csv("https://raw.githubusercontent.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/main/Datasets/adnimerge.csv")
cognitive<-read.csv("https://raw.githubusercontent.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/main/Datasets/uwnpsychsum.csv")
MRI<-read.csv("https://raw.githubusercontent.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/main/Datasets/MRI_tidy.csv")
abeta<-read.csv("https://raw.githubusercontent.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/main/Datasets/ucberkeleyav45.csv")
tau <- read.csv("https://raw.githubusercontent.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/main/Datasets/ucberkeleyav1451_pvc.csv")
FDG<-read.csv("https://raw.githubusercontent.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/main/Datasets/FDG_tidy.csv")

# Can the "proprocessing" go inside the parent function (also is there a way to automate this?)
demographic<-adnimerge %>% select(RID,VISCODE,PTGENDER,PTEDUCAT,AGE,
                                  ADAS11,ADAS13,MMSE,Years.bl,APOE4,DX) %>%group_by(RID) %>%
    mutate(PTGENDER=as.factor(ifelse(PTGENDER=="Female",0,
                                     ifelse(PTGENDER=="Male",1,NA))),
           Years.init=Years.bl-Years.bl[1]) %>% ungroup()

cognitive<-cognitive %>% select(RID,VISCODE,ADNI_MEM,ADNI_EF,ADNI_LAN,ADNI_VS)

abeta <- abeta %>% rename_if(stringr::str_detect(names(.),"SUVR"),~paste0(.,".AB")) %>%
    rename_if(stringr::str_detect(names(.),"VOLUME"),~paste0(.,".AB")) %>%
    select(-c(EXAMDATE,ORIGPROT))

tau <- tau %>% rename_if(stringr::str_detect(names(.),"SUVR"),~paste0(.,".T")) %>%
    rename_if(stringr::str_detect(names(.),"VOLUME"),~paste0(.,".T")) %>%
    select(-c(EXAMDATE,ORIGPROT))

upennbiomk9<-read.csv("https://raw.githubusercontent.com/YoungSeo1118/Shiny-Alzheimer-s-Disease-Data-Analysis-Tool/main/Datasets/upennbiomk9.csv")
#Amyloid Beta
abeta_row_above<-str_which(upennbiomk9$ABETA,">")
abeta_index_above<-upennbiomk9[abeta_row_above,]$BARCODE
#Tau
tau_row_above<-str_which(upennbiomk9$TAU,">")
tau_index_above<-upennbiomk9[tau_row_above,]$BARCODE
tau_row_below<-str_which(upennbiomk9$TAU,"<")
tau_index_below<-upennbiomk9[tau_row_below,]$BARCODE
#PTau
ptau_row_above<-str_which(upennbiomk9$PTAU,">")
ptau_index_above<-upennbiomk9[ptau_row_above,]$BARCODE
ptau_row_below<-str_which(upennbiomk9$PTAU,"<")
ptau_index_below<-upennbiomk9[ptau_row_below,]$BARCODE


for (i in upennbiomk9$BARCODE){
    #Amyloid
    if((upennbiomk9$BARCODE[upennbiomk9$BARCODE==i] %in% abeta_index_above)){
        upennbiomk9$ABETA[upennbiomk9$BARCODE==i] <-
            as.integer(parse_number(upennbiomk9$COMMENT[upennbiomk9$BARCODE==i]))
    }
    #Tau
    else if((upennbiomk9$BARCODE[upennbiomk9$BARCODE==i] %in% tau_index_above)){
        upennbiomk9$TAU[upennbiomk9$BARCODE==i] <-
            as.integer(parse_number(upennbiomk9$TAU[upennbiomk9$BARCODE==i]))
    }
    else if((upennbiomk9$BARCODE[upennbiomk9$BARCODE==i] %in% tau_index_below)){
        upennbiomk9$TAU[upennbiomk9$BARCODE==i] <-
            as.integer(parse_number(upennbiomk9$TAU[upennbiomk9$BARCODE==i]))
    }
    #PTAU
    else if((upennbiomk9$BARCODE[upennbiomk9$BARCODE==i] %in% ptau_index_above)){
        upennbiomk9$PTAU[upennbiomk9$BARCODE==i] <-
            as.integer(parse_number(upennbiomk9$PTAU[upennbiomk9$BARCODE==i]))
    }
    else if((upennbiomk9$BARCODE[upennbiomk9$BARCODE==i] %in% ptau_index_below)){
        upennbiomk9$PTAU[upennbiomk9$BARCODE==i] <-
            as.integer(parse_number(upennbiomk9$PTAU[upennbiomk9$BARCODE==i]))
    }
    else {NA}
}

csf <- upennbiomk9 %>% select (RID,VISCODE,BARCODE,ABETA,TAU,PTAU) %>%
    mutate(ABETA = as.numeric(ABETA),
           TTAU = as.numeric(TAU),
           PTAU = as.numeric(PTAU),
           P_A = as.numeric(PTAU/ABETA))

Columns<-c(colnames(demographic),colnames(abeta),colnames(tau),
           colnames(FDG),colnames(MRI),colnames(cognitive),colnames(csf)) %>% unique()
not_sel<-"Not Selected"

shinyUI(navbarPage(theme=shinytheme("cerulean"),
                   title = "Shiny Alzheimer's Disease Prognostic Tool",
                   main_page <- tabPanel(
                       title = "Analysis",
                       titlePanel("Analysis"),
                       sidebarLayout(
                           sidebarPanel(
                               title="Inputs",
                               #fileInput("upload", NULL, accept = c(".csv", ".tsv"),multiple=T),
                               selectInput("independent","Independent Varible:",choices = c(not_sel,Columns)),
                               selectInput("dependent","Dependent Variable:",choices = c(not_sel,Columns)),
                               selectInput("covariate","Covariate:",choices = c("NULL","DX","APOE4", "DX, APOE4")),
                               actionButton("run_button","Run Analysis",icon("play"))
                           ),
                           mainPanel(tabsetPanel(
                               tabPanel(
                                   title = "Table",
                                   uiOutput("table")
                               ),
                               tabPanel(
                                   title = "Plot",
                                   plotOutput("plot")
                               )
                           )
                           )
                       )
                   ),
                   about_page <- tabPanel(
                       title = "About",
                       titlePanel("About"),
                       "This is the first prototype of the Shiny Alzheimer's Disease (AD) Prognostic 
                Tool created by Younghoon Seo during his internship at Samsung Medical Center.
                The purpose of the app is to assist clinical neurologists in analyzing the relationship
                between various AD-related variables by generating linear mixed effect models between the 
                variables of the user's choice from the ADNI cohort database. Using these relationships,
                the app then prognosticates the biological and clinicl features of disease progression.
                Later modifications will aim to facilitate the user's usage of the app (e.g., simplify the 
                variable names) and enhance the overall aesthetics of the app.",
                       br()
                   )
))
