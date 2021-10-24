#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("/Users/macintosh/Desktop/R_App/Data_Analysis_App/")

library(shiny)
library(shinythemes)
Columns<-c(colnames(demographic),colnames(abeta),colnames(tau),
           colnames(FDG),colnames(MRI),colnames(cognitive)) %>% unique()
not_sel<-"Not Selected"

# Define UI for application that draws a histogram
shinyUI(navbarPage(theme=shinytheme("cerulean"),
                         title = "Shiny Alzheimer's Disease Data Analysis Tool",
                         main_page <- tabPanel(
                             title = "Analysis",
                             titlePanel("Analysis"),
                             sidebarLayout(
                                 sidebarPanel(
                                     title="Inputs",
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
                             "This is the first prototype of the Shiny Alzheimer's Disease (AD) Data Analysis 
                Tool created by Younghoon Seo during his internship at Samsung Medical Center.
                The purpose of the app is to assist clinical neurologists in analyzing the relationship
                between various AD-related variables by generating linear mixed effect models between the 
                variables of the user's choice from the ADNI cohort database. Later modifications will aim to
                facilitate the user's usage of the app (e.g., simplify the variable names) and enhance the 
                overall aesthetics of the app.",
                             br()
                         )
))
