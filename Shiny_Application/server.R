library(shiny)
library(tidyverse)
library(broom.mixed)
library(lmerTest)
library(bestNormalize)
library(XML)
library(Hmisc)
library(htmlTable)

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

datasets<-list(demographic,cognitive,abeta,tau,MRI,FDG,csf)
col_df<-NULL
for(i in 1:length(datasets)){
    col_df[[i]]<-colnames(datasets[[i]])
}
find_datasets<-function(IV,DV){
    index_IV<-str_which(col_df,paste0("\\b",IV,"\\b"))
    index_DV<-str_which(col_df,paste0("\\b",DV,"\\b"))
    list(index_IV,index_DV)
}


merge_data <- function(data1,data2){
    if ((demographic %in% c(data1,data2))[1]){
        if ((dim(data1)==dim(data2))[1]){
            data1 %>% group_by(RID) %>% filter(n()>1) %>% ungroup()
        }
        else {
            merge(data1,data2,by=c("RID","VISCODE")) %>% #,all.x=T
                group_by(RID) %>% filter(n()>1) %>% ungroup()
        }
    }
    else {
        if ((dim(data1)==dim(data2))[1]){
            merge(demographic,data1,by=c("RID","VISCODE")) %>% 
                group_by(RID) %>% filter(n()>1) %>% ungroup()
        }
        else {
            merge(demographic,data1,by=c("RID","VISCODE")) %>%
                merge(data2,by=c("RID","VISCODE")) %>%
                group_by(RID) %>% filter(n()>1) %>% ungroup()
        }
    }
}


Normalize<-function(IV,DV,df){
    body(bestNormalize)[[4]]<-substitute(methods <- c("no_transform","log_x","sqrt_x"))
    x<-with(df,bestNormalize(eval(parse(text=IV)), standardize = F, allow_exp = F, allow_orderNorm = F))
    y<-with(df,bestNormalize(eval(parse(text=DV)), standardize = F, allow_exp = F, allow_orderNorm = F))
    return(list(x$x.t,y$x.t))
    #df<-df%>%mutate(IV=x$x.t,DV=y$x.t)
}

lmer_fun<-function(df,IV,DV,covars=NULL){
    if(is.null(covars)){
        if (IV %in% colnames(MRI)) {
            lmer(as.formula(paste(DV,"~Years.init*",IV,"+AGE+PTGENDER+ICV+Mag+(Years.init|RID)")),
                 data=df,na.action=na.exclude)
        }
        else if (IV %in% colnames(cognitive)){
            lmer(as.formula(paste(DV,"~Years.init*",IV,"+AGE+PTEDUCAT+PTGENDER+(Years.init|RID)")),
                 data =df,na.action = na.exclude)
        }
        else {
            lmer(as.formula(paste(DV,"~Years.init*",IV,"+AGE+PTGENDER+(Years.init|RID)")),
                 data =df,na.action = na.exclude)
        }
    }
    else{
        if (length(covars)==1){
            if (IV %in% colnames(MRI)) {
                lmer(as.formula(paste(DV,"~Years.init*",IV,"+AGE+PTGENDER+ICV+Mag+(Years.init|RID)+",covars[[1]])),
                     data=df,na.action=na.exclude)
            }
            else if (IV %in% colnames(cognitive)){
                lmer(as.formula(paste(DV,"~Years.init*",IV,"+AGE+PTEDUCAT+PTGENDER+(Years.init|RID)+",covars[[1]])),
                     data =df,na.action = na.exclude)
            }
            else {
                lmer(as.formula(paste(DV,"~Years.init*",IV,"+AGE+PTGENDER+(Years.init|RID)+",covars[[1]])),
                     data =df,na.action = na.exclude)
            }       
        }
        else {
            if (IV %in% colnames(MRI)) {
                lmer(as.formula(paste(DV,"~Years.init*",IV,"+AGE+PTGENDER+",covars[[1]],"+ICV+Mag+(Years.init|RID)+",covars[[2]])),
                     data=df,na.action=na.exclude)
            }
            else if (IV %in% colnames(cognitive)){
                lmer(as.formula(paste(DV,"~Years.init*",IV,"+AGE+PTEDUCAT+PTGENDER+",covars[[1]],"+(Years.init|RID)+",covars[[2]])),
                     data =df,na.action = na.exclude)
            }
            else {
                lmer(as.formula(paste(DV,"~Years.init*",IV,"+AGE+PTGENDER+",covars[[1]],"+(Years.init|RID)+",covars[[2]])),
                     data =df,na.action = na.exclude)
            }
        }
    }
}


wrapper_function<-function(IV,DV,covars=NULL){
    datasets<-list(demographic,cognitive,abeta,tau,MRI,FDG,csf)
    col_df<-NULL
    for(i in 1:length(datasets)){
        col_df[[i]]<-colnames(datasets[[i]])
    }
    
    IV_dataset<-as.data.frame(datasets[find_datasets(IV,DV)[[1]]])
    DV_dataset<-as.data.frame(datasets[find_datasets(IV,DV)[[2]]])
    df<-merge_data(IV_dataset,DV_dataset)
    
    IV_N<-Normalize(IV,DV,df)[[1]]
    DV_N<-Normalize(IV,DV,df)[[2]]
    df[IV]<-IV_N; df[DV]<-DV_N
    
    mod<-lmer_fun(df,IV,DV,covars)
    #t<-summary(mod)$coef
    t<-sjPlot::tab_model(mod, show.p=T,collapse.ci=T,
                         show.se=T,show.stat=T,show.re.var=F)
    t<-data.frame(readHTMLTable(htmlParse(t))[1])
    colnames(t) <- t[1,]
    t <- t[-1,]
    rownames(t)<-NULL
    # Want to change this later
    
    #p2<-sjPlot::plot_model(mod,show.values=TRUE, show.p=TRUE)
    p<-ggeffects::ggpredict(mod,terms = c("Years.init",IV)) %>% plot()
    list(t,p)
}

shinyServer(function(input,output){
    independent<-eventReactive(input$run_button,input$independent)
    dependent<-eventReactive(input$run_button,input$dependent)
    
    covariate<-reactive({
        switch(input$covariate,
               "NULL" = NULL,
               "DX" = list("DX"),
               "APOE4" = list("APOE4"),
               "DX, APOE4" = list("DX","APOE4"))
    })
    
    # covariate<-eventReactive(input$run_button,covariate())
    
    table<-eventReactive(input$run_button,{
        htmlTable(wrapper_function(independent(),dependent(),covariate())[[1]])
    })
    output$table<-renderUI(HTML(table()))
    
    plot<-eventReactive(input$run_button,{
        wrapper_function(independent(),dependent(),covariate())[[2]]
    })
    output$plot<-renderPlot(plot())
})
