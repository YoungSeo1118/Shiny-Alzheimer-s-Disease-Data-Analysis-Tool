setwd("/Users/macintosh/Desktop/R_App/Data_Analysis_App/")

install.packages("ADNIMERGE_0.0.1.tar.gz", repos = NULL, type = "source")
library(ADNIMERGE)
library(tidyverse)
library(broom.mixed)
library(lmerTest)
library(bestNormalize)
library(XML)
data(adnimerge)
cognitive<-uwnpsychsum
MRI<-read.csv("MRI_tidy.csv")
abeta<-ucberkeleyav45
tau <- ucberkeleyav1451_pvc
FDG<-read.csv("FDG_tidy.csv")

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

# Find Datasets
datasets<-list(demographic,cognitive,abeta,tau,MRI,FDG)
col_df<-NULL
for(i in 1:length(datasets)){
        col_df[[i]]<-colnames(datasets[[i]])
}
#names(col_df)<-c("demographic","cognitive","abeta","tau","MRI","FDG")
find_datasets<-function(IV,DV){
        index_IV<-str_which(col_df,paste0("\\b",IV,"\\b"))
        index_DV<-str_which(col_df,paste0("\\b",DV,"\\b"))
        list(index_IV,index_DV)
}

#dim(data.frame(datasets[find_datasets("Cingulate","MMSE")[[1]]]))

#Data Merge --> Do I have to merge cognitive data?
merge_data <- function(data1,data2){
        if ((demographic %in% c(data1,data2))[1]){
                if ((dim(data1)==dim(data2))[1]){
                        data1 %>% group_by(RID) %>% filter(n()>1) %>% ungroup()
                }
                else {
                        merge(data1,data2,by=c("RID","VISCODE"),all.x=T) %>%
                                group_by(RID) %>% filter(n()>1) %>% ungroup()
                }
        }
        else {
                if ((dim(data1)==dim(data2))[1]){
                        merge(demographic,data1,by=c("RID","VISCODE"),all.x=T) %>% 
                                group_by(RID) %>% filter(n()>1) %>% ungroup()
                }
                else {
                        merge(demographic,data1,by=c("RID","VISCODE"),all.x=T) %>%
                                merge(data2,by=c("RID","VISCODE"),all.x=T) %>%
                                group_by(RID) %>% filter(n()>1) %>% ungroup()
                }
        }
}

#dim(merge_data(FDG,demographic))


# Normalize
#wrapper<-function(IV,DV,df){
#        IV_N<-Normalize(IV,DV,df)[[1]]
#        DV_N<-Normalize(IV,DV,df)[[2]]
#        df[IV]<-IV_N
#        df[DV]<-DV_N
#        View(df)
#}


Normalize<-function(IV,DV,df){
        body(bestNormalize)[[4]]<-substitute(methods <- c("no_transform","log_x","sqrt_x"))
        x<-with(df,bestNormalize(eval(parse(text=IV)), standardize = F, allow_exp = F, allow_orderNorm = F))
        y<-with(df,bestNormalize(eval(parse(text=DV)), standardize = F, allow_exp = F, allow_orderNorm = F))
        return(list(x$x.t,y$x.t))
        #df<-df%>%mutate(IV=x$x.t,DV=y$x.t)
}

#LMER Function -> unsure: how the identification of IV, DV would affect the type of analysis (need to develop)
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


#lmer_fun(df,"Cingulate","MMSE",covars=list("DX","APOE4")) %>% tidy()


#Wrapper Function
wrapper_function<-function(IV,DV,covars=NULL){
        datasets<-list(demographic,cognitive,abeta,tau,MRI,FDG)
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
                             show.se=T,show.stat=T)
        t<-data.frame(readHTMLTable(htmlParse(t))[1])
        colnames(t) <- t[1,]
        t <- t[-1,]
        # Want to change this later
        
        #p2<-sjPlot::plot_model(mod,show.values=TRUE, show.p=TRUE)
        p<-ggeffects::ggpredict(mod,terms = c("Years.init",IV)) %>% plot()
        list(t,p)
}

# library(htmlTable)
# wrapper_function("Cingulate","MMSE",covars=list("APOE4","DX"))[[1]]
