library(ggplot2)


rm(list=ls());
lst <- list.files(pattern="*.R");
lst <- lst[lst!="runtlf.R"]
lst <- lst[lst!="plot-pkratio.R"]
lst <- replace(lst, c(1,11),lst[c(11,1)]);
lst <- replace(lst, c(5,7),lst[c(7,5)]);
lst <- replace(lst, c(11,3),lst[c(3,11)]);
lst <- replace(lst, c(13,7),lst[c(7,13)]);
for (f in lst) {
  #print(f)
  p<- paste0(getwd(),'/',f)
  #print(p)
  source(p)
}

generate_df_column <-function(r,output0,output1) {
  
  if (r == 1)
  { 
    return(output0)
  }
  else
  { 
    return(output1)
  }
  
}

N<-20;

df <- data.frame(
  "IndividualID" = seq(1,N),
  "Population" = sapply(round(runif(N,0,1)),generate_df_column,output0="Asian",output1="European"),
  "Gender" = sapply(round(runif(N,0,1)),generate_df_column,output0="M",output1="F"),
  "Age" = round(runif(N,4,8)),
  "Compound" = sapply(round(runif(N,0,1)),generate_df_column,output0="Aspirin",output1="Sugar"),
  "Dose" = sapply(round(runif(N,0,1)),generate_df_column,output0=3,output1=6),
  "Organ" = sapply(round(runif(N,0,1)),generate_df_column,output0="VenousBlood",output1="VenousBlood"),
  "Compartment" = sapply(round(runif(N,0,1)),generate_df_column,output0="Plasma",output1="Plasma"),
  "Simulated" = runif(N,1,20),
  "Observed" = runif(N,1,20)
)
df[["Ratio"]]<-(df$Simulated)/(df$Observed)


source('C:/Users/ahamadeh/Dropbox/GitHub/input-Table-TLF-Library-develop/R/plot-pkratio.R')

dfinp_color <- data.frame("Gender" = c("M","F") , "Caption" = c("Male","Female"))
ageBounds<-list(c(3,6),c(7,8))
dfinp_bnds<-data.frame(   "Age" = I(  ageBounds ) ,  "Caption" = c("Patients aged 3-6" , "Patients aged 7-8") )
newDataMapping <- PKRatioDataMapping$new( shapeGrouping = c("Compound","Population") ,
                                          colorGroupingDataFrame = dfinp_color , colorLegendTitle = "Custom Caption",
                                          sizeGroupingDataFrame = dfinp_bnds , sizeLegendTitle = "Bin var caption"  
                                          )

show(plotPKRatio(df, dataMapping = newDataMapping))
