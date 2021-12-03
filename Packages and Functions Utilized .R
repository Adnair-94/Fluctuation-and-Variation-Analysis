#required libraries
library(extrafont)
loadfonts(device="win")
windowsFonts()
library(isofor)
library(tidyverse)
library(ggpubr)
library(ggsci)
#theme for journal publicaton (further modification can be done according to required specific graphs)
journal_theme=theme(text=element_text(family="Arial",size=10),
rect=element_blank(),
panel.grid=element_blank(),
axis.line=element_line(color="black",size=0.5),axis.ticks.length=unit(-0.15, "cm"),
axis.ticks = element_line(colour = "black", size = 0.5),
axis.text.x = element_text(color="black",size = 10, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
axis.text.y = element_text(color="black",size = 10, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
panel.border = element_rect(colour = "black", fill=NA, size=0.5))
theme_set(journal_theme)
#isolation forest analysis function made using the "isofor" package. Before using the function confirm the columns of the .csv file. This function includes all columns in the analysis except for the first.
isofor<-function(x){ #where x is a data frame created with the .csv file.
model=iForest(x[-c(1)],nt=100) #excludes first column from the analysis, uses 100 isolation trees, subsample (phi) not speficied.
iso.score=predict(model,x[-c(1)]) #generates the isolation score for the data  
x$process_score=iso.score #adds a column called process_score (using the generated isolation score) to the data frame.
x$category=ifelse(x$process_score>=0.60,"Outlier","Normal") # adds a column called category to the data frame which labels the data as outlier or normal based on isolation score
return(x)
}
#




