library(extrafont)
loadfonts(device="win")
windowsFonts()
library(isofor)
library(tidyverse)
library(ggpubr)
library(ggsci)
################
journal_theme=theme(text=element_text(family="Arial",size=10),
rect=element_blank(),
panel.grid=element_blank(),
axis.line=element_line(color="black",size=0.5),axis.ticks.length=unit(-0.15, "cm"),
axis.ticks = element_line(colour = "black", size = 0.5),
axis.text.x = element_text(color="black",size = 10, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
axis.text.y = element_text(color="black",size = 10, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
panel.border = element_rect(colour = "black", fill=NA, size=0.5))
############################
isofor<-function(x){
model=iForest(x[-c(1)],nt=100)
iso.score=predict(model,x[-c(1)])
x$process_score=iso.score
x$category=ifelse(x$process_score>=0.60,"Outlier","Normal")
return(x)
}
############




