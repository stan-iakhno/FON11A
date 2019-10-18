#fecth the file
ft <- read.csv("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/Caroline_fon11a/FT2.csv")

#remove undesirable variables
ft <- ft[ft$variable != "mucus_fibrosis_atrophy",]
ft <- ft[ft$variable !="LP_vacuolisation_oedema",]

ft$variable
#manually reorder factor
ft$variable <- as.character(ft$variable)
ft$variable<- factor(ft$variable, levels=unique(ft$variable))
ft$variable <- factor(ft$variable, levels=c("diagnose",
                                            "LP_eosinophils",
                                            "LP_lymphocytes_&_plasma cells",
                                            "LP_macrophages", 
                                            "LP_neutrophils",
                                            "crypt_abscess",
                                            "intra-epithelial_lymphocytes",
                                            "epithelial_damage"))
# plot and flip the axises
library(ggplot2)
pl01<-ggplot(ft, aes(x=variable, y=Freq, fill=value))+
  geom_bar(stat="identity")+
  facet_grid(.~diet)+
  coord_flip()+
  labs(y = "Frequency", x="Histology analysis")+
  theme_minimal()+
  theme(legend.title = element_text(hjust = 0.5))+
  labs(fill="Histology score", title = "B")
  
        
pl01 
#save the plot on the HDD
ggsave("hist001.png",device = "png", 
       plot = pl01, 
       dpi = "retina", 
       width = 18, 
       height = 7, 
