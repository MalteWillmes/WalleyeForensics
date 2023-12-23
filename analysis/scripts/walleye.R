# Load libraries ----------------------------------------------------------
library(tidyverse)
library(viridis)
library(strucchange)

#Clear the workspace
rm(list = ls())

# Load data ---------------------------------------------------------------
walleye_readin<- read.csv("data/wem_sr87sr86_data_20230501.csv")


# Clean and organize data -------------------------------------------------
walleye<- walleye_readin %>%
  filter(str_detect(Sample_ID, "WEM"))%>%
  mutate(Sample=case_when(Petro_no == "WEM1" ~"Walleye",
                          Petro_no == "WEM2" ~"Yellow Perch 1",
                          Petro_no == "WEM3" ~"Yellow Perch 2",
                          Petro_no == "WEM4" ~"Yellow Perch 3"))%>%
  mutate(Species=case_when(Petro_no == "WEM1" ~"Walleye",
                            Petro_no == "WEM2" ~"Yellow Perch",
                            Petro_no == "WEM3" ~"Yellow Perch",
                            Petro_no == "WEM4" ~"Yellow Perch"))%>%
  mutate(Habitat=case_when(Species=="Yellow Perch" ~"Lake Cascade",
                           Species=="Walleye" & Distance_um <990 ~"Unknown",
                           Species=="Walleye" & Distance_um >990 & Distance_um <1100 ~"Transition",
                           Species=="Walleye" & Distance_um >1100 ~"Lake Cascade"
                           ))%>%
  mutate(Age_f=factor(Age))%>%
  group_by(Sample, Habitat)%>%
  mutate(sample_mean=round(mean(Sr8786_norm, na.rm=T),5),
         sample_2sd=round(2*sd(Sr8786_norm, na.rm=T),5),
         n_spots=n())%>%
  ungroup()

#Summary dataframe
walleye_summary <- walleye %>%
  distinct(Sample, Species, Habitat, sample_mean,sample_2sd, n_spots)
write.csv(walleye_summary, "outputs/walleye_summary.csv", row.names=F)

#Breakpoint analysis
breakpoint_data<-walleye%>%
  data.frame()%>%
  filter(Petro_no=="WEM1")%>%
  dplyr::select(Sr8786_norm)%>%
  as.ts()

walleye_break <- breakpoints(breakpoint_data ~ 1)
walleye_break
plot(walleye_break)
summary(walleye_break)

ci_walleye_break <- confint(walleye_break, breaks = 1)
plot(breakpoint_data)
lines(ci_walleye_break)


#Walleye and Perch lifetime profiles
p_walleye <-ggplot()+
  geom_hline(yintercept = 0.70800, color="black", linetype="dashed")+
  geom_hline(yintercept = 0.70842, color="black", linetype="dashed")+
  geom_segment(aes(x = 0 , y = 0.7087789, xend = 1000, yend = 0.7087789), color="blue", linetype="dashed") +
  geom_line(data=walleye%>%filter(Species=="Yellow Perch"), aes(x=Distance_um, y=Sr8786_norm, group=Sample), 
            color="grey",alpha=.5)+
  geom_point(data=walleye%>%filter(Species=="Yellow Perch"), aes(x=Distance_um, y=Sr8786_norm, group=Sample, shape=Habitat), 
              fill="grey", color="black", alpha=.5)+
  geom_line(data=walleye%>%filter(Species=="Walleye"), aes(x=Distance_um, y=Sr8786_norm, color=Age_f, group=Sample_ID), 
            linewidth=1)+
  geom_point(data=walleye%>%filter(Species=="Walleye"), aes(x=Distance_um, y=Sr8786_norm, fill=Age_f, shape=Habitat), 
             size=2.5)+
  scale_x_continuous(expression("Distance from Core ["*mu*"m]"),limits = c(0, 2550))+
  scale_y_continuous(name=expression(""^"87"*"Sr/"^"86"*"Sr"), breaks = seq(0.707, 0.710, 0.0002))+
  scale_color_viridis_d("Age")+
  scale_fill_viridis_d("Age")+
  scale_shape_manual("Habitat", values=c(21, 23, 22))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p_walleye
ggsave("outputs/sr_walleye.png", p_walleye,  width = 7, height = 4, dpi=600)


#T-tests , one sample two sided
walleye_early <- walleye %>%filter(Habitat=="Unknown")%>%
  dplyr::select(Sr8786_norm)
walleye_late<- walleye %>%filter(Species=="Walleye" & Habitat=="Lake Cascade")%>%
  dplyr::select(Sr8786_norm)
lake_cascade <- walleye_summary %>%filter(Species=="Yellow Perch")%>%
  dplyr::select(sample_mean)
mean(lake_cascade$sample_mean)

res_early<- t.test(walleye_early, mu =mean(lake_cascade$sample_mean), alternative = "two.sided")
res_early

res_late<- t.test(walleye_late, mu =mean(lake_cascade$sample_mean), alternative = "two.sided")
res_late
