library(meta)# for meta analysis
library(tidyverse)
library(reshape2)# for data transformation
source("function/comb_mean.R")#function to combine mean value
source("function/comb_sd.R")# function to combine sd value

# Height gain meta --------------------------------------------------------

protMeta <- read.csv("data/if-1022-rqq.csv")

# data of weight gain
bmi.oa <- protMeta %>% 
  mutate(treat=case_when(pro=="hm"~"hm",TRUE~"fm")) %>% # 把所有配方粉的组合并为一个标签
  select(id,author,year,treat,n,age,bmi,bmisd) %>% 
  mutate(label=paste(author," (",year," )")) %>% 
  filter(age != 0)

bmic.oa <- bmi.oa %>% 
  group_by(label,age,treat) %>% 
  summarize(cmean=comb_mean(bmi,bmisd),
            cn=sum(n),
            csd=comb_sd(bmi,n,bmisd)) %>% 
  filter(!is.na(cmean))

# Tidy-up data structure for meta package

bmic.m <- melt(bmic.oa,id=c("label","age","treat"))
bmic.d <- dcast(bmic.m,label+age~variable+treat)

# delete study with missing value
bmic.d <- bmic.d %>% 
  filter(!is.na(cmean_hm))

# since month 5 has only 1 study, combine it with month 6
bmic.d$age[bmic.d$age==6] <- 5

bmic.oa.meta <- metacont(cn_fm,cmean_fm,csd_fm,
                        cn_hm,cmean_hm,csd_hm,
                        studlab = label,
                        subgroup = age,
                        data = bmic.d,
                        tau.common=F)

print(summary(bmic.oa.meta), digits=2)

monb.r <- metareg(bmic.oa.meta, age)

print(monb.r, digits=2)

# Subgroup plot -----------------------------------------------------------

sub.bmi <- data.frame(age=c("Month 1","Month 2","Month 3","Month 4","Month 5 and 6"),
                     md=c(0.09,0.24,0.11,0.60,0.36),
                     cil=c(-0.21,-0.01,-0.25,0.38,0.09),
                     ciu=c(0.40,0.49,0.48,0.81,0.63),
                     ord=c(1:5))

ggplot(data = sub.bmi)+
  geom_point(aes(x=age,y=md))+
  geom_line(aes(x=age,y=md,group=1),size=1)+
  geom_line(aes(x=age,y=cil,group=1), color="gray",alpha=0.5)+
  geom_line(aes(x=age,y=ciu,group=1), color="gray",alpha=0.5)+
  geom_ribbon(aes(x=age,ymax=ciu,ymin=cil,group=1),fill="gray", alpha=.3)+
  geom_hline(yintercept = 0)+
  theme_minimal()+theme(
    panel.border = element_rect(color = "black",fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black",size = 15),
    axis.text.y = element_text(color = "black",size = 15),
    axis.title = element_text(color = "black",size = 20),
    
  )+
  annotate("text",x=2,y=0.8,parse=T,label="'estimate = 0.10 [0.01;0.18], p = 0.03,'*I^2 == '8.27%'",size=8)+
  # annotate("text",x=1,y=0.3,label="n.s.",size=8)+
  # annotate("text",x=2,y=0.3,label="n.s.",size=8)+
  # annotate("text",x=3,y=0.3,label="n.s.",size=8)+
  # annotate("text",x=4,y=0.3,label="n.s",size=8)+
  annotate("text",x=4,y=1,label="*",size=8)+
  annotate("text",x=5,y=1,label="*",size=8)+
  xlab("Infant age")+
  ylab(expression(paste("Mean difference of BMI (kg/",m^2,")")))
  # ylab("Mean difference of BMI (kg/'*m^2')")