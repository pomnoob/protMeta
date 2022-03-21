library(meta)# for meta analysis
library(tidyverse)
library(reshape2)# for data transformation
source("function/comb_mean.R")#function to combine mean value
source("function/comb_sd.R")# function to combine sd value

# Height gain meta --------------------------------------------------------

protMeta <- read.csv("data/if-1022-rqq.csv")

# data of weight gain
hg.oa <- protMeta %>% 
  mutate(treat=case_when(pro=="hm"~"hm",TRUE~"fm")) %>% # 把所有配方粉的组合并为一个标签
  select(id,author,year,treat,n,age,hg,hgsd) %>% 
  mutate(label=paste(author," (",year," )")) %>% 
  filter(age != 0)

hgc.oa <- hg.oa %>% 
  group_by(label,age,treat) %>% 
  summarize(cmean=comb_mean(hg,hgsd),
            cn=sum(n),
            csd=comb_sd(hg,n,hgsd)) %>% 
  filter(!is.na(cmean))

# Tidy-up data structure for meta package

hgc.m <- melt(hgc.oa,id=c("label","age","treat"))
hgc.d <- dcast(hgc.m,label+age~variable+treat)

# delete study with missing value
hgc.d <- hgc.d %>% 
  filter(!is.na(cmean_hm))

# since month 5 has only 1 study, combine it with month 6
hgc.d$age[hgc.d$age==6] <- 5

hgc.oa.meta <- metacont(cn_fm,cmean_fm,csd_fm,
                        cn_hm,cmean_hm,csd_hm,
                        studlab = label,
                        subgroup = age,
                        data = hgc.d,
                        tau.common=F)

print(summary(hgc.oa.meta), digits=2)

forest(hgc.oa.meta,overall = T)

monh.r <- metareg(hgc.oa.meta, age)

print(monh.r, digits=2)

# Subgroup plot -----------------------------------------------------------

sub.hg <- data.frame(age=c("Month 1","Month 2","Month 3","Month 4"),
                     md=c(0.03,0.02,0.01,0.03),
                     cil=c(-0.09,-0.01,-0.02,0.00),
                     ciu=c(0.15,0.05,0.05,0.05),
                     ord=c(1:4))

ggplot(data = sub.hg)+
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
  annotate("text",x=3,y=0.1,parse=T,label="'estimate = 0.00 [-0.02;0.02], p = 0.98,'*I^2 == '97.0%'",size=8)+
  # annotate("text",x=1,y=0.3,label="n.s.",size=8)+
  # annotate("text",x=2,y=0.3,label="n.s.",size=8)+
  # annotate("text",x=3,y=0.3,label="n.s.",size=8)+
  # annotate("text",x=4,y=0.3,label="n.s",size=8)+

  xlab("Infant age")+
  ylab("Mean difference of Height gain (mm/day)")