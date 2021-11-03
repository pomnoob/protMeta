library(meta)# for meta analysis
library(tidyverse)
library(reshape2)# for data transformation
source("function/comb_mean.R")#function to combine mean value
source("function/comb_sd.R")# function to combine sd value

# Weight gain meta --------------------------------------------------------

protMeta <- read.csv("data/if-1022-rqq.csv")

# data of weight gain
wg.oa <- protMeta %>% 
  mutate(treat=case_when(pro=="hm"~"hm",TRUE~"fm")) %>% # 把所有配方粉的组合并为一个标签
  select(id,author,year,treat,n,age,wg,wgsd) %>% 
  mutate(label=paste(author," (",year," )")) %>% 
  filter(age != 0)

wgc.oa <- wg.oa %>% 
  group_by(label,age,treat) %>% 
  summarize(cmean=comb_mean(wg,wgsd),
            cn=sum(n),
            csd=comb_sd(wg,n,wgsd)) %>% 
  filter(!is.na(cmean))

# Tidy-up data structure for meta package

wgc.m <- melt(wgc.oa,id=c("label","age","treat"))
wgc.d <- dcast(wgc.m,label+age~variable+treat)

# delete study with missing value
wgc.d <- wgc.d %>% 
  filter(!is.na(cmean_hm))

# since month 5 has only 1 study, combine it with month 6
wgc.d$age[wgc.d$age==6] <- 5

wgc.oa.meta <- metacont(cn_fm,cmean_fm,csd_fm,
                cn_hm,cmean_hm,csd_hm,
                studlab = label,
                subgroup = age,
                data = wgc.d,
                tau.common=F)

print(summary(wgc.oa.meta), digits=2)

forest(wgc.oa.meta,overall = T)

mon.r <- metareg(wgc.oa.meta, age)

print(mon.r, digits=2)

# Subgroup plot -----------------------------------------------------------

sub.wg <- data.frame(age=c("Month 1","Month 2","Month 3","Month 4","Month 5 and 6"),
                     md=c(0.27,0.98,0.77,1.92,2.23),
                     cil=c(-1.46,-0.30,-0.52,0.90,0.53),
                     ciu=c(2.01,2.25,2.06,2.95,3.93),
                     ord=c(1:5))

ggplot(data = sub.wg)+
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
  annotate("text",x=4,y=-1,parse=T,label="'estimate = 0.50 [0.02;0.98], p = 0.04,'*I^2 == '90.8%'",size=8)+
  annotate("text",x=1,y=4,label="n.s.",size=8)+
  annotate("text",x=2,y=4,label="n.s.",size=8)+
  annotate("text",x=3,y=4,label="n.s.",size=8)+
  annotate("text",x=4,y=4,label="*",size=8)+
  annotate("text",x=5,y=4,label="*",size=8)+
  xlab("Infant age")+
  ylab("Mean difference of Weight gain (gram/day)")
  

# Subgroup analysis for protein content -----------------------------------

protMeta.m <- read.csv("data/if-1022-rqq - rename hm.csv")
protMeta.m <- protMeta.m %>% 
  mutate(pro=case_when(pro>2.2~10,TRUE~pro))

sub.pro <- protMeta.m %>% 
  mutate(treat=case_when(pro==0~"hm",TRUE~"fm")) %>% # 把所有配方粉的组合并为一个标签
  select(id,author,year,treat,pro,cho,n,age,wg,wgsd) %>% 
  mutate(label=paste(author," (",year," )")) %>% 
  filter(age != 0)

sub.pro.c <- sub.pro %>% 
  group_by(label,age,pro,treat) %>% 
  summarize(cmean=comb_mean(wg,wgsd),
            cn=sum(n),
            csd=comb_sd(wg,n,wgsd)) %>% 
  filter(!is.na(cmean))


# cast data to wide format
sub.m <- melt(sub.pro.c,id=c("label","age","treat","pro"))
wgc.d.rev <- dcast(sub.m,label+age+pro~variable+treat)
#Meli(2014)数据有些问题，先删掉
wgc.d.rev <- slice(wgc.d.rev,-39)
wgc.d.rev[is.na(wgc.d.rev)] <- 0

prot.sub <- wgc.d.rev %>% 
  slice(106)

test <- wgc.d.rev %>% 
  group_by(label,age) %>% 
  group_rows()

for (i in 1:length(test)) {
  
  cc <- dplyr::slice(wgc.d.rev,test[[i]])
  
  for (j in which(cc$pro!=0)) {
    
    cc[j,4:9] <- cc[j,4:9]+cc[which(cc$pro==0),4:9]
    
  }
  
  cc <- dplyr::slice(cc,-which(cc$pro==0))
  
  prot.sub <- rbind(prot.sub,cc)
  
}

write.csv(prot.sub,file = "data/调整后的数据-lkf.csv",row.names = F)

#删掉pro=1.4的研究
#pro=10改成2.5

prot.sub2 <- prot.sub %>% 
  filter(pro!=1.4) %>% 
  mutate(pro=case_when(pro==10~2.5,TRUE~pro))

revised.m4 <- prot.sub2 %>% 
  filter(age==4)

rem4 <- metacont(cn_fm,cmean_fm,csd_fm,
                 cn_hm,cmean_hm,csd_hm,
                 subgroup = pro,
                 data = revised.m4,
                 tau.common=F)

print(summary(rem4), digits=2)

rem4.r <- metareg(rem4, pro)

print(rem4.r, digits=2) 

forest(rem4,overall = T)

# plot for protein groups
sub.pro <- data.frame(pro=c("1.7","1.8","1.9","2.0","2.1","2.2","over 2.2"),
                     md=c(2.35,2.03,2.37,1.26,2.34,2.60,3.07),
                     cil=c(1.53,1.39,1.32,-1.58,1.64,2.03,1.75),
                     ciu=c(3.18,2.68,3.43,4.10,3.04,3.17,4.39),
                     ord=c(1:7))

ggplot(data = sub.pro)+
  geom_point(aes(x=pro,y=md))+
  geom_line(aes(x=pro,y=md,group=1),size=1)+
  geom_line(aes(x=pro,y=cil,group=1), color="gray",alpha=0.5)+
  geom_line(aes(x=pro,y=ciu,group=1), color="gray",alpha=0.5)+
  geom_ribbon(aes(x=pro,ymax=ciu,ymin=cil,group=1),fill="gray", alpha=.3)+
  geom_hline(yintercept = 0)+
  theme_minimal()+theme(
    panel.border = element_rect(color = "black",fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black",size = 15),
    axis.text.y = element_text(color = "black",size = 15),
    axis.title = element_text(color = "black",size = 20),
    
  )+
  annotate("text",x=1,y=4.5,label="*",size=8)+
  annotate("text",x=2,y=4.5,label="*",size=8)+
  annotate("text",x=3,y=4.5,label="*",size=8)+
  annotate("text",x=4,y=4.5,label="n.s.",size=8)+
  annotate("text",x=5,y=4.5,label="*",size=8)+
  annotate("text",x=6,y=4.5,label="*",size=8)+
  annotate("text",x=7,y=4.5,label="*",size=8)+
  
  xlab("Proten Content (gram/100kcal)")+
  ylab("Mean difference of Weight gain (gram/day)")

# Subgroup analysis for CHO content -----------------------------------

protMeta.m <- read.csv("data/if-1022-rqq - rename hm.csv")
protMeta.m <- protMeta.m %>% 
  mutate(pro=case_when(pro>2.2~10,TRUE~pro))

sub.cho <- protMeta.m %>% 
  mutate(treat=case_when(pro==0~"hm",TRUE~"fm")) %>% # 把所有配方粉的组合并为一个标签
  select(id,author,year,treat,pro,cho,n,age,wg,wgsd) %>% 
  mutate(label=paste(author," (",year," )")) %>% 
  filter(age != 0)

sub.cho.c <- sub.cho %>% 
  group_by(label,age,cho,treat) %>% 
  summarize(cmean=comb_mean(wg,wgsd),
            cn=sum(n),
            csd=comb_sd(wg,n,wgsd)) %>% 
  filter(!is.na(cmean))


# cast data to wide format
sub.cho.m <- melt(sub.cho.c,id=c("label","age","treat","cho"))
wgc.dcho.rev <- dcast(sub.cho.m,label+age+cho~variable+treat)
# 删掉没有CHO数据的研究

wgc.dcho.rev <- filter(wgc.dcho.rev, !is.na(cho))

# 无数据的研究用0代替
wgc.dcho.rev[is.na(wgc.dcho.rev)] <- 0

cho.sub <- wgc.dcho.rev %>% 
  slice(71)

test1 <- wgc.dcho.rev %>% 
  group_by(label,age) %>% 
  group_rows()

for (i in 1:length(test1)) {
  
  cc <- dplyr::slice(wgc.dcho.rev,test1[[i]])
  
  for (j in which(cc$cho!=0)) {
    
    cc[j,4:9] <- cc[j,4:9]+cc[which(cc$cho==0),4:9]
    
  }
  
  cc <- dplyr::slice(cc,-which(cc$cho==0))
  
  cho.sub <- rbind(cho.sub,cc)
  
}

write.csv(prot.sub,file = "data/调整后的数据-lkf.csv",row.names = F)

#删掉pro=1.4的研究
#pro=10改成2.5


cho.m4 <- cho.sub %>% 
  filter(age==4)

rem4.cho <- metacont(cn_fm,cmean_fm,csd_fm,
                 cn_hm,cmean_hm,csd_hm,
                 subgroup = cho,
                 data = cho.m4,
                 tau.common=F)
re.cho <- metacont(cn_fm,cmean_fm,csd_fm,
                  cn_hm,cmean_hm,csd_hm,
                  subgroup = cho,
                  data = cho.sub,
                  tau.common=F)

print(summary(rem4.cho), digits=2)
print(summary(re.cho), digits=2)

re.cho.r <- metareg(re.cho, cho+age)

print(re.cho.r, digits=2) 

forest(rem4,overall = T)

# Subgroup analysis for CHO group -----------------------------------

protMeta.m <- read.csv("data/if-1022-rqq - rename hm.csv")
protMeta.m <- protMeta.m %>% 
  mutate(pro=case_when(pro>2.2~10,TRUE~pro))

sub.chog <- protMeta.m %>% 
  mutate(chog=case_when(cho<=10.4&cho!=0~1,
                        cho>=11.2~3,
                        cho>=10.7 & cho <=11.1~2,
                        cho==0~cho),
         treat=case_when(pro==0~"hm",TRUE~"fm")) %>% # 把所有配方粉的组合并为一个标签
  select(id,author,year,treat,chog,n,age,wg,wgsd) %>% 
  mutate(label=paste(author," (",year," )")) %>% 
  filter(age != 0)

sub.chog.c <- sub.chog %>% 
  filter(!is.na(chog)) %>% 
  group_by(label,age,chog,treat) %>% 
  summarize(cmean=comb_mean(wg,wgsd),
            cn=sum(n),
            csd=comb_sd(wg,n,wgsd)) %>% 
  filter(!is.na(cmean))


# cast data to wide format
sub.chog.m <- melt(sub.chog.c,id=c("label","age","treat","chog"))
wgc.dchog.rev <- dcast(sub.chog.m,label+age+chog~variable+treat)
# 删掉没有CHO数据的研究

wgc.dcho.rev <- filter(wgc.dcho.rev, !is.na(cho))

# 无数据的研究用0代替
wgc.dchog.rev[is.na(wgc.dchog.rev)] <- 0

chog.sub <- wgc.dchog.rev %>% 
  slice(71)

testg <- wgc.dchog.rev %>% 
  group_by(label,age) %>% 
  group_rows()

for (i in 1:length(testg)) {
  
  cc <- dplyr::slice(wgc.dchog.rev,testg[[i]])
  
  for (j in which(cc$chog!=0)) {
    
    cc[j,4:9] <- cc[j,4:9]+cc[which(cc$chog==0),4:9]
    
  }
  
  cc <- dplyr::slice(cc,-which(cc$chog==0))
  
  chog.sub <- rbind(chog.sub,cc)
  
}

write.csv(prot.sub,file = "data/调整后的数据-lkf.csv",row.names = F)

#删掉pro=1.4的研究
#pro=10改成2.5


chog.m4 <- chog.sub %>% 
  filter(age==4)

rem4.chog <- metacont(cn_fm,cmean_fm,csd_fm,
                     cn_hm,cmean_hm,csd_hm,
                     subgroup = chog,
                     data = chog.m4,
                     tau.common=F)

re.chog <- metacont(cn_fm,cmean_fm,csd_fm,
                   cn_hm,cmean_hm,csd_hm,
                   subgroup = chog,
                   data = chog.sub,
                   tau.common=F)

print(summary(rem4.chog), digits=2)
print(summary(re.chog), digits=2)


