library(meta)# for meta analysis
library(tidyverse)
library(reshape2)# for data transformation

# Tidy up the data ---------------------------------------------------------

protMeta <- read.csv("data/if.csv")

# Here some subgroups need to be combined
# eg groups of different protein content

source("function/comb_mean.R")#function to combine mean value
source("function/comb_sd.R")# function to combine sd value

# data of weight gain
protMeta.wg <- protMeta %>% 
  mutate(treat=case_when(pro=="hm"~"hm",TRUE~"fm")) %>% # 把所有配方粉的组合并为一个标签
  select(id,treat,n,age,wg,wgsd) %>% 
  filter(age != 0)

protMeta.wgc <- protMeta.wg %>% 
  group_by(id,age,treat) %>% 
  summarize(cmean=comb_mean(wg,wgsd),
            cn=sum(n),
            csd=comb_sd(wg,n,wgsd)) %>% 
  filter(!is.na(cmean))

# cast data to wide format
wgc.m <- melt(protMeta.wgc,id=c("id","age","treat"))
wgc.d <- dcast(wgc.m,id+age~variable+treat)

# need to combine month 5 and 6 as month 5 has only one study
wgc.d$age[wgc.d$age==6] <- 5

# Meta-analysis -----------------------------------------------------------


names(wgc.d)

mon <- metacont(cn_fm,cmean_fm,csd_fm,
                 cn_hm,cmean_hm,csd_hm,
                 subgroup = age,
                data = wgc.d,
                tau.common=T)

print(summary(mon), digits=2)

mon.r <- metareg(mon, age)

print(mon.r, digits=2)


# Revised data with protein content ---------------------------------------
protMeta.rev <- read.csv("data/if-1022-rqq.csv")

protMeta.wg.rev <- protMeta.rev %>% 
  select(id, author, year, pro, n, age, wg, wgsd) %>% 
  mutate(treat=case_when(pro=="hm"~"hm",TRUE~"fm")) %>% 
  filter(age != 0)

protMeta.wgc.rev <- protMeta.wg.rev %>% 
  group_by(id,age,pro,treat) %>% 
  summarize(cmean=comb_mean(wg,wgsd),
            cn=sum(n),
            csd=comb_sd(wg,n,wgsd)) %>% 
  filter(!is.na(cmean))


# cast data to wide format
wgc.m.rev <- melt(protMeta.wgc.rev,id=c("id","age","treat","pro"))
wgc.d.rev <- dcast(wgc.m.rev,id+age+pro~variable+treat)

write.csv(wgc.d.rev,file = "data/revised protein and growth.csv",row.names = F)
# 数据合并后重新导入
wgc.d.revised <- read.csv("data/revised protein and growth-merged.csv")


# Revised data meta analysis ----------------------------------------------

revised.m4 <- wgc.d.revised %>% 
  filter(age==4)

rem4 <- metacont(cn_fm,cmean_fm,csd_fm,
                cn_hm,cmean_hm,csd_hm,
                subgroup = pro,
                data = revised.m4,
                tau.common=F)

print(summary(rem4), digits=2)

rem4.r <- metareg(rem4, pro)

print(rem4.r, digits=2) 
