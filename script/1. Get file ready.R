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
