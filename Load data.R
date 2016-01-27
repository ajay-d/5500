rm(list=ls(all=TRUE))

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)

options(stringsAsFactors = FALSE,
        scipen = 10) 

f.2014 <- read_csv("data/f_5500_2014_latest.csv")
f.2014.sb <- read_csv("data/F_SCH_SB_2014_latest.csv")

problems(f.2014.sb) %>% count(col)

names(f.2014.sb)

l1 <- f.2014.sb %>% map_chr(class)
l2 <- f.2014.sb %>% map(class)
l <- lapply(f.2014.sb, class)

identical(l, l2)

as.data.frame(l1)

str(l)
str(l1)

c <- unlist(l)
c <- flatten(l)
c <- flatten_chr(l)

l <- lapply(f.2014.sb, class)
l <- as.data.frame(l) %>%
  gather(var, type)

l %>% count(type)

l <- l %>%
  mutate(type.read = c('character'='c', 'Date'='D', 'integer'='d', 'numeric'='d')[type])

paste0(as.character(l$type.read), collapse = '')

f.2014.sb <- read_csv("data/F_SCH_SB_2014_latest.csv",
                      col_types = paste0(as.character(l$type.read), collapse = ''))

f.2014.c <- read_csv("data/F_SCH_C_PART1_ITEM2_2014_latest.csv")
f.2014.codes <- read_csv("data/F_SCH_C_PART1_ITEM2_CODES_2014_latest.csv")

f.2014.c %>% filter(row_number()==26449)

f.2014 %>%
  count(ACK_ID) %>%
  filter(n > 1)

f.2014 %>% count(TYPE_PLAN_ENTITY_CD)
f.2014 %>% count(SCH_R_ATTACHED_IND, SCH_MB_ATTACHED_IND, SCH_SB_ATTACHED_IND)
f.2014 %>% count(SCH_C_ATTACHED_IND) #Service provider info

sort(names(f.2014))

f.2014.filtered <- f.2014 %>%
  select(ACK_ID, TYPE_PLAN_ENTITY_CD, PLAN_NAME, SPONSOR_DFE_NAME, SPONS_DFE_EIN,
         SPONS_DFE_MAIL_US_STATE, 
         TOT_PARTCP_BOY_CNT, TOT_ACTIVE_PARTCP_CNT, TOT_ACT_PARTCP_BOY_CNT,
         TOT_ACT_RTD_SEP_BENEF_CNT, #end of year
         TYPE_PENSION_BNFT_CODE, TYPE_WELFARE_BNFT_CODE,
         SCH_R_ATTACHED_IND, SCH_MB_ATTACHED_IND, SCH_SB_ATTACHED_IND)

f.2014 %>% count(TYPE_WELFARE_BNFT_CODE, sort=TRUE)
f.2014 %>% count(TYPE_PENSION_BNFT_CODE, sort=TRUE)

f.2014.sb %>% 
  count(ACK_ID) %>%
  filter(n > 1)

sort(names(f.2014.sb))

f.2014.sb.filtered <- f.2014.sb %>%
  select(ACK_ID, SB_EIN, SB_PLAN_TYPE_CODE,
         CURR_VALUE=SB_CURR_VALUE_AST_01_AMT, ACTRL_VALUE=SB_ACTRL_VALUE_AST_AMT,
         SB_RTD_PARTCP_CNT, SB_RTD_FNDNG_TGT_AMT, SB_ACT_PARTCP_CNT, SB_ACT_NONVSTD_FNDNG_TGT_AMT,
         SB_TOT_PARTCP_CNT, SB_TOT_FNDNG_TGT_AMT, #beginning year?
         SB_PLAN_AT_RISK_IND,
         TGT_NRML_COST=SB_TGT_NRML_COST_01_AMT)

f.2014.combined <- f.2014.filtered %>%
  inner_join(f.2014.sb.filtered, by = 'ACK_ID')

f.2014.combined %>%
  filter(TOT_PARTCP_BOY_CNT != SB_TOT_PARTCP_CNT)


f.2014.combined %>% filter(SPONS_DFE_EIN==362167060)

f.2014.filtered %>% filter(str_detect(SPONSOR_DFE_NAME, "GILA RIVER"))
t1 <- f.2014.filtered %>% filter(str_detect(SPONSOR_DFE_NAME, "GILA RIVER"))

a1 <- f.2014.c %>% filter(ACK_ID=='20150928141914P030016240333001')
a2 <- f.2014.codes %>% filter(ACK_ID=='20150928141914P030016240333001')

a1 <- f.2014.c %>% filter(ACK_ID=='20150731101952P040122479287001')
a2 <- f.2014.codes %>% filter(ACK_ID=='20150731101952P040122479287001')
a <- f.2014 %>% filter(ACK_ID=='20150731101952P040122479287001')

f.2014.c %>% count(PROVIDER_OTHER_RELATION, sort=TRUE)

f.2014.combined <- f.2014.combined %>%
  inner_join(f.2014.c %>%
               group_by(ACK_ID) %>%
               summarise(PROVIDER_OTHER_DIRECT_COMP_AMT = sum(PROVIDER_OTHER_DIRECT_COMP_AMT),
                         N_PROVIDERS = n()),
             by='ACK_ID')

f.2014.combined %>%
  filter(PROVIDER_OTHER_DIRECT_COMP_AMT == max(PROVIDER_OTHER_DIRECT_COMP_AMT, na.rm=TRUE))

summary(f.2014.combined$PROVIDER_OTHER_DIRECT_COMP_AMT)
summary(f.2014.combined$SB_TOT_PARTCP_CNT)
summary(f.2014.combined$N_PROVIDERS)

ggplot(f.2014.combined %>% filter(PROVIDER_OTHER_DIRECT_COMP_AMT<1e6)) + geom_histogram(aes(PROVIDER_OTHER_DIRECT_COMP_AMT))
ggplot(f.2014.combined %>% filter(SB_TOT_PARTCP_CNT<30000)) + geom_histogram(aes(SB_TOT_PARTCP_CNT))
ggplot(f.2014.combined) + geom_histogram(aes(N_PROVIDERS))

ggplot(f.2014.combined) +
  geom_point(aes(x=SB_TOT_PARTCP_CNT, y=PROVIDER_OTHER_DIRECT_COMP_AMT)) +
  scale_x_log10() +
  scale_y_log10() + 
  xlab('Total Participant Count') +
  ylab('Total Provider Comp')

ggplot(f.2014.combined) +
  geom_point(aes(x=TOT_ACT_RTD_SEP_BENEF_CNT, y=PROVIDER_OTHER_DIRECT_COMP_AMT)) +
  scale_x_log10() +
  scale_y_log10() + 
  xlab('Total Active Participant Count') +
  ylab('Total Provider Comp')

ggplot(f.2014.combined) +
  geom_point(aes(x=CURR_VALUE, y=PROVIDER_OTHER_DIRECT_COMP_AMT)) +
  scale_x_log10() +
  scale_y_log10() + 
  xlab('Total Market Value') +
  ylab('Total Provider Comp')

ggplot(f.2014.combined) +
  geom_point(aes(x=CURR_VALUE, y=ACTRL_VALUE)) +
  scale_x_log10() +
  scale_y_log10() + 
  xlab('Total Market Value') +
  ylab('Total Actuarial Value')

ggplot(f.2014.combined) +
  geom_point(aes(x=N_PROVIDERS, y=PROVIDER_OTHER_DIRECT_COMP_AMT)) +
  scale_x_log10() +
  scale_y_log10() + 
  xlab('N Providers') +
  ylab('Total Provider Comp')

ggplot(f.2014.combined) +
  geom_point(aes(x=CURR_VALUE, y=PROVIDER_OTHER_DIRECT_COMP_AMT/N_PROVIDERS)) +
  scale_x_log10() +
  scale_y_log10() + 
  xlab('N Providers') +
  ylab('Total Provider Comp')


