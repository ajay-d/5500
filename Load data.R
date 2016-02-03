rm(list=ls(all=TRUE))

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)

options(stringsAsFactors = FALSE,
        scipen = 10) 

f.2014 <- read_csv("data/f_5500_2014_latest.csv.bz2",
                   col_types = "cDDdcdddddddddcccDccccccccccccccccccccccccccddccccccccccccccdcccTcTcTcddddddddddccddddddddddddddddddcDcccdddccccccccccccccdcdddd")
f.2014.sb <- read_csv("data/F_SCH_SB_2014_latest.csv.bz2",
                      col_types = "cDDccddDddddddcddddddddddDcccccccccccccdcddddddddddddddddddddddddddddddddddddddddddddcddddddddDdddddddcdddcddddddddddddd")
f.2014.c <- read_csv("data/F_SCH_C_PART1_ITEM2_2014_latest.csv.bz2",
                     col_types = "cdcccccccccccccccddddd")
f.2014.codes <- read_csv("data/F_SCH_C_PART1_ITEM2_CODES_2014_latest.csv.bz2")

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

codes <- f.2014$TYPE_WELFARE_BNFT_CODE[1:200]
#codes <- str_replace_na(codes)
codes.l <- str_extract_all(codes, '[A-Z0-9]{2}')
codes.len <- codes.l %>% map(length) %>% flatten_int()
codes.l <- codes.l %>% map(paste0, collapse='.') %>% flatten_chr()
max(codes.len)
quantile(codes.len, probs=c(.75,.9,.95,.99))
sum(codes.len %>% map_lgl(~.>5))
sum(codes.len %>% map_lgl(~.>10))
sum(codes.len %>% map_lgl(~.>6))

codes.l <- str_extract_all(codes, '[A-Z0-9]{2}')

codes[43]
codes[58]
codes[75]

codes.l[43]
codes.l[58]
codes.l[75]

codes.3[43]
codes.3[58]
codes.3[75]

codes.3 <- str_extract_all(codes, '[A-Z0-9]{2}') %>% map_if(~length(.)>=4, 4)
codes.3 <- str_extract_all(codes, '[A-Z0-9]{2}') %>% map_int(length)
log.vec <- str_extract_all(codes, '[A-Z0-9]{2}') %>% map_lgl(~length(.)>=4)

str_extract_all(codes[log.vec], '[A-Z0-9]{2}') %>% map_if(~length(.)>=4, 4)
str_extract_all(codes[log.vec], '[A-Z0-9]{2}') %>% map_chr(4)
ind <- which(log.vec, arr.ind=TRUE)
codes[ind]
codes.3 <- rep(NA, length(codes.l))
codes.3[log.vec] <- str_extract_all(codes[log.vec], '[A-Z0-9]{2}') %>% map_chr(4)


codes.3 <- str_extract_all(codes, '[A-Z0-9]{2}') %>% map_if(~length(.)>=4, 4)

codes.1 <- str_extract(codes, '[A-Z0-9]{2}')
#codes.2 <- str_extract_all(codes, '[A-Z0-9]{2}') %>% map_if(~length(.)>=2, 2) %>% flatten_chr()
for (i in 2:7) {
  
  var.name <- paste0('codes.', i)
  tmp <- rep(NA, length(codes.1))
  #assign(var.name, rep(NA, length(codes.1)))
  
  log.vec <- str_extract_all(codes, '[A-Z0-9]{2}') %>% map_lgl(~length(.)>=i)
  elements <- str_extract_all(codes[log.vec], '[A-Z0-9]{2}') %>% map_chr(i)
  tmp[log.vec] <- elements
  assign(var.name, tmp)
}


