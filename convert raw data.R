rm(list=ls(all=TRUE))

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)

options(stringsAsFactors = FALSE,
        scipen = 10) 

f.2014 <- read_csv("raw/f_5500_2014_latest.csv")
f.2014.sb <- read_csv("raw/F_SCH_SB_2014_latest.csv")

#All AMTs
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

f.2014.sb <- read_csv("raw/F_SCH_SB_2014_latest.csv",
                      col_types = paste0(as.character(l$type.read), collapse = ''))

##This should work for all datasets:
#fails for multi class
#col.names <- f.2014 %>% map_chr(class)
col.names <- f.2014 %>% map(class) %>% map(1) %>% flatten_chr
col.names %>% unique
#convert ints to double
col.letters <- c('character'='c', 'Date'='D', 'integer'='d', 'numeric'='d', 'POSIXct'='T')[col.names]
paste0(col.letters, collapse = '')

f.2014 <- read_csv("raw/f_5500_2014_latest.csv",
                   col_types = paste0(col.letters, collapse = ''))


f.2014.c <- read_csv("raw/F_SCH_C_PART1_ITEM2_2014_latest.csv")
f.2014.codes <- read_csv("raw/F_SCH_C_PART1_ITEM2_CODES_2014_latest.csv")

col.names <- f.2014.c %>% map(class) %>% map(1) %>% flatten_chr
col.names %>% unique
#convert ints to double
col.letters <- c('character'='c', 'Date'='D', 'integer'='d', 'numeric'='d', 'POSIXct'='T')[col.names]
paste0(col.letters, collapse = '')

f.2014.c <- read_csv("raw/F_SCH_C_PART1_ITEM2_2014_latest.csv",
                     col_types = paste0(col.letters, collapse = ''))

#save compressed
file <- paste0("data/F_SCH_SB_2014_latest", ".csv.bz2")
write.csv(f.2014.sb, bzfile(file), row.names=FALSE)

file <- paste0("data/f_5500_2014_latest", ".csv.bz2")
write.csv(f.2014, bzfile(file), row.names=FALSE)

file <- paste0("data/F_SCH_C_PART1_ITEM2_2014_latest", ".csv.bz2")
write.csv(f.2014.c, bzfile(file), row.names=FALSE)

file <- paste0("data/F_SCH_C_PART1_ITEM2_CODES_2014_latest", ".csv.bz2")
write.csv(f.2014.codes, bzfile(file), row.names=FALSE)


