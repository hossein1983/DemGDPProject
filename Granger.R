###########################################################################################################################################
### Granger Tests
###########################################################################################################################################

library(Hmisc)
library(tidyverse)
library(lmtest)
library(xlsx)
library(WDI)



###########################################################################################################################################

# Load country standard names for matching
standard_names <- read.xlsx2('D:/Dropbox/_Classes/Data/Country Lists.xlsx',sheetIndex=1,stringsAsFactors=F) %>%
  as.tibble()

###########################################################################################################################################

# Load UDS data
data_uds <- read_csv('D:/Dropbox/_Classes/Data/UDS/uds_summary.csv')

# load WDI indicators
wdi_vars <- read.xlsx2('D:/Dropbox/_Classes/Data/World Bank Data/Variables.xlsx',sheetIndex = 1) %>%
  as.tibble() %>% mutate_all('as.character')

# GDP data - GDP per capita (current US$)
data_gdp <- WDI(country = 'all',indicator = 'NY.GDP.PCAP.CD',start = 1950,end=2018) %>%
  as.tibble()

# merge data
data_all <- data_uds %>%
  filter(year>=1960) %>%
  left_join(standard_names %>% select(uds,standard),by=c('country'='uds')) %>%
  left_join(data_gdp %>% 
              left_join(standard_names %>% select(wb,standard),by=c('country'='wb')),
            by=c('standard','year')) %>%
  mutate(country=standard,
         Democracy=mean,
         GDPperCapita=NY.GDP.PCAP.CD) %>%
  mutate(GDPperCapita_s=stats::filter(as.ts(GDPperCapita,start=min(year)),method='convolution',rep(1,5))) %>%
  select(country,year,Democracy,GDPperCapita,GDPperCapita_s)
  
  
  
###########################################################################################################################################

data_all %>% filter(country=='South Korea') %>% print(n=Inf)


results <- expand.grid(country=unique(data_all$country),
                       start=1960:2007,
                       end=1965:2012,
                       order=5,
                       grangerP_Dem=NA,
                       grangerP_GDP=NA) %>% as.tibble()

for (i in 1:nrow(results)){
  print(i)
  results$grangerP_Dem[i] <- try(grangertest(Democracy~GDPperCapita,
                                             data=data_all %>% 
                                               select(-GDPperCapita_s) %>%
                                               filter(country==results$country[i],
                                                      year>=results$start[i],
                                                      year <= results$end[i]),
                                             order=results$order[i])[[4]][2],silent=T)
  
  results$grangerP_GDP[i] <- try(grangertest(GDPperCapita~Democracy,
                                             data_all %>% 
                                               select(-GDPperCapita_s) %>%
                                               filter(country==results$country[i],
                                                      year>=results$start[i],
                                                      year <= results$end[i]),
                                                  order=results$order[i])[[4]][2],silent=T)
}


results_filt <- results %>%
  mutate(grangerP_Dem=as.numeric(grangerP_Dem),
         grangerP_GDP=as.numeric(grangerP_GDP)) %>%
  filter(grangerP_Dem < .05 | grangerP_GDP<.05) %>%
  left_join(data_all %>% select(-GDPperCapita_s) %>% 
              rename(Democracy_start=Democracy,
                     GDPperCapita_start=GDPperCapita,
                     start=year),by=c('country','start')) %>%
  left_join(data_all %>% select(-GDPperCapita_s) %>% 
              rename(Democracy_end=Democracy,
                     GDPperCapita_end=GDPperCapita,
                     end=year),by=c('country','end')) %>%
  mutate(Democracy_change = Democracy_end-Democracy_start,
         GDPperCapita_change = GDPperCapita_end-GDPperCapita_start) %>%
  select(-Democracy_end,-Democracy_start,-GDPperCapita_end,-GDPperCapita_start) %>%
  mutate(len=end-start) %>% 
  filter(len<=20) %>%
  arrange(country,start)
  

results_filt %>%
  left_join(results_filt %>%
  select(country,start,end,grangerP_Dem) %>%
  group_by(country) %>%
  summarize(min_p = min(grangerP_Dem)),by='country') %>%
  filter(grangerP_Dem<.05,
         grangerP_Dem==min_p,
         !is.na(GDPperCapita_change),
         !is.na(Democracy_change)) %>%
  write.xlsx2('GDP_to_Democracy.xlsx')

