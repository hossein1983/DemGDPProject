###########################################################################################################################################
### Explore the effect of education
### Jul. 2018
###########################################################################################################################################

library(tidyverse)
library(WDI)
library(xlsx)
library(plotly)

###########################################################################################################################################

# Load country standard names for matching
standard_names <- read.xlsx2('D:/Dropbox/_Classes/Data/Country Lists.xlsx',sheetIndex=1,stringsAsFactors=F)

###########################################################################################################################################

# Load UDS data
uds_orig <- read_csv('D:/Dropbox/_Classes/Data/UDS/uds_summary.csv')

# Plot data
uds_orig %>% filter(country %in% c('Iran','Korea South','United States','Saudi Arabia')) %>% 
  ggplot(aes(x=year,y=mean,color=country))+geom_line()+geom_point()+
  ggtitle('Average UDS Score by Year')+geom_hline(yintercept = 0,linetype='dashed')

###########################################################################################################################################

# load WDI indicators
wdi_vars <- read.xlsx2('D:/Dropbox/_Classes/Data/World Bank Data/Variables.xlsx',sheetIndex = 1) %>%
  as.tibble() %>% mutate_all('as.character')

# Iran and South Korea data
data_iran <- WDI(country='IRN',indicator=wdi_vars$Indicator.Code,start=1960,end=2018) %>% as.tibble()
data_southKorea <- WDI(country='KOR',indicator=wdi_vars$Indicator.Code,start=1960,end=2018) %>% as.tibble()

###########################################################################################################################################

pdf('Iran_SKorea.pdf')
for (i in 1:nrow(wdi_vars)){
  if (!(wdi_vars$Indicator.Code[i] %in% colnames(data_iran))) next
  if (data_iran[,wdi_vars$Indicator.Code[i]] %>% na.omit() %>% nrow() < 5 ) next
  cat(paste0('Plotting ',wdi_vars$Indicator.Name[i],'\n'))
  print(bind_rows(data_iran,data_southKorea) %>%
          ggplot(aes_string(x='year',y=wdi_vars$Indicator.Code[i],group='country',color='country'))+
          geom_point()+geom_line()+ggtitle(wdi_vars$Indicator.Name[i]))
}
dev.off()





