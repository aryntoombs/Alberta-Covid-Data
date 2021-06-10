library(tidyverse)
library(rvest)
library(rjson)
library(stringr)
library(scales)

setwd("~/projects/Covid")

fill <- tidyr::fill

stats_url <- "https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"

covid_19_ab_html <- read_html(stats_url)

#------------------------------------------------------------------------------------------------------#
# CURRENT DATE
#------------------------------------------------------------------------------------------------------#

cur_date_html <- covid_19_ab_html %>% html_nodes('div#covid-19-in-alberta') %>%
  html_nodes("span#last-updated") %>% html_text()
cur_date <- as.Date(cur_date_html, format="%B %d, %Y")

#------------------------------------------------------------------------------------------------------#
# VARIANTS OF CONCERN
#------------------------------------------------------------------------------------------------------#

voc_html <- covid_19_ab_html %>% html_nodes('div#variants-of-concern')

voc_widget <- voc_html %>% html_nodes('div.plotly') %>% html_attr('id')
voc_json <- voc_html %>% 
  html_nodes(xpath = paste('//script[@data-for="',voc_widget[1],'"]',sep="")) %>%
  html_text()

if(!file.exists(paste("Data/variants/voc-data ",cur_date,".json",sep=""))) {
  write(voc_json, paste("Data/variants/voc-data ",cur_date,".json",sep=""))
}

voc_export_json <- fromJSON(voc_json)

b117_export <- data.frame(DATE = voc_export_json$x$data[[1]]$x,
                          B117 = voc_export_json$x$data[[1]]$y)

SA351_export <- data.frame(DATE = voc_export_json$x$data[[2]]$x,
                           SA351 = voc_export_json$x$data[[2]]$y)

B1617_export <- data.frame(DATE = voc_export_json$x$data[[3]]$x,
                           B1617 = voc_export_json$x$data[[3]]$y)


P1_export <- data.frame(DATE = voc_export_json$x$data[[4]]$x,
                        P1 = voc_export_json$x$data[[4]]$y)


VOC_export <- b117_export %>% 
  full_join(SA351_export, by=c("DATE")) %>%
  full_join(P1_export, by=c("DATE")) %>%
  full_join(B1617_export, by=c("DATE")) %>%
  mutate(TOTAL = B117 + SA351 + P1 + B1617) %>%
  mutate(C_SUM = cumsum(TOTAL))

if(!file.exists(paste("Data/variants/voc-dates ",cur_date,".csv", sep=""))) {
  write.csv(VOC_export, paste("Data/variants/voc-dates ",cur_date,".csv", sep=""),row.names = FALSE)
}

VOC_Ul <- voc_html %>% html_nodes(xpath="//div[@id='summary-1']/ul")

T_Active_Cases <- 0
T_VOC_Stats <- VOC_Ul %>% html_nodes("li") %>% .[[3]] %>% html_nodes("strong") %>% html_text() %>% .[1]
T_VOC_Deaths <- VOC_Ul %>% html_nodes("li") %>% .[[3]] %>% html_nodes("strong") %>% html_text() %>% .[2]
VOC_Spec <- VOC_Ul %>% html_nodes("li") %>% .[[1]] %>% html_nodes("strong") %>% html_text() %>% .[1]

VOC_Perc <- data.frame(DATE=cur_date,
                       T_ACTIVE = 0,
                       VOC_ACTIVE = 0,
                       T_VOC = str_replace(T_VOC_Stats,",",""),
                       VOC_RECOV = 0,
                       VOC_DEATHS = str_replace(T_VOC_Deaths,",",""),
                       VOC_SPEC = str_replace(VOC_Spec,",","")
)

if(!file.exists(paste("Data/variants/voc-stats ",cur_date,".csv", sep=""))) {
  write.csv(VOC_Perc, paste("Data/variants/voc-stats ",cur_date,".csv", sep=""),row.names = FALSE)
}

variants_html_table <- voc_html %>% html_nodes("table") %>% 
  html_table(header=TRUE)
variants_table <- variants_html_table[[1]]
variants_table$Zone <- str_replace(variants_table$Zone,"Alberta","In Alberta")

active_html_table <- voc_html %>% html_nodes("table") %>% 
  html_table(header=TRUE)
active_html_table <- active_html_table[[2]]

variant_date <- cur_date + 1
variant_colnames <- c("Location","B.1.1.7 (UK variant)","B.1.351 (South African variant)","B.1.617 India Variant","P.1 (Brazilian variant)","Total")
colnames(variants_table) <- variant_colnames

if(!file.exists(paste("Data/variants/variant-table ",variant_date,".csv",sep=""))) {
  write.csv(variants_table, paste("Data/variants/variant-table ",variant_date,".csv",sep=""),row.names = FALSE)
}

if(!file.exists(paste("Data/variants/active-variant-t ",variant_date,".csv",sep=""))) {
  write.csv(active_html_table, paste("Data/variants/active-variant-t ",variant_date,".csv",sep=""),row.names = FALSE)
}
