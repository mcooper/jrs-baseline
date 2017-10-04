setwd('D://Documents and Settings/mcooper/Google Drive/JRS/Lemuridae_Madagascar/')

library(rgbif)
library(dplyr)
library(rvest)
library(ggplot2)
library(lubridate)
library(broom)


nulls.df <- function(...){
  dots <- substitute(list(...))[-1]
  L <- list(...)
  names <- sapply(dots, deparse)
  
  for (l in 1:length(L)){
    if (is.null(L[[l]]) | identical(L[[l]], character(0))| identical(L[[l]], logical(0))){
      L[[l]] <- NA
    }
  }
  
  DF <- as.data.frame(matrix(unlist(L), 1, dimnames = list(NULL, names)))
  DF
}


getPublisherInfo <- function(uuid){
  df <- data.frame()
  for(publishingorgkey in uuid){
    result <- read_html(paste0("http://www.gbif.org/publisher/", publishingorgkey))
    
    publisher <- result %>%
      html_nodes("#information") %>%
      html_nodes("p") %>%
      html_text() %>%
      .[1]
    
    publisherCountry <- result %>%
      html_nodes("#information") %>%
      html_nodes(".right") %>%
      html_nodes('.country') %>%
      html_text()
    
    df <- bind_rows(df, nulls.df(publishingorgkey, publisher, publisherCountry))
  }
  df
}

getDatasetInfo <- function(uuids){
  df <- data.frame()
  for(datasetKey in uuids){
    result <- datasets(uuid=datasetKey)
    
    DatasetName <- result$data$title
    
    datasetRegistrationDate <- result$data$created
    datasetPublicationDate <- result$data$pubDate
    
    
    df <- bind_rows(df, nulls.df(datasetKey, DatasetName,
                                 datasetRegistrationDate,
                                 datasetPublicationDate))
  }
  
  df
}

getTopX <- function(vector, x=5){
  vector <- as.character(vector)
  tab <- table(vector)[rev(order(table(vector)))]
  sel <- names(tab[1:x])
  vector[!vector %in% sel] <- 'Other'
  vector <- factor(vector, levels=c(sel, 'Other'))
  vector
}

getColors <- function(vector, x=5){
  vector <- as.character(vector)
  tab <- table(vector)[rev(order(table(vector)))]
  sel <- names(tab[1:x])
  vector[!vector %in% sel] <- 'Other'
  vector <- factor(vector, levels=c(sel, 'Other'))
  
  cols <- c('#7D2D2E', '#6C832E', '#00727B', '#49113A', '#F6BC33')
  colvect <- rep('#414241', length(vector))
  for (i in 1:x){
    colvect[vector==sel[i]] <- cols[i]
  }
  colvect <- factor(colvect, levels=c(cols[1:x], '#414241'))
  colvect
}

mkTable <- function(vector){
  tab <- table(vector)
  df <- data.frame(names=names(tab), count=as.vector(tab)) %>%
    arrange(desc(count))
  str <- ''
  for (r in 1:nrow(df)){
    str <- paste0(str, df$names[r], '\t', df$count[r], '\n')
  }
  cat(str)
}

taxa <- 'Lemuridae'
rank <- 'family'
country <- 'MG'

out <- name_backbone(name=taxa, rank=rank)

fields <- c('name', 'key', 'family',
            'datasetKey', 'publishingOrgKey', 'publishingCountry',
            'year', "datasetName", "country", 'genus',
            'basisOfRecord')

res <- occ_search(out$familyKey, country=country, fields=fields,
                  limit=200000)

dat <- res$data

write.csv(dat, paste0(taxa, '_Madagascar.csv'), row.names=F)

#publishers <- getPublisherInfo(unique(dat$publishingOrgKey))
#dat <- merge(dat, publishers)

datasets <- getDatasetInfo(unique(dat$datasetKey))
dat <- merge(dat, datasets)

#dat$DatasetName[dat$DatasetName=='List of animalia, fungi and plant species recorded through naturalist observations and research activities in Benin. Data mobilized in the framework of a JRS Biodiversity Foundation project of Benin'] <- 'JRS Biodiversity Foundation project of Benin'

###################
#Datasets over Time
##################

dat$DatasetNameCat <- getTopX(dat$DatasetName)
dat$DatasetNameCol <- getColors(dat$DatasetName)

dat$datasetDate <- ymd_hms(dat$datasetRegistrationDate) %>% as.Date(tz='UTC')

counts <- dat %>%
  group_by(DatasetName, DatasetNameCat, DatasetNameCol, datasetDate) %>%
  summarize(size=n())

range <- seq(floor_date(min(counts$datasetDate, na.rm=T), unit="month"), 
             floor_date(today(), unit="month"), 
             by="1 month")

plotdf <- data.frame()

for(i in 1:nrow(counts)){
  vals <- rep(counts$size[i], length(range))
  vals[range < counts$datasetDate[i]] <- 0
  plotdf <- bind_rows(plotdf, data.frame(range, DatasetName=counts$DatasetName[i], 
                                         DatasetNameCat=counts$DatasetNameCat[i], 
                                         DatasetNameCol=counts$DatasetNameCol[i],
                                         values=vals))
}

plotdf <- merge(plotdf, datasets)

plotdf$DatasetName <- as.factor(plotdf$DatasetName)

dscols <- plotdf[ , c('DatasetName', 'DatasetNameCol')] %>% unique

cols <- as.character(dscols$DatasetNameCol)
names(cols) <- dscols$DatasetName

plotdf <- plotdf %>% 
  group_by(DatasetName) %>%
  summarize(MinRegDate=min(ymd_hms(datasetRegistrationDate) %>% as.Date(tz='UTC'))) %>% 
  merge(plotdf)

plotdf$DatasetName <- factor(plotdf$DatasetName, 
                             levels=plotdf$DatasetName[rev(order(plotdf$MinRegDate))],
                             ordered=T)

ggplot(plotdf, aes(x=range, fill=DatasetName, y=values)) + 
  geom_bar(stat='identity', width=100) + 
  xlab('Date') + 
  ylab('Number of Records Available in GBIF') + 
  theme_bw() + 
  guides(fill=guide_legend(title="Dataset Name")) + 
  scale_fill_manual(values=cols) + 
  theme(legend.position="none")
ggsave('Datasets_time.png')

#####################
#Datasets Pie Chart
#####################
ggplot(dat) + geom_bar(aes(x='', fill=DatasetNameCat), width=1) + coord_polar("y", start=0) + theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +
  xlab('') + ylab('') + 
  guides(fill=guide_legend(title="Dataset Name")) + 
  scale_fill_manual(values=levels(dat$DatasetNameCol)) + 
  theme(legend.position="bottom",legend.direction="vertical")
ggsave('Datasets_pie.png')

mkTable(getTopX(dat$DatasetName, 10))

#######################
#Country of Record
########################
# dat$countryName <- getTopX(dat$country)
# dat$countryColor <- getColors(dat$country)
# 
# ggplot(dat) + geom_bar(aes(x='', fill=countryName), width=1) +
#   coord_polar("y", start=0) + theme_bw() +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid  = element_blank()) +
#   xlab('') + ylab('') +
#   scale_fill_manual(values=levels(dat$countryColor)) +
#   guides(fill=guide_legend(title="Country of Record"))#+
# #  theme(legend.position="bottom",legend.direction="vertical")
# ggsave('Countries_pie.png')
# 
# mkTable(getTopX(dat$country, 15))

######################
#Publishing Org Country
############################

iso <- read.csv('../ISO2.csv')
dat <- merge(dat, iso, all.x=T, all.y=F)

dat$publishingCountryFullName <- getTopX(dat$publishingCountryFull)
dat$publishingCountryFullColor <- getColors(dat$publishingCountryFull)

#dat$publishingCountryFullName <- as.character(dat$publishingCountryFullName)
#dat$publishingCountryFullName[is.na(dat$publishingCountryFullName)] <- 'Unknown'
ggplot(dat) + geom_bar(aes(x='', fill=publishingCountryFullName), width=1) + coord_polar("y", start=0) + theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +
  xlab('') + ylab('') + 
  scale_fill_manual(values=levels(dat$publishingCountryFullColor)) + 
  guides(fill=guide_legend(title="Country of Publisher"))#+
#theme(legend.position="bottom",legend.direction="vertical")
ggsave('PublishingCountries_pie.png')

mkTable(as.character(dat$publishingCountryFull))

###########
#Genera
#############
#dat$Genus <- getTopX(dat$genus, 40)

dat$genus <- as.character(dat$genus)
dat$genus[is.na(dat$genus)] <- 'Unknown'

ggplot(dat, aes(genus)) + geom_bar() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values=as.character(dat$fill)) + 
  #guides(fill=guide_legend(title="Dataset Name")) + 
  coord_flip() + 
  ylab('Number of Records') + 
  xlab('Genus')
ggsave('Genera_Table.png')

mkTable(getTopX(as.character(dat$genus), 20))

#####################
#Basis of Record
#####################

simpleCap <- function(x) {
  s <- strsplit(x, "_")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}


dat$basisOfRecord <- getTopX(sapply(X=as.character(dat$basisOfRecord), FUN=simpleCap))
dat$basisOfRecordColor <- getColors(dat$basisOfRecord)

ggplot(dat) + geom_bar(aes(x='', fill=basisOfRecord), width=1) + coord_polar("y", start=0) + theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +
  xlab('') + ylab('') + 
  scale_fill_manual(values=levels(dat$basisOfRecordColor)) + 
  guides(fill=guide_legend(title="Basis of Record"))#+
#theme(legend.position="bottom",legend.direction="vertical")
ggsave('BasisOfRecord_pie.png')

mkTable(as.character(dat$basisOfRecord))

####################
#Record Observation Date
####################

ggplot(dat) + geom_histogram(aes(x=year), binwidth=1) + 
  theme_bw() +
  theme(legend.position="bottom",legend.direction="vertical") +
  scale_x_continuous(limits=c(1825, 2018), expand = c(0, 0))# + 
#guides(fill=guide_legend(title="Dataset Name")) + 
#scale_fill_manual(values=levels(dat$DatasetNameCol))
ggsave('ObservationDate.png')
