#Load utility functions and background data
setwd('D:/Documents and Settings/mcooper/Google Drive/JRS/Bats Kenya/')

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
  for(u in uuid){
    result <- read_html(paste0("http://www.gbif.org/publisher/", u))
    
    data <- result %>%
      html_nodes("#information") %>%
      html_nodes("p") %>%
      html_text() %>%
      .[1]
    
    df <- bind_rows(df, data.frame(publishingOrgKey=u, Publisher=data))
  }
  df
}

getDatasetInfo <- function(uuids){
  df <- data.frame()
  for(datasetKey in uuids){
    result <- read_html(paste0("http://www.gbif.org/dataset/", datasetKey))
    
    DatasetName <- result %>%
      html_nodes("#summary") %>%
      html_nodes("p") %>%
      html_text() %>%
      .[1]
    
    dates <- result %>%
      html_nodes("#summary") %>%
      html_nodes(".right") %>%
      html_children() %>%
      html_text()
    
    datasetRegistrationDate <- dates[which(dates=="Registration Date") + 1]
    datasetPublicationDate <- dates[which(dates=="Publication Date") + 1]
    
    
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

#7D2D2E JRS Red
#6C832E JRS Yellowgreen
#00727B JRS Blue
#F6BC33 JRS Bright Yellow
#49113A JRS Purple
#414241 JRS Black

taxa <- 'Chiroptera'
rank <- 'order'
country <- 'KE'

out <- name_backbone(name=taxa, rank=rank)

fields <- c('name', 'key', 'family',
            'datasetKey', 'publishingOrgKey', 'publishingCountry',
            'year', "datasetName", "country", 'genus',
            'basisOfRecord')

res <- occ_search(out$orderKey, country=country, #fields=fields,
                  limit=200000)

dat <- res$data

write.csv(dat, paste0(taxa, '_', country, '.csv'), row.names=F)

#publishers <- getPublisherInfo(unique(dat$publishingOrgKey))
dat <- merge(dat, publishers)

datasets <- getDatasetInfo(unique(dat$datasetKey))
dat <- merge(dat, datasets)


###################
#Datasets over Time
##################

dat$DatasetNameCat <- getTopX(dat$DatasetName)
dat$DatasetNameCol <- getColors(dat$DatasetName)

dat$datasetDate <- mdy(dat$datasetRegistrationDate)

counts <- dat %>%
  group_by(DatasetName, DatasetNameCat, DatasetNameCol, datasetDate) %>%
  summarize(size=n())

range <- seq(floor_date(min(counts$datasetDate), unit="month"), 
             floor_date(today(), unit="month"), 
             by="1 month")

plotdf <- data.frame()

for(i in 1:nrow(counts)){
  vals <- rep(counts$size[i], length(range))
  vals[range < ymd(counts$datasetDate[i])] <- 0
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
  summarize(MinRegDate=min(mdy(datasetRegistrationDate))) %>% 
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

#####################
#Publishers Over Time
####################
# dat$PublisherCat <- getTopX(dat$Publisher)
# dat$PublisherCol <- getColors(dat$Publisher)
# ggplot(dat) + geom_bar(aes(x=year, fill=PublisherCat)) + 
#   theme_bw() +
#   theme(legend.position="bottom",legend.direction="vertical") + 
#   scale_x_continuous(limits=c(1880, 2018), expand = c(0, 0)) + 
#   guides(fill=guide_legend(title="Publisher Name")) + 
#   scale_fill_manual(values=levels(dat$PublisherCol))
# ggsave('Publishers_time.png')
# 
# #######################
# #Publishers Pie Chart
# ######################
# dat$PublisherCat <- as.character(dat$PublisherCat)
# 
# dat$PublisherCat[is.na(dat$PublisherCat)] <- 'Unknown'
# ggplot(dat) + geom_bar(aes(x='', fill=PublisherCat), width=1) + coord_polar("y", start=0) + theme_bw() +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid  = element_blank()) +
#   xlab('') + ylab('') + 
#   scale_fill_manual(values=levels(dat$countryColor)) + 
#   guides(fill=guide_legend(title="Publisher Name")) + 
#   scale_fill_manual(values=levels(dat$PublisherCol))+
#   theme(legend.position="bottom",legend.direction="vertical")
# ggsave('Publishers_pie.png')
# 
# mkTable(dat$Publisher)

######################
#Countries
####################

# library(rgdal)
# 
# country <- dat %>% 
#   group_by(country) %>%
#   summarize(count=n()) %>%
#   data.frame
# 
# sp <- readOGR('../ne_50m_admin_0_countries', 'ne_50m_admin_0_countries')
# 
# if(length(unique(dat$country)[!unique(dat$country) %in% sp@data$SOVEREIGNT]) > 1){
#   stop("Countries are not matching!")
# }
# 
# 
# sp@data = data.frame(sp@data, 
#                      country[match(as.character(sp@data[,'SOVEREIGNT']), 
#                                    as.character(country[,'country'])),])
# 
# sp@data$id <- sapply(slot(sp, "polygons"), function(x) slot(x, "ID"))
# 
# fort <- tidy(sp)
# 
# 
# ggplot(sp@data) + geom_map(aes(map_id=id, fill=count), map=fort) +
#   geom_polygon(data=fort, aes(x=long, y=lat, group=group), colour='#414241', fill=NA) + 
#   #expand_limits(x=fort$long, y=fort$lat) + 
#   scale_color_manual(values='white') + 
#   scale_fill_gradientn(colors=c('#7D2D2E', '#6C832E', '#00727B'),
#                        #midpoint=median(sp@data$count, na.rm=T),
#                        na.value='#e0e0e0',
#                        guide=guide_colorbar("Number of Occurances")) + 
#   coord_map(xlim=c(-20, 52), ylim=c(-40, 35)) + 
#   theme_bw()+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# ggsave('Countries_map.png')
#   

#   
# 
# library(rworldmap)
# 
# country <- dat %>% 
#   group_by(country) %>%
#   summarize(count=n())
# 
# country$country[country$country == "Côte d'Ivoire"] <- "Ivory Coast"
# 
# mapdat <- joinCountryData2Map(country, joinCode='NAME', nameJoinColumn ='country', mapResolution = 'high',
#                            suggestForFailedCodes = T)
# 
# par(mai=c(0.1,0.1,0.1,0.1),xaxs="i",yaxs="i")
# mapBubbles(mapdat,
#            nameZSize='count',
#            oceanCol='lightblue',
#            landCol='wheat',
#            mapRegion='Africa',
#            mapTitle='Number of Records by Publishing Country')
# 
# 
dat$countryName <- getTopX(dat$country)
dat$countryColor <- getColors(dat$country)

dat$countryName <- as.character(dat$countryName)
dat$countryName[is.na(dat$countryName)] <- 'Unknown'
ggplot(dat) + geom_bar(aes(x='', fill=countryName), width=1) +
  coord_polar("y", start=0) + theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +
  xlab('') + ylab('') +
  scale_fill_manual(values=as.character(dat$countryColor)) +
  guides(fill=guide_legend(title="Country of Record"))+
  theme(legend.position="bottom",legend.direction="vertical")
ggsave('Countries_pie.png')

mkTable(dat$country)

######################
#Publishing Org Country
############################

# PublishingCountry <- dat %>% group_by(publishingCountry) %>%
#   summarize(count=n())
# 
# mapdat <- joinCountryData2Map(PublishingCountry, joinCode='ISO2', nameJoinColumn ='publishingCountry', mapResolution = 'high', suggestForFailedCodes = T)
# 
# 
# 
# 
# par(mai=c(0.1,0.1,0.1,0.1),xaxs="i",yaxs="i")
# mapCountryData( mapdat
#                 , nameColumnToPlot="count"
#                 , numCats=10
#                 , catMethod='quantiles'
#                 , colourPalette="heat"
#                 , oceanCol="lightblue"
#                 , missingCountryCol="white"
#                 , borderCol="black"
#                 , mapTitle=''
# )
# 
iso <- read.csv('../ISO2.csv')
dat <- merge(dat, iso, all.x=T, all.y=F)
# 
# pubCountry <- dat %>% 
#   group_by(publishingCountryFull) %>%
#   summarize(pubCount=n()) %>%
#   data.frame
# 
# if(length(unique(dat$publishingCountryFull)[!unique(dat$publishingCountryFull) %in% sp@data$SOVEREIGNT]) > 1){
#   stop("Countries are not matching!")
# }
# 
# 
# sp@data = data.frame(sp@data, 
#                      pubCountry[match(as.character(sp@data[,'SOVEREIGNT']), 
#                                    as.character(pubCountry[,'publishingCountryFull'])),])
# 
# fort <- tidy(sp)
# 
# 
# ggplot(sp@data) + geom_map(aes(map_id=id, fill=pubCount), map=fort) +
#   geom_polygon(data=fort, aes(x=long, y=lat, group=group), colour='#414241', fill=NA) + 
#   #expand_limits(x=fort$long, y=fort$lat) + 
#   scale_color_manual(values='white') + 
#   scale_fill_gradientn(colors=c('#7D2D2E', '#6C832E', '#00727B'),
#                        #midpoint=median(sp@data$count, na.rm=T),
#                        na.value='#e0e0e0',
#                        guide=guide_colorbar("Number of Occurances")) + 
#   #coord_map(xlim=c(-20, 52), ylim=c(-40, 35)) + 
#   theme_bw()+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   guides(fill=guide_legend(title="Country of Record"))
# ggsave('Publishers_country.png')


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
  guides(fill=guide_legend(title="Country of Publisher"))+
  theme(legend.position="bottom",legend.direction="vertical")
ggsave('PublishingCountries_pie.png')

mkTable(as.character(dat$publishingCountryFull))

###########
#Genera
#############
#dat$Genus <- getTopX(dat$genus, 40)

dat$family <- as.character(dat$family)
dat$family[is.na(dat$family)] <- 'Unknown'

ggplot(dat, aes(family)) + geom_bar() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values=as.character(dat$fill)) + 
  #guides(fill=guide_legend(title="Dataset Name")) + 
  #coord_flip() + 
  ylab('Number of Records') + 
  xlab('Family')
ggsave('Genera_Table.png')

mkTable(as.character(dat$family))
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
  theme(legend.position="bottom",legend.direction="vertical")
  #scale_x_continuous(limits=c(1880, 2018), expand = c(0, 0)) + 
  #guides(fill=guide_legend(title="Dataset Name")) + 
  #scale_fill_manual(values=levels(dat$DatasetNameCol))
ggsave('ObservationDate.png')

