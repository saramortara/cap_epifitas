#### Reading information from flora do brasil ####
### Primeira versão por Sara Mortara em 20.03.2019

# set diretorio de trabalho
setwd("~/sara/em_andamento/cap_epifitas/codigos")

## carregando pacote para ler Darwin Core
library(finch) 
library(stringr) # for character extracting
library(dplyr)
library(ggplot2)
library(viridis)

## lendo os dados de Ramos et al 2019
ramos <- read.table("../ramos/epiphyte_data_11032019.txt", header=TRUE, sep="\t", as.is=TRUE)


head(ramos)

## lendo os dados do ipt flora do brasil a partir da url
url <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil&v=393.177"

out <- dwca_read(url, read=TRUE, encoding="UTF-8")

## checking files
out$files
out$files$data_paths

## selecting only fields we need
taxon <- out$data[[1]] 
distr <- out$data[[6]]
habitat <- out$data[[7]]
spec <- out$data[[4]]

## selecting only MA and epiphyte species
MA <- str_detect(distr$occurrenceRemarks, "Mata Atlântica")
ep <- str_detect(habitat$lifeForm, "Epífita")

distr.MA <- distr[MA,]
habitat.ep <- habitat[ep,]

## inserting columns with endemism and phytogeographic domain
head(distr.MA)
distr.MA$endemica <- ifelse(str_detect(distr.MA$occurrenceRemarks, "Endemica"), "endemica", "nao_endemica")

domain <- sapply(strsplit(distr.MA$occurrenceRemarks, "phytogeographicDomain"), function(x) x[2])

distr.MA$Am <- ifelse(str_detect(domain, "Amazônia"), 1, 0)
distr.MA$MA <- ifelse(str_detect(domain, "Mata Atlântica"), 1, 0)
distr.MA$Ca <- ifelse(str_detect(domain, "Caatinga"), 1, 0)
distr.MA$Pan <- ifelse(str_detect(domain, "Pantanal"), 1, 0)
distr.MA$Pam <- ifelse(str_detect(domain, "Pampa"), 1, 0)
distr.MA$Ce <- ifelse(str_detect(domain, "Cerrado"), 1, 0)

head(distr.MA)

# removing duplicated ids from distr.MA
distr.MA <- distr.MA[!duplicated(distr.MA[,c(1,5)]),]
dim(distr.MA)

## creating object with id of epiphytes of MA
ep.MA.id <- habitat.ep$id[habitat.ep$id%in%distr.MA$id]

length(ep.MA.id) # 2,337 epiphytes of MA

## selecting taxon names of epiphytes of MA
ep.MA <- taxon[taxon$id%in%ep.MA.id,]

dim(ep.MA)

ep.MA$group <- sapply(strsplit(ep.MA$higherClassification, ";"), function(x) x[2])

## generating species list
sp.list <- select(ep.MA, id, group, family, genus, specificEpithet)
sp.list$species <- paste(sp.list$genus, sp.list$specificEpithet, sep=" ")

## there are duplicated entries, lets remove them
dupl <- sp.list[duplicated(sp.list[,-1]),]
dim(dupl)

sp.list <- sp.list[!duplicated(sp.list[,-1]),]

dim(sp.list)

head(sp.list)

sp.list$species <- paste(sp.list$genus, sp.list$specificEpithet, sep=" ")

# merge sp.list with information from distr.MA
sp.listMA <- merge(sp.list, distr.MA[,c(1,6:12)], by=c("id"), all.x=TRUE, all.y=FALSE) 

dim(sp.list)
dim(distr.MA)

dim(sp.listMA)

head(sp.listMA)

# writing table
#write.table(sp.listMA, "../dados/ep_floraBR.csv", sep=",", row.names=FALSE, col.names=TRUE)

#####################################################
##### selecting only endemic species from ramos #### 
#####################################################
dim(ramos)
dim(sp.listMA)

names(sp.listMA)[6] <- "EPIPHYTE_SPECIES"

names(ramos)

head(sp.listMA)

ramos.flora <- merge(ramos, sp.listMA[,c(1,6:13)], by="EPIPHYTE_SPECIES", all.x=TRUE, all.y=FALSE)
dim(ramos.flora)
dim(ramos)

table(ramos.flora$endemica)

ramos.en <- ramos.flora[ramos.flora$endemica=="endemica" & ramos.flora$EPIPHYTE_GROUP=="Angiosperms",]
dim(ramos.en)


length(unique(ramos.en$EPIPHYTE_SPECIES))

## contando N de registros por especie
N.reg <- data.frame(sort(table(ramos.en$EPIPHYTE_SPECIES), decreasing=TRUE))

names(N.reg)[1] <- "EPIPHYTE_SPECIES"

head(N.reg)

## juntando info do N de registros no df

ramos.end <- merge(ramos.en, N.reg, by="EPIPHYTE_SPECIES", all.x=TRUE, all.y=FALSE)

head(ramos.end)

names(ramos.end)

coord.end <- ramos.end[,c("EPIPHYTE_FAMILY", "EPIPHYTE_SPECIES", "LONGITUDE_X", "LATITUDE_Y", "PRECISION", "Freq")]

head(coord.end)

names(coord.end) <- c("family", 'sp', 'lon', 'lat', 'precision', 'freq')

write.table(coord.end, "../dados/endemicasMA.csv", sep=",", row.names=FALSE, col.names=TRUE)

### exportando a tabela



### checking numbers
apply(sp.list, 2, function(x)  length(unique(x)))

## epiphyte per groups
table(sp.list$group)/nrow(sp.list)

## endemic species # 1,665, ~75%
table(sp.listMA$endemica)
table(sp.listMA$endemica)/nrow(sp.listMA)

names(sp.listMA)

head(sp.listMA)


N.domain <- rowSums(sp.listMA[,c(8:13)])
table(N.domain)
table(N.domain)/nrow(sp.list)

## Ma & Ce
## especies que ocorrem fora da MA
foraMA <- sp.listMA[rowSums(sp.listMA[,c(8:13)])>1, ]
head(foraMA)

foraMA2 <- foraMA[rowSums(foraMA[,c(8:13)])==2,]

MA.Ce <- foraMA2[foraMA2$MA==1 & foraMA2$Ce==1,]
MA.Am <- foraMA2[foraMA2$MA==1 & foraMA2$Am==1,]
MA.Pan <- foraMA2[foraMA2$MA==1 & foraMA2$Pan==1,]
MA.Ca <- foraMA2[foraMA2$MA==1 & foraMA2$Ca==1,]
MA.Pam <- foraMA2[foraMA2$MA==1 & foraMA2$Pam==1,]

nrow(MA.Pan)
nrow(MA.Ca)
nrow(MA.Pam)

(nrow(MA.Ce) +
nrow(MA.Am) +
nrow(MA.Pan))/nrow(sp.listMA)

dim(sp.listMA)

## create lists of species for each domain
names(sp.listMA)

domain <- sp.listMA[,c(8,9,13)]
other.domain <- sp.listMA[,10:12]

domain.list <- apply(domain, 2, function(x) sp.list$species[x==1])
other.list <- unlist(apply(other.domain, 2, function(x) sp.list$species[x==1]))
other.list <- other.list[!duplicated(other.list)]
domain.list$other <- as.character(other.list)

length(domain.list)

names(domain.list)

library(VennDiagram)

domain.list

domain.list$other


domain.list[1:3]
#The goal of the Venn Diagram is to count how many words are common between SNP_pop_1 and SNP_pop_2, between SNP_pop_1 and SNP_pop_3 and so on...
#The venn.diagram function do it automatically and draw it! (you will get a png file in your current working directory)

venn.diagram(
  x = domain.list[c(1,3,4)],
  category.names = c("Amazonia" , "Cerrado", "Other"),
  filename = '#14_venn_diagramm.png',
  output = TRUE ,
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  lwd = 2,
  lty = 'blank',
  fill = c('yellow', 'purple', 'green'),
  cex = 1,
  fontface = "bold",
  fontfamily = "sans",
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)
##### making a cool barplot #### 
top10 <- aggregate(sp.listMA$species, list(sp.listMA$family), function(x) length(unique(x)))

top10 <- top10$Group.1[order(top10$x, decreasing=T)][1:10]
top10

sp.count <- aggregate(sp.listMA$species, list(sp.listMA$group, sp.listMA$family, sp.listMA$endemica), function(x) length(unique(x)))
head(sp.count)

sp.count <- sp.count[sp.count$Group.2%in%top10,]

head(sp.count, 10)

df <- sp.count

# other <- c("all", "other families", sum(sp.count$x[-1*1:10]), NA)
# df <- rbind(df, other)
# df$x <- as.numeric(df$x)# 

df$prop <- df$x/nrow(sp.listMA)

df


nomes <- c(Angiospermas="Angiosperms", 'Samambaias e Licófitas'='Ferns', 'Briófitas'="Briophytes")

png("../figuras/endemic.png")
ggplot(aes(x=reorder(Group.2, prop), y=prop, fill=Group.3), data=df) +
    labs(x="The 10 Richest Families", y="Proportion of Epiphyte Species") +
    geom_bar(stat="identity") + 
    scale_fill_manual(values=c(viridis(2, alpha=.7)[2], 'grey'), 
                      name="Endemic", labels=c("yes", 'no')) +
   facet_grid(Group.1 ~., labeller=as_labeller(nomes), scales='free', space="free" ) +
    theme_minimal(base_size = 14) + coord_flip()
dev.off()

###### Making a cool map #####
library(tidyverse)
library(maps)
library(ggrepel)

head(ramos)

ramos$id_site <- paste(round(ramos$LONGITUDE_X, 4), round(ramos$LATITUDE_Y, 4), sep=";") 
sp.site <- aggregate(ramos$EPIPHYTE_SPECIES, list(ramos$id_site), function(x) length(unique(x)))

sp.site$lon <- lapply(strsplit(sp.site$Group.1, ";"), function(x) x[1])
sp.site$lat <- lapply(strsplit(sp.site$Group.1, ";"), function(x) x[2])

head(sp.site)

sp.df <- as.data.frame(cbind(lon=as.numeric(sp.site$lon), 
               lat=as.numeric(sp.site$lat), 
               riq=sp.site$x))

head(sp.df)
summary(sp.df)
class(sp.df)

BR <- map_data("world") %>% filter(region=="Brazil")

#data <- world.cities %>% filter(country.etc=="Brazil")

sp.df2 <- head(sp.df %>%
  arrange(desc(riq)), 500)

mybreaks=c(20, 75, 100, 200)
  
png("../figuras/mapa.png")
  sp.df2 %>%
    arrange(riq) %>%
  # mutate( name=factor(name, unique(name))) %>%
    ggplot() +
    geom_polygon(data = BR, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_point(aes(x=lon, y=lat, size=riq, color=riq, alpha=riq), shape=20, stroke=FALSE) +
    scale_size_continuous(name="Number of species", trans="log", range=c(1,12), breaks=mybreaks) +
    scale_alpha_continuous(name="Number of species", trans="log", range=c(0.1, .7), breaks=mybreaks) +
    scale_color_viridis(option="viridis", trans="log", breaks=mybreaks, name="Number of species" ) +
    theme_void()  + coord_map() + 
    guides( colour = guide_legend()) +
    ggtitle("The 500 richest localities in the Atlantic Forest") +
    theme(
      legend.position = c(0.15, 0.2),
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )  
dev.off()
  
  

  #visualize records
  ggplot() +
    geom_polygon(data = BR, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_point(data = ramos, aes(x = LONGITUDE_X, y = LATITUDE_Y),
               colour = "darkblue", size = 0.5)


  
  