####################################################################
## author:    Dominic Brotschi
## contact:   dominic.brotschi@student.unisg.ch
## file name: Tchorddiag.R
## context:   chorddiagramm year 2016
## started:   2019-11-01
## summary:   creates chorddiagramm
######################################################################
rm(list = ls())

library("tidyverse")
#devtools::install_github("mattflor/chorddiag")
library(circlize)
library(igraph)
library(tidygraph)
library(chorddiag)
library(countrycode)

getwd()
### TOSCO all data
mydata <- read.csv("data/tosco_merged.csv")

## VDEM data
vdem <- vdemdata::vdem
vdem <- subset(vdem, vdem$year > 1999)
vdem$cc <- countrycode(vdem$country_name, "country.name", "cowc")
vdem3 <- vdem %>% dplyr::select(year,cc, v2x_polyarchy, v2x_regime)
vdem2019 <- subset(vdem3,vdem3$year == 2019)

###prepare data
Data = mydata[mydata$year == 2019,]
Data$commercial_name = as.character(Data$commercial_name)
Data$company_id = as.character(Data$company_id)

Data <- left_join(Data, vdem2019, by = c("year", "cc"))
Data <- Data %>% filter(v2x_regime <= 1) 
Data <- dplyr::select(Data, -v2x_regime)

Data$hq_cc <- countrycode(Data$sh_group_hq,"country.name","cowc")
Data <- Data %>% drop_na(hq_cc)
Data$foreign <- ifelse(Data$cc != Data$hq_cc,1,0)
Data <- subset(Data, Data$foreign == 1)
Data <- left_join(Data, vdem2019,c("hq_cc" = "cc","year"))

Data <- Data %>% dplyr::select(year,country,cc,company_id,commercial_name, hq_cc, sh_group_hq,v2x_regime)

Data_aut <- Data %>% filter(v2x_regime < 2)

#aggregate by subsidiaries
subsidiaries = aggregate(Data_aut$commercial_name,by = list(Data_aut$company_id,Data_aut$country),unique)
#calculate number of subsidiaries per company
number_of_subs = as.data.frame(table(subsidiaries$x))
names(subsidiaries) = c("cc","country","Var1")
subsidiaries_with_number = left_join(subsidiaries,number_of_subs,by ="Var1")
subsidiaries_with_number$cc = countrycode(subsidiaries_with_number$country,"country.name", "iso3c")
#subsidiaries_with_number <- merge(subsidiaries_with_number,vdem2019, by= "cc",all.y = F)
#subsidiaries_with_number <- subset(subsidiaries_with_number,subsidiaries_with_number$v2x_regime < 2)

subsidiaries_with_number <- subsidiaries_with_number[c(2,3,4)]
# change names to improve readablity:
subsidiaries_with_number$country[subsidiaries_with_number$country == "Central African Republic"] <- "CAR"
subsidiaries_with_number$country[subsidiaries_with_number$country == "Congo Brazzaville"] <- "Congo"
subsidiaries_with_number$country[subsidiaries_with_number$country == "Congo Kinshasa"] <- "DRC"
subsidiaries_with_number$country[subsidiaries_with_number$country == "Equatorial Guinea"] <- "Equ. Guinea"
test <- subset(subsidiaries_with_number, !subsidiaries_with_number$Freq < 1)
colnames(test) = c("from","to","value")
test <- spread(test, to, value)
test[is.na(test)] <- 0
test2 <- test[,-1]
rownames(test2) <- test[,1]


### change order of rows
#test2 <- test2[,c(9,15,16,1,2,7,4,3,5,6,8,10,13,14,17)]
mat <- as.matrix(test2)

othercol = structure(rep("grey", length(mat)), names = rownames(mat))
# grid.col = c("Africell" = "#9ecae1", "Airtel" = "#fed976", "Econet" = "#ffeda0","Moov" = "#fcbba1","MTN" = "#fed976","Orange" = "#084594","Telecel" = "#abd9e9", "Viettel" = "#67000d","Vodacom" = "#2171b5", "Vodafone" = "#08519c","Zain" = "#67000d","Tigo"="#cb181d","Telecel"="#ef3b2c", "Smart"="#fb6a4a","Ooredoo"="#67000d","Glo"="#fee0d2","Azur"="#fff5f0","Unitel"="#08519c","TNM"="#2171b5","Tigo"="#67000d", othercol)

grid.col = c("Africell" = "#e3d2d2","Azur"="#e8baaf","Chinguitel"="#fcbba1","Djezzy"="#eda28c","Etisalat"="#fb6a4a","Gecom"="#ef3b2c","Inwi"="#d14949","Libertis"="#983535","Maroc Telecom"="#67000d", "Moov" = "#aee2ff","Movitel" = "#8dc1ff","Muni" = "#22aacc","Nationlink" = "#6c729d", "Ooredoo" = "#1a80b3","Smart" = "#2171b5", "Somafone" = "#08519c","Somtel" = "#115599","Telcom Somalia"="#092b80","Tigo"="#000066", "Viettel"="#3d405b","Zain"="#4c4c97", othercol)

pdf("output/chord_aut.pdf", width = 6,height =6)
chordDiagram(t(mat), annotationTrack = "grid", 
             preAllocateTracks = 1, 
             grid.col = grid.col 
             #col = grid.col
             #directional = 1
)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .001, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), col = "black",cex = 0.75)
}, bg.border = NA)



dev.off()


#### Use Sankey Plot instead of circle


library(networkD3)

# A connection data frame is a list of flows with intensity for each flow
links <- Data_aut %>% select(country,company_id, commercial_name) %>%
  rename(source = commercial_name,
         target = country) %>%
  unique()%>%
  select(-company_id)
  
links$value <- 1

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)

p
# save the widget
library(htmlwidgets)
p$x$options$fontSize<-25
p
htmlwidgets::onRender(p,
                      jsCode = '
  function(el,x) {
    d3.select(el)
      .selectAll(".node text")
      .filter(d => d.source)
      .attr("x", -165)
      .attr("text-anchor", "end");
  }
  '
)
p$x$nodes$source <- nodes$source
p <- onRender(p,
               '
  function(el) {
    d3.select(el)
      .selectAll(".node text")
      .filter(d => d.source)
      .attr("x", -160)
      .attr("text-anchor", "end");
  }
  '
)
p
saveWidget(p, file="output/sankey_plot.html")

#transform into PDF
library(webshot)
webshot("output/sankey_plot.html", "output/sankey_plot.png")

