library(leaflet)
library(htmltools)
library(sf)             
library(dplyr)          
library(leaflet)        
library(htmltools)     
library(leaflet.extras)


#Initial SETUP for lat/long

#Generate Lat/Long using Google Sheet Script ---> https://discourse.looker.com/t/get-latitude-longitude-for-any-location-through-google-sheets-and-plot-these-in-looker/5402

options(digits=12)

df <- read.csv("YOURCSV.csv", stringsAsFactors = T, encoding = "UTF-8")

----
#icons input --> http://rstudio.github.io/leaflet/markers.html
  


Icon1 <- makeIcon(
  iconUrl = "marker-red.png",
  shadowUrl = "marker-shadow.png",
  iconWidth = 20, iconHeight = 35, 
  shadowWidth = 25, shadowHeight = 35,
  shadowAnchorX = 5, shadowAnchorY = 20
)

Icon2 <- makeIcon(
  iconUrl = "marker-blue.png",
  shadowUrl = "marker-shadow.png",
  iconWidth = 20, iconHeight = 35, 
  shadowWidth = 25, shadowHeight = 35,
  shadowAnchorX = 5, shadowAnchorY = 20
)

Icon3 <- makeIcon(
  iconUrl = "marker-green.png",
  shadowUrl = "marker-shadow.png",
  iconWidth = 20, iconHeight = 35, 
  shadowWidth = 25, shadowHeight = 35,
  shadowAnchorX = 5, shadowAnchorY = 20
)


#Add Legend for custom icons
html_legend <- "<img src='LINK TO RED MARKER'
                style='width:15px;height:25px;'>Legend Text<br/>
                <img src='LINK TO BLUE MARKER'
                style='width:15px;height:25px;'> Petitions to Keep Statues<br/>
                <img src='LINK TO GREEN MARKER'
                style='width:15px;height:25px;'> Petitions for New Statues"

---
  
#paste in links
city2 <- city %>% 
  dplyr::mutate(label2 = paste0('<a href="', city$link,'"target="_blank">Link here</a>')) 

----
  
#Seperate petitions into groups N.B. Filters OUT groups
Group1 <- city2 %>% 
dplyr::filter(group != "Group2Remove", group != "Group3Remove")

Group2 <- city2 %>% 
  filter(group != "Group1Remove", group != "Group3Remove") 

Group3 <- city2 %>% 
  filter(group != group != "Group1Remove", group != "Group2Remove") 

----
  
#introduce data - in this case I'm using scraped date from change.org inc. paragraphs and href into marker using lapply
labs1 <- lapply(seq(nrow(Group1)), function(i) {
    paste0( '<b>Title: </b>', Group1[i, "title"], '<p></p>', 
            '<b>Number of Supporters: </b>', Group1[i, "supporters"],'<p></p>', 
            '', Group1[i, "Column for URL Link to petition"]) 
})
  
labs2 <- lapply(seq(nrow(Group2)), function(i) {
  paste0( '<b>Title: </b>', Group2[i, "title"], '<p></p>', 
          '<b>Number of Supporters: </b>', Group2[i, "supporters"],'<p></p>', 
          '', Group2[i, "Column for URL Link to petition"]) 
})

labs3 <- lapply(seq(nrow(Group2)), function(i) {
  paste0( '<b>Title: </b>', Group2[i, "title"], '<p></p>', 
          '<b>Number of Supporters: </b>', Group2[i, "supporters"],'<p></p>', 
          '', Group2[i, "Column for URL Link to petition"]) 
})

----
  
#FINAL GRAPH w/ grouped layers
  lplot <- (leaflet(data = city2)) %>% 
  addTiles() %>% 
  setView(lng=-4, lat=54, zoom = 6) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(data = Group1,
             popup = lapply(labs1, htmltools::HTML), #add popup here
             group = "Group1",
             labelOptions = labelOptions(noHide = F, textsize = "12px", direction = "bottom"),icon = Icon1) %>% 
  addMarkers(data = Group2, # second group
             popup = lapply(labs2, htmltools::HTML), #add popup here
             group = "Group2",
             labelOptions = labelOptions(noHide = F, textsize = "12px", direction = "bottom"), icon = Icon1) %>%
  addMarkers(data = Group3, # third group
             popup = lapply(labs3, htmltools::HTML),#add popup here
             group = "Group3",
             labelOptions = labelOptions(noHide = F, textsize = "12px", direction = "bottom"), icon = Icon1) %>%
  addLayersControl(
  overlayGroups = c("Group1", "Group2", "Group3"),
  options = layersControlOptions(collapsed = F, position = "topleft")) %>%
  addControl(html = html_legend, position = "topright") %>%
  hideGroup("Group2", "Group3") #Option to hide groups and show one toggled group as default

#AND Plot!
lplot

#Save as HTML and embed in your own HTML e.g.:

  #    <div class=">
  #    <iframe style="width: 100%; height: 580px;" src="URL of html file on server">
  #    </iframe>
