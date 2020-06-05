## ---- LoadLibraries ----
library(dplyr)
library(tidyr)
library(purrr)
library(leaflet)
library(sf)
library(readxl)
library(leaflet)
library(htmltools)
## ---- HelperFunctions ----

#change name to be consistent with world map

fixCountries <- function(oldcountries) {
  namematch <- tibble(oldname=c("USA", "Great Britain", "Moldova", "Macedonia", "South Korea","North Korea",
                                "Laos", "Vietnam","Syria","Iran","Western sub-Saharan Africa","Congo \\(Brazzaville\\)",
                                "Democratic Republic of the\r\nCongo","C?te d'Ivoire","Tanzania","Libya"),
                      newname=c("United States","United Kingdom","Republic of Moldova","The former Yugoslav Republic of Macedonia",
                                "Korea, Republic of","Korea, Democratic People's Republic of","Lao People's Democratic Republic",
                                "Viet Nam","Syrian Arab Republic","Iran (Islamic Republic of)","Western Sahara",
                                "Congo","Democratic Republic of the Congo","Cote d'Ivoire","United Republic of Tanzania",
                                "Libyan Arab Jamahiriya"))
  oc <- tibble(o=oldcountries)
  oc <- left_join(oc, namematch, by=c("o"="oldname"))
  oc <- mutate(oc, newname=ifelse(is.na(newname), o, newname))
  return(oc$newname)
}

addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}


mkPopup2 <- function(WB){
  wb <- st_set_geometry(WB, NULL)
  g <- gather(select(wb, NAME, `Combined Primary/Secondary Prevention Score`, `Overall Score for Primary Prevention`, `Overall Score for Secondary Prevention`, Economy),
              key="Rowname", value="Score", -NAME)
  g <- nest(group_by(g, NAME))
  g <- mutate(g, popup=map2_chr(data, NAME, ~knitr::kable(.x, format="html", col.names=c(.y, ""))))
  WB <- left_join(WB, select(g, NAME, popup), by="NAME")
  return(WB)
}

mkPopup1 <- function(WB){
  wb <- st_set_geometry(WB, NULL)
  g <- gather(select(wb, NAME, Surveillance, Acute, Rehabilitation, Economy),
              key="Rowname", value="Score", -NAME)
  g <- nest(group_by(g, NAME))
  g <- mutate(g, popup=map2_chr(data, NAME, ~knitr::kable(.x, format="html", col.names=c(.y, ""))))
  WB <- left_join(WB, select(g, NAME, popup), by="NAME")
  return(WB)
}


quintileFactors <- function(V) {
  qq <- c(-0.01, 0.01, c( 0.3, 0.50, 0.75, 1))*100
  
  cut(V, breaks=qq, labels=c("Absent", "Poor", "Acceptable", "Good", "Very good"),
      right=FALSE,
      ordered_result=TRUE)
}

hooktext <- "
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }"

googleanalytics <- paste(readLines("analytics.js"), collapse="\n")

## ---- LoadData ----
##############
#stroke service Figure 2
TableS1<-read_xlsx("acutecareservice.xlsx",sheet = "TableS1")
TableS1<-mutate(TableS1,
                Surveillance=as.numeric(Surveillance),
                Prevention=as.numeric(Prevention),
                Acute=as.numeric(Acute),
                Economy=factor(Economy, levels=c("LI", "LMI", "UMI", "HI"), ordered=TRUE),
                Rehabilitation=as.numeric(Rehabilitation))

#stroke prevention Figure 4
TableS8<-read_xlsx("acutecareservice.xlsx",sheet = "TableS8")
TableS8<-mutate(TableS8,
                `Overall Score for Primary Prevention`= as.numeric(`Overall Score for Primary Prevention`),
                `Overall Score for Secondary Prevention`=as.numeric(`Overall Score for Secondary Prevention`)
                
                
)
TableS8 <- left_join(TableS8, select(TableS1, Countries, Prevention), by=c("Country"="Countries"))

TableS1 <- mutate(TableS1, Countries = fixCountries(Countries))
TableS8 <- mutate(TableS8, Country = fixCountries(Country))

## ---- LoadDataNew ----
##############
#stroke service Figure 2

mapdat <- select(read_xlsx("Data_for_Maps.xlsx", sheet = "Sheet1", na = c("", "N/A")), -starts_with(".."))

mapdat <- mutate(mapdat, Country = fixCountries(Country))
TableS1 <- select(mapdat, Country, Surveillance, Prevention, `Acute Care`, Rehabilitation, Economy, `No. of Hospitals`)
TableS1 <- rename(TableS1, Countries = Country, Acute = `Acute Care`)

TableS8 <- select(mapdat, Country, `Overall Score for Primary Prevention`, `Overall Score for Secondary Prevention`, Prevention)

TableS1 <- mutate(TableS1, Countries = fixCountries(Countries))
TableS8 <- mutate(TableS8, Country = fixCountries(Country))

## ---- LoadDataWorld ----

worldborders.orig <- st_read("TM_WORLD_BORDERS_SIMPL-0.3.shp")
worldborders <- left_join(worldborders.orig, TableS1, by=c("NAME"="Countries"))

worldborders <- mutate(worldborders,
                       Surveillance=quintileFactors(Surveillance),
                       Acute=quintileFactors(Acute),
                       Rehabilitation=quintileFactors(Rehabilitation)
)

lancetColours <- colorFactor(c("red", "orange", "yellow", "cyan", "green"), ordered=TRUE,
                             na.color="white", domain=worldborders$Surveillance)

economyColours <- colorFactor(rev(scales::hue_pal()(4)), ordered=TRUE,
                              na.color="white", domain=worldborders$Economy)

worldborders.fig4 <- left_join(worldborders.orig, TableS8, by=c("NAME"="Country"))
worldborders.fig4 <- left_join(worldborders.fig4, select(TableS1, Countries, Economy), by=c("NAME"="Countries"))

worldborders.fig4 <- mutate(worldborders.fig4,
                            `Combined Primary/Secondary Prevention Score`=quintileFactors(Prevention),
                            `Overall Score for Primary Prevention`=quintileFactors(`Overall Score for Primary Prevention`),
                            `Overall Score for Secondary Prevention`=quintileFactors(`Overall Score for Secondary Prevention`),
)

## ---- Map1 ----

worldborders <- mkPopup1(worldborders)

worldstrokemap1 <- 
  leaflet(worldborders) %>% 
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(Surveillance), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Surveillance") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(Acute), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Acute") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(Rehabilitation), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Rehabilitation") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~economyColours(Economy), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Economy") %>%
  addLegend_decreasing(pal=lancetColours, 
                       values=~Surveillance, 
                       na.label = "No data provided", 
                       decreasing = TRUE,
                       group="Surveillance") %>% 
  addLegend_decreasing(pal=lancetColours, 
                       values=~Acute, 
                       na.label = "No data provided", 
                       decreasing = TRUE,
                       group="Acute") %>% 
  addLegend_decreasing(pal=lancetColours, 
                       values=~Rehabilitation, 
                       na.label = "No data provided", 
                       decreasing = TRUE,
                       group="Rehabilitation") %>% 
  addLegend_decreasing(pal=economyColours, 
                       values=~Economy, 
                       na.label = "No data provided", 
                       decreasing=TRUE,
                       group="Economy") %>% 
  addLayersControl(baseGroups=c("Surveillance","Acute", "Rehabilitation","Economy"),
                   position="topleft",
                   options=layersControlOptions(collapsed=FALSE)) %>% 
  addTiles() %>%
  setView(lat=0, lng=0, zoom=2) %>% 
  htmlwidgets::onRender(hooktext) %>% 
  htmlwidgets::prependContent(tags$header(includeScript("analytics.js")))



worldstrokemap1a <- 
  leaflet(worldborders) %>% 
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(Surveillance), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Surveillance") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(Acute), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Acute") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(Rehabilitation), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Rehabilitation") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~economyColours(Economy), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Economy") %>%
  addLegend_decreasing(pal=lancetColours, 
                       values=~Surveillance, 
                       na.label = "No data provided", 
                       decreasing = TRUE,
                       group="Surveillance") %>% 
  addLayersControl(baseGroups=c("Surveillance","Acute", "Rehabilitation","Economy"),
                   position="topleft",
                   options=layersControlOptions(collapsed=FALSE)) %>% 
  setView(lat=0, lng=0, zoom=2) %>% 
  addTiles() 
## ---- Map2 ----


worldborders.fig4 <- mkPopup2(worldborders.fig4)

worldstrokemap2 <- leaflet(worldborders.fig4) %>% 
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(`Combined Primary/Secondary Prevention Score`), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Combined Primary/Secondary Prevention Score") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(`Overall Score for Primary Prevention`), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Overall Score for Primary Prevention") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(`Overall Score for Secondary Prevention`), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Overall Score for Secondary Prevention") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~economyColours(Economy), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Economy") %>%
  addLegend_decreasing(pal=lancetColours, 
                       values=~`Combined Primary/Secondary Prevention Score`, 
                       na.label = "No data provided", 
                       decreasing = TRUE,
                       group="Combined Primary/Secondary Prevention Score") %>% 
  addLegend_decreasing(pal=lancetColours, 
                       values=~`Overall Score for Primary Prevention`, 
                       na.label = "No data provided", 
                       decreasing = TRUE,
                       group="Overall Score for Primary Prevention") %>% 
  addLegend_decreasing(pal=lancetColours, 
                       values=~`Overall Score for Secondary Prevention`, 
                       na.label = "No data provided", 
                       decreasing = TRUE,
                       group="Overall Score for Secondary Prevention") %>% 
  addLegend_decreasing(pal=economyColours, 
                       values=~Economy, 
                       na.label = "No data provided", 
                       decreasing=TRUE,
                       group="Economy") %>% 
  addLayersControl(baseGroups=c("Combined Primary/Secondary Prevention Score", "Overall Score for Primary Prevention", "Overall Score for Secondary Prevention", "Economy"),
                   position="topleft",
                   options=layersControlOptions(collapsed=FALSE)) %>% 
  addTiles() %>%
  setView(lat=0, lng=0, zoom=2) %>%
  htmlwidgets::onRender(hooktext) %>% 
  htmlwidgets::prependContent(tags$header(includeScript("analytics.js")))

worldstrokemap2a <- leaflet(worldborders.fig4) %>% 
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(`Combined Primary/Secondary Prevention Score`),
              fillOpacity = 0.5,
              popup=~popup,
              group="Combined Primary/Secondary Prevention Score") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(`Overall Score for Primary Prevention`), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Overall Score for Primary Prevention") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~lancetColours(`Overall Score for Secondary Prevention`), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Overall Score for Secondary Prevention") %>%
  addPolygons(stroke=FALSE, 
              fillColor = ~economyColours(Economy), 
              fillOpacity = 0.5,
              popup=~popup,
              group="Economy") %>%
  addLegend_decreasing(pal=lancetColours, 
                       values=~`Combined Primary/Secondary Prevention Score`, 
                       na.label = "No data provided", 
                       decreasing = TRUE,
                       group="Combined Primary/Secondary Prevention Score") %>% 
  addLayersControl(baseGroups=c("Combined Primary/Secondary Prevention Score", "Overall Score for Primary Prevention", "Overall Score for Secondary Prevention", "Economy"),
                   position="topleft",
                   options=layersControlOptions(collapsed=FALSE)) %>% 
  setView(lat=0, lng=0, zoom=2) %>% 
  addTiles()

## ---- SaveMaps ----
htmlwidgets::saveWidget(worldstrokemap1, "worldstrokemap1.html", title="Commission on Stroke")
htmlwidgets::saveWidget(worldstrokemap2, "worldstrokemap2.html", title="Commission on Stroke")

## fewer legends so that webshot works
htmlwidgets::saveWidget(worldstrokemap1a, "worldstrokemap1a.html")
htmlwidgets::saveWidget(worldstrokemap2a, "worldstrokemap2a.html")


tt <- webshot::webshot("worldstrokemap1a.html", "wsm1.png")
tt <- webshot::webshot("worldstrokemap2a.html", "wsm2.png")


