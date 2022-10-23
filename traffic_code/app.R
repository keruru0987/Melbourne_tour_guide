# install packages
if (!require(data.table)) install.packages('data.table')
if (!require(leaflet)) install.packages('leaflet')
if (!require(dplyr)) install.packages('dplyr')
if (!require(htmltools)) install.packages('htmltools')
if (!require(stringr)) install.packages('stringr')
if (!require(shinyWidgets)) install.packages('shinyWidgets')
if (!require(shiny)) install.packages('shiny')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(sf)) install.packages('sf')
if (!require(plotly)) install.packages('plotly')
if (!require(shinydashboard)) install.packages('shinydashboard')
if (!require(fontawesome)) install.packages('fontawesome')


# import libraries
library(data.table)
library(leaflet)
library(dplyr)
library(htmltools)
library(stringr)
library(shinyWidgets)
library(shiny)
library(ggplot2)
library(sf)
library(plotly)
library(shinydashboard)
library(fontawesome)

####geo data
load('geo.RData')

street.labels<-lapply(street$name,function(x)
paste('<span style="font-style:italic;font-weight:bold">',x,'</span>') %>% htmltools::HTML()
)

bus2<-bus %>% 
      mutate(label=
      paste(
      paste('<span style="font-weight:bold">Route:',ROUTE_SHORT_NAME,'</span>'),
      paste('<span style="font-weight:bold">Name:',ROUTE_LONG_NAME,'</span>'),
      paste('<span style="font-weight:bold">Distance:',ROUTE_KM,' km</span>'),
      sep="<br/>"))

bus.labels<-lapply(bus2$label,htmltools::HTML)


#####crowd data
load('sensor.RData')


bins <- quantile(den$count,seq(0,1,0.2))
bins<-c(5e3,32e4,80e4,180e4,360e4,1500e4)/1e3
pal <- colorBin("YlOrRd", domain = den$count, bins =ceiling(bins))


###########basemap
basemap<-leaflet()  %>%
         addProviderTiles(providers$CartoDB.Positron) %>%
         #setView(lat=-37.840935, lng=144.946457,zoom=13)%>%
         addLayersControl(
             position = "bottomleft",
         	#baseGroups="Melbourne Municipal",
             overlayGroups = c("Melbourne Municipal","Tram CityCircle","Street","Bus Route","Crowd","Bike Rent"),
             options = layersControlOptions(collapsed = FALSE)) %>% 
         hideGroup(c("Tram CityCircle","Street","Bus Route","Crowd","Bike Rent")) %>%
         addLegend(pal = pal, values=bins,opacity = 0.7, title = 'Crowd(thousands)',position = "bottomleft")


#################
ui<- fluidPage(
useShinydashboard(),

tags$style(type = "text/css", ".outer {position: fixed; top: 0px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),

# introduce css file,need
tags$head(includeCSS("styles.css")),

div(class="outer",
leafletOutput('mapout',height='100%'),

####controller1
absolutePanel(id = "controls", class = "panel panel-default",
              top = 10, right = 10, width = 350, fixed=TRUE,
              draggable = TRUE, height = "auto",

tabBox(
      tabPanel(tags$p(fa("person-walking", fill = "#0072bb"),'CROWD'),
      sliderTextInput('whichmonth','Which month?',width=300,choices=month.abb,selected='May',grid = FALSE,animate=animationOptions(interval = 3000, loop = FALSE)),

      span(strong("March and December have higher degree of crowdedness than other months:")),
      
      br(),
      plotlyOutput('barPout',width = "100%", height = "250px")
),



tabPanel(tags$p(fa("bus", fill = "#0072bb"),'BUS'),
selectInput('whichBUS','Select Bus Route',choices=sort(unique(bus$ROUTE_SHORT_NAME)),selected='401'),
uiOutput('busSpecInfo'),
switchInput(
   inputId = "busshow",
   label = "Bus on map", 
   labelWidth = "100px",size = "sm"
)

),

tabPanel(tags$p(fa("street-view", fill = "#0072bb"),'STREET'),
selectInput('whichStreet','Select Street',choices=unique(str_sort(str_to_title(street$name))),selected='Alexandra Avenue'),
verbatimTextOutput('checkout'),
switchInput(
   inputId = "streetshow",
   label = "Street on map", 
   labelWidth = "100px",size = "sm"
)
),


width=12
)
),

###wheather controller
absolutePanel(
id = "controlsWeather", class = "panel panel-default",
top = 10, left = 40, width = 300, fixed=TRUE,
draggable = TRUE, height = "auto",
shinydashboard::box(width=12,title='Current Weather',status='primary',solidHeader=T,
collapsible = TRUE,
HTML(R"(<div id="ww_c4bc88bdc73cf" v='1.3' loc='id' a='{"t":"horizontal","lang":"en","ids":["wl2863"],"font":"Times","sl_ics":"one_a","sl_sot":"celsius","cl_bkg":"#455A64","cl_font":"#FFFFFF","cl_cloud":"#FFFFFF","cl_persp":"#81D4FA","cl_sun":"#FFC107","cl_moon":"#FFC107","cl_thund":"#FF5722","el_phw":3,"el_whr":3}'>Weather Data Source: <a href="https://weerlabs.nl/weer_melbourne/" id="ww_c4bc88bdc73cf_u" target="_blank">weer in Melbourne</a></div><script async src="https://app1.weatherwidget.org/js/?id=ww_c4bc88bdc73cf"></script>)")
)
)
##end weather

)
)

server<-function(input,output,session){

output$mapout<-renderLeaflet({
        basemap %>%
			   
		addPolygons(data=muni,
            weight = 2,
              opacity = 0.5,
              dashArray = "3",
              fillOpacity = 0.2,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = 'City of Melbourne',
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
			group="Melbourne Municipal") %>%
			
	  addPolygons(data=citycir,
                weight=2,
                color='pink',
                opacity = 0.5,
                dashArray = "",
                fillOpacity = 0.2,
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "#666",
                    dashArray = "3",
                    fillOpacity = 0.3,
                    bringToFront = TRUE),
                  label = 'City Circle Route',
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
			group="Tram CityCircle") %>%
			
	 addPolylines(data=street,weight=3,color='#54C571',dashArray = "",
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "red",
                    dashArray = "3",
                    fillOpacity = 0.3,
                    bringToFront = TRUE),
				label=street.labels,
				group='Street') %>%
				
	  addPolylines(data=bus2,weight=3,color='#FDBD01',dashArray = "",
                  highlightOptions = highlightOptions(
                    weight = 6,
                    color = "red",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE),
				label=bus.labels,
				group='Bus Route'
				)
	  

})

observeEvent(input$whichmonth,{
  whichMonth<-input$whichmonth

  denMAP<-den[Month==whichMonth][sen1,on=.(Sensor_ID)][!is.na(count)]
  denMAP[,label:=
  paste(
  paste0('<span style="font-weight:bold">Sensor:',sensor_description,'</span><br/>'),
  '<span style="font-weight:bold">Count:<em>',scales::comma(count),'</em></span>')
  ]
  
  leafletProxy("mapout") %>% 
      clearMarkers() %>%
      #clearShapes() %>%
  	  addCircleMarkers(data=denMAP,radius=10,lng=~longitude,lat=~latitude,popup=~label,stroke=F,opacity=0.6,fillOpacity=0.6,color=~pal(count),group='Crowd') %>%	
	  addMarkers(data=bike,popup=~name,
	             icon=makeIcon(iconUrl='bike.png',iconWidth=30,iconHeight=30),
	             group='Bike Rent')
})

###############
observe({
   if(!input$busshow)
   {
     leafletProxy("mapout") %>% 
   	  removeShape(layerId='busONM')
   }
})

observe({
   buss<-input$whichBUS
   inter.bus<-bus[match(buss,bus$ROUTE_SHORT_NAME),]
   if(input$busshow)
   {
     leafletProxy("mapout") %>% 
   	  removeShape(layerId='busONM') %>%
   	  addPolylines(data=inter.bus,weight=6,color='red',dashArray = "3",layerId='busONM')
   }	  
})


observe({
   if(!input$streetshow)
   {
     leafletProxy("mapout") %>% 
   	  removeShape(layerId='streetONM')
   }
})

observe({
  streetss<-input$whichStreet
  inter.street<-street[str_to_title(street$name )%in% streetss,]
  if(input$streetshow)
  {
    leafletProxy("mapout") %>% 
      removeShape(layerId='busONM') %>%
      addPolylines(data=inter.street,weight=6,color='red',dashArray = "3",layerId='streetONM')
  }	  
})
##output$checkout<-renderPrint({
##input$busshow
##})



output$barPout<-renderPlotly({
  whichMonth<-input$whichmonth
  deng<-den[,.(count=sum(count)),by=Month]
  
  barP<-ggplot(deng,aes_string(x='Month',y='count'))+
    geom_bar(stat='identity')+
    geom_bar(data=deng[Month==whichMonth],colour='#B30000',fill='#FC8D59',stat='identity')+
    theme_bw()+
    scale_x_discrete(breaks=sort(unique(deng[,Month])),expand=expansion(0.01))+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
    labs(y='Count(thousands)',x='')+
    scale_y_continuous(breaks=NULL,expand=expansion(c(0,0.05)))
  
  ggplotly(barP)
})


####
output$busSpecInfo<-renderUI({
buss<-input$whichBUS
inter.bus<-bus[match(buss,bus$ROUTE_SHORT_NAME),]

tags$ul(
tags$li(tags$b('route:'),inter.bus$ROUTE_SHORT_NAME),
tags$li(tags$b('name:'),inter.bus$ROUTE_LONG_NAME),
tags$li(tags$b('from:'),inter.bus$FIRST_STOP_NAME),
tags$li(tags$b('to:'),inter.bus$LAST_STOP_NAME),
tags$li(tags$b('distance:'),inter.bus$ROUTE_KM),
tags$li(tags$b('#stops:'),inter.bus$NUM_OF_STOPS)
)

})


}

shinyApp(ui,server)
