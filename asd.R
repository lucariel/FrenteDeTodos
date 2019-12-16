#gender_asesores 
library(tidyverse)
library(geojsonio)
library(rmapshaper)
library(sp)

geodata_arg<- rgdal::readOGR('./arg_adm1/ARG_adm1.shp')
states_json <- geojson_json(geodata_arg, geometry = "polygon", group = "group")
states_sp <- geojson_sp(states_json)
states_very_simp <- ms_simplify(states_sp, keep = 0.001)

senadores_df<-read_csv('datos_legisladores.csv')
senadores_df1<-senadores_df%>%
  mutate(
  total_asesores = asesores_masculinos+asesores_femeninos,
  porcentaje_m = asesores_femeninos/total_asesores,
  porcentaje_h = asesores_masculinos/total_asesores
  
)

por_pcia <-senadores_df1%>%group_by(pcia)%>%summarise(
  asesores_masculinos = sum(asesores_masculinos),
  asesores_femeninos = sum(asesores_femeninos),
  total_ases = asesores_masculinos+asesores_femeninos,
  porcentaje_ases_h = asesores_masculinos/total_ases,
  porcentaje_ases_m = asesores_femeninos/total_ases
  
)

por_partido <-senadores_df1%>%group_by(partido)%>%summarise(
  asesores_masculinos = sum(asesores_masculinos),
  asesores_femeninos = sum(asesores_femeninos),
  total_ases = asesores_masculinos+asesores_femeninos,
  porcentaje_ases_h = asesores_masculinos/total_ases,
  porcentaje_ases_m = asesores_femeninos/total_ases
  
)
senadores_df1[c('porcentaje_m','porcentaje_h')]
senadores_df1$pcia

ggplot(data = por_partido, aes(x =partido , y = porcentaje_ases_h)) +
  geom_bar(position = "fill", stat = 'identity')



# rshaping data to long form for ggplot2
library(reshape2)

por_partido3<-por_partido%>%mutate(
  alianza = case_when(
    partido  == "Alianza Cambiemos"  ~ 'Cambiemos y aliados',
    partido  == "Alianza Cambiemos San Juan"  ~ 'Cambiemos y aliados',
    partido  == "Alianza Encuentro Corrientes"  ~ 'Cambiemos y aliados',
    partido  == "Alianza Frente Cívico"  ~ 'Cambiemos y aliados',
    partido  == "Alianza Frente P/Victoria (Peronismo Pampeano)"  ~ 'Justicialismo y aliados',
    partido  == "Alianza Frente para la Victoria"  ~ 'Justicialismo y aliados',
    partido  == "Alianza Frente Progresista"  ~ 'Cambiemos y aliados',
    partido  == "Alianza Unen"  ~ 'Cambiemos y aliados',
    partido  == "Alianza Unión por Córdoba"  ~ 'Justicialismo y aliados',
    partido  == "Alianza Unión por Entre Ríos"  ~ 'Cambiemos y aliados',
    partido  == "Alianza Unión Pro"  ~ 'Cambiemos y aliados',
    partido  == "Avanzar y Cambiemos por San Luis"  ~ 'Cambiemos y aliados',
    partido  == "Cambiemos Buenos Aires"  ~ 'Cambiemos y aliados',
    partido  == "Cambiemos Fuerza Civica Riojana"  ~ 'Cambiemos y aliados',
    partido  == "Chubut Somos Todos"  ~ 'Justicialismo y aliados',
    partido  == "Frente Amplio Formoseño Cambiemos"  ~ 'Cambiemos y aliados',
    partido  == "Frente Cambiemos"  ~ 'Cambiemos y aliados',
    partido  == "Frente Cívico por Santiago"  ~ 'Otros',
    partido  == "Frente Cívico y Social"  ~ 'Cambiemos y aliados',
    partido  == "Frente de la Victoria"  ~ 'Justicialismo y aliados',
    partido  == "Frente Popular"  ~ 'Otros',
    partido  == "Frente Popular Salteño"  ~ 'Otros',
    partido  == "Frente Renovador de la Concordia"  ~ 'Justicialismo y aliados',
    partido  == "Frente Todos"  ~ 'Justicialismo y aliados',
    partido  == "Frente Unidad Justicialista San Luis"  ~ 'Justicialismo y aliados',
    partido  == "Movimiento Popular Fueguino"  ~ 'Justicialismo y aliados',
    partido  == "Movimiento Popular Neuquino"  ~ 'Justicialismo y aliados',
    partido  == "Unidad Ciudadana"  ~ 'Justicialismo y aliados',
    partido  == "Union para Vivir Mejor Cambiemos"  ~ 'Cambiemos y aliados',
    partido  == "Unión por Chaco"  ~ 'Otros',
    T~'Otros'
    
    
    
    
  )
)

por_partido4<-por_partido3%>%group_by(alianza)%>%summarise(
  asesores_masculinos = sum(asesores_masculinos),
  asesores_femeninos = sum(asesores_femeninos)
)
unique(por_partido$partido)
por_partido2<-por_partido[c('partido', 'asesores_masculinos','asesores_femeninos')]
meltd<- melt(por_partido4)
meltd$partido
#plot
library(ggplot2)
ggplot(meltd, aes(x=alianza, y=value, fill=variable)) +
  geom_bar(stat="identity", position = 'fill')+
  coord_flip()+ 
  scale_fill_manual("legend", values = c("asesores_masculinos" = "darkgreen", "asesores_femeninos" = "orange"))








library(reshape2)
por_pcia2<-por_pcia[c('pcia', 'asesores_masculinos','asesores_femeninos')]
meltd<- melt(por_pcia2)
#meltd$partido
#plot
library(ggplot2)
ggplot(meltd, aes(x=pcia, y=value, fill=variable)) +
  geom_bar(stat="identity", position = 'fill')+
  coord_flip()+ 
  scale_fill_manual("legend", values = c("asesores_masculinos" = "darkgreen", "asesores_femeninos" = "orange"))

library(tidyverse)
library(readxl)

#geodata_arg<-read_csv('argentina_df.csv')
gabinete <- read_excel("/media/lucariel/Cosas/Proyectos/p leg/gabinete.xlsx", 
                       col_names = FALSE)
colnames(gabinete)=c("Provincia","Partido","Ministerio","Nombre","Genero","Sector","asd")


gabinete['Mujer']=gabinete$Genero=='M'
gabinete['Varon']=-gabinete['Mujer']+1
gabinete$Mujer=as.numeric(gabinete$Mujer)

gabinete = gabinete %>% select(-Genero, -asd)

gabinete_pcia = gabinete %>% group_by(Provincia) %>% summarise(
  Mujeres = sum(Mujer),
  Varones = sum(Varon),
  Total = Mujeres+Varones,
  Prop_Mujeres = round(Mujeres/Total*100),
  Prop_Varones = round(Varones/Total*100)
)
gabinete_pcia=gabinete_pcia %>% mutate(
  Provincia = case_when(
    Provincia=="Ciudad Buenos Aires"~"Ciudad de Buenos Aires",
    Provincia=="Cordoba"~"Córdoba",
    Provincia=="Entre Rios"~"Entre Ríos",
    Provincia=="Neuquen"~"Neuquén",
    Provincia=="Rio Negro"~"Río Negro",
    Provincia=="Tucuman"~"Tucumán",
    Provincia=="Santaigo del  Estero"~"Santiago del Estero",
    T~Provincia
    
    
  )
)



# Load libraries
library(dplyr)        # data wrangling
library(cartogram)    # for the cartogram
library(ggplot2)      # to realize the plots
library(broom)        # from geospatial format to data frame
library(tweenr)       # to create transition dataframe between 2 states
library(gganimate)    # To realize the animation
library(maptools)     # world boundaries coordinates
library(viridis)      # for a nice color palette
colnames(gabinete_pcia)[colnames(gabinete_pcia)=='Provincia'] = "NAME_1"
gabinete_pcia[24,] = 100
gabinete_pcia$NAME_1[24]="Tierra del Fuego"
gabinete_pcia$Prop_Varones[24]=0
#gabinete_pcia$NAME_1[24]='Tierra Del Fuego'

library(sf)
#Encontrar otra proyeccion
ukgrid = "+init=epsg:3857"
arg <- spTransform(geodata_arg, CRS(ukgrid))
arg@data[24,]
gabinete_pcia[24,]
arg@data=right_join(arg@data,gabinete_pcia)
argc_v <- cartogram_cont(arg, "Prop_Varones", itermax=10)
argc_m <- cartogram_cont(arg, "Prop_Mujeres", itermax=10)
argdf= tidy(arg)
arg@data %>% View()
unique(arg@data$ID_1)
unique(argdf$id)
argdf$id=as.numeric(argdf$id)+1
argdf$id = factor(argdf$id, levels=unique(argdf$id))
arg_df <- argdf %>% left_join(tidy(arg) , arg@data) 

argc_df = tidy(argc)
unique(argc_df$id)
unique(argc@data$ID_1)
unique(arg_cartogram_df1$Prop_Mujeres)
arg_cartogram_df1 <- tidy(argc_v) %>% left_join(. , argc_v@data, by=c("id"="ID_1")) 
arg_cartogram_df <- tidy(argc_m) %>% left_join(. , argc_m@data, by=c("id"="ID_1")) 
arg_cartogram_df1$Prop_Mujeres
arg_cartogram_df1 %>% View()

Prop_Mujeres_plot = ggplot() +
  geom_polygon(data = arg_cartogram_df, aes(fill = Prop_Mujeres, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name="% Ministras", guide = guide_legend(label.position = "bottom", 
                                                              title.position = 'top', nrow=1)) +
  labs( title = "Argentina", subtitle="Ministras" )  +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.65, 0.2)
  ) 
require(gridExtra)
p = grid.arrange(Prop_Mujeres_plot, Prop_Varones_plot, ncol=2)
Prop_Varones_plot = ggplot() +
  geom_polygon(data = arg_cartogram_df1, aes(fill = Prop_Varones, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name="% Ministros", guide = guide_legend(label.position = "bottom", 
                                                              title.position = 'top', nrow=1)) +
  labs( title = "Argentina", subtitle="Ministros" )  +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.65, 0.2)
  ) 

arg_df
arg_cartogram_df
# Give an id to every single point that compose the boundaries
arg_cartogram_df$id <- seq(1,nrow(arg_cartogram_df))
arg_df$id <- seq(1,nrow(arg_df))

# Bind both map info in a data frame. 3 states: map --> cartogram --> map
data <- rbind(arg_df %>% mutate(time=1), arg_cartogram_df%>% mutate(time=2), arg_df%>% mutate(time=3))

# Set transformation type + time
data$ease <- "cubic-in-out"
data$time <- rep(c(1:3), each=nrow(arg_df))

# Calculate the transition between these 2 objects?
dt <- tween_elements(data, time='time', group='id', ease='ease', nframes = 5)
dt$
# check a few frame
p<- ggplot() + 
  geom_polygon(data = dt  %>% arrange(order) , aes(fill = Prop_Mujeres, x = long, y = lat, group = group, frame=.frame) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name="% Ministras", guide = guide_legend(label.position = "bottom", 
                                                              title.position = 'top', nrow=1)) +
  labs( title = "Argentina", subtitle="Ministras" )  +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.65, 0.2)
  ) 
