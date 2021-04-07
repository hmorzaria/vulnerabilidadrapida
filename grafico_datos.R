#' @title Graficas de indices de vulnerabilidad
#' @description  Graficas espaciales, por estado y por municipio
#' @details INPUT: 1) data
#' @details OUTPUT: 1) graficos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


grafico_datos <- function(cost.data.pob, datos.pca, datos.indice, tabla.edos, nom.indice, num.corte, nom.abr) {

   
    susc.res <- cost.data.pob %>% 
      dplyr::select(NOM_ENT, NOM_MUN, COM_ID, dec_lon, dec_lat) %>% 
      left_join(datos.indice, by="COM_ID") %>% 
      bind_cols(datos.pca) %>% 
      mutate(PCA_norm = (PC1-min(PC1))/(max(PC1)-min(PC1))) %>% 
      left_join(tabla.edos, by=c("NOM_ENT")) %>% 
      arrange(region)
    
    write_csv(susc.res,paste0(nom.abr,"_datos.csv"))
    
    
susc.coords <- susc.res

coordinates(susc.coords) <- c("dec_lon","dec_lat")

#definir proyeccion geografica y proyectar a utm
proj4string(susc.coords) <- crs.proj.wgs
susc.coords.sf <- st_as_sf(susc.coords)
susc.coords.proj <- st_transform(susc.coords.sf, crs.proj.utm) 

ent.ord <- susc.res  %>%  distinct(NOM_ENT) %>% pull(NOM_ENT)

susc.res.ord <-  susc.res %>% 
  mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord))    # This trick update the factor levels

boxplot.estados <- susc.res.ord %>% 
  ggplot()+
  geom_boxplot(aes(x = PCA_norm, y=NOM_ENT_fac, color = region), show.legend = TRUE) +
  scale_color_paletteer_d("NineteenEightyR::sunset2", name = "Región")+
  theme_light()+
  labs(y="Entidad",
       x=nom.indice)

ggsave(paste0(nom.indice,"_edos.png"), boxplot.estados, device="png", width = 8, height = 8)


if(nom.indice == "Susceptibilidad") {
  
  barplot.data <- susc.res %>% 
  filter(PCA_norm>num.corte) %>% 
  group_by(region,NOM_ENT,NOM_MUN) %>% 
  summarise(mean_pca=mean(PCA_norm), std_error_pca = std.error(PCA_norm)) %>% 
  mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord),
         std_error_max = mean_pca + std_error_pca,
         std_error_min = mean_pca + (-1*std_error_pca)) 
}

if(grepl("Exposición",nom.indice)) {
  
  barplot.data <- susc.res %>% 
    filter(PCA_norm>num.corte) %>% 
    group_by(region,NOM_ENT,NOM_MUN) %>% 
    summarise(mean_pca=mean(PCA_norm), std_error_pca = std.error(PCA_norm)) %>% 
    mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord),
           std_error_max = mean_pca + std_error_pca,
           std_error_min = mean_pca + (-1*std_error_pca)) 
}
  
if(nom.indice == "Capacidad de adaptación") {

  barplot.data <- susc.res %>% 
  filter(PCA_norm<num.corte) %>% 
  group_by(region,NOM_ENT,NOM_MUN) %>% 
  summarise(mean_pca=mean(PCA_norm), std_error_pca = std.error(PCA_norm)) %>% 
  mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord),
         std_error_max = mean_pca + std_error_pca,
         std_error_min = mean_pca + (-1*std_error_pca))
}
 

barplot.municipios <- barplot.data %>% 
  #   std_error_susc_max = if_else(is.na(std_error_susc_max),0,std_error_susc_max),
  #   std_error_susc_min = if_else(is.na(std_error_susc_min),0,std_error_susc_min)) %>%    # This trick update the factor levels
  ggplot()+
  # geom_bar(aes(y= mean_susc, x=NOM_MUN, fill=region), stat="identity", show.legend = FALSE) +
  geom_col(aes(y= mean_pca, x=NOM_MUN, fill=region), show.legend = FALSE) +
    geom_errorbar(aes(y= mean_pca, x=NOM_MUN, ymin = std_error_min, ymax = std_error_max), color="gray50") +
  # scale_fill_brewer()+
  facet_wrap(~ NOM_ENT_fac, scales = "free") +
  coord_flip() +
  theme_light() +
  labs(x = "Municipio",
       y=paste(nom.indice,"media")) 

if(nom.indice == "Capacidad de adaptación") {

  barplot.municipios <- barplot.municipios  +
    scale_y_continuous(breaks = c(0,0.10,0.20, 0.30),
                       limits = c(0,0.3))
  
}


if(nom.indice == "Exposición" | nom.indice == "Susceptibilidad") {
  
  barplot.municipios <- barplot.municipios  +
    scale_y_continuous(breaks = c(0,0.25,0.5, 0.75, 1),
                       limits = c(0,1))
  
}

ggsave(paste0(nom.indice,"_mun.png"), barplot.municipios, device="png", width = 18, height = 15)


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#simbolos http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

mapa.susc1 <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = susc.coords.proj, aes(size=PCA_norm, color = PCA_norm), shape = 16) +
  scale_colour_steps(low="aliceblue", high = "dodgerblue4", breaks = c(0.25, 0.5,0.75)) +
 # geom_sf(data = est.2019, fill= NA) + 
  coord_sf(xlim = c(-118, -109), ylim = c(22, 33), expand = FALSE) +
  xlab("Longitud") + ylab("Latitud") +
  theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                        size = 0.1)) +
  theme(legend.position = "none") +
  theme_light() +
  labs(color = nom.indice,
       size = nom.indice)

mapa.susc2 <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = susc.coords.proj, aes(size=PCA_norm, color = PCA_norm), shape = 16) +
  scale_colour_steps(low="aliceblue", high = "dodgerblue4", breaks = c(0.25, 0.5,0.75)) +
#  geom_sf(data = est.2019, fill= NA) + 
  coord_sf(xlim = c(-109, -97), ylim = c(12, 28), expand = FALSE) +
  xlab("Longitud") + ylab("Latitud") +
  theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                        size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
  theme(legend.position = "none")

mapa.susc3 <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = susc.coords.proj, aes(size=PCA_norm, color = PCA_norm), shape = 16) +
  scale_colour_steps(low="aliceblue", high = "dodgerblue4", breaks = c(0.25, 0.5,0.75)) +
#  geom_sf(data = est.2019, fill= NA) + 
  #geom_text(data = estados.mx, aes(X, Y, label = NOM_ENT), size = 5) +
  coord_sf(xlim = c(-97, -85), ylim = c(12, 28), expand = FALSE) +
  xlab("Longitud") + ylab("Latitud") +
  theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                        size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
  theme(legend.position = "none")

#map.grid <- grid.arrange(mapa.susc1, mapa.susc2, mapa.susc3, nrow=2,ncol=2, widths = c(1.5,2),
#heights= c(1.5,2), padding = 0.01)

#map.grid <-ggarrange(mapa.susc1, mapa.susc2, mapa.susc3, widths = c(2,2), heights = c(1,2), ncol = 2, nrow = 2)

#ggsave("susceptibilidad.png", map.grid,device="png", width = 8)


mapa.susc.panel <- mapa.susc1 + mapa.susc2 + mapa.susc3 + guide_area() + 
  plot_layout(guides = 'collect')

ggsave(paste0(nom.indice,".png"), mapa.susc.panel, device="png", width = 10, height = 12)

}