###  
library(tidyverse)
library(data.table)
library(openxlsx)
library(ggpubr)
library(ggpmisc)
library(insol)
library(mgcv)
# library(latex2exp)

#####
my_theme <- 
  theme_bw()+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size=12,face="bold",family = "serif"),
    strip.background = element_rect(fill = "white" ),
    axis.line = element_line(color = 'black'),
    axis.ticks.length.x = unit(-0.1,"cm"),
    axis.ticks.length.y = unit(-0.1,"cm"),
    axis.title.y = element_text(size = 12),
    text = element_text(family = "serif" ),
    axis.text = element_text(family = "serif" )
  )
#####

### Fig 5 ## Result of sim cells 
data.Fig5.Obs <- read.csv( "Data\\Obsdata\\ObserveCellTrait.csv" ) %>% 
  mutate( Col = paste(spe, "Obs",sep = "_") )
factor( data.Fig5.Obs$key) %>% levels()

data.Fig5.Sim <- list(
  read.xlsx("Data\\SimResults\\exp\\LSFMexp_Outputs.xlsx",sheet = 1) %>% mutate(spe= 'FM'),
  read.xlsx("Data\\SimResults\\exp\\LSFMexp_Outputs.xlsx",sheet = 2) %>% mutate(spe= 'FM'),
  read.xlsx("Data\\SimResults\\exp\\LSBPexp_Outputs.xlsx",sheet = 1) %>% mutate(spe= 'BP'),
  read.xlsx("Data\\SimResults\\exp\\LSBPexp_Outputs.xlsx",sheet = 2) %>% mutate(spe= 'BP')
)

names(data.Fig5.Sim) <- c( "FMring","FMcell","BPring","BPcell" )

## Fig 5


data.Fig5.Sim <- rbindlist( data.Fig5.Sim[c(2,4) ] ) %>%  
  left_join(rbindlist( data.Fig5.Sim[c(1,3) ] )) %>% 
  mutate(VRR = (Raddist )/RingWidth/10  ,
         CRR = (Raddist + 0.5* CRD)/RingWidth/10 ,
         Col = paste(spe, "Sim",sep = "_")) 

PlotYear1 <- seq(2016,2018)
pva <- 
  data.Fig5.Obs %>% filter(Year %in% PlotYear1,
                           key == "VesselLA"  ) %>% 
  ggplot()+
  my_theme+
  labs(title = "(c)",y =expression( paste("Vessel lumen area (",μm^2,")",sep = "" )) ,x = "Tree ring (%)")+
  theme(strip.placement = "inside")+ #,strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  
  geom_point( aes(x = RRadDistR, y = value,color= Col) ,size = 1,alpha=0.3,shape = 8 )+
  geom_smooth( aes(x = RRadDistR, y = value,color = Col) ,  method ='gam',se = F,formula = y~s(x,k=6))+
  
  geom_point( aes(x =VRR , y = VCV,color= Col ),# linewidth =1,
              data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 )  )+
  
  facet_grid( spe ~ Year,scales = 'free') ##,scales = "free"
pva



pca <-
  data.Fig5.Obs %>% filter(Year %in% PlotYear1,
                           key == "FiberLA") %>% 
  ggplot()+
  my_theme+
  labs(title = "(a)",y = expression( paste("Fiber lumen area (",μm^2,")" ,sep = "") ) ,x = NULL)+
  theme(strip.placement = "inside")+ #,strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  
  geom_point( aes(x = RRadDistR, y = value,color= Col),size = 1,alpha=0.1,shape = 8 )+ #
  geom_smooth( aes(x = RRadDistR, y = value,color= Col) ,  method ='gam',se = F,formula = y~s(x,k=6))+
  
  geom_point( aes(x =CRR , y = CV,color= Col),size =1,
              data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 )   )+
  facet_grid( spe ~ Year,scales = 'free') ##,scales = "free"
pca

pct <-
  data.Fig5.Obs %>% filter(Year %in% PlotYear1,
                           key == "FiberCWT") %>% 
  ggplot()+
  my_theme+
  labs(title = "(b)",y ="Fiber wall thickness (μm)" ,x = NULL)+
  theme(strip.placement = "inside",)+#axis.text.x = element_blank(),strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  
  geom_point( aes(x = RRadDistR, y = value,color= Col),size = 1,shape = 8,alpha=0.1)+ ##
  geom_smooth( aes(x = RRadDistR, y = value,color= Col) ,  method ='gam',se = F,formula = y~s(x,k=6))+
  
  geom_point( aes(x =CRR , y = WT,color= Col ),
              data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 )  )+
  
  facet_grid( spe ~ Year,scales = 'free')



ggarrange(pca,pct,pva,ncol = 1,align = "hv",common.legend = T,legend = "top")
ggsave("Fig5_cells.jpg",width =18,height = 22,units = "cm",dpi = 800)
### Fig 5 end -----

#### Fig S11

PlotYear1 <- seq(2010,2019)
pva <- 
  data.Fig5.Obs %>% filter(Year %in% PlotYear1,
                           key == "VesselLA"  ) %>% 
  ggplot()+
  my_theme+
  labs(title = "(c)",y =expression( paste("Vessel lumen area (",μm^2,")",sep = "" )) ,x = "Tree ring (%)")+
  theme(strip.placement = "inside")+ #,strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  
  geom_point( aes(x = RRadDistR, y = value,color= Col) ,size = 1,alpha=0.3,shape = 8 )+
  geom_smooth( aes(x = RRadDistR, y = value,color = Col) ,  method ='gam',se = F,formula = y~s(x,k=6))+
  
  geom_point( aes(x =VRR , y = VCV,color= Col ),# linewidth =1,
              data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 )  )+
  
  facet_grid( spe ~ Year,scales = 'free') ##
pva



pca <-
  data.Fig5.Obs %>% filter(Year %in% PlotYear1,
                           key == "FiberLA") %>% 
  ggplot()+
  my_theme+
  labs(title = "(a)",y = expression( paste("Fiber lumen area (",μm^2,")" ,sep = "") ) ,x = NULL)+
  theme(strip.placement = "inside")+ #,strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  geom_point( aes(x = RRadDistR, y = value,color= Col),size = 1,alpha=0.1,shape = 8 )+ #
  geom_smooth( aes(x = RRadDistR, y = value,color= Col) ,  method ='gam',se = F,formula = y~s(x,k=6))+
  
  geom_point( aes(x =CRR , y = CV,color= Col),size =1,
              data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 )   )+
  facet_grid( spe ~ Year,scales = 'free') ##,scales = "free"
pca

pct <-
  data.Fig5.Obs %>% filter(Year %in% PlotYear1,
                           key == "FiberCWT") %>% 
  ggplot()+
  my_theme+
  labs(title = "(b)",y ="Fiber wall thickness (μm)" ,x = NULL)+
  theme(strip.placement = "inside",)+#axis.text.x = element_blank(),strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  
  geom_point( aes(x = RRadDistR, y = value,color= Col),size = 1,shape = 8,alpha=0.1)+ ##
  geom_smooth( aes(x = RRadDistR, y = value,color= Col) ,  method ='gam',se = F,formula = y~s(x,k=6))+
  
  geom_point( aes(x =CRR , y = WT,color= Col ),
              data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 )  )+
  
  facet_grid( spe ~ Year,scales = 'free')



ggarrange(pca,pct,pva,ncol = 1,align = "hv",common.legend = T,legend = "top")

ggsave("FigS11_cells.jpg",width =32,height = 17,units = "cm",dpi = 600)


## Fig 6 Result of sim Vessels 

data.Fig6.Obs <- data.Fig5.Obs %>% filter(
  Year %in% c( 2010:2019),
  key == "VesselLA" ) %>% 
  arrange(Year, TID,RRadDistR ) %>% 
  mutate(VesselNumber = 1 ) %>% 
  group_by( Year,TID, spe, Col) %>% 
  reframe(
    RR =RR2, RRadDistR = RRadDistR,
    VesselNumber = cumsum(VesselNumber),
    CNV = max(VesselNumber ),
    NVN = VesselNumber/CNV*100 )

data.Fig6.Sim <- data.Fig5.Sim %>% 
  filter( !is.na( VEDOY  ) ) %>% 
  mutate( NVN = VNoV/VesselNumber *100 )


BP.lm<-lm(formula = NVN ~ RR+0, data = data.Fig6.Obs %>% filter( spe == 'BP') )
summary(BP.lm)
BP.formula <- sprintf('italic(y) == %.2f * italic(x)',round(coef(BP.lm)[1],2),digits = 2)
BP.R <- sprintf('italic(R^2) == %.2f',summary(BP.lm)$r.squared,round(summary(BP.lm)$r.squared,2),digits = 2)
BP.P <- 'italic(p) < 0.001'
BP.label <- data.frame(formula=BP.formula,r2=BP.R,P=BP.P, stringsAsFactors=F)
pbpv <- 
  ggplot(data = data.Fig6.Obs %>% filter( spe == 'BP') ,aes(x = RR , y = NVN))+
  my_theme+
  theme(#legend.position= c(0.1, .7),
    legend.background = element_blank() ,
    text = element_text(family = "serif"),
    axis.text.x = element_blank())+
  labs(title = "(a)",x = NULL ,y = "Normalized vessel number (%)")+
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Observed" ,"BP_Simulated","FM_Observed" ,"FM_Simulated"),
                       labels = c("BP_Observed" ,"BP_Simulated","FM_Observed" ,"FM_Simulated")  )+
  scale_fill_manual( name = NULL,values = c("#207f4c","#006AD0"),
                     breaks =  c("BP_VN" ,"FM_VN" ),
                     labels = c("BP_VN" ,"FM_VN" )  )+
  geom_point( aes(x = RR , y = NVN ,color = 'BP_Observed'))+
  
  scale_y_continuous(limits = c(0,100),
                     sec.axis = sec_axis(~.*4,name = "Vessel number",
                     ))+
  geom_histogram(aes( x = NVN,y = ..count.. /4, fill = "BP_VN" ),
                 binwidth=11,alpha = 0.3,colour="#2b333e")+

  geom_text(data=BP.label, mapping=aes(x=0, y=95, label=formula), parse=T, hjust = 0,inherit.aes=F,size=3, family="serif") + 
  geom_text(data=BP.label, mapping=aes(x=0, y=87, label=r2), parse=T, hjust = 0,inherit.aes=F,size=3, family="serif") +
  geom_text(data=BP.label, mapping=aes(x=0, y=79, label=P), parse=T, hjust = 0,inherit.aes=F,size=3, family="serif") +
  
  geom_point( data = data.Fig6.Sim %>% filter( spe == 'BP' ) ,aes(x = VRR , y = NVN,color = "BP_Simulated" ))+
  
  geom_smooth(method = "lm",color = "#2b333e",formula = y~x+0  )  
  
pbpv

FM.lm<-lm(formula = NVN ~ RR+0, data = data.Fig6.Obs %>% filter( spe == 'FM')  )
summary(FM.lm)
FM.formula <- sprintf('italic(y) == %.2f * italic(x)',0.91,digits = 2)
FM.R <- sprintf('italic(R^2) == %.2f',summary(FM.lm)$r.squared,round(summary(FM.lm)$r.squared,2),digits = 2)
FM.P <- 'italic(p) < 0.001'
FM.label <- data.frame(formula=FM.formula,r2=FM.R,P=FM.P, stringsAsFactors=F)
pfmv <- 
  ggplot(data = data.Fig6.Obs %>% filter( spe == 'BP') ,aes(x = RR , y = NVN))+
  my_theme+
  theme(#legend.position= c(0.1, .7),
    legend.background = element_blank() ,
    text = element_text(family = "serif"))+
  labs(title = "(b)",x = "Tree ring (%)",y = "Normalized vessel number (%)")+
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Observed" ,"BP_Simulated","FM_Observed" ,"FM_Simulated"),
                       labels = c("BP_Observed" ,"BP_Simulated","FM_Observed" ,"FM_Simulated")  )+
  scale_fill_manual( name = NULL,values = c("gray70","#006AD0"),
                     breaks =  c("BP_VN" ,"FM_VN" ),
                     labels = c("BP_VN" ,"FM_VN" )  )+
  scale_y_continuous(limits = c(0,100),
                     sec.axis = sec_axis(~.,name = "Vessel number",
                     ))+
  geom_point( aes(x = RR , y = NVN ,color = 'FM_Observed'))+
  
  scale_y_continuous(limits = c(0,100),
                     sec.axis = sec_axis(~.*4,name = "Vessels number",
                     ))+
  geom_histogram(aes( x = NVN,y = ..count.. /4, fill = "FM_VN" ),
                 binwidth=11,alpha = 0.3,colour="#2b333e")+
  
  geom_text(data=FM.label, mapping=aes(x=0, y=95, label=formula), parse=T, hjust = 0,inherit.aes=F,size=3, family="serif") + 
  geom_text(data=FM.label, mapping=aes(x=0, y=87, label=r2), parse=T, hjust = 0,inherit.aes=F,size=3, family="serif") +
  geom_text(data=FM.label, mapping=aes(x=0, y=79, label=P), parse=T, hjust = 0,inherit.aes=F,size=3, family="serif") +
  
  geom_point( data = data.Fig6.Sim %>% filter( spe == 'FM' ) ,aes(x = VRR , y = NVN,color = "FM_Simulated" ))+
  
  geom_smooth(method = "lm",color = "#2b333e",formula = y~x+0  )  
pfmv




ggarrange(pbpv,pfmv, ncol = 1,align = "hv")

ggsave("Fig6_Vessels.jpg",width = 16,height = 14,units = "cm",dpi=1000)

### Fig 6 end ------------

## Fig 7 S-shaped growth

data.Fig7.Sim <- list(
  read.xlsx("Data\\SimResults\\exp\\LSFMexp_Outputs.xlsx",sheet = 3) %>% mutate(spe= 'FM'),
  read.xlsx("Data\\SimResults\\exp\\LSBPexp_Outputs.xlsx",sheet = 3) %>% mutate(spe= 'BP')
) %>% 
  rbindlist() %>% filter( Year %in% c( 2010:2019)  ) %>% 
  mutate( 
    ELayer = CellLayer,
    TLayer = fiberInT + fiberMature,
    ERW = rwMature + rwInT + rwInE, TRW =  rwMature + rwInT  ) %>% 
  group_by( Year ,spe ) %>% 
  reframe (
    DOY = DOY, 
    maxCL = max(CellLayer),
    nELayer =  ELayer/maxCL*100,
    nTLayer =  TLayer/maxCL*100 ,
    nMLayer = fiberMature/maxCL*100,
    
    maxRW = max(RingWidth)*1000,
    nERW = ERW/maxRW*100 ,
    nTRW = TRW/maxRW*100  ,
    nMRW = rwMature/maxRW*100   ) 

head(data.Fig7.Sim)

data.Fig7.Sim.Cell <- data.Fig7.Sim %>%
  group_by(  spe,DOY ) %>% 
  summarise(
    MEL = max( nELayer ), mEL = min( nELayer),
    MTL = max( nTLayer ), mTL = min( nTLayer),
    MML = max( nMLayer ), mML = min( nMLayer) )

data.Fig7.Sim.Cell2 <- data.Fig7.Sim[,c(1:7)] %>% gather(key = key, value = val,c(5:7))

data.Fig7.Sim.Cell2$key <- factor(data.Fig7.Sim.Cell2$key , 
                                  levels = c('nELayer',"nTLayer", "nMLayer") ,
                                  labels = c("Enlargement" ,"Wall thickening","Lignification" )
                                  )
titles <- data.frame( spe = c('BP','FM') , t= c('(a)','(b)' ) )


ggplot( data.Fig7.Sim.Cell )+
  my_theme+
  theme( legend.position = "top", 
         strip.text = element_blank(),
         strip.background = element_blank() )+
  labs(y = "Normalized fiber layers (%)",x= "Day of Year (DOY)" )+
  scale_alpha_manual( values = c(1,0.4))+
  scale_x_continuous(limits = c(100,320))+
  scale_colour_manual( name = "",values = c("#009652","#007DFF","#E91E31"),
                       breaks =  c("Enlargement" ,"Wall thickening","Lignification" ),
                       labels =  c("Enlargement" ,"Wall thickening","Lignification" ) )+
  scale_fill_manual( name = "",values = c("#009652","#007DFF","#E91E31"),
                     breaks =  c("Enlargement" ,"Wall thickening","Lignification" ),
                     labels =  c("Enlargement" ,"Wall thickening","Lignification" ) )+
  
  geom_ribbon(  aes( ymin = mEL, ymax = MEL ,x = DOY ,fill = "Enlargement" ), alpha = 0.3 )+
  geom_ribbon(  aes( ymin = mTL, ymax = MTL ,x = DOY ,fill = "Wall thickening" ) , alpha = 0.3)+
  geom_ribbon(  aes( ymin = mML, ymax = MML ,x = DOY ,fill = "Lignification" ) , alpha = 0.3)+
  
  geom_smooth( aes(x = DOY, y = val ,color = key),
               data = data.Fig7.Sim.Cell2 ,
               method = "gam",se = F ,linewidth = 0.5 )+
  
  geom_text(  aes(y = 98 , x = 125, label = t  ),family = "serif",data = titles   )+
  
  facet_grid(.~spe)

ggsave("Fig7_S.jpg", width = 12,height = 6,units = "cm",dpi = 1000)


## Fig 8 Result of sim Tree ring width

Ddir<- dir( 'Data\\SimResults\\exp',full.names = T)
Ndir <- dir( 'Data\\SimResults\\exp') %>% str_sub(1,4)

RWdatas <-  map(Ddir, read.xlsx ) 
names(RWdatas) <- str_sub(Ndir)
RWdatas <-  rbindlist(RWdatas, use.names=TRUE, fill=TRUE, idcol="sName") %>% rename(MRW = RingWidth)

RWS <- read.xlsx("Data\\Obsdata\\RingWidths.xlsx") %>% rename(RingWidth = ring.width,sName = name) #%>% filter( spe== "FM")  ## spe == "FM"

head(RWS,2)
head(RWdatas,2)
Finaldata <-  left_join(RWS,RWdatas)

Finaldata$sName <- factor( Finaldata$sName, levels = c('BSFM','LSFM','LSBP', 'HNFM','JCFM','WYBP') ,
                           labels = c( c('Baishan','Liangshui',' Liangshui ', 'Hua\'nan','Jianchang','Wuying') )   )



ggplot(Finaldata)+
  my_theme+
  theme(legend.position = "top",
  )+
  # scale_x_continuous( limits = c(1990,2020   ) ,breaks = seq(1990,2020,5))+
  labs( y = expression(paste( "Ring width (mm)"  )  ) , x = 'Year' )+
  scale_color_manual( name = NULL,values =c("#006AD0","#F88802"),labels=c("Observed","Simulated" ) ,breaks = c("RingWidth","MRW" ) )+
  geom_line(aes( x = Year, y = RingWidth,color = "RingWidth" ) )+
  geom_line(aes( x = Year, y = MRW,color = "MRW" ))+
  facet_wrap(. ~ sName,scale= "free")


ggsave("Fig8_RWexp.jpg", width = 16,height = 10,units = "cm",dpi = 1000)

## Fig S14
Ddir<- dir( 'Data\\SimResults\\spl',full.names = T)
Ndir <- dir( 'Data\\SimResults\\spl') %>% str_sub(1,4)


RWdatas <-  map(Ddir, read.xlsx ) 
names(RWdatas) <- str_sub(Ndir)
RWdatas <-  rbindlist(RWdatas, use.names=TRUE, fill=TRUE, idcol="sName") %>% rename(MRW = RingWidth)

RWS <- read.xlsx("Data\\Obsdata\\RingWidths.xlsx") %>% rename(RingWidth = ring.width,sName = name) #%>% filter( spe== "FM")  ## spe == "FM"

head(RWS,2)
head(RWdatas,2)
Finaldata <-  left_join(RWS,RWdatas)

Finaldata$sName <- factor( Finaldata$sName, levels = c('BSFM','LSFM','LSBP', 'HNFM','JCFM','WYBP') ,
                           labels = c( c('Baishan','Liangshui',' Liangshui ', 'Hua\'nan','Jianchang','Wuying') )   )



ggplot(Finaldata)+
  my_theme+
  theme(legend.position = "top",
  )+
  labs( y = expression(paste( "Ring width (mm)"  )  ) , x = 'Year' )+
  scale_color_manual( name = NULL,values =c("#006AD0","#F88802"),labels=c("Observed","Simulated" ) ,breaks = c("RingWidth","MRW" ) )+
  geom_line(aes( x = Year, y = RingWidth,color = "RingWidth" ) )+
  geom_line(aes( x = Year, y = MRW,color = "MRW" ))+
  facet_wrap(. ~ sName,scale= "free")


ggsave("FigS14_RWspl.jpg", width = 16,height = 10,units = "cm",dpi = 1000)
