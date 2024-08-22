### Fig 5 - 8 

#### packages #### 
###  
library(tidyverse)
library(data.table)
library(openxlsx)
library(ggpubr)
library(ggpmisc)
# library(gghalves)
library(ggsignif)
# library(insol)
library(mgcv)
# library(latex2exp)

#### theme ####
my_theme <- 
  theme_bw()+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size=12,face="bold",family = "serif",color = 'black'),
    strip.background = element_rect(fill = "white" ),
    axis.line = element_line(color = 'black',linewidth = 0),
    panel.border =  element_rect(color = 'black',linewidth = 1),
    axis.ticks.length.x = unit(-0.1,"cm"),
    axis.ticks.length.y = unit(-0.1,"cm"),
    axis.title.y = element_text(size = 12),
    text = element_text(family = "serif",color = 'black' ),
    axis.text = element_text(family = "serif",color = 'black' )
  )
#####

#### read Results ####

LSFM <- list(
  annaulRing = read.xlsx("Data\\SimResults\\LSFM_Outputs.xlsx",sheet = "annaulRing"),
  xylem_trait = read.xlsx("Data\\SimResults\\LSFM_Outputs.xlsx",sheet = "xylem_trait"),
  IntraAnnualGrowth = read.xlsx("Data\\SimResults\\LSFM_Outputs.xlsx",sheet = "IntraAnnualGrowth")
)

LSBP <- list(
  annaulRing = read.xlsx("Data\\SimResults\\LSBP_Outputs.xlsx",sheet = "annaulRing"),
  xylem_trait = read.xlsx("Data\\SimResults\\LSBP_Outputs.xlsx",sheet = "xylem_trait"),
  IntraAnnualGrowth = read.xlsx("Data\\SimResults\\LSBP_Outputs.xlsx",sheet = "IntraAnnualGrowth")
)

AllsiteRW <- list(
  BSFM = read.xlsx("Data\\SimResults\\BSFM_Outputs.xlsx",sheet = "annaulRing") |>
   dplyr::select( Year, RingWidth ), #|> mutate( Site = "BSFM" ),
  HNFM = read.xlsx("Data\\SimResults\\HNFM_Outputs.xlsx",sheet = "annaulRing") |>
    dplyr::select( Year, RingWidth ), #|> mutate( Site = "HNFM" ),
  JCFM = read.xlsx("Data\\SimResults\\JCFM_Outputs.xlsx",sheet = "annaulRing") |>
    dplyr::select( Year, RingWidth ), #|> mutate( Site = "JCFM" ),
  LSFM = read.xlsx("Data\\SimResults\\LSFM_Outputs.xlsx",sheet = "annaulRing") |>
    dplyr::select( Year, RingWidth ), #|> mutate( Site = "LSFM" ),
  LSBP = read.xlsx("Data\\SimResults\\LSBP_Outputs.xlsx",sheet = "annaulRing") |>
    dplyr::select( Year, RingWidth ), #|> mutate( Site = "LSBP" ),
  WYBP = read.xlsx("Data\\SimResults\\WYBP_Outputs.xlsx",sheet = "annaulRing") |>
    dplyr::select( Year, RingWidth ) #|> mutate( Site = "WYBP" )
) |> rbindlist(use.names = T,fill = T,idcol = T) |>
  rename( Sname =  .id  )

#### read OBS ####
data.Fig5.Obs <- fread( "Data\\Obsdata\\ObserveCellTrait.csv" ) %>% 
  mutate( Col = paste(spe, "Obs",sep = "_") ) 
factor( data.Fig5.Obs$key) %>% levels()

# data.Fig6.Obs <- read.csv("Data/Obsdata/ObsV.csv")

ObsLS <- read.csv("Data\\Obsdata\\BRS2.csv")

RingWidthS <- read.xlsx("Data\\Obsdata\\RingWidths.xlsx") 

#### model test ####
LsFMs  <- ObsLS |> filter(spe == "FM") |> inner_join( LSFM$annaulRing)
LsBPs  <- ObsLS |> filter(spe == "BP") |> inner_join( LSBP$annaulRing)

test.LsSite <- rbind(
  BPRW = rBTR::mod.test( y_actual = LsBPs$MRW, y_predicted = LsBPs$RingWidth   ),
  FMRW =  rBTR::mod.test( LsFMs$MRW,LsFMs$RingWidth  ),
  
  BPCD = rBTR::mod.test( LsBPs$CD,LsBPs$VesselDensity   ),
  FMCD = rBTR::mod.test( LsFMs$CD,LsFMs$VesselDensity   )
)

RW5sites <-  RingWidthS |> rename(MRW = RingWidth ) |>
  inner_join(  AllsiteRW   )

RW5sitesFig8 <-  RingWidthS |> rename(MRW = RingWidth ) |>
  full_join(  AllsiteRW   )

test.AllSiteRW <- rbind(
  BSFM = rBTR::mod.test( y_actual = RW5sites$MRW[RW5sites$Sname == "BSFM"], y_predicted = RW5sites$RingWidth[RW5sites$Sname == "BSFM"]   ),
  HNFM = rBTR::mod.test( y_actual = RW5sites$MRW[RW5sites$Sname == "HNFM"], y_predicted = RW5sites$RingWidth[RW5sites$Sname == "HNFM"]   ),
  LSFM = rBTR::mod.test( y_actual = RW5sites$MRW[RW5sites$Sname == "LSFM"], y_predicted = RW5sites$RingWidth[RW5sites$Sname == "LSFM"]   ),
  JCFM = rBTR::mod.test( y_actual = RW5sites$MRW[RW5sites$Sname == "JCFM"], y_predicted = RW5sites$RingWidth[RW5sites$Sname == "JCFM"]   ),
  LSBP = rBTR::mod.test( y_actual = RW5sites$MRW[RW5sites$Sname == "LSBP"], y_predicted = RW5sites$RingWidth[RW5sites$Sname == "LSBP"]   ),
  WYBP = rBTR::mod.test( y_actual = RW5sites$MRW[RW5sites$Sname == "WYBP"], y_predicted = RW5sites$RingWidth[RW5sites$Sname == "WYBP"]   )
) %>% round(2) %>% mutate(COR = paste0("r = ",COR,"**")) |> rownames_to_column("Sname")

## CellsFM 

FM.ObsVa <- data.Fig5.Obs %>%  filter( key == 'VesselLA', spe== "FM") %>% rename( LA = value)
FM.ObsFa <- data.Fig5.Obs %>%  filter( key == 'FiberLA', spe== "FM") %>% rename( LA = value)
FM.ObsFw <- data.Fig5.Obs %>%  filter( key == 'FiberCWT', spe== "FM") %>% rename( CWTall = value)

LSFMcells <- list(
  # FLA = rBTR::mod.test.cells( devOutputs = LSFM ,OBSdata = FM.ObsFa,celltype = 'fiber', Years = c(2010: 2019) ,CellParam = "LA", method = 'spearman',k=6 ),
  FLA = rBTR::mod.test.cells( devOutputs = LSFM ,OBSdata = FM.ObsFa,celltype = 'fiber', Years = c(2010: 2019) ,CellParam = "LA" ,k=6) ,

  # FWT = rBTR::mod.test.cells( devOutputs = LSFM ,OBSdata = FM.ObsFw,celltype = 'fiber', Years = c(2010: 2019) ,CellParam = "CWTall", method = 'spearman' ,k=6),
  FWT = rBTR::mod.test.cells( devOutputs = LSFM ,OBSdata = FM.ObsFw,celltype = 'fiber', Years = c(2010: 2019) ,CellParam = "CWTall" ,k=3) ,

  # VLA = rBTR::mod.test.cells( devOutputs = LSFM ,OBSdata = FM.ObsVa,celltype = 'vessel', Years = c(2010: 2019) ,CellParam = "LA", method = 'spearman',k=6 )
  VLA = rBTR::mod.test.cells( devOutputs = LSFM ,OBSdata = FM.ObsVa,celltype = 'vessel', Years = c(2010: 2019) ,CellParam = "LA",k=4 )
) |> rbindlist() |> dplyr::select(Year, celltype,CellParam,COR,pvalue ,rsquare, RMSE,NRMSD,MAE,MAPE) |>
  mutate( COR = sprintf("%.2f", as.numeric(COR)) , 
          res = case_when(as.numeric(pvalue)  >0.05~ paste0("r = ",COR), as.numeric(pvalue) <=0.05 ~ paste0("r = ",COR,"*" )   ))

## Cells BP
BP.ObsVa <- data.Fig5.Obs %>%  filter( key == 'VesselLA', spe== "BP") %>% rename( LA = value)
BP.ObsFa <- data.Fig5.Obs %>%  filter( key == 'FiberLA', spe== "BP") %>% rename( LA = value)
BP.ObsFw <- data.Fig5.Obs %>%  filter( key == 'FiberCWT', spe== "BP") %>% rename( CWTall = value)

LSBPcells <- list(
  FLA = rBTR::mod.test.cells( devOutputs = LSBP ,OBSdata = BP.ObsFa,celltype = 'fiber', Years = c(2010: 2019) ,CellParam = "LA", method = 'spearman',k=4 ),
  # FLA = rBTR::mod.test.cells( devOutputs = LSBP ,OBSdata = BP.ObsFa,celltype = 'fiber', Years = c(2010: 2019) ,CellParam = "LA" ,k=6) ,
  
  FWT = rBTR::mod.test.cells( devOutputs = LSBP ,OBSdata = BP.ObsFw,celltype = 'fiber', Years = c(2010: 2019) ,CellParam = "CWTall", method = 'spearman' ,k=3),
  # FWT = rBTR::mod.test.cells( devOutputs = LSBP ,OBSdata = BP.ObsFw,celltype = 'fiber', Years = c(2010: 2019) ,CellParam = "CWTall" ,k=3) ,
  
  VLA = rBTR::mod.test.cells( devOutputs = LSBP ,OBSdata = BP.ObsVa,celltype = 'vessel', Years = c(2010: 2019) ,CellParam = "LA", method = 'spearman',k=4 )
  # VLA = rBTR::mod.test.cells( devOutputs = LSBP ,OBSdata = BP.ObsVa,celltype = 'vessel', Years = c(2010: 2019) ,CellParam = "LA",k=6 )
) |> rbindlist() |> dplyr::select(Year, celltype,CellParam,COR,pvalue ,rsquare, RMSE,NRMSD,MAE,MAPE) |>
  mutate( COR = sprintf("%.2f", as.numeric(COR)) , 
          res = case_when(as.numeric(pvalue)  >0.05~ paste0("r = ",COR), as.numeric(pvalue) <=0.05 ~ paste0("r = ",COR,"*" )   ))

LsCells <- bind_rows( 
  mutate( LSFMcells, spe = "FM"),
  mutate(LSBPcells, spe = "BP")
  )


## Figue s 

data.Fig5.Sim <- bind_rows(
  mutate(LSFM$xylem_trait, spe = "FM", Col = "FM_Sim"),
  mutate(LSBP$xylem_trait, spe = "BP", Col = "BP_Sim")  )


# PlotYear1 <- c(2016:2018) ; sizer = 4 ### Fig 5
# PlotYear1 <- c(2010:2018); sizer = 3 ## Fig S11
pva <- 
  data.Fig5.Obs %>% filter(Year %in% PlotYear1,
                           key == "VesselLA", !is.na( TID )  ) %>% 
  ggplot()+
  my_theme+
  labs(title = "(c)",y =expression( paste("Vessel lumen area (",μm^2,")",sep = "" )) ,x = "Tree ring (%)")+
  theme(strip.placement = "inside")+ #,strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  
  # scale_linewidth_manual(   )+
  
  scale_linetype_manual(name = "Tree ID",values = c('longdash','dashed', "twodash","dotted",'longdash','dashed', "twodash","dotted","dotdash","twodash" ),
                        # breaks = c( 'BP01' ,'BP06','BP08','FM01','FM03','FM05','FM07','FM08'  )
                        )+
  
  geom_smooth( aes(x = RRadDistR, y = value,color = Col ,linetype = TID ) , linewidth = 0.4 , # linetype = 'longdash',  ### 
               method ='gam',se = F,formula = y~s(x,k=6),alpha = 0.6 ) +  
  
  # geom_point( aes(x = RRadDistR, y = value,color= Col) ,size = 1,alpha=0.3,shape = 8 )+
  
  
  
  geom_point( aes(x =rrVessel , y = VCV,color= Col ),# linewidth =1,
              data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 )  )+
  geom_smooth( aes(x = RRadDistR, y = value,color = Col) ,  method ='gam',se = F,formula = y~s(x,k=6),linewidth = 0.75)+  
  geom_smooth( aes(x =rrVessel , y = VCV,color= Col ),# linewidth =1,
               data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 ) , alpha = 0.6,  
               method ='gam',se = F,formula = y~s(x,k=6) ,linewidth = 0.75 )+
  
  geom_text( data = LsCells %>% filter( celltype == "vessel", CellParam == "LA" ,Year %in% PlotYear1 ),
    aes(x = Inf, y = Inf, label = res,  vjust = 1.6,   hjust = 1.05),
    inherit.aes = FALSE, size = sizer,   family = "serif", fontface = "italic" )+
  
  facet_grid( spe ~ Year,scales = 'free') ##,scales = "free"
pva



pca <-
  data.Fig5.Obs %>% filter(Year %in% PlotYear1,
                           key == "FiberLA", !is.na( TID )) %>% 
  ggplot()+
  my_theme+
  labs(title = "(a)",y = expression( paste("Fiber lumen area (",μm^2,")" ,sep = "") ) ,x = NULL)+
  theme(strip.placement = "inside")+ #,strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  
  scale_linetype_manual(name = "Tree ID",values = c('longdash','dashed', "twodash",'longdash','dashed', "twodash","dotted","dotdash" ),
                        breaks = c( 'BP01' ,'BP06','BP08','FM01','FM03','FM05','FM07','FM08'  ) )+
  geom_smooth( aes(x = RRadDistR, y = value,color = Col ,linetype = TID ) , linewidth = 0.4 ,   ### linewidth = 1 ,
               method ='gam',se = F,formula = y~s(x,k=4),alpha = 0.6)+  
  
  # geom_point( aes(x = RRadDistR, y = value,color= Col),size = 1,alpha=0.1,shape = 8 )+ #
  
  
  geom_point( aes(x =rrFiber , y = CV,color= Col),size =1,
              data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 )  )+
  geom_smooth( aes(x = RRadDistR, y = value,color= Col) ,  method ='gam',se = F,formula = y~s(x,k=4),linewidth = 0.75)+  
  geom_smooth( aes(x =rrFiber , y = CV,color= Col),linewidth = 0.75 ,
               data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 ),  
               method ='gam',se = F,formula = y~s(x,k=4) , alpha = 0.6 )+
  
  geom_text( data = LsCells %>% filter( celltype == "fiber", CellParam == "LA" ,Year %in% PlotYear1 ),
             aes(x = Inf, y = Inf, label = res,  vjust = 1.6,   hjust = 1.05),
             inherit.aes = FALSE, size = sizer,   family = "serif", fontface = "italic" )+
  
  
  facet_grid( spe ~ Year,scales = 'free') ##,scales = "free"
pca

pct <-
  data.Fig5.Obs %>% filter(Year %in% PlotYear1,
                           key == "FiberCWT", !is.na( TID )) %>% 
  ggplot()+
  my_theme+
  labs(title = "(b)",y ="Fiber wall thickness (μm)" ,x = NULL)+
  theme(strip.placement = "inside",)+#axis.text.x = element_blank(),strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  
  scale_linetype_manual(name = "Tree ID",values = c('longdash','dashed', "twodash",'longdash','dashed', "twodash","dotted","dotdash" ),
                        breaks = c( 'BP01' ,'BP06','BP08','FM01','FM03','FM05','FM07','FM08'  ) )+
  geom_smooth( aes(x = RRadDistR, y = value,color = Col ,linetype = TID ) , linewidth = 0.4 ,   ### linewidth = 1 ,
               method ='gam',se = F,formula = y~s(x,k=3),alpha = 0.6)+  
  
  # geom_point( aes(x = RRadDistR, y = value,color= Col),size = 1,shape = 8,alpha=0.1)+ ##
  
  
  geom_point( aes(x =rrFiber , y = WT,color= Col ),
              data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 ) )+
  
  geom_smooth( aes(x = RRadDistR, y = value,color= Col) ,  method ='gam',se = F,formula = y~s(x,k=3),linewidth = 0.75)+  
  geom_smooth( aes(x =rrFiber , y = WT,color= Col),
               data = data.Fig5.Sim %>% filter(Year %in% PlotYear1 ),  
               method ='gam',se = F,formula = y~s(x,k=4) ,linewidth = 0.75 , alpha = 0.6)+
  
  geom_text( data = LsCells %>% filter( celltype == "fiber", CellParam == "CWTall" ,Year %in% PlotYear1 ),
             aes(x = Inf, y = Inf, label = res,  vjust = 1.6,   hjust = 1.05),
             inherit.aes = FALSE, size = sizer,   family = "serif", fontface = "italic" )+
  
  facet_grid( spe ~ Year,scales = 'free')
pct


ggarrange(pca,pct,pva,ncol = 1,align = "hv",common.legend = T,legend = "right")
ggsave("Fig5_cells.jpg",width =18,height = 20,units = "cm",dpi = 800)
# ggsave("FigS11_cells.jpg",width =32,height = 20,units = "cm",dpi = 800)

## Fig 6

Fig6a <-    
  data.Fig5.Obs %>% filter( Year %in% c(2010:2019), #PlotYear1,
                           key == "VesselLA", !is.na( TID )  ) %>% 
ggplot()+
  my_theme+
  labs(title = "(a)",y =expression( paste("Vessel lumen area (",μm^2,")",sep = "" )) ,x = "Tree ring (%)")+
  theme(strip.placement = "inside")+ #,strip.background.x = element_blank(),strip.text.x = element_blank()
  scale_colour_manual( name = "",values = c("#207f4c","#F9C116","#006AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
  
  scale_linewidth_manual( name = "Year", values = rep(0.4,10) )+
  # geom_smooth( aes(x = RRadDistR, y = value,color = Col ,linewidth =factor(Year)    ) , linetype = 'longdash',  ### linewidth = 1 ,
  #              method ='gam',se = F,formula = y~s(x,k=6))+  
  
  geom_point( aes(x = RRadDistR, y = value,color= Col) ,size = 1,alpha=0.3,shape = 20 )+
  
  
  
  geom_point( aes(x =rrVessel , y = VCV,color= Col ),# linewidth =1,
              data = data.Fig5.Sim %>% filter(Year %in% c(2010:2019) ) ,alpha = 0.6 )+ ## 
  geom_smooth( aes(x = RRadDistR, y = value,color = Col) ,  method ='gam',se = F,formula = y~s(x,k=6),linewidth = 0.75)+  
  geom_smooth( aes(x =rrVessel , y = VCV,color= Col ),# linewidth =1,
               data = data.Fig5.Sim %>% filter(Year %in% c(2010:2019) ) , alpha = 0.6,  
               method ='gam',se = F,formula = y~s(x,k=6) ,linewidth = 0.75 )+
  
  geom_text( data = LsCells %>% filter( celltype == "vessel", CellParam == "LA" ,Year == "all" ),
             aes(x = Inf, y = Inf, label = res,  vjust = 1.6,   hjust = 1.05),
             inherit.aes = FALSE, size = 4,   family = "serif", fontface = "italic" )+
  
  facet_grid( spe ~ .,scales = 'free') ##,scales = "free"
  
  
  ## 区间导管密度 
  dtALL <- data.Fig5.Obs %>% filter(path == "Vessel", Year >= 2010 ) %>% 
    group_by( TID,Year ) %>% 
    summarise( VNall = n()   )
    
  data.Fig6.Obs <- data.Fig5.Obs %>% filter(path == "Vessel", Year > 2010) %>% 
    mutate(  Part = cut(RRadDistR,c(0,20,40,60,80,100), c("0~20","20~40","40~60","60~80","80~100") ) ) %>% 
    left_join(dtALL) %>% 
    group_by( TID,Year,Part,spe ) %>% ## 
    summarise( VN = n() , VNall = unique(VNall), sVN = VN/VNall,Col = unique(Col) ,path = 'Obs'  )
  
  data.Fig6.Sim <- data.Fig5.Sim %>% dplyr::select( Year,spe,Col,VVN,rrVessel ,VNoV  ) %>% 
    filter(!is.na(VVN), Year > 2010 ) %>% 
    group_by(   Year,spe,Col ) %>% 
    reframe(  VVN = VVN  ,VNoV = max(VNoV) , Part = cut(rrVessel , c(0,20,40,60,80,100) , c("0~20","20~40","40~60","60~80","80~100"))    ) %>%
    group_by(   Year,spe,Col,Part ) %>%
    summarise( VN = n(),VNall = unique(VNoV) , sVN = VN/VNall , Col = unique(Col), TID = "Sim",path = 'Sim' )  ## 
  
  # library(ggbeeswarm)
  
Fig6b <- bind_rows( data.Fig6.Sim ,data.Fig6.Obs ) %>%  
  ggplot( aes(x= Part  , y =sVN ,fill = Col  ) )+## aes(x= factor( Part ) , y =sVN ,fill = Col , group = path ,side = path)

    stat_boxplot(geom = "errorbar", width=0.3,position = position_dodge(0.6), linewidth = 0.3  )+ 
  
    # geom_boxplot(aes(group =Col ,fill= "white" ), outliers = F, position = position_dodge(0.6), width = 0.5 , linewidth = 0.2 ,alpha = 0.8)+
                 
    geom_boxplot( outliers = F, position = position_dodge(0.6), width = 0.5 , linewidth = 0.3 )+
    
    stat_compare_means(  method = 'anova' , aes(label = ..p.signif..),vjust = 0.9 ,family = 'serif'   )+ ## 

    facet_grid( spe ~ .,scales = 'free') + ##,scales = "free"
    
    my_theme+
    labs(title = "(b)",y = "Ratio of vessel number" ,x = "Tree ring (%)")+
    theme(strip.placement = "inside")+ #,strip.background.x = element_blank(),strip.text.x = element_blank()
 
    scale_color_manual( name = "",values = c("#208f4c","#F9C116","#008AD0","#F88802"),
                       breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                       labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )+
    scale_fill_manual( name = "",values = c("#208f4c","#F9C116","#008AD0","#F88802"),
                         breaks =  c("BP_Obs" ,"BP_Sim","FM_Obs" ,"FM_Sim"),
                         labels = c("Observed BP" ,"Simulated BP","Observed FM" ,"Simulated FM")  )

ggarrange(Fig6a,Fig6b, ncol = 1 , align = 'hv')  
ggsave("Fig6_Vessels.jpg",width = 16,height = 14,units = "cm",dpi=800)


## Fig 7
data.Fig7.Sim <- list(
  read.xlsx("Data\\SimResults\\LSFM_Outputs.xlsx",sheet = 3) %>% mutate(spe= 'FM'),
  read.xlsx("Data\\SimResults\\LSBP_Outputs.xlsx",sheet = 3) %>% mutate(spe= 'BP')
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

ggsave("Fig7_Shape.jpg", width = 12,height = 6,units = "cm",dpi = 800)

## Fig 8 Result of sim Tree ring width

head( RW5sites ) 

Finaldata <- RW5sitesFig8 %>% mutate( Sname = factor(Sname , levels = c('BSFM','LSFM','LSBP', 'HNFM','JCFM','WYBP') ,
                           labels = c( c('Baishan','Liangshui',' Liangshui ', 'Hua\'nan','Jianchang','Wuying') )   )   )

Fig.AllSiteTest <- test.AllSiteRW %>%  
  mutate( Sname = factor(Sname , levels = c('BSFM','LSFM','LSBP', 'HNFM','JCFM','WYBP') ,
          labels = c( c('Baishan','Liangshui',' Liangshui ', 'Hua\'nan','Jianchang','Wuying') )   ) ,
          Figs = c( '(a)','(d)','(b)','(e)','(c)','(f)' )
          )

ggplot(Finaldata )+ ## [Finaldata$Year >= 1930,]
  my_theme+
  theme(legend.position = "top",
  )+
  scale_x_continuous( limits = c(1920,2020   ) ,breaks = seq(1920,2020,20))+
  labs( y = expression(paste( "Ring width (mm)"  )  ) , x = 'Year' )+
  scale_color_manual( name = NULL,values =c("#F88802","#006AD0"),labels=c("Simulated","Observed" ) ,breaks = c("RingWidth", "MRW") )+

  geom_line(aes( x = Year, y = MRW,color = "MRW" ))+
  geom_line(aes( x = Year, y = RingWidth,color = "RingWidth" ),alpha =0.8 )+  
  
  geom_text( data = Fig.AllSiteTest,
             aes(x = -Inf, y = Inf, label =  COR ,  vjust = 1.6,   hjust = -0.5),
             inherit.aes = FALSE, size = 3.5,   family = "serif", fontface = "italic" )+
  
  geom_text( data = Fig.AllSiteTest,
             aes(x = -Inf, y = Inf, label = Figs,  vjust = 1.6, hjust = -0.5  ),#
             inherit.aes = FALSE, size = 3.5,   family = "serif" )+
  
  
  
  facet_wrap(. ~ Sname,scale= "free_y")


ggsave("Fig8_RW.jpg", width = 16,height = 10,units = "cm",dpi = 1000)




## Fig S12 
test.LsSite$spe <- c('BP','FM','BP','FM')


ggplot( )+ 
  geom_line( aes(x = Year , y = CD , color = "CD" ) ,data = LsBPs  )+
  geom_line( aes(x = Year , y = VesselDensity , color = "VesselDensity" ) ,data = LsBPs  )+
  geom_line( aes(x = Year , y = CD , color = "CD" ) ,data = LsFMs  )+
  geom_line( aes(x = Year , y = VesselDensity , color = "VesselDensity" ) ,data = LsFMs  )+
  facet_grid( spe ~ .,scales = "free_y"  )+
  
  geom_text( data = test.LsSite[c(3,4),] ,
             aes(x = Inf, y = Inf, label = paste0("r = ",round(COR,2),"* ",
                                                  "  MAE = ", round( MAE,2),
                                                  "  MAPE = ", round(MAPE,2)
                                                    ) ,  vjust = 1.6, hjust = 1.5 ),#
             inherit.aes = FALSE, size = 4,   family = "serif" , fontface = "italic")+

  my_theme+
  theme(legend.position = "top",
  )+
  scale_x_continuous( limits = c(1964,2020   ) ,breaks = seq(1964,2020,10))+
  labs( y = expression(paste( "Vessel density ( N· ",mm^-2, " )"  )  ) , x = 'Year' )+
  scale_color_manual( name = NULL,values =c("#F88802","#006AD0"),labels=c("Simulated","Observed" ) ,breaks = c("VesselDensity", "CD") )

ggsave("FigS12_CD.jpg", width = 16,height = 8,units = "cm",dpi = 1000)


testRes  <- list(
  Liangshuis = test.LsSite %>% rownames_to_column('Names'),
  AllSiteRW =  test.AllSiteRW,
  LSFMcells =  LSFMcells,
  LSBPcells =  LSBPcells
  
)

write.xlsx( testRes, "ModelTest.xlsx" )

dirpara <- dir("Data/SimParameters",full.names = T)
Ndir <- dir("Data/SimParameters") %>% str_sub(1,4)

AllParam <- map(dirpara,read.xlsx, sheet = 1 ) 

names(AllParam) <- Ndir

# AllParam <- rbindlist(AllParam,use.names = T,idcol = T) %>% 
#   as.data.frame() %>% 
#   dplyr::select( .id, parameter,values) %>% 
#   spread( key = ".id", value = values)


AllParam <- read.xlsx(dirpara[1], sheet = 1  )
names(AllParam)[names(AllParam) == "values"] <-Ndir[1]

  for (i in 2:6) {
    tt <- read.xlsx(dirpara[i], sheet = 1  )
    names(tt)[names(tt) == "values"] <-Ndir[i]
    
    AllParam <- full_join(AllParam,   tt  ) 
  }

Tages <- read.xlsx(dirpara[1], sheet = 2  ) %>% select(Year,age,Tage, Lage  ) %>% 
  mutate(Name =Ndir[1] )

for (i in 2:6) {
  Tages <- bind_rows( Tages,   read.xlsx(dirpara[i], sheet = 2  ) %>% select(Year,age,Tage, Lage  ) %>% 
    mutate(Name =Ndir[i] ) )
}

paras <- list(param = AllParam , Trends = Tages)

write.xlsx( paras, 'Params.xlsx'  )



