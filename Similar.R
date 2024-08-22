
devtools::install_github("kdoodk/rBTR")

library(tidyverse)
library(data.table)
library(openxlsx)
library(rBTR)
library(ggpmisc)
library(mgcv)
library(ggpubr)

BS <- read.xlsx('Data\\Obsdata\\RingWidths.xlsx') %>%
  rename(MRW = ring.width)

Names <- levels(factor(BS$name))

paramdirs <- dir( "Data\\SimParameters\\" ,full.names = T   )

for (i in 1:length(Names) ) {
  
  BRS <- BS[ BS$name == Names[i]  ,c('Year', 'MRW' ) ]
  
  clims <- read.xlsx('Data\\Clim_5.xlsx')  %>% filter(site ==  str_sub(Names[i],1,2)   )
  # st  r(clims)
  
  rm(param,age)
  param <- read.xlsx( grep( Names[i] , paramdirs,value = T  ) ,sheet = 1   )

  age <- read.xlsx( grep( Names[i] , paramdirs,value = T  ) ,sheet = 2   )
  
  syear = max(min(age$Year), min(BRS$Year), min( clims$Year ) )
  # syear = 2012
  eyear = min( max(age$Year), max(BRS$Year), max( clims$Year ) )
  
  SimRes <- rBTR::btr_parallel( clims, param,age = age , syear = syear, eyear = eyear,Named =  Names[i], Cores = 12 ) %>%  dev.outputs()

  reslong <- left_join(SimRes$annaulRing ,BRS) %>% na.omit()
  t1 <- mod.test(y_actual = reslong$MRW,y_predicted = reslong$RingWidth,method = "spearman")
 
  print(
    ggpubr::ggarrange(
      ggplot()+
        theme_bw()+
        labs(title = Names[i])+
        geom_line(aes(x =Year, y = RingWidth,color ="Sim") ,data = SimRes[[1]])+
        geom_line(aes(x = Year, y = MRW,color = "Obs"), data= BRS)
      ,
      ggplot()+
        theme_test()+
        annotate(geom = "table",x =0,y  = 0,
                 label = round(t1,3 )  ) ,
      ncol = 1,heights = c(1,0.3)
    )
  )
  
  ggsave( paste0("Data\\" , Names[i],".jpg" ) ,width = 6 , dpi = 200 )
}
 
