
devtools::install_github("kdoodk/rBTR")

library(tidyverse)
library(data.table)
library(openxlsx)
library(rBTR)
library(ggpmisc)
library(mgcv)
library(ggpubr)
library(doParallel)
library(foreach)


BS <- read.xlsx('Data\\Obsdata\\RingWidths.xlsx') %>%
  rename(MRW = ring.width)

Names <- levels(factor(BS$name))

paramdirs <- dir( "Data\\SimParameters\\" ,full.names = T   )

for (i in 1:length(Names) ) {
  
  BRS <- BS[ BS$name == Names[i]  ,c('Year', 'MRW' ) ]
  
  clims <- read.xlsx('Data\\Clim_5.xlsx')  %>% filter(site ==  str_sub(Names[i],1,2)   )
  # st  r(clims)
  
  rm(param,age)
  param1 <- read.xlsx( grep(paste0( Names[i],"exp" ) , paramdirs,value = T  ) ,sheet = 1   )
  param2 <- read.xlsx( grep(paste0( Names[i],"spl" ) , paramdirs,value = T  ) ,sheet = 1   )
  
  age1 <- read.xlsx( grep(paste0( Names[i],"exp" ) , paramdirs,value = T  ) ,sheet = 2   )
  age2 <- read.xlsx( grep(paste0( Names[i],"spl" ) , paramdirs,value = T  ) ,sheet = 2   )
  
  # syear = max( min(age2$Year) ,min(age1$Year), min(BRS$Year), min( clims$Year ) )
  syear = 2012
  eyear = min( max(age2$Year) ,max(age1$Year), max(BRS$Year), max( clims$Year ) )
  
  gg1 <- rBTR::btr( clims, param1,age = age1 , syear = syear, eyear = eyear ,intraannual = F,division = "clim",
                    CZgR = c(1, 0, 0, 1), Pbar = T, testMod = F,Named = paste0("exp_", Names[i])  ) %>%  dev.outputs()
  
  gg2 <- rBTR::btr( clims, param2,age = age2 , syear = syear, eyear = eyear ,intraannual = F,division = "clim",
                    CZgR = c(1, 0, 0, 1), Pbar = T, testMod = F,Named = paste0("spl_", Names[i]) ) %>%  dev.outputs()
  
  reslong <- left_join(gg1$annaulRing ,BRS) %>% na.omit()
  t1 <- mod.test(y_actual = reslong$MRW,y_predicted = reslong$RingWidth,method = "spearman")
  reslong <- left_join(gg2$annaulRing ,BRS) %>% na.omit()
  t1 <- rbind(t1, mod.test(y_actual = reslong$MRW,y_predicted = reslong$RingWidth,method = "spearman"))
  rownames(t1) <- c('exp', 'spl')
  
  print(
    ggpubr::ggarrange(
      ggplot()+
        theme_bw()+
        labs(title = Names[i])+
        geom_line(aes(x =Year, y = RingWidth,color ="Sim_spl") ,data = gg2[[1]])+
        geom_line(aes(x =Year, y = RingWidth,color ="Sim_exp") ,data = gg1[[1]])+
        geom_line(aes(x = Year, y = MRW,color = "Obs"), data= BRS)
      ,
      ggplot()+
        theme_test()+
        annotate(geom = "table",x =0,y  = 0,
                 label = list(cbind( rownames_to_column(round(t1,3 ),"RW")    )) ),
      ncol = 1,heights = c(1,0.3)
    )
  )
  
  ggsave( paste0("Data\\" , Names[i],".jpg" ) ,width = 6 , dpi = 200 )
}
 
