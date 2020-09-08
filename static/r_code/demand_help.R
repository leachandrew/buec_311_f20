library(tidyverse)
library(ggthemes)
library(ggrepel)
library(patchwork)
library(scales)
library(directlabels)

class_theme<-function(){
  theme_classic()+
    theme(
      plot.subtitle = element_text(color="grey10",size=rel(.7)),
      plot.title = element_text(face="bold",size=rel(.8)),
      plot.caption = element_text(color="grey50",size=rel(.5),hjust=0),
      legend.title = element_text(color="grey10",size=rel(.5)),
      legend.text = element_text(color="grey10",size=rel(.5)),
      axis.text = element_text(size=rel(1.5)),
      axis.title = element_text(size=rel(1.5)),
      panel.grid.major = element_line(size = .2,colour = "black"), 
      #plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"),
      plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
      #axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      NULL)
}



class_clean<-function(){
  theme_classic()+
    theme(
      plot.subtitle = element_text(color="grey10",size=rel(.7)),
      plot.title = element_text(face="bold",size=rel(.8)),
      plot.caption = element_text(color="grey50",size=rel(.5),hjust=0),
      legend.title = element_text(color="grey10",size=rel(.5)),
      legend.text = element_text(color="grey10",size=rel(.5)),
      axis.text = element_text(size=rel(1.5)),
      axis.title = element_text(size=rel(1.5)),
      #panel.grid.major = element_line(size=0,colour = "black"), 
      #plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"),
      plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
      #axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      NULL)
}



#u of a palette

colors_ua10 <- function()
{
  #return(c("#007C41", "#FFDB05", "#7D9AAA", "#CA7700", "#165788", "#A8B400",
  #         "#E0D760", "#404545", "#8D3C1E", "#004250"))
  c("#007C41", "#FFDB05", "#7D9AAA","#165788","#404545","#8D3C1E","#3CB6CE")
}


# start and make the basic demand curve

p_demand<-function(q,d_int=200,d_slope=1){
  ## Demand = Y - d_slope q
  d_int-(d_slope*q)
}

p_supply<-function(q,s_int=0,s_slope=1){
  ## Supply = s_slope q
  (s_int+s_slope*q)
}



map<-c(0,10)
demand_1 <- Hmisc::bezier(x = map,
                          y = p_demand(map,d_int = 5,d_slope =.5 )) %>%
  as_tibble()

demand_2 <- Hmisc::bezier(x = map,
                          y = p_demand(map,d_int = 10,d_slope =2 )) %>%
  as_tibble()


all_demand_curves <- bind_rows(demand_1, .id = "id")

supply_1 <- Hmisc::bezier(x = map,
                          y = p_supply(map,s_int = 0,s_slope =1/3 )) %>%
  as_tibble()





eg_blank<-ggplot(demand_1, aes(x = x, y = y)) + 
  #geom_path(data=demand_2,size = 2,color="dodgerblue") +
  scale_color_manual(values = c("#0073D9", "#001F40", "#80DBFF")) +
  class_clean() + 
  #coord_equal()+
  
  #function labels
  geom_segment(aes(xend = 30, y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35, yend = p_demand(30,d_int = 200,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),color="dodgerblue",size=1.5)+
  geom_text(aes(y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35.5),
            label= "Demand~curve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,11))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,6))+
  #vertical segments
  #geom_segment(aes(x = 10, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = 0), lty = "dotted") +
  #geom_segment(aes(x = 30, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = 0), lty = "dotted") +
  
  #horizontal segments
  #geom_segment(aes(x = 0, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = p_demand(10,d_int = 200,d_slope =5)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = p_demand(20,d_int = 200,d_slope =5)), lty = "dotted") +
  #geom_segment(aes(x = 0, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = p_demand(30,d_int = 200,d_slope =5)), lty = "dotted") +
  
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  theme(legend.position = "blank")

eg_1<-eg_blank+
  geom_path(size = 2,color="dodgerblue",linetype="dashed") +
  geom_path(size = 2,color="dodgerblue") + 
  NULL
eg_1
ggsave("static/images/eq_1.png",width = 8,height = 6,dpi = 600)

eg_blank
ggsave("static/images/eq_blank.png",width = 8,height = 6,dpi = 600)


eg_supply<-eg_blank+
  geom_path(data=supply_1,size = 2,color="firebrick") + 
  NULL
eg_supply
ggsave("static/images/eg_supply.png",width = 8,height = 6,dpi = 600)


eg_both<-eg_1+
  geom_path(data=supply_1,size = 2,color="firebrick") + 
  NULL
eg_both
ggsave("static/images/eg_both.png",width = 8,height = 6,dpi = 600)

equil_q<-
uniroot(function(z) p_demand(z,d_int=5,d_slope = .5) -p_supply(z,s_int = 0,s_slope = 1/3),c(0,10))[[1]]
equil_p<-p_demand(equil_q,d_int=5,d_slope = .5)


eg_both+
  #vertical
geom_segment(aes(x = equil_q, y = equil_p, xend = equil_q, yend = 0), lty = "dotted") +
  #horizontal segments
  geom_segment(aes(x = 0, y = equil_p, xend = equil_q, yend = equil_p), lty = "dotted") +
    NULL
ggsave("static/images/eg_equil.png",width = 8,height = 6,dpi = 600)




eg_wrong<-ggplot(demand_1, aes(x = x, y = y)) + 
  geom_path(data=demand_2,size = 2,color="dodgerblue") +
  scale_color_manual(values = c("#0073D9", "#001F40", "#80DBFF")) +
  class_clean() + 
  #coord_equal()+
  
  #function labels
  geom_segment(aes(xend = 30, y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35, yend = p_demand(30,d_int = 200,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),color="dodgerblue",size=1.5)+
  geom_text(aes(y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35.5),
            label= "Demand~curve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,11))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,11))+
  #vertical segments
  #geom_segment(aes(x = 10, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = 0), lty = "dotted") +
  #geom_segment(aes(x = 30, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = 0), lty = "dotted") +
  
  #horizontal segments
  #geom_segment(aes(x = 0, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = p_demand(10,d_int = 200,d_slope =5)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = p_demand(20,d_int = 200,d_slope =5)), lty = "dotted") +
  #geom_segment(aes(x = 0, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = p_demand(30,d_int = 200,d_slope =5)), lty = "dotted") +
  
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  theme(legend.position = "blank")

eg_wrong
ggsave("static/images/eq_wrong.png",width = 8,height = 6,dpi = 600)






gasequil<-ggplot() + 
  geom_path(data=demand_1,size = 2,aes(x = x, y = y,color="Demand")) +
  geom_path(data=supply_1, aes(x = x, y = y,colour="Supply"),size = 2) +
  scale_color_manual(values = colors_ua10()[-2]) +
  class_clean() + 
  #coord_equal()+
  
  #function labels
  geom_segment(aes(colour="Supply",xend = 30, y = p_supply(30,s_int = 30,s_slope =3)-20, x = 35, yend = p_supply(30,s_int = 30,s_slope =3)),
               arrow = arrow(length = unit(0.5,"cm")),size=1)+
  geom_text(aes(y = p_supply(30,s_int = 30,s_slope =3)-20, x = 35.5),
            label= "Supply~curve~S[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  geom_segment(aes(colour="Demand",xend = 30, y = p_demand(30,d_int = 200,d_slope =5 )+20, x = 35, yend = p_demand(30,d_int = 200,d_slope =5 )),
               arrow = arrow(length = unit(0.5,"cm")),size=1.25)+
  geom_text(aes(y = p_demand(30,d_int = 200,d_slope =5 )+20, x = 35.5),
            label= "Demand~curve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  geom_segment(aes(xend = equil_q, y = equil_p+40, x = equil_q, yend = equil_p+4),
               arrow = arrow(length = unit(0.5,"cm")),size=1.25,colour="grey40")+
  geom_text(aes(y = equil_p+44, x = equil_q),
            label= "Market~equilibrium~S[1]==D[1]",parse = TRUE, hjust=0.5, size = rel(3.5))+
  
  
  
  geom_point(aes(x=equil_q,y=equil_p),color="black",size=2)+
  
  scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,45))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,220))+
  #vertical segments
  geom_segment(aes(x = equil_q, y =equil_p, xend = equil_q, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = equil_p, xend =equil_q, yend = equil_p), lty = "dotted") +
  
  labs(x = "Quantity supplied (q)",
       y = "Price (p)")+
  theme(legend.position = "blank")
ggsave("static/images/basic_equil.png",width = 8,height = 6,dpi = 600)


equil_base<-ggplot() + 
  geom_path(data=demand_1,size = 2,aes(x = x, y = y,color="Demand")) +
  geom_path(data=supply_1, aes(x = x, y = y,colour="Supply"),size = 2) +
  scale_color_manual(values = colors_ua10()[-2]) +
  class_clean() + 
  #coord_equal()+
  scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,45))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,220))+
  
  
  #function labels
  geom_segment(aes(colour="Supply",xend = 30, y = p_supply(30,s_int = 30,s_slope =3)-20, x = 35, yend = p_supply(30,s_int = 30,s_slope =3)),
               arrow = arrow(length = unit(0.5,"cm")),size=1)+
  geom_text(aes(y = p_supply(30,s_int = 30,s_slope =3)-20, x = 35.5),
            label= "Supply~curve~S[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  geom_segment(aes(colour="Demand",xend = 30, y = p_demand(30,d_int = 200,d_slope =5 )+20, x = 35, yend = p_demand(30,d_int = 200,d_slope =5 )),
               arrow = arrow(length = unit(0.5,"cm")),size=1.25)+
  geom_text(aes(y = p_demand(30,d_int = 200,d_slope =5 )+20, x = 35.5),
            label= "Demand~curve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  labs(x = "Quantity supplied (q)",
       y = "Price (p)")+
  theme(legend.position = "blank")+
  NULL
  
  equil_base+
    
  geom_segment(aes(xend = equil_q+5, y = equil_p+40, x = equil_q, yend = p_supply(equil_q+5,s_int = 30,s_slope =3)),
               arrow = arrow(length = unit(0.5,"cm")),size=1.25,colour="grey40")+
  geom_label(aes(y = equil_p+44, x = equil_q),
              label= "Is this an equilibrium?",parse = FALSE, hjust=0.5, size = rel(3.5))+
    geom_point(aes(x=equil_q+5,y=p_supply(equil_q+5,s_int = 30,s_slope =3)),color="black",size=2)+
    
    #vertical segments
    geom_segment(aes(x = equil_q+5, y = p_supply(equil_q+5,s_int = 30,s_slope =3), xend = equil_q+5, yend = 0), lty = "dotted") +
    
    #horizontal segments
    geom_segment(aes(x = 0, y = p_supply(equil_q+5,s_int = 30,s_slope =3), xend = equil_q+5, yend = p_supply(equil_q+5,s_int = 30,s_slope =3)), lty = "dotted") +
    geom_segment(aes(x = 0, y = p_demand(equil_q+5,d_int = 200,d_slope =5), xend = equil_q+5, yend = p_demand(equil_q+5,d_int = 200,d_slope =5)), lty = "dotted") +
    
    NULL
ggsave("static/images/equil_expl_1.png",width = 8,height = 6,dpi = 600)

equil_base+
  
  geom_segment(aes(xend = equil_q+5, y = equil_p+40, x = equil_q, yend = p_supply(equil_q+5,s_int = 30,s_slope =3)),
               arrow = arrow(length = unit(0.5,"cm")),size=1.25,colour="grey40")+
  geom_label(aes(y = equil_p+44, x = equil_q),
             label= "Is this an equilibrium?",parse = FALSE, hjust=0.5, size = rel(3.5))+
  
  geom_point(aes(x=equil_q+5,y=p_supply(equil_q+5,s_int = 30,s_slope =3)),color="black",size=2)+
  
  #vertical segments
  geom_segment(aes(x = equil_q+5, y = p_supply(equil_q+5,s_int = 30,s_slope =3), xend = equil_q+5, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = p_supply(equil_q+5,s_int = 30,s_slope =3), xend = equil_q+5, yend = p_supply(equil_q+5,s_int = 30,s_slope =3)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(equil_q+5,d_int = 200,d_slope =5), xend = equil_q+5, yend = p_demand(equil_q+5,d_int = 200,d_slope =5)), lty = "dotted") +
  
  
  #labels
  geom_label(aes(y = p_supply(equil_q+5,s_int = 30,s_slope =3), x = .5),
             label= "Price needed for supplier",parse = FALSE, hjust=0, size = rel(3.5))+
  
  geom_label(aes(y = p_demand(equil_q+5,d_int = 200,d_slope =5), x =.5),
             label= "Willingness to pay of consumer",parse = FALSE, hjust=0, size = rel(3.5))+
  
  
  NULL
ggsave("static/images/equil_expl_2.png",width = 8,height = 6,dpi = 600)


equil_base+
  geom_segment(aes(xend = equil_q-5, y = equil_p+40, x = equil_q, yend = p_demand(equil_q-5,d_int = 200,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),size=1.25,colour="grey40")+
  geom_label(aes(y = equil_p+44, x = equil_q),
             label= "What about this?",parse = FALSE, hjust=0.5, size = rel(3.5))+
  
  geom_point(aes(x=equil_q-5,y=p_demand(equil_q-5,d_int = 200,d_slope =5)),color="black",size=2)+
  
  #vertical segments
  geom_segment(aes(x = equil_q-5, y = p_demand(equil_q-5,d_int = 200,d_slope =5), xend = equil_q-5, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = p_supply(equil_q-5,s_int = 30,s_slope =3), xend = equil_q-5, yend = p_supply(equil_q-5,s_int = 30,s_slope =3)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(equil_q-5,d_int = 200,d_slope =5), xend = equil_q-5, yend = p_demand(equil_q-5,d_int = 200,d_slope =5)), lty = "dotted") +
  
  NULL
ggsave("static/images/equil_expl_3.png",width = 8,height = 6,dpi = 600)


equil_base+
  geom_segment(aes(xend = equil_q-5, y = equil_p+40, x = equil_q, yend = p_demand(equil_q-5,d_int = 200,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),size=1.25,colour="grey40")+
  geom_label(aes(y = equil_p+44, x = equil_q),
             label= "What about this?",parse = FALSE, hjust=0.5, size = rel(3.5))+
  
  geom_point(aes(x=equil_q-5,y=p_demand(equil_q-5,d_int = 200,d_slope =5)),color="black",size=2)+
  
  #vertical segments
  geom_segment(aes(x = equil_q-5, y = p_demand(equil_q-5,d_int = 200,d_slope =5), xend = equil_q-5, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = p_supply(equil_q-5,s_int = 30,s_slope =3), xend = equil_q-5, yend = p_supply(equil_q-5,s_int = 30,s_slope =3)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(equil_q-5,d_int = 200,d_slope =5), xend = equil_q-5, yend = p_demand(equil_q-5,d_int = 200,d_slope =5)), lty = "dotted") +
  

  #labels
  geom_label(aes(y = p_supply(equil_q-5,s_int = 30,s_slope =3), x = .5),
             label= "Price needed for supplier",parse = FALSE, hjust=0, size = rel(3.5))+
  
  geom_label(aes(y = p_demand(equil_q-5,d_int = 200,d_slope =5), x =.5),
             label= "Willingness to pay of consumer",parse = FALSE, hjust=0, size = rel(3.5))+
  
  
  NULL
ggsave("static/images/equil_expl_4.png",width = 8,height = 6,dpi = 600)




#working on problem set 2

#solve for new x at price 100
uniroot(function(z) z+3*3/2000 - (4000-2000*3/3),c(0,5000))[[1]]

uniroot(function(z) (.5+z/1000) - (4000-2000*z/3),c(0,5000))[[1]]



