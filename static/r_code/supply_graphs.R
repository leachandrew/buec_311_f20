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

supply_1=function(x){x}
mpc=function(x){x-2}
demand_1=function(x){10-x}
mr=function(x){10-2*x}

# start and make the basic demand curve

p_demand<-function(q,d_int=200,d_slope=1){
  ## Demand = Y - d_slope q
  d_int-(d_slope*q)
}

p_supply<-function(q,s_int=0,s_slope=1){
  ## Supply = s_slope q
  (s_int+s_slope*q)
}



map<-c(0,10,20,50)
supply_1 <- Hmisc::bezier(x = map,
                         y = p_supply(map,s_int = 30,s_slope =3)) %>%
  as_tibble()


gas_supply_1<-ggplot(supply_1, aes(x = x, y = y)) + 
  #geom_path(data=demand_2,size = 2,color="dodgerblue") +
  geom_path(size = 2,color="dodgerblue",linetype="dashed") +
  geom_path(size = 2,color="dodgerblue") + 
  scale_color_manual(values = c("#0073D9", "#001F40", "#80DBFF")) +
  class_clean() + 
  #coord_equal()+
  
  #function labels
  geom_segment(aes(xend = 30, y = p_supply(30,s_int = 30,s_slope =3)-20, x = 35, yend = p_supply(30,s_int = 30,s_slope =3)),
               arrow = arrow(length = unit(0.5,"cm")),color="dodgerblue",size=1.5)+
  geom_text(aes(y = p_supply(30,s_int = 30,s_slope =3)-20, x = 35.5),
            label= "Supply~cuve~S[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,45))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,220))+
  #vertical segments
  geom_segment(aes(x = 20, y = p_supply(20,s_int = 30,s_slope =3), xend = 20, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = p_supply(20,s_int = 30,s_slope =3), xend = 20, yend = p_supply(20,s_int = 30,s_slope =3)), lty = "dotted") +
  
  labs(x = "Gasoline supplied (q, in hundreds of litres per year)",
       y = "Price (p, in cents per litre)")+
  theme(legend.position = "blank")
  

gas_supply_1
ggsave("static/images/gas_supply.png",width = 8,height = 6,dpi = 600)


#cost shift

supply_2 <- Hmisc::bezier(x = map,
                          y = p_supply(map,s_int = 50,s_slope =3)) %>%
  as_tibble()


gas_supply_1+
  geom_path(data=supply_2,size = 2,color="red") +
  geom_segment(aes(xend = 30, y = p_supply(30,s_int = 50,s_slope =3)+20, x = 25, yend = p_supply(30,s_int = 50,s_slope =3)),
               arrow = arrow(length = unit(0.5,"cm")),color="red",size=1.5)+
  geom_text(aes(y = p_supply(30,s_int = 50,s_slope =3)+20, x = 24.5),
            label= "New~Supply~cuve~S[2]",parse = TRUE, hjust=1, size = rel(3.5))+
  #vertical segments
  geom_segment(aes(x = 20, y = p_supply(20,s_int = 50,s_slope =3), xend = 20, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = p_supply(20,s_int = 50,s_slope =3), xend = 20, yend = p_supply(20,s_int = 50,s_slope =3)), lty = "dotted") +
  
  
    NULL
  
ggsave("static/images/gas_sup_shift.png",width = 8,height = 6,dpi = 600)


supply_3 <- Hmisc::bezier(x = map,
                          y = p_supply(map,s_int = 20,s_slope =3)) %>%
  as_tibble()


gas_supply_1+
  geom_path(data=supply_3,size = 2,color="red") +
  geom_segment(aes(xend = 30, y = p_supply(30,s_int = 20,s_slope =3)-20, x = 35, yend = p_supply(30,s_int = 20,s_slope =3)),
               arrow = arrow(length = unit(0.5,"cm")),color="red",size=1.5)+
  geom_text(aes(y = p_supply(30,s_int = 20,s_slope =3)-20, x = 35.5),
            label= "New~Supply~cuve~S[3]",parse = TRUE, hjust=0, size = rel(3.5))+
  #vertical segments
  geom_segment(aes(x = 20, y = p_supply(20,s_int = 20,s_slope =3), xend = 20, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = p_supply(20,s_int = 20,s_slope =3), xend = 20, yend = p_supply(20,s_int = 20,s_slope =3)), lty = "dotted") +
  
  
  NULL

ggsave("static/images/gas_sup_shift_2.png",width = 8,height = 6,dpi = 600)




gas_stack<-ggplot(supply_1, aes(x = x, y = y)) + 
  #geom_path(data=demand_2,size = 2,color="dodgerblue") +
  geom_path(aes(color="1 supplier"),size = 2,linetype="solid") +
  geom_path(aes(x = 2*x, y = y,color="2 suppliers"),size = 2) +
  geom_path(aes(x = 3*x, y = y,color="3 suppliers"),size = 2) +
  geom_path(aes(x = 4*x, y = y,color="4 suppliers"),size = 2) + 
  scale_color_manual("Number of suppliers",values=colors_ua10()) +
  class_clean() + 
  #coord_equal()+
  
  #function labels
  #geom_segment(aes(xend = 30, y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35, yend = p_demand(30,d_int = 200,d_slope =5)),
  #             arrow = arrow(length = unit(0.5,"cm")),color="dodgerblue",size=1.5)+
  #geom_text(aes(y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35.5),
  #          label= "Demand~cuve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  scale_x_continuous(breaks=seq(0,160,20),expand=c(0,0),limits=c(0,40*4+5))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,220))+
  #vertical segments
  #geom_segment(aes(x = 10, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20, y = p_supply(20,s_int = 30,s_slope =3), xend = 20, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20*2, y = p_supply(20,s_int = 30,s_slope =3), xend = 20*2, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20*3, y = p_supply(20,s_int = 30,s_slope =3), xend = 20*3, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20*4, y = p_supply(20,s_int = 30,s_slope =3), xend = 20*4, yend = 0), lty = "dotted") +
  
  #geom_segment(aes(x = 30, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = 0), lty = "dotted") +
  
  #horizontal segments
  #geom_segment(aes(x = 0, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = p_demand(10,d_int = 200,d_slope =5)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_supply(20,s_int = 30,s_slope =3), xend = 20*4, yend = p_supply(20,s_int = 30,s_slope =3)), lty = "dotted") +
  #geom_segment(aes(x = 0, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = p_demand(30,d_int = 200,d_slope =5)), lty = "dotted") +
  
  labs(x = "Gasoline consumption (q, in hundreds of litres per year)",
       y = "Price (p, in cents per litre)")+
  theme(legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1)),
        legend.position = c(0.8, 0.2))+
  NULL
gas_stack
ggsave("static/images/gas_sup_stack.png",width = 8,height = 6,dpi = 600)



#now we need to run the equilbrium graphs


