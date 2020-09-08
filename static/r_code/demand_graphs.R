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



map<-c(0,10,20,50)
demand_1 <- Hmisc::bezier(x = map,
                          y = p_demand(map,d_int = 200,d_slope =5 )) %>%
  as_tibble()

demand_2 <- Hmisc::bezier(x = map,
                          y = p_demand(map,d_int = 220,d_slope =5 )) %>%
  as_tibble()


all_demand_curves <- bind_rows(demand_1, .id = "id")




#demand graphs

gas_1<-ggplot(demand_1, aes(x = x, y = y)) + 
  #geom_path(data=demand_2,size = 2,color="dodgerblue") +
  geom_path(size = 2,color="dodgerblue",linetype="dashed") +
  geom_path(size = 2,color="dodgerblue") + 
  scale_color_manual(values = c("#0073D9", "#001F40", "#80DBFF")) +
  class_clean() + 
  #coord_equal()+
  
  #function labels
  geom_segment(aes(xend = 30, y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35, yend = p_demand(30,d_int = 200,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),color="dodgerblue",size=1.5)+
  geom_text(aes(y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35.5),
            label= "Demand~curve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,45))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,220))+
  #vertical segments
  #geom_segment(aes(x = 10, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = 0), lty = "dotted") +
  #geom_segment(aes(x = 30, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = 0), lty = "dotted") +
  
  #horizontal segments
  #geom_segment(aes(x = 0, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = p_demand(10,d_int = 200,d_slope =5)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = p_demand(20,d_int = 200,d_slope =5)), lty = "dotted") +
  #geom_segment(aes(x = 0, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = p_demand(30,d_int = 200,d_slope =5)), lty = "dotted") +
  
  labs(x = "Gasoline consumption (q, in hundreds of litres per year)",
       y = "Price (p, in cents per litre)")+
  theme(legend.position = "blank")
  

gas_1
ggsave("static/images/gas_zoom.png",width = 8,height = 6,dpi = 600)


gas_1+
  geom_segment(aes(x = 10, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = 0), lty = "dotted") +
  #geom_segment(aes(x = 20, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 30, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = p_demand(10,d_int = 200,d_slope =5)), lty = "dotted") +
  #geom_segment(aes(x = 0, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = p_demand(20,d_int = 200,d_slope =5)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = p_demand(30,d_int = 200,d_slope =5)), lty = "dotted") +
ggsave("static/images/gas_demand.png",width = 8,height = 6,dpi = 600)


#income shift

gas_1+
  geom_path(data=demand_2,size = 2,color="red") +
  geom_segment(aes(xend = 10, y = p_demand(10,d_int = 220,d_slope =5)+20, x = 15, yend = p_demand(10,d_int = 220,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),color="red",size=1.5)+
  geom_text(aes(y = p_demand(10,d_int = 220,d_slope =5)+20, x = 15.5),
            label= "New~Demand~curve~D[2]",parse = TRUE, hjust=0, size = rel(3.5))
ggsave("static/images/gas_inc.png",width = 8,height = 6,dpi = 600)

#solve for new x at price 100
new_x<-uniroot(function(z) 100 - p_demand(z,d_int = 220,d_slope =5),c(0,50))[[1]]

gas_1+
  geom_path(data=demand_2,size = 2,color="red") +
  #vertical segment
  geom_segment(aes(x = 20, y = p_demand(20,d_int = 220,d_slope =5), xend = 20, yend = 0), lty = "dotted") +
  geom_segment(aes(x = new_x, y = p_demand(20,d_int = 200,d_slope =5), xend = new_x, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = p_demand(20,d_int = 200,d_slope =5), xend = new_x, yend = p_demand(20,d_int = 200,d_slope =5)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(20,d_int = 220,d_slope =5), xend = 20, yend = p_demand(20,d_int = 220,d_slope =5)), lty = "dotted") +
  
#shifted curve labels
    geom_segment(aes(xend = 10, y = p_demand(10,d_int = 220,d_slope =5)+20, x = 15, yend = p_demand(10,d_int = 220,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),color="red",size=1.5)+
  geom_text(aes(y = p_demand(10,d_int = 220,d_slope =5)+20, x = 15.5),
            label= "New~Demand~curve~D[2]",parse = TRUE, hjust=0, size = rel(3.5))+

geom_segment(aes(xend = 20, y = p_demand(20,d_int = 220,d_slope =5)+20, x = 25, yend = p_demand(20,d_int = 220,d_slope =5)),
             arrow = arrow(length = unit(0.5,"cm")),color="black",size=1.5)+
geom_text(aes(y = p_demand(20,d_int = 220,d_slope =5)+20, x = 25.5),
            label= "Willing to pay higher p for the same q", hjust=0, size = rel(3.5))+

geom_segment(aes(xend = new_x, y = p_demand(20,d_int = 200,d_slope =5)+20, x = new_x+5, yend = p_demand(20,d_int = 200,d_slope =5)),
             arrow = arrow(length = unit(0.5,"cm")),color="black",size=1.5)+
  geom_text(aes(y = p_demand(20,d_int = 200,d_slope =5)+20, x = new_x+5.5),
            label= "Consume more q at the same p", hjust=0, size = rel(3.5))
ggsave("static/images/gas_inc2.png",width = 8,height = 6,dpi = 600)


#increase in the price of a complement

demand_3 <- Hmisc::bezier(x = map,
                          y = p_demand(map,d_int = 180,d_slope =5 )) %>%
  as_tibble()


#equivalent to a downward shift in income in this case
#solve for new x at price 100
new_x<-uniroot(function(z) 100 - p_demand(z,d_int = 180,d_slope =5),c(0,50))[[1]]


gas_1+
  geom_path(data=demand_3,size = 2,color="orange") +
  #vertical segment
  geom_segment(aes(x = 20, y = p_demand(20,d_int = 180,d_slope =5), xend = 20, yend = 0), lty = "dotted") +
  geom_segment(aes(x = new_x, y = p_demand(20,d_int = 200,d_slope =5), xend = new_x, yend = 0), lty = "dotted") +
  
  #horizontal segments
  geom_segment(aes(x = 0, y = p_demand(20,d_int = 180,d_slope =5), xend = new_x, yend = p_demand(20,d_int = 180,d_slope =5)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(20,d_int = 180,d_slope =5), xend = 20, yend = p_demand(20,d_int = 180,d_slope =5)), lty = "dotted") +
  
  #shifted curve labels
  geom_segment(aes(xend = 10, y = p_demand(10,d_int = 180,d_slope =5)+20, x = 15, yend = p_demand(10,d_int = 180,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),color="orange",size=1.5)+
  geom_text(aes(y = p_demand(10,d_int = 180,d_slope =5)+20, x = 15.5),
            label= "New~Demand~curve~D[3]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  geom_segment(aes(xend = 20, y = p_demand(20,d_int = 180,d_slope =5)+20, x = 25, yend = p_demand(20,d_int = 180,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),color="black",size=1.5)+
  geom_text(aes(y = p_demand(20,d_int = 180,d_slope =5)+20, x = 25.5),
            label= "Willing to pay lower p for the same q", hjust=0, size = rel(3.5))+
  
  geom_segment(aes(xend = new_x, y = p_demand(20,d_int = 200,d_slope =5)+20, x = new_x+5, yend = p_demand(20,d_int = 200,d_slope =5)),
               arrow = arrow(length = unit(0.5,"cm")),color="black",size=1.5)+
  geom_text(aes(y = p_demand(20,d_int = 200,d_slope =5)+20, x = new_x+5.5),
            label= "Consume less q at the same p", hjust=0, size = rel(3.5))
ggsave("static/images/gas_inc_comp2.png",width = 8,height = 6,dpi = 600)



#decrease in the price of a complement



#summation of demand curves


gas_stack<-ggplot(demand_1, aes(x = x, y = y)) + 
  #geom_path(data=demand_2,size = 2,color="dodgerblue") +
  geom_path(aes(color="1 person"),size = 2,linetype="solid") +
  geom_path(aes(x = 2*x, y = y,color="2 people"),size = 2) +
  geom_path(aes(x = 3*x, y = y,color="3 people"),size = 2) +
  geom_path(aes(x = 4*x, y = y,color="4 people"),size = 2) + 
  scale_color_manual("Number of people",values=colors_ua10()) +
  class_clean() + 
  #coord_equal()+
  
  #function labels
  #geom_segment(aes(xend = 30, y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35, yend = p_demand(30,d_int = 200,d_slope =5)),
  #             arrow = arrow(length = unit(0.5,"cm")),color="dodgerblue",size=1.5)+
  #geom_text(aes(y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35.5),
  #          label= "Demand~curve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  scale_x_continuous(breaks=seq(0,160,20),expand=c(0,0),limits=c(0,40*4+5))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,220))+
  #vertical segments
  #geom_segment(aes(x = 10, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20*2, y = p_demand(20,d_int = 200,d_slope =5), xend = 20*2, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20*3, y = p_demand(20,d_int = 200,d_slope =5), xend = 20*3, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20*4, y = p_demand(20,d_int = 200,d_slope =5), xend = 20*4, yend = 0), lty = "dotted") +
  
  #geom_segment(aes(x = 30, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = 0), lty = "dotted") +
  
  #horizontal segments
  #geom_segment(aes(x = 0, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = p_demand(10,d_int = 200,d_slope =5)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(20,d_int = 200,d_slope =5), xend = 20*4, yend = p_demand(20,d_int = 200,d_slope =5)), lty = "dotted") +
  #geom_segment(aes(x = 0, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = p_demand(30,d_int = 200,d_slope =5)), lty = "dotted") +
  
  labs(x = "Gasoline consumption (q, in hundreds of litres per year)",
       y = "Price (p, in cents per litre)")+
  theme(legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1)),
        legend.position = c(0.8, 0.6))+
  NULL
gas_stack
ggsave("static/images/gas_stack.png",width = 8,height = 6,dpi = 600)



demand_4 <- Hmisc::bezier(x = map,
                          y = p_demand(map,d_int = 100,d_slope =2.5 )) %>%
  as_tibble()


gas_stack2a<-ggplot(demand_1, aes(x = x, y = y)) + 
  geom_path(aes(color="Person A"),data=demand_4,size = 2) +
  geom_path(aes(color="Person B"),size = 2,linetype="solid") +
  scale_color_manual("People",values=colors_ua10()) +
  class_clean() + 
  #coord_equal()+
  
  #function labels
  #geom_segment(aes(xend = 30, y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35, yend = p_demand(30,d_int = 200,d_slope =5)),
  #             arrow = arrow(length = unit(0.5,"cm")),color="dodgerblue",size=1.5)+
  #geom_text(aes(y = p_demand(30,d_int = 200,d_slope =5)+20, x = 35.5),
  #          label= "Demand~curve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
  scale_x_continuous(breaks=seq(0,160,20),expand=c(0,0),limits=c(0,40))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),limits=c(0,220))+
  #vertical segments
  #geom_segment(aes(x = 10, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = 0), lty = "dotted") +
  geom_segment(aes(x = 20, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = 0), lty = "dotted") +
  #geom_segment(aes(x = 20*2, y = p_demand(20,d_int = 200,d_slope =5), xend = 20*2, yend = 0), lty = "dotted") +
  #geom_segment(aes(x = 20*3, y = p_demand(20,d_int = 200,d_slope =5), xend = 20*3, yend = 0), lty = "dotted") +
  #geom_segment(aes(x = 20*4, y = p_demand(20,d_int = 200,d_slope =5), xend = 20*4, yend = 0), lty = "dotted") +
  
  #geom_segment(aes(x = 30, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = 0), lty = "dotted") +
  
  #horizontal segments
  #geom_segment(aes(x = 0, y = p_demand(10,d_int = 200,d_slope =5), xend = 10, yend = p_demand(10,d_int = 200,d_slope =5)), lty = "dotted") +
  geom_segment(aes(x = 0, y = p_demand(20,d_int = 100,d_slope =2.5), xend = 20, yend = p_demand(20,d_int = 100,d_slope =2.5)), lty = "dotted") +
    geom_segment(aes(x = 0, y = p_demand(20,d_int = 200,d_slope =5), xend = 20, yend = p_demand(20,d_int = 200,d_slope =5)), lty = "dotted") +
  #geom_segment(aes(x = 0, y = p_demand(30,d_int = 200,d_slope =5), xend = 30, yend = p_demand(30,d_int = 200,d_slope =5)), lty = "dotted") +
  
  labs(x = "Gasoline consumption (q, in hundreds of litres per year)",
       y = "Price (p, in cents per litre)")+
  theme(legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1)),
        legend.position = c(0.8, 0.6))+
  NULL
gas_stack2a
ggsave("static/images/gas_stack2a.png",width = 8,height = 6,dpi = 600)


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
            label= "Supply~curve~S[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
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
            label= "New~Supply~curve~S[2]",parse = TRUE, hjust=1, size = rel(3.5))+
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
            label= "New~Supply~curve~S[3]",parse = TRUE, hjust=0, size = rel(3.5))+
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
  #          label= "Demand~curve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
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



#equilbrium

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
demand_1 <- Hmisc::bezier(x = map,
                          y = p_demand(map,d_int = 200,d_slope =5 )) %>%
  as_tibble()

demand_2 <- Hmisc::bezier(x = map,
                          y = p_demand(map,d_int = 220,d_slope =5 )) %>%
  as_tibble()


all_demand_curves <- bind_rows(demand_1, .id = "id")

map<-c(0,10,20,50)
supply_1 <- Hmisc::bezier(x = map,
                          y = p_supply(map,s_int = 30,s_slope =3)) %>%
  as_tibble()


uniroot(function(z) 200-5*z-30-3*z,c(0,100) )[[1]]

uniroot(function(z) 10+z/3-20,c(0,100) )[[1]]


#solve for new x at price 100
equil_q<-uniroot(function(z) p_supply(z,s_int = 30,s_slope =3) - p_demand(z,d_int = 200,d_slope =5),c(0,50))[[1]]

equil_p<-p_supply(equil_q,s_int = 30,s_slope =3)




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




