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


ce<-
  ggplot(data.frame(x=c(0,10)), aes(x=x))+
  stat_function(fun=demand_1, geom="line", size=2, color = "blue")+
  geom_label(aes(x=9.1,y=demand_1(9),hjust=0.5), nudge_y = +rel(1.25), color = "blue", label=str_wrap("Demand = Marginal Benefit",10), size = rel(3.5))+
  
  stat_function(fun=supply_1, geom="line", size=2, color = "firebrick")+
  geom_label(aes(x=9,y=supply_1(8.9),hjust=.5), nudge_y = -rel(1.25), color = "firebrick", label=str_wrap("Supply = Marginal Cost",10), size = rel(3.5))+

  geom_segment(x=0, xend=5, y=5, yend=5, size=1, linetype="dotted")+
  geom_segment(x=5, xend=5, y=0, yend=5, size=1, linetype="dotted")+
  
  geom_label(aes(x=5,y=1,hjust=.5), color = "black", label=str_wrap("Competitive Market Quantity",10), size = rel(3.5))+
  geom_label(aes(x=1,y=5,hjust=.5), color = "black", label=str_wrap("Competitive Market Price",10), size = rel(3.5))+
  
  #scale_x_continuous(expand=c(0,0))
  scale_x_continuous(breaks=seq(0,10,1),expand=c(0,0))+
  scale_y_continuous(breaks=seq(0,10,1),expand=c(0,0),
                     labels = function(x){paste("$", x, sep="")}
                     )+
  expand_limits(x = c(0,10),y=c(0,10))+
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  coord_equal()+
  class_theme()+
  NULL
ce
ggsave("graph_ce.png",width = 10,height = 10,dpi=600)




ce<-
  ggplot(data.frame(x=c(0,10)), aes(x=x))+
  stat_function(fun=demand_1, geom="line", size=2, color = "blue")+
  geom_label(aes(x=10,y=demand_1(10),hjust=0.5), nudge_y = +rel(1.25), color = "blue", label=str_wrap("Demand = Marginal Benefit",10), size = rel(3.5))+
  
  stat_function(fun=supply_1, geom="line", size=2, color = "firebrick")+
  geom_label(aes(x=10,y=supply_1(10),hjust=.5), nudge_y = -rel(1.25), color = "firebrick", label=str_wrap("Supply = Marginal Cost",10), size = rel(3.5))+
  
  geom_label(aes(x=5,y=0,hjust=.5), nudge_y = -rel(.75), color = "black", label=str_wrap("Competitive Market Quantity",10), size = rel(3.5))+
  geom_label(aes(x=0,y=5,hjust=.5), nudge_x = -rel(.75), color = "black", label=str_wrap("Competitive Market Price",10), size = rel(3.5))+
  
  geom_segment(x=0, xend=5, y=5, yend=5, size=1, linetype="dotted")+
  geom_segment(x=5, xend=5, y=0, yend=5, size=1, linetype="dotted")+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,10,1),
                     labels = function(x){paste("$", x, sep="")},
                     limits = c(-1,10))+
  expand_limits(x = c(0,10.5),y=c(0,10))+
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  blake_theme()+
  theme(plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"))+
  NULL
ce
ggsave("graph_ce.png",width = 10,height = 10)




mp<-
  ggplot(data.frame(x=c(0,10)), aes(x=x))+
  stat_function(fun=demand_1, geom="line", size=2, color = "blue")+
  geom_label(aes(x=10,y=demand_1(10),hjust=0.5), nudge_x = rel(.5), color = "blue", label=str_wrap("Marginal Social Benefit",10), size = rel(3.5))+

  stat_function(fun=mr, linetype="dashed", size=2, color = "blue")+
  geom_label(aes(x=1.25,y=mr(1.25),hjust=1),nudge_x = +rel(.5), color = "blue", label=str_wrap("Marginal Revenue",10), size = rel(3.5))+

  geom_segment(x=0, xend=5, y=5, yend=5, size=1, linetype="dotted")+
  geom_segment(x=5, xend=5, y=0, yend=5, size=1, linetype="dotted")+
  
  geom_label(aes(x=5,y=0,hjust=.5), nudge_x = rel(.5), color = "black", label=str_wrap("Competitive Market Quantity",10), size = rel(3.5))+
  geom_label(aes(x=0,y=5,hjust=.5), nudge_x = -rel(.2), color = "black", label=str_wrap("Competitive Market Price",10), size = rel(3.5))+
  
      
  stat_function(fun=supply_1, geom="line", size=2, color = "firebrick")+
  geom_label(aes(x=10,y=supply_1(10),hjust=.5), nudge_x = rel(.5), color = "firebrick", label=str_wrap("Marginal Social Cost",10), size = rel(3.5))+

  
  
  geom_segment(x=10/3, xend=10/3, y=0, yend=demand_1(10/3), size=1, linetype="dotted",color="black")+
  geom_segment(x=0, xend=10/3, y=demand_1(10/3), yend=demand_1(10/3), size=1, linetype="dotted",color="black")+
  geom_segment(x=0, xend=10/3, y=supply_1(10/3), yend=supply_1(10/3), size=1, linetype="dotted",color="black")+
  
  
  geom_label(aes(x=10/3,y=0,hjust=.5), nudge_x = -rel(.5), color = "black", label=str_wrap("Monopoly Quantity",10), size = rel(3.5))+
  
  geom_label(aes(x=0,y=supply_1(10/3),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Monopoly Marginal Cost",10), size = rel(3.5))+
  geom_label(aes(x=0,y=demand_1(10/3),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Monopoly Price",10), size = rel(3.5))+
  
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,10,1),
                     labels = function(x){paste("$", x, sep="")},
                     limits = c(0,10))+
  expand_limits(x = c(0,10.5),y=c(0,10))+
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  blake_theme()+
  theme(plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"))+
  NULL
mp
ggsave("graph_mp.png",width = 10,height = 10)






dwl_neg<-tribble(
  ~x, ~y,
  10/3,10-10/3,
  5, 5,
  10/3, 10-20/3
)



  
  mp2<-
    ggplot()+
    geom_polygon(data = dwl_neg,
                 aes(x = x,
                     y = y),
                 fill = "black",
                 alpha = 0.3)+
    geom_text_repel(aes(x=4,y=5),label=str_wrap("Welfare loss from monopoly",10),
                    nudge_y      = 2.1,
                    nudge_x      = 1,
                    direction    = "x",
                    vjust        = 0,
                    segment.size = 0.2
    )+   
    
    stat_function(fun=demand_1, geom="line", size=2, color = "blue")+
    geom_label(aes(x=10,y=demand_1(10),hjust=0.5), nudge_x = rel(.5), color = "blue", label=str_wrap("Marginal Social Benefit",10), size = rel(3.5))+
    
    stat_function(fun=mr, linetype="dashed", size=2, color = "blue")+
    geom_label(aes(x=1.25,y=mr(1.25),hjust=1),nudge_x = +rel(.5), color = "blue", label=str_wrap("Marginal Revenue",10), size = rel(3.5))+
    
    geom_segment(aes(x=0, xend=5, y=5, yend=5), size=1, linetype="dotted")+
    geom_segment(aes(x=5, xend=5, y=0, yend=5), size=1, linetype="dotted")+
    
    geom_label(aes(x=5,y=0,hjust=.5), nudge_x = rel(.5), color = "blue", label=str_wrap("Competitive Market Quantity",10), size = rel(3.5))+
    geom_label(aes(x=0,y=5,hjust=.5), nudge_x = -rel(.2), color = "blue", label=str_wrap("Competitive Market Price",10), size = rel(3.5))+
    
    
    stat_function(fun=supply_1, geom="line", size=2, color = "firebrick")+
    geom_label(aes(x=10,y=supply_1(10),hjust=.5), nudge_x = rel(.5), color = "firebrick", label=str_wrap("Marginal Social Cost",10), size = rel(3.5))+
    
    
    
    geom_segment(aes(x=10/3, xend=10/3, y=0, yend=demand_1(10/3)), size=1, linetype="dotted",color="black")+
    geom_segment(aes(x=0, xend=10/3, y=demand_1(10/3), yend=demand_1(10/3)), size=1, linetype="dotted",color="black")+
    geom_segment(aes(x=0, xend=10/3, y=supply_1(10/3), yend=supply_1(10/3)), size=1, linetype="dotted",color="black")+
    
    
    geom_label(aes(x=10/3,y=0,hjust=.5), nudge_x = -rel(.5), color = "black", label=str_wrap("Monopoly Quantity",10), size = rel(3.5))+
    
    geom_label(aes(x=0,y=supply_1(10/3),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Monopoly Marginal Cost",10), size = rel(3.5))+
    geom_label(aes(x=0,y=demand_1(10/3),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Monopoly Price",10), size = rel(3.5))+
    
    scale_x_continuous(breaks=seq(0,10,1))+
    scale_y_continuous(breaks=seq(0,10,1),
                       labels = function(x){paste("$", x, sep="")},
                       limits = c(0,10))+
    expand_limits(x = c(0,10.5),y=c(0,10))+
    labs(x = "Quantity (q)",
         y = "Price (p)")+
    blake_theme()+
    theme(plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"))+
    NULL
mp2
ggsave("graph_mp2.png",width = 10,height = 10)



dwl_quota<-tribble(
  ~x, ~y,
  10/3,10-10/3,
  5, 5,
  10/3, 10-20/3
)

rent_quota<-tribble(
  ~x, ~y,
  0,10-10/3,
  10/3,10-10/3,
  10/3, 10-20/3,
  0,10-20/3
)



quota_graph<-
  ggplot()+
  geom_polygon(data = rent_quota,
               aes(x = x,
                   y = y),
               fill = "blue",
               alpha = 0.3)+
    geom_polygon(data = dwl_quota,
               aes(x = x,
                   y = y),
               fill = "black",
               alpha = 0.3)+
  geom_text_repel(aes(x=4,y=5),label=str_wrap("Welfare loss from supply management",10),
                  nudge_y      = 2.1,
                  nudge_x      = 1,
                  direction    = "x",
                  vjust        = 0,
                  segment.size = 0.2
  )+   
  
  geom_text_repel(aes(x=2,y=5.5),label=str_wrap("Excess profit (rent) from supply management",15),
                  nudge_y      = 1.5,
                  nudge_x      = -1,
                  direction    = "x",
                  vjust        = 0,
                  segment.size = 0.2
  )+   
  
  
  stat_function(fun=demand_1, geom="line", size=2, color = "blue")+
  geom_label(aes(x=10,y=demand_1(9),hjust=0.5), nudge_x = rel(.5), color = "blue", label=str_wrap("Demand = Marginal Social Benefit",10), size = rel(3.5))+
  
  geom_segment(aes(x=0, xend=5, y=5, yend=5), size=1, linetype="dotted")+
  geom_segment(aes(x=5, xend=5, y=0, yend=5), size=1, linetype="dotted")+
  
  geom_label(aes(x=5,y=1,hjust=.5), nudge_x = rel(.5), color = "blue", label=str_wrap("Competitive Market Quantity",10), size = rel(3.5))+
  geom_label(aes(x=0,y=5,hjust=.5), nudge_x = -rel(.2), color = "blue", label=str_wrap("Competitive Market Price",10), size = rel(3.5))+
  
  
  stat_function(fun=supply_1, geom="line", size=2, color = "firebrick")+
  geom_label(aes(x=10,y=supply_1(9),hjust=.5), nudge_x = rel(.5), color = "firebrick", label=str_wrap("Supply = Marginal Social Cost",10), size = rel(3.5))+
  
  
  
  geom_segment(aes(x=10/3, xend=10/3, y=0, yend=demand_1(10/3)), linetype="solid",color="black",size=rel(3.5))+
  geom_segment(aes(x=0, xend=10/3, y=demand_1(10/3), yend=demand_1(10/3)), size=1, linetype="dotted",color="black")+
  geom_segment(aes(x=0, xend=10/3, y=supply_1(10/3), yend=supply_1(10/3)), size=1, linetype="dotted",color="black")+
  
  
  geom_label(aes(x=10/3,y=1,hjust=.5), nudge_x = -rel(.5), color = "black", label=str_wrap("Quota-restricted Quantity",10), size = rel(3.5))+
  
  geom_label(aes(x=0,y=supply_1(10/3),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Marginal Cost w Supply Managment",10), size = rel(3.5))+
  geom_label(aes(x=0,y=demand_1(10/3),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Supply Management Price",10), size = rel(3.5))+
  
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,10,1),
                     labels = function(x){paste("$", x, sep="")},
                     limits = c(0,10))+
  expand_limits(x = c(0,10.5),y=c(0,10))+
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  blake_theme()+
  theme(plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"))+
  NULL
quota_graph
ggsave("graph_quota.png",width = 10,height = 10)




dwl_neg<-tribble(
  ~x, ~y,
  6,6,
  5, 5,
  6, 4
)




ext<-
  ggplot()+
  geom_polygon(data = dwl_neg,
               aes(x = x,
                   y = y),
               fill = "black",
               alpha = 0.3)+
  geom_text_repel(aes(x=5.5,y=5),label=str_wrap("Welfare loss from pollution",10),
                  nudge_y      = 2.1,
                  nudge_x      = -1,
                  direction    = "x",
                  vjust        = 0,
                  segment.size = 0.2
  )+   
  
  stat_function(fun=demand_1, geom="line", size=2, color = "blue")+
  geom_label(aes(x=10,y=demand_1(10),hjust=0.5), nudge_x = rel(.5), color = "blue", label=str_wrap("Marginal Social Benefit",10), size = rel(3.5))+
  
  geom_segment(aes(x=0, xend=5, y=5, yend=5), size=1, linetype="dotted")+
  geom_segment(aes(x=5, xend=5, y=0, yend=5), size=1, linetype="dotted")+
  
  geom_label(aes(x=5,y=0,hjust=.5),nudge_y =+rel(.5), nudge_x = -rel(.5), color = "blue", label=str_wrap("Optimal quantity with all costs internal",10), size = rel(3.5))+
  geom_label(aes(x=0,y=5,hjust=.5), nudge_x = -rel(.2), color = "blue", label=str_wrap("Optimal Market Price",10), size = rel(3.5))+
  
  
  stat_function(fun=supply_1, geom="line", size=2, color = "firebrick")+
  geom_label(aes(x=10,y=supply_1(10),hjust=.5), nudge_x = rel(.5), color = "firebrick", label=str_wrap("Marginal Social Cost incl. Pollution",10), size = rel(3.5))+
  
  stat_function(fun=mpc, linetype="dashed", size=2, color = "blue")+
  geom_label(aes(x=10,y=mpc(10),hjust=.5),nudge_x = rel(.5),nudge_y = -rel(.5), color = "blue", label=str_wrap("Marginal Private Cost",10), size = rel(3.5))+
  
  
  geom_segment(aes(x=0, xend=6, y=demand_1(6), yend=demand_1(6)), size=1, linetype="dotted",color="black")+
  geom_segment(aes(x=0, xend=6, y=supply_1(6), yend=supply_1(6)), size=1, linetype="dotted",color="black")+
  geom_segment(aes(x=6, xend=6, y=0, yend=supply_1(6)), size=1, linetype="dotted",color="black")+
  
  
  geom_label(aes(x=6,y=0,hjust=.5), nudge_x =+rel(.5),nudge_y =+rel(.5), color = "black", label=str_wrap("Quantity with external pollution costs",10), size = rel(3.5))+
  
  geom_label(aes(x=0,y=supply_1(6),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Actual social cost",10), size = rel(3.5))+
  geom_label(aes(x=0,y=demand_1(6),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Actual market price",10), size = rel(3.5))+
  
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,10,1),
                     labels = function(x){paste("$", x, sep="")},
                     limits = c(0,10)
                     )+
  expand_limits(x = c(0,10.5),y=c(0,10.5))+
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  blake_theme()+
  theme(plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"))+
  NULL
ext
ggsave("graph_ext.png",width = 10,height = 10)




#carbon pricing

ext_tax<-
  ggplot()+
  geom_polygon(data = dwl_neg,
               aes(x = x,
                   y = y),
               fill = "black",
               alpha = 0.3)+
  geom_text_repel(aes(x=5.5,y=5),label=str_wrap("Avoided welfare loss from pollution",10),
                  nudge_y      = 2.1,
                  nudge_x      = -1,
                  direction    = "x",
                  vjust        = 0,
                  segment.size = 0.2
  )+   
  
  stat_function(fun=demand_1, geom="line", size=2, color = "blue")+
  geom_label(aes(x=10,y=demand_1(10),hjust=0.5), nudge_x = rel(.5), color = "blue", label=str_wrap("Marginal Social Benefit",10), size = rel(3.5))+
  
  geom_segment(aes(x=0, xend=5, y=5, yend=5), size=1, linetype="dotted")+
  geom_segment(aes(x=5, xend=5, y=0, yend=5), size=1, linetype="dotted")+
  
  geom_label(aes(x=5,y=0,hjust=.5),nudge_y =+rel(.5), nudge_x = -rel(.5), color = "blue", label=str_wrap("Quantity with pollution charge",10), size = rel(3.5))+
  geom_label(aes(x=0,y=5,hjust=.5), nudge_x = -rel(.2), color = "blue", label=str_wrap("Market price = marginal social cost",15), size = rel(3.5))+
  
  
  stat_function(fun=supply_1, geom="line", size=2, color = "firebrick")+
  geom_label(aes(x=10,y=supply_1(10),hjust=.5), nudge_x = rel(.5), color = "firebrick", label=str_wrap("Marginal Social Cost incl. Pollution",15), size = rel(3.5))+
  
  
  stat_function(fun=supply_1, linetype="dotted", size=2, color = "blue")+
  geom_label(aes(x=10,y=supply_1(10),hjust=.5), nudge_x = -rel(1), color = "blue", label=str_wrap("Marginal Private Cost + Pollution Charge",20), size = rel(3.5))+
  
  
  stat_function(fun=mpc, linetype="dashed", size=2, color = "blue")+
  geom_label(aes(x=10,y=mpc(10),hjust=.5),nudge_x = rel(.5),nudge_y = -rel(.5), color = "blue", label=str_wrap("Marginal Private Cost",10), size = rel(3.5))+
  
  
  geom_segment(aes(x=0, xend=6, y=demand_1(6), yend=demand_1(6)), size=1, linetype="dotted",color="black")+
  #geom_segment(aes(x=0, xend=6, y=supply_1(6), yend=supply_1(6)), size=1, linetype="dotted",color="black")+
  geom_segment(aes(x=6, xend=6, y=0, yend=supply_1(6)), size=1, linetype="dotted",color="black")+
  
  
  #arrows for changes
  geom_segment(aes(x=1,xend = 1,y=4,yend = 5),
               size = 1, arrow = arrow(length = unit(.5, "cm"))
  )+
  
  geom_segment(aes(x=6,xend = 5,y=1.5,yend = 1.5),
               size = 1, arrow = arrow(length = unit(.5, "cm"))
  )+
  
  geom_label(aes(x=6,y=0,hjust=.5), nudge_x =+rel(.5),nudge_y =+rel(.5), color = "black", label=str_wrap("Quantity with external pollution costs",15), size = rel(3.5))+
  
  #geom_label(aes(x=0,y=supply_1(6),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Actual social cost",10), size = rel(3.5))+
  geom_label(aes(x=0,y=demand_1(6),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Market price with unpriced pollution",15), size = rel(3.5))+
  
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,10,1),
                     labels = function(x){paste("$", x, sep="")},
                     limits = c(0,10)
  )+
  expand_limits(x = c(0,10.5),y=c(0,10.5))+
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  blake_theme()+
  theme(plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"))+
  NULL
ext_tax
ggsave("graph_ext_tax.png",width = 10,height = 10)



ext_quota<-
  ggplot()+
  geom_polygon(data = dwl_neg,
               aes(x = x,
                   y = y),
               fill = "black",
               alpha = 0.3)+
  geom_text_repel(aes(x=5.5,y=5),label=str_wrap("Avoided welfare loss from pollution",10),
                  nudge_y      = 2.1,
                  nudge_x      = 1,
                  direction    = "x",
                  vjust        = 0,
                  segment.size = 0.2
  )+   
  
  stat_function(fun=demand_1, geom="line", size=2, color = "blue")+
  geom_label(aes(x=10,y=demand_1(10),hjust=0.5), nudge_x = rel(.5), color = "blue", label=str_wrap("Marginal Social Benefit",10), size = rel(3.5))+
  
  geom_segment(aes(x=0, xend=5, y=5, yend=5), size=1, linetype="dotted")+
  #geom_segment(aes(x=5, xend=5, y=0, yend=5), size=1, linetype="dotted")+
  
  
  #hard cap
  
  geom_segment(aes(x=5, xend=5, y=0, yend=10), size=rel(2.5), linetype="dashed")+
  
  
  
  geom_label(aes(x=5,y=0,hjust=.5),nudge_y =+rel(.5), nudge_x = -rel(.75), color = "black", label=str_wrap("Maximum quantity with quota",10), size = rel(3.5))+
  
  
  
  geom_label(aes(x=0,y=5,hjust=.5), nudge_x = -rel(.2), color = "black", label=str_wrap("Market price with restricted quantities",12), size = rel(3.5))+
  
  
  
  
  stat_function(fun=supply_1, geom="line", size=2, color = "firebrick")+
  geom_label(aes(x=8,y=supply_1(10),hjust=.5), nudge_x = rel(.5), color = "firebrick", label=str_wrap("Marginal Social Cost incl. Pollution",15), size = rel(3.5))+
  

  stat_function(fun=mpc, linetype="dashed", size=2, color = "blue")+
  geom_label(aes(x=10,y=mpc(10),hjust=.5),nudge_x = rel(.5),nudge_y = -rel(.5), color = "blue", label=str_wrap("Marginal Private Cost",10), size = rel(3.5))+
  
  
  
  #geom_segment(aes(x=0, xend=6, y=supply_1(6), yend=supply_1(6)), size=1, linetype="dotted",color="black")+
  geom_segment(aes(x=6, xend=6, y=0, yend=supply_1(6)), size=1, linetype="dotted",color="black")+
  
  
  geom_segment(aes(x=0, xend=5, y=mpc(5), yend=mpc(5)), size=1, linetype="dotted",color="black")+
  
  geom_label(aes(x=0,y=3,hjust=.5), nudge_x = -rel(.2), color = "black", label=str_wrap("Marginal private cost at quota level",12), size = rel(3.5))+
  
  
  #arrows for changes
  geom_segment(aes(x=1,xend = 1,y=4,yend = 5),
               size = 1, arrow = arrow(length = unit(.5, "cm"))
  )+
  
  geom_segment(aes(x=1,xend = 1,y=4,yend = 3),
               size = 1, arrow = arrow(length = unit(.5, "cm"))
  )+
  
  
  geom_segment(aes(x=6,xend = 5,y=1.5,yend = 1.5),
               size = 1, arrow = arrow(length = unit(.5, "cm"))
  )+
  
  geom_label(aes(x=6,y=0,hjust=.5), nudge_x =+rel(1),nudge_y =+rel(.5), color = "black", label=str_wrap("Quantity with external pollution costs",15), size = rel(3.5))+
  
  #geom_label(aes(x=0,y=supply_1(6),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Actual social cost",10), size = rel(3.5))+
  geom_label(aes(x=2,y=demand_1(6),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Value of pollution quota",15), size = rel(3.5))+
  
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,10,1),
                     labels = function(x){paste("$", x, sep="")},
                     limits = c(0,10)
  )+
  expand_limits(x = c(0,10.5),y=c(0,10.5))+
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  blake_theme()+
  theme(plot.margin = margin(t = .3, r = .3, b = .1, l = .3,unit= "cm"))+
  NULL
 
ext_quota
ggsave("graph_ext_quota.png",width = 10,height = 10)


#MAC Style


mac_1=function(x){10-x}



mac_equil_1<-
  ggplot()+
  geom_polygon(data = dwl_neg,
               aes(x = x,
                   y = y),
               fill = "black",
               alpha = 0.3)+
  geom_text_repel(aes(x=5.5,y=5),label=str_wrap("Welfare loss from pollution",10),
                  nudge_y      = 2.1,
                  nudge_x      = -1,
                  direction    = "x",
                  vjust        = 0,
                  segment.size = 0.2
  )+   
  
  stat_function(fun=mac_1, geom="line", size=2, color = "blue")+
  geom_label(aes(x=10,y=demand_1(10),hjust=0.5), nudge_x = rel(.5), color = "blue", label=str_wrap("Marginal Social Benefit",10), size = rel(3.5))+
  
  geom_segment(aes(x=0, xend=5, y=5, yend=5), size=1, linetype="dotted")+
  geom_segment(aes(x=5, xend=5, y=0, yend=5), size=1, linetype="dotted")+
  
  geom_label(aes(x=5,y=0,hjust=.5),nudge_y =+rel(.5), nudge_x = -rel(.5), color = "blue", label=str_wrap("Optimal quantity with all costs internal",10), size = rel(3.5))+
  geom_label(aes(x=0,y=5,hjust=.5), nudge_x = -rel(.2), color = "blue", label=str_wrap("Optimal Market Price",10), size = rel(3.5))+
  
  
  stat_function(fun=supply_1, geom="line", size=2, color = "firebrick")+
  geom_label(aes(x=10,y=supply_1(10),hjust=.5), nudge_x = rel(.5), color = "firebrick", label=str_wrap("Marginal Social Cost incl. Pollution",10), size = rel(3.5))+
  
  stat_function(fun=mpc, linetype="dashed", size=2, color = "blue")+
  geom_label(aes(x=10,y=mpc(10),hjust=.5),nudge_x = rel(.5),nudge_y = -rel(.5), color = "blue", label=str_wrap("Marginal Private Cost",10), size = rel(3.5))+
  
  
  geom_segment(aes(x=0, xend=6, y=demand_1(6), yend=demand_1(6)), size=1, linetype="dotted",color="black")+
  geom_segment(aes(x=0, xend=6, y=supply_1(6), yend=supply_1(6)), size=1, linetype="dotted",color="black")+
  geom_segment(aes(x=6, xend=6, y=0, yend=supply_1(6)), size=1, linetype="dotted",color="black")+
  
  
  geom_label(aes(x=6,y=0,hjust=.5), nudge_x =+rel(.5),nudge_y =+rel(.5), color = "black", label=str_wrap("Quantity with external pollution costs",10), size = rel(3.5))+
  
  geom_label(aes(x=0,y=supply_1(6),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Actual social cost",10), size = rel(3.5))+
  geom_label(aes(x=0,y=demand_1(6),hjust=.5), nudge_x = -rel(.2),color = "black", label=str_wrap("Actual market price",10), size = rel(3.5))+
  
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,10,1),
                     labels = function(x){paste("$", x, sep="")},
                     limits = c(0,10)
  )+
  expand_limits(x = c(0,10.5),y=c(0,10.5))+
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  blake_theme()+
  theme(plot.margin = margin(t = .1, r = .1, b = .1, l = .1,unit= "cm"))+
  NULL
mac_equil_1
ggsave("graph_mac1.png",width = 10,height = 10)



quota_data<-get_cansim(3210005601)

quota_data <- quota_data %>% clean_names()%>% mutate(geo=as.factor(geo),
                                                 date=ymd(paste(ref_date,"12-31",sep="-")),
                                                 year=year(date)) %>% filter(commodities=="Quota")


provs<-c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario",
         "Quebec","Atlantic Provinces")  

quota_data<-quota_data %>% filter(geo!="Canada")%>%
  mutate(geo=fct_other(geo,drop=c("New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador"),other_level="Atlantic Provinces"))%>%
  group_by(geo,date,year) %>% summarize(value=sum(value))%>%
  mutate(geo=factor(geo,levels=provs))%>%
  ungroup()


ggplot(filter(quota_data)) +
  #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
  geom_line(size=1.2,aes(date,value/1000,group=geo,colour="Total quota value (millions of dollars)"))+
  #geom_point(size=1.5,aes(date,pmax(new_cases,0),group=province_state,shape="Daily new cases"),color=blakes_blue)+
  facet_wrap(~geo,nrow = 1)+
  blake_theme()+ theme(panel.spacing = unit(.5,"lines"))+
  theme(#legend.position = "bottom",
        axis.text.x = element_text(angle=90,hjust = .5,vjust=.5 ),
        strip.text = element_text(size=rel(.7) )
        )+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_date(date_breaks = "10 years",date_labels="%Y")+
  scale_y_continuous(breaks = pretty_breaks())+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="Total value of quota assets (millions of dollars)",x="",
       #title="Total value of supply management quota",
       #subtitle=paste("Balance sheet value of supply management system quota as of December 31st in millions of dollars",sep=""),
       #caption="Source: Statistics Canada Table 32-10-0056-01, Balance sheet of the agricultural sector as at December 31st, graph by Andrew Leach",
       NULL
       )
ggsave("can_quota.png",width = 16,height = 10)




#indifference map


#simple utility funciton U = (x1x2)^(0.5)

U <- function(x,y) (x*y)

#build a grid
x<- seq(0,10,0.5)
y<- seq(0,10,0.5)

z<-tibble()
for(x_val in x){
  for(y_val in y){
  z<-rbind(z,tibble(x_val,y_val))
  }
}

z<-z %>% mutate(z_val=U(x_val,y_val))

#solve utility max problem and then graph it above and below

p_x<-1
p_y<-1
income<-10

budget <- function(x,y) (income-p_x*x-p_y*y)


z<-z %>% mutate(x_cons=income/p_x-y_val*p_y,
                util_cons=U(x_cons,y_val),
                )

#define my utility function


u_test<-function(cons){
  #I want to plot how optim works so will gather the parameters
  #it selects for each iteration
  i <<- i + 1
  vals[[i]] <<- cons
 - U(cons[1],cons[2])
}


p_mat<- matrix(c(-p_x,-p_y), 1, 2)
c_mat<- matrix(-income,1,1)
theta <- c(2,2)
p_mat %*% theta - c_mat

i <- 0
vals <- list()
test<-constrOptim(theta, u_test, NULL, ui = (p_mat), ci = c_mat,method = "Nelder-Mead")


#rbind the values per iteration

p2 <- vals %>%
  do.call(rbind, .) %>% 
  #add a column for iteration number
  as.data.frame() %>% rename("x_hat"=V1,"y_hat"=V2)%>%
  mutate(iteration = 1:n(),utility=U(x_hat,y_hat),budget=budget(x_hat,y_hat),
         label=paste("Utility=",round(utility,2),"\nRemaining budget=",round(budget,2),sep="")
         ) 





paste("budget constraint satisfied at value",budget(c(test$par)))

paste("utility maximized at value",u_cons(c(test$par)),"with X=",test$par[1],"and y=",test$par[2])

paste("utility maximized at value",u_cons(c(test$par)),"with X=",test$par[1],"and y=",test$par[2])

#optimized utility is
optim_U<-U(test$par[1],test$par[2])
contour_breaks<-c(.5*optim_U,optim_U,1.5*optim_U)
#base

base_plot <- ggplot(z)+
  geom_contour(aes(x_val,y_val, z = z_val),breaks=contour_breaks,size=1.5)+
  geom_dl(aes(x_val-.75,y_val+1, z = z_val,label=paste("Utility=",round(..level..,1))), method="bottom.pieces", 
          stat="contour",breaks =contour_breaks)+
  #geom_point(aes(test$par[1],test$par[2]))+
  #geom_text_contour(stroke = 0.2)+
  geom_line(aes(x_cons,y_val),colour="red",size=1.5)+
  geom_label(aes(x=8.25,y=0,hjust=.5), nudge_y = rel(.5), color = "red", label=str_wrap("Budget constraint",10), size = rel(3.5))+
  #geom_label(aes(x=0,y=5,hjust=.5), nudge_x = -rel(.2), color = "blue", label=str_wrap("Competitive Market Price",10), size = rel(3.5))+
  
theme_bw()+
  theme(#legend.position = "bottom",
    #axis.text.x = element_text(angle=90,hjust = .5,vjust=.5 ),
    panel.grid.major =element_line(colour = "grey60",linetype="dotted",size=.05),
    #panel.grid.minor =element_line(colour = "black",linetype="dashed",size=.1),
    strip.text = element_text(size=rel(.7) )
  )+
  #annotate("text", x = as.Date(Sys.Date()-days(7)), y=0, label = "Last 14 days",size=1,hjust=0.5,vjust=0)+
  scale_x_continuous(breaks = pretty_breaks(),limits = c(min(x),max(x)),expand = c(0,0))+
  scale_y_continuous(breaks = pretty_breaks(),limits = c(min(y),max(y)),expand = c(0,0))+
  expand_limits(y = 0)+ 
  scale_colour_manual("",values = c("grey30"))+
  scale_fill_manual("",values = c("grey70"))+
  scale_shape_manual("",values = c(20))+
  labs(y="Consumption of Y",x="Consumption of X",
       #title="Indifference map",
       #subtitle= expression(Utility~"function"~is~U~"="~sqrt(XY)),
       #caption="Source: Statistics Canada Table 32-10-0056-01, Balance sheet of the agricultural sector as at December 31st, graph by Andrew Leach",
       NULL)
ggsave("utils.png",width=6,height=6)


anim<-base_plot+
  geom_point(data=filter(p2,iteration<61),aes(x_hat,y_hat))+
  geom_label_repel(data=filter(p2,iteration<61),aes(y=y_hat,x=x_hat,label=label),nudge_y = 1.5,nudge_x = 1.5)+
  #show the iteration in the title
  labs(title = 'iteration: {frame_time}') +
  #aesthetics
  #gganimate
  transition_time(iteration)

animate(anim,duration = 30,fps = 2)
anim_save("util_max.gif")




if(!require(devtools)) install.packages("devtools")
devtools::install_github('thomasp85/gganimate',force = T)
if(!require(gapminder)) install.packages("gapminder")
# Loading the two libraries required

# Load required package
library(gganimate)
library(gifski)

p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point()

plot(p)


anim <- p +
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

anim


#now to animate a demand shift

supply_1=function(x){x}
mpc=function(x){x-2}
demand_1=function(x,income){income-x}
mr=function(x){10-2*x}


demand<-function(d_int,d_slope,q){
  ## Demand = Y - d_slope q
  d_int-(d_slope*q)
}

supply<-function(s_int,s_slope,q){
  ## Supply = s_slope q
  (s_int+s_slope*q)
}


equil<-function(d_int,d_slope,s_int,s_slope){
#generic supply and demand solution
## Demand = d_int - d_slope q
## Supply = s_int+ s_slope q

equil_q<-(d_int-s_int)/(d_slope+s_slope)
equil_p<- d_int-d_slope *equil_q
#print("income sent:")
#store<<-(income)
list(equil_p,equil_q)

}

equil_p<-function(d_int,d_slope,s_int,s_slope){
    equil(d_int,d_slope,s_int,s_slope)[[1]]
}

equil_q<-function(d_int,d_slope,s_int,s_slope){
  equil(d_int,d_slope,s_int,s_slope)[[2]]
}

equil(c(10,10),d_slope,c(0,0),s_slope)


d_slope<-1
s_slope<-1.5
s_int<-0


store<-c(1,1)

ce_shift<-
  ggplot(data.frame(x=c(0,10,0,10,0,10),income=c(10,10,11,11,12,12)))+
  geom_line(aes(x,demand(min(income),d_slope,x)),linetype="dashed", size=2, color = "blue")+
  geom_line(aes(x,demand(income,d_slope,x)), size=2, color = "blue")+
  geom_label(aes(x=max(x),y=demand(income,d_slope,max(x)),hjust=0.5,
                 label=str_wrap(paste("Demand at income=",income,sep=""),10)), 
             nudge_x = -rel(1),nudge_y = +rel(2), color = "blue", size = rel(3))+
  geom_line(aes(x,supply(s_int,s_slope,x)), size=2, color = "firebrick")+
  geom_label(aes(x=max(x),y=supply(s_int,s_slope,max(x)),hjust=.5), nudge_y = -rel(2),nudge_x = -rel(2.5), color = "firebrick", label=str_wrap("Supply = Marginal Cost",10), size = rel(3.5))+
  geom_segment(aes(x=0, xend=equil_q(income,d_slope,s_int,s_slope), 
               y=equil_p(income,d_slope,s_int,s_slope), 
               yend=equil_p(income,d_slope,s_int,s_slope)),
               size=1, linetype="dotted")+
  geom_segment(aes(y=0, yend=equil_p(income,d_slope,s_int,s_slope), 
                   x=equil_q(income,d_slope,s_int,s_slope), 
                   xend=equil_q(income,d_slope,s_int,s_slope)),
               size=1, linetype="dotted")+
  geom_label(aes(x=0,y=equil_p(income,d_slope,s_int,s_slope),hjust=.5,
                 label=str_wrap(paste("Competitive Market Price=",equil_p(income,d_slope,s_int,s_slope),sep=""),10)), nudge_x = rel(.75), color = "black", size = rel(2.5))+
  
  geom_label(aes(y=0,x=equil_q(income,d_slope,s_int,s_slope),hjust=.5,
                 label=str_wrap(paste("Competitive Market Quantity=",equil_q(income,d_slope,s_int,s_slope),sep=""),10)), nudge_y = rel(.75), color = "black", size = rel(2.5))+
  scale_x_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0),
                     labels = function(x){paste("$", x, sep="")})+
  expand_limits(x = c(0),y=c(0))+
  labs(x = "Quantity (q)",
       y = "Price (p)")+
  class_theme()+
  #coord_equal()+
  NULL


anim_ce<- ce_shift+ transition_states(income,
                             transition_length = 2,
                             state_length = 1)

#anim_ce


animate(anim_ce,duration = 4)
anim_save("static/images/ce_dyn.gif")



ce
ggsave("graph_ce.png",width = 10,height = 10)



#heiss stuff

library(dplyr)
library(ggplot2)
supply <- Hmisc::bezier(x = c(1, 8, 9),
                        y = c(1, 5, 9)) %>%
  as_data_frame()

ggplot(supply, aes(x = x, y = y)) + 
  geom_path(color = "#0073D9", size = 1) + 
  theme_classic() + 
  coord_equal()


#add more curves
  supply1 <- Hmisc::bezier(x = c(1, 5, 9),
                           y = c(1, 5, 9)) %>%
  as_data_frame()

supply2 <- Hmisc::bezier(x = c(1, 9, 9),
                         y = c(1, 2, 9)) %>%
  as_data_frame()

all_supply_curves <- bind_rows(supply, supply1, supply2, .id = "id")

ggplot(all_supply_curves, aes(x = x, y = y, colour = id)) + 
  geom_path(size = 1) + 
  scale_color_manual(values = c("#0073D9", "#001F40", "#80DBFF")) +
  theme_classic() + 
  coord_equal()

#Demand : We can make a downward-sloping demand curve the same way. Since we’re using two geom_path() layers here, we remove the data parameter to the main ggplot() function, but keep the aesthetic mapping.

demand <- Hmisc::bezier(c(1, 3, 9),
                        c(9, 3, 1)) %>%
  as_data_frame()

ggplot(mapping = aes(x = x, y = y)) + 
  geom_path(data = supply, color = "#0073D9", size = 1) + 
  geom_path(data = demand, color = "#FF4036", size = 1) + 
  theme_classic() + 
  coord_equal()

Generate demand curve
#approxfun() takes a matrix of data and approximates a function to fit that data. For example, we can generate a function for the supply curve and then plug in any x value to calculate the corresponding y. Here are the y values for 2, 6, and 8 (they should match the graphs above):
# I honestly have no idea why rule = 2, but things break when rule = 1, so ¯\_(ツ)_/¯
  fun_supply <- approxfun(supply$x, supply$y, rule = 1)

fun_supply(c(2, 6, 8))
## [1] 1.590161 4.521605 6.805785
Magic.

#The uniroot() function can take a function and search across an interval for the root of that function (or, in this case, where two functions intersect). As said in the r-help post, we want to the root of the difference of the supply and demand curves. uniroot only accepts a single function, so we create an anonymous function where we calculate the difference between the two (function(x) fun_supply(x) - fun_demand(x)). We also want to search along the whole range of x, which currently goes from 1 to 9:
  
  fun_demand <- approxfun(demand$x, demand$y, rule = 1)

intersection_funs <-  uniroot(function(x) fun_supply(x) - fun_demand(x), c(1, 9))
intersection_funs
## $root
## [1] 4.654098
## 
## $f.root
## [1] 0.000002875289
## 
## $iter
## [1] 5
## 
## $init.it
## [1] NA
## 
## $estim.prec
## [1] 0.00006103516
#This gives a lot of output, but we only really care about the $root value, which is 4.654. And sure enough, it calculated the correct intersection!
  
  ggplot(mapping = aes(x = x, y = y)) + 
  geom_path(data = supply, color = "#0073D9", size = 1) + 
  geom_path(data = demand, color = "#FF4036", size = 1) + 
  geom_vline(xintercept = intersection_funs$root, linetype = "dotted") +
  theme_classic() + 
  coord_equal()

  #Supply demand intersection for just x

#To get the horizontal intersection, we just have to find where the vertical intersection (4.654) shows up in the demand function. We calculate this by plugging the intersection into fun_demand():
  
  y_root <- fun_demand(intersection_funs$root)

ggplot(mapping = aes(x = x, y = y)) + 
  geom_path(data = supply, color = "#0073D9", size = 1) + 
  geom_path(data = demand, color = "#FF4036", size = 1) + 
  geom_vline(xintercept = intersection_funs$root, linetype = "dotted") +
  geom_hline(yintercept = y_root, linetype = "dotted") +
  theme_classic() + 
  coord_equal()
#Supply demand intersection for both x and y

#Finding the intersections involves a lot of code, so we can put it all in a single function to make life easier later. This function only works on one intersection—it’ll find the first intersection in the full range of the first curve. Finding multiple intersections requires more complicated logic, but since I’m not planning on plotting anything more complicated, I’m fine with this.

# curve1 and curve2 should be data.frames with an x and y column
# For instance, as_data_frame(Hmisc::bezier(c(1, 8, 9), c(1, 5, 9)))
#
curve_intersect <- function(curve1, curve2) {
  # Approximate the functional form of both curves
  curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
  curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)
  
  # Calculate the intersection of curve 1 and curve 2 along the x-axis
  point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x), 
                     c(min(curve1$x), max(curve1$x)))$root
  
  # Find where point_x is in curve 2
  point_y <- curve2_f(point_x)
  
  # All done!
  return(list(x = point_x, y = point_y))
}
#The function returns a list with x and y values:
  
intersection_xy <- curve_intersect(supply, demand)


intersection_xy
## $x
## [1] 4.654098
## 
## $y
## [1] 3.395557
#We can use this simpler list in the plot. Here, we stop using geom_vline() and geom_hline() and plot segments instead, stopping at the intersection of the curves (with a point at the intersection, just for fun):
  
  intersection_xy_df <- intersection_xy %>% as_data_frame()

ggplot(mapping = aes(x = x, y = y)) + 
  geom_path(data = supply, color = "#0073D9", size = 1) + 
  geom_path(data = demand, color = "#FF4036", size = 1) + 
  geom_segment(data = intersection_xy_df, 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = intersection_xy_df, 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") + 
  geom_point(data = intersection_xy_df, size = 3) +
  labs(x = "Quantity", y = "Price") +
  theme_classic() + 
  coord_equal()
#Simple supply demand intersection

#Now that we can quickly calculate the intersection of two curves, we can make more complicated plots, like adding a second demand curve and showing the change in price that results from the shift:
  
  demand2 <- Hmisc::bezier(c(3, 5, 11),
                           c(11, 5, 3)) %>%
  as_data_frame()

# Make a data frame of the intersections of the supply curve and both demand curves
intersections <- bind_rows(curve_intersect(supply, demand),
                           curve_intersect(supply, demand2))

ggplot(mapping = aes(x = x, y = y)) + 
  geom_path(data = supply, color = "#0073D9", size = 1) + 
  geom_path(data = demand, color = "#FF4036", size = 1, linetype = "dashed") + 
  geom_path(data = demand2, color = "#FF4036", size = 1) + 
  geom_segment(data = intersections, 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = intersections, 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") + 
  geom_point(data = intersections, size = 3) +
  labs(x = "Quantity", y = "Price") +
  theme_classic() + 
  coord_equal()
#Add second demand curve

#Super magic!
  
  #We can put a few final touches on it:
  
  #Add an arrow with annotate("segment", ...)
#Force the line segments to the axes with scale_*_continuous(expand = c(0, 0), ...)
#Add breaks and labels on the axes for the segments with scale_*_continuous(..., breaks = XXX, labels = XXX)
#We use plotmath to get superscripted text in the labels using expression(Q[1], Q[2])
#Add text annotations directly to the plot with geom_text(). We can use plotmath here too, but only if parse = TRUE.
#Use a nicer font and make the title slightly bigger
# Create a data frame for the in-plot labels
plot_labels <- data_frame(label = c("S", "D[1]", "D[2]"),
                          x = c(8, 1, 5),
                          y = c(8, 8, 8))

ggplot(mapping = aes(x = x, y = y)) + 
  geom_path(data = supply, color = "#0073D9", size = 1) + 
  geom_path(data = demand, color = "#FF4036", size = 1, linetype = "dashed") + 
  geom_path(data = demand2, color = "#FF4036", size = 1) + 
  geom_segment(data = intersections, 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = intersections, 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") + 
  geom_text(data = plot_labels,
            aes(x = x, y = y, label = label), parse = TRUE,
            family = "Source Sans Pro") +
  annotate("segment", x = 3.5, xend = 4.5, y = 6, yend = 7,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50") +
  geom_point(data = intersections, size = 3) +
  scale_x_continuous(expand = c(0, 0), breaks = intersections$x,
                     labels = expression(Q[1], Q[2])) +
  scale_y_continuous(expand = c(0, 0), breaks = intersections$y,
                     labels = expression(P[1], P[2])) +
  labs(x = "Quantity", y = "Price",
       title = "Rightward shift in demand",
       subtitle = "As demand increases, so does price") +
  coord_equal() +
  theme_classic(base_family = "Source Sans Pro") + 
  theme(plot.title = element_text(family = "Source Sans Pro Semibold", size = rel(1.3)))
#All together now!
  
#  Perfect!
  
#  The only thing I have left to figure out is shading areas under the lines and curves to show consumer and producer surplus, but I’ll get to that later (in theory, it should be a matter of using geom_ribbon(), like this.)

#tl;dr
#All that explanation above makes the process sound more complicated than it actually is. Here’s a complete example:
  
  supply <- Hmisc::bezier(c(1, 8, 9),
                          c(1, 5, 9)) %>%
  data.frame()

demand1 <- Hmisc::bezier(c(1, 3, 9),
                         c(9, 3, 1)) %>%
  data.frame()

demand2 <- Hmisc::bezier(c(3, 5, 11),
                         c(11, 5, 3)) %>%
  data.frame()

# Calculate the intersections of the two curves
intersections <- bind_rows(curve_intersect(supply, demand1),
                           curve_intersect(supply, demand2))

plot_labels <- data_frame(label = c("S", "D[1]", "D[2]"),
                          x = c(8, 1, 5),
                          y = c(8, 8, 8))

ggplot(mapping = aes(x = x, y = y)) + 
  geom_path(data = supply, color = "#0073D9", size = 1) + 
  geom_path(data = demand, color = "#FF4036", size = 1, linetype = "dashed") + 
  geom_path(data = demand2, color = "#FF4036", size = 1) + 
  geom_segment(data = intersections, 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = intersections, 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") + 
  geom_text(data = plot_labels,
            aes(x = x, y = y, label = label), parse = TRUE,
            family = "Source Sans Pro") +
  annotate("segment", x = 3.5, xend = 4.5, y = 6, yend = 7,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50") +
  geom_point(data = intersections, size = 3) +
  scale_x_continuous(expand = c(0, 0), breaks = intersections$x,
                     labels = expression(Q[1], Q[2])) +
  scale_y_continuous(expand = c(0, 0), breaks = intersections$y,
                     labels = expression(P[1], P[2])) +
  labs(x = "Quantity", y = "Price",
       title = "Rightward shift in demand",
       subtitle = "As demand increases, so does price") +
  coord_equal() +
  theme_classic(base_family = "Source Sans Pro") + 
  theme(plot.title = element_text(family = "Source Sans Pro Semibold", size = rel(1.3)))




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



supply2 <- Hmisc::bezier(x = c(1, 9, 9),
                         y = c(1, 2, 9)) %>%
  


supply1 <- Hmisc::bezier(x = c(1, 5, 9),
                         y = c(1, 5, 9)) %>%
  as_data_frame()

supply2 <- Hmisc::bezier(x = c(1, 9, 9),
                         y = c(1, 2, 9)) %>%
  as_data_frame()

all_supply_curves <- bind_rows(supply, supply1, supply2, .id = "id")



segment_set<-function(x,xend,y,yend){
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), lty = "dotted")
}


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
            label= "Demand~cuve~D[1]",parse = TRUE, hjust=0, size = rel(3.5))+
  
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
            label= "New~Demand~cuve~D[2]",parse = TRUE, hjust=0, size = rel(3.5))
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
            label= "New~Demand~cuve~D[2]",parse = TRUE, hjust=0, size = rel(3.5))+

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
            label= "New~Demand~cuve~D[3]",parse = TRUE, hjust=0, size = rel(3.5))+
  
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






