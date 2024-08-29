library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(did)
library(patchwork)
library(gt)

theme_set(
  ggthemes::theme_clean()+
  theme(legend.position = "bottom",
        legend.title=element_blank())
)

fig1_data<-
  read_csv(list.files("Output",pattern="cs-did-job",full.names = T), guess_max = 10000)


fig1_data|>
  filter(type=="dynamic",T_type==C_type,districts=="All")|>
  filter(event.time %in% -10:8)|>
  mutate(break_point=paste(vendor,event.time>=0))|>
  ggplot(aes(x=event.time,y=estimate, ymin=conf.low,ymax=conf.high, color=vendor,group=break_point))+
  geom_hline(yintercept = 0)+
  geom_pointrange(position = position_dodge2(width=0.75))+
  geom_line(position = position_dodge2(width=0.75))+
  xlab("Time, relative to school opening")+ylab(element_blank())+
  facet_grid(depvar2~T_type, scales="free_y",labeller = label_wrap_gen())

ggsave("Baseline_graph.pdf", width=8,height = 8)
  
fig1_data|>
  filter(type=="dynamic",districts=="All",T_type!=C_type)|>
  filter(event.time %in% -10:8)|>
  mutate(break_point=paste(vendor,event.time>=0))|>
  group_by(depvar2,depvar)|>
  group_map(
    \(x,n) {
      x|>
        ggplot(aes(x=event.time,y=estimate, ymin=conf.low,ymax=conf.high, color=vendor,group=break_point))+
        geom_hline(yintercept = 0)+
        geom_pointrange(position = position_dodge2(width=0.75))+
        geom_line(position = position_dodge2(width=0.75))+
        xlab("Time, relative to school opening")+ylab(element_blank())+
        facet_grid(C_type~T_type, scales="free_y",labeller = label_wrap_gen())+
        ggtitle(n$depvar2)
      
      ggsave(glue::glue("Cross_type{n$depvar}.pdf"), width=8,height = 8)
    }
  )


    
  