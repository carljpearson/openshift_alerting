#l1 analysis

#expertise
p.exp <- df %>%
  ggplot(aes(exp,fill=exp)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2),size=5) +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(y="Count",
       x="Expertise") +
  theme(
    axis.text.x= element_text(size=10,angle=45),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none" )

ggsave(p.exp,"/Users/carlpearson/Documents/r_github/openshift_alerting/plots/exp.png",bg="transparent",width = 8,height = 6)

#use
p.use <- df %>%
  ggplot(aes(use,fill=use)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2),size=5) +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Weekly Use") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(
    axis.text.x= element_text(size=10,angle=45),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none")

ggsave(p.use,"/Users/carlpearson/Documents/r_github/openshift_alerting/plots/use.png",bg="transparent",width = 8,height = 6)

exp_and_use <- gridExtra::grid.arrange(p.exp, p.use, nrow = 1)
ggsave(exp_and_use,
       file="/Users/carlpearson/Documents/r_github/openshift_alerting/plots/exp_and_use.png",
       bg="transparent",
       width = 10,
       height = 6,
       device="png")

#version
df %>%
  select(id,contains("vers")) %>%
  pivot_longer(-id) %>%
  mutate(name = case_when(
    name == "vers_1" ~ "3.9 or earlier",
    name == "vers_2" ~ "3.10",
    name == "vers_3" ~ "3.11",
    name == "vers_4" ~ "4.0",
    name == "vers_5" ~ "4.1",
    name == "vers_6" ~ "4.2",
  ),
  name = factor(name, levels = c("3.9 or earlier","3.10", "3.11","4.0","4.1","4.2")),
  value = factor(value, levels = c(NA,"Have not used","Used in demo","Little experience","Some experience","Lots of experience"))
  ) %>%
  ggplot(aes(name,fill=value)) +
  geom_bar(position = "stack") +
  geom_text(stat = 'count',aes(label =..count..,vjust = 1.4),size=4,position = position_stack(),color="#585858") +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Version",
       fill="Use") +
  theme(
    axis.text.x= element_text(size=15),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  scale_fill_brewer(palette = "YlOrRd") +
  scale_y_reverse()

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/vers2.png",bg="transparent",width = 8,height = 6)

#version 2
df %>%
  select(id,contains("vers")) %>%
  pivot_longer(-id) %>%
  mutate(name = case_when(
    name == "vers_1" ~ "3.9 or earlier",
    name == "vers_2" ~ "3.10",
    name == "vers_3" ~ "3.11",
    name == "vers_4" ~ "4.0",
    name == "vers_5" ~ "4.1",
    name == "vers_6" ~ "4.2",
  ),
  name = factor(name, levels = c("3.9 or earlier","3.10", "3.11","4.0","4.1","4.2")),
  value = factor(value, levels = c("Have not used","Used in demo","Little experience","Some experience","Lots of experience"))
  ) %>%
  na.omit() %>%
  group_by(name) %>%
  mutate(total=n()) %>%
  group_by(value,name,total) %>%
  count() %>%
  mutate(prop=n/total) %>%
  ggplot(aes(y=prop,x=name,fill=version)) +
  geom_bar(stat="identity",position = "stack") +
  geom_text(aes(label = prop),size=4,position = position_stack(),color="#585858") +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Version",
       fill="Use") +
  theme(
    axis.text.x= element_text(size=15),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) 

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/vers2.png",bg="transparent",width = 8,height = 6)

#version external
df %>%
  filter(rh=="No, I'm not a Red Hat employee.") %>%
  select(id,contains("vers")) %>%
  pivot_longer(-id) %>%
  mutate(name = case_when(
    name == "vers_1" ~ "3.9 or earlier",
    name == "vers_2" ~ "3.10",
    name == "vers_3" ~ "3.11",
    name == "vers_4" ~ "4.0",
    name == "vers_5" ~ "4.1",
    name == "vers_6" ~ "4.2",
  ),
  name = factor(name, levels = c("3.9 or earlier","3.10", "3.11","4.0","4.1","4.2")),
  value = factor(value, levels = c("Have not used","Used in demo","Little experience","Some experience","Lots of experience"))
  ) %>%
  ggplot(aes(name,fill=value)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., y=..count..),size=7,position = position_stack(vjust = .5),color="#585858") +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Version",
       fill="Use",
       caption="Customers only") +
  theme(
    axis.text.x= element_text(size=15),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/vers_ex.png",bg="transparent",width = 8,height = 6)

#version
df %>%
  filter(rh=="No, I'm not a Red Hat employee.") %>%
  select(id,contains("vers")) %>%
  pivot_longer(-id) %>%
  mutate(name = case_when(
    name == "vers_1" ~ "3.9 or earlier",
    name == "vers_2" ~ "3.10",
    name == "vers_3" ~ "3.11",
    name == "vers_4" ~ "4.0",
    name == "vers_5" ~ "4.1",
    name == "vers_6" ~ "4.2",
  ),
  name = factor(name, levels = c("3.9 or earlier","3.10", "3.11","4.0","4.1","4.2")),
  value = factor(value, levels = c("Have not used","Used in demo","Little experience","Some experience","Lots of experience"))
  ) %>%
  na.omit() %>%
  ggplot(aes(name,fill=value)) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count',aes(label =..count..,vjust = -0.2),size=4,position = position_dodge(width = .9),color="#585858") +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Version",
       fill="Use") +
  theme(
    axis.text.x= element_text(size=15),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) 

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/vers_ex2.png",bg="transparent",width = 8,height = 6)



#alert env

df %>%
  mutate(
    env = gsub("personal,","personal;",env),
    env = gsub("sandbox,","sandbox;",env)
  ) %>%
  separate(env,sep = ",",into = paste0("env_",1:6)) %>%
  select(id,contains("env_"),contains("reso_"),-contains("TEXT")) %>%
  pivot_longer(-id) %>%
  separate(name,into = c("var","num"),sep="_")  %>%
  mutate(num2 = case_when(
    var == "env" & value == "A personal; sandbox; or test environment" ~ 1,
    var == "env" & value == "My or my team's development cluster" ~ 2,
    var == "env" & value == "My company's production cluster" ~ 3,
    var == "env" & value == "A different company's production cluster (I provide support to their users)" ~ 4,
    var == "env" & value == "I don't view/receive alerts" ~ 5,
    var == "env" & value == "Other" ~ 6,
    var == "reso" & num == 1 ~ 1,
    var == "reso" & num == 2 ~ 2,
    var == "reso" & num == 3 ~ 3,
    var == "reso" & num == 4 ~ 4,
    var == "reso" & num == 5 ~ 5,
    var == "reso" & num == 6 ~ 6
    
  )) %>%
  na.omit(num2) %>%
  select(-num) %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  #filter(num2!=5) %>% #remove non-responders
  mutate(env=str_wrap(env,width=20),
         reso=factor(reso, levels = c("Unsure","No","Sometimes","Often"))) %>% 
  na.omit(env) %>%
  ggplot(aes(env,fill=reso)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., y=..count..),size=5,position = position_stack(vjust = .5),color="#585858") +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom") +
  labs(y="Count",
       fill="Do they resolve the alert?") +
  coord_flip()

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/env_res.png",bg="transparent",width = 8,height = 6)

#channel
df %>%
  separate(channel,into = paste0("channel_",1:5),sep=",") %>%
  select(id,contains("channel_"),use,rh) %>%
  pivot_longer(cols=contains("channel_")) %>%
  rename(Weekly_Use = use) %>%
  na.omit(value) %>%
  mutate(value=str_wrap(value,30)) %>%
  group_by(value,rh) %>%
  filter(value!="Other") %>%
  ggplot((aes(value,fill=Weekly_Use))) +
  geom_bar() +
  # geom_text(stat = 'count',aes(label =..count.., y=..count..+1.5),size=7) +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Version",
       fill="Use") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) +
  coord_flip()

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/channel.png",bg="transparent",width = 8,height = 6)

#confidence dist across alerts

df_long %>%
  ggplot(aes(x=alert_name,y=conf ,fill=alert_name)) +
  #geom_violin(alpha=.5,color="white")+
  stat_summary(fun=mean,size=1) +
  ggthemes::theme_tufte(base_family = "sans",base_size = 15) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) + 
  scale_y_continuous(breaks = c(1,4,7)) +
  coord_flip(ylim=c(1,7)) +
  labs(y="Confidence",
       subtitle = "Point = mean, line = SD, color = response distributions")

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/conf_viol2_alt.png",width = 12,height = 8)


#plotting heatmaps
#get images
images <- as.data.frame(list.files("/Users/carlpearson/Documents/r_github/openshift_alerting/img/"))
colnames(images) <- "images"
images <- images %>%
  mutate(path="/Users/carlpearson/Documents/r_github/openshift_alerting/img/") %>%
  separate(col = images, into = c("number","drop"),remove = F,sep="-") %>%
  mutate(number=as.numeric(number)) %>%
  arrange(number) %>%
  select(images,path) %>%
  unite(img,c("path","images"),sep="")

#custom heat maps

#all data minus none

img_p <- png::readPNG(images[1,1])

df_long %>%
  filter(exp != "None") %>%
  mutate(
    click_1_y = dim(img_p)[1] - click_1_y
  ) %>%
  ggplot(aes(click_1_x,click_1_y))  + 
  annotation_raster(img_p, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  stat_density2d(geom = "polygon", aes(fill=..level..),alpha=.1) + 
  geom_point(size=2,shape=4,color="red")+
  scale_fill_gradient(low="green",high="red") + 
  scale_x_continuous(limits=c(0,dim(img_p)[2]),expand=c(0,0))+
  scale_y_continuous(limits=c(0,dim(img_p)[1]),expand=c(0,0))+
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")


ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/heatmaps/all.png",width = 10.05,height = 6,bg="transparent")



#experience heatmap
df_long%>%
  filter(exp != "None") %>%
  mutate(
    click_1_y = dim(img_p)[1] - click_1_y
  ) %>%
  ggplot(aes(click_1_x,click_1_y))  + 
  annotation_raster(img_p, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  stat_density2d(geom = "polygon", aes(fill=..level..),alpha=.1) + 
  geom_point(size=2,shape=4,color="red")+
  scale_fill_gradient(low="green",high="red") + 
  scale_x_continuous(limits=c(0,dim(img_p)[2]),expand=c(0,0))+
  scale_y_continuous(limits=c(0,dim(img_p)[1]),expand=c(0,0))+
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap(~exp)


ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/heatmaps/all_exp.png",width = 10.05,height = 6,bg="transparent")


#exp heatmap

df %>%
  select(id,contains("vers")) %>%
  pivot_longer(-id) %>%
  mutate(name = case_when(
    name == "vers_1" ~ "v3.9_or_earlier",
    name == "vers_2" ~ "v3.10",
    name == "vers_3" ~ "v3.11",
    name == "vers_4" ~ "v4.0",
    name == "vers_5" ~ "v4.1",
    name == "vers_6" ~ "v4.2",
  ),
  name = factor(name, levels = c("v3.9_or_earlier","v3.10", "v3.11","v4.0","v4.1","v4.2"))) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  mutate(version=if_else(
    v4.0 %in% c("Some experience","Lots of experience") & v4.1 %in% c("Some experience","Lots of experience") & v4.2 %in% c( "Some experience","Lots of experience"),
    "Limited or no experience with 4.x",
    "Experienced with 4.x"
    
  )
  ) %>% 
  right_join(df_long) %>%
  filter(exp != "None") %>%
  mutate(
    click_1_y = dim(img_p)[1] - click_1_y
  ) %>%
  ggplot(aes(click_1_x,click_1_y))  + 
  annotation_raster(img_p, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  stat_density2d(geom = "polygon", aes(fill=..level..),alpha=.1) + 
  geom_point(size=2,shape=4,color="red")+
  scale_fill_gradient(low="green",high="red") + 
  scale_x_continuous(limits=c(0,dim(img_p)[2]),expand=c(0,0))+
  scale_y_continuous(limits=c(0,dim(img_p)[1]),expand=c(0,0))+
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap(~version)

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/heatmaps/all_exp.png",width = 10.05,height = 6,bg="transparent")


df_long%>%
  filter(exp != "None") %>%
  mutate(
    click_1_y = dim(img_p)[1] - click_1_y
  ) %>%
  ggplot(aes(click_1_x,click_1_y))  + 
  annotation_raster(img_p, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  stat_density2d(geom = "polygon", aes(fill=..level..),alpha=.1) + 
  geom_point(size=2,shape=4,color="red")+
  scale_fill_gradient(low="green",high="red") + 
  scale_x_continuous(limits=c(0,dim(img_p)[2]),expand=c(0,0))+
  scale_y_continuous(limits=c(0,dim(img_p)[1]),expand=c(0,0))+
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap(~exp)


ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/heatmaps/version.png",width = 10.05,height = 6,bg="transparent")

#loop for heatmaps
for(i in 1:11) {
  
  img_p <- png::readPNG(images[i,1])
  
  df_long %>%
    filter(alert_number==paste0("alert_",i)) %>%
    mutate(
      click_1_y = dim(img_p)[1] - click_1_y
    ) %>%
    ggplot(aes(click_1_x,click_1_y))  + 
    annotation_raster(img_p, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
    stat_density2d(geom = "polygon", aes(fill=..level..),alpha=.1) + 
    geom_point(size=2,shape=4,color="red")+
    scale_fill_gradient(low="green",high="red") + 
    scale_x_continuous(limits=c(0,dim(img_p)[2]),expand=c(0,0))+
    scale_y_continuous(limits=c(0,dim(img_p)[1]),expand=c(0,0))+
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none")
  
  
  ggsave(paste0("/Users/carlpearson/Documents/r_github/openshift_alerting/heatmaps/alert_",i,".png"),width = 10.05,height = 6,bg="transparent")
  
}



#loop for combo
for(i in 1:11) {
  
  #90% confidence
  zval=1.64
  
  #read images
  img_p <- png::readPNG(images[i,1])
  
  #heatmap
  p1  <- df_long %>%
    filter(alert_number==paste0("alert_",i)) %>%
    mutate(
      click_1_y = dim(img_p)[1] - click_1_y
    ) %>%
    ggplot(aes(click_1_x,click_1_y))  + 
    annotation_raster(img_p, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
    stat_density2d(geom = "polygon", aes(fill=..level..),alpha=.1) + 
    geom_point(size=2,shape=4,color="red")+
    scale_fill_gradient(low="green",high="red") + 
    scale_x_continuous(limits=c(0,dim(img_p)[2]),expand=c(0,0))+
    scale_y_continuous(limits=c(0,dim(img_p)[1]),expand=c(0,0))+
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none")
  
  #click area proportions
  p2 <- df_long %>%
    group_by(alert_name) %>%
    summarize(total=n()) %>%
    right_join(df_long,by="alert_name") %>%
    left_join(area_key) %>%
    group_by(area,total,alert_number) %>%
    count() %>%
    mutate(
      count = n,
      n=total, #rename
      prop = count / total, #get cis
      prop = count / n, #exact proportion from succesess/trials
      laplace = (count + 1) / (n + 2), #laplace point estimate
      p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval)), #adjust p for wald calculation
      n_adj = n + (zval * zval), #adjust n for wald calculation
      marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj), #wald margin value
      lowerci = p_adj - marg, #lower wald ci
      lowerci = ifelse(lowerci <= 0, 0, lowerci), #keep lower ci above 0
      upperci = p_adj + marg, #upper wald ci
      upperci = ifelse(upperci >= 1, 1, upperci)) %>% #keep upper ci below 1
    arrange(-prop) %>%
    na.omit() %>%
    filter(alert_number==paste0("alert_",i)) %>%
    ggplot(aes(y=prop,x= area, fill= area)) + 
    geom_bar(stat="identity",position=position_dodge()) +
    geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="black",position="dodge",width=.5) + #add adjusted wald CIs
    ggthemes::theme_tufte(base_family = "sans") + 
    theme(
      axis.text.x = element_text(angle = 45,hjust = 1)
      
    ) +
    theme(legend.position = "none") +
    labs(y="Proportion",x="Click region")
  
  p3 <- df_long %>%
    filter(alert_number==paste0("alert_",i)) %>%
    select(conf,id,exp) %>%
    ggplot(aes(x=exp,y=conf,fill=exp)) +
    geom_violin() +
    geom_point(position = position_jitter(width = .05),alpha=.5) +
    ggthemes::theme_tufte(base_family = "sans") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45,hjust = 1)) +
    labs(y="Confidence responses",x="Expertise")
  
  p = grid::rectGrob()
  p <- gridExtra::grid.arrange(p1,p2, p3,
                               layout_matrix = rbind(
                                 c(1,1,1,2), 
                                 c(1,1,1,3))
                               
  )
  
  ggsave(paste0("/Users/carlpearson/Documents/r_github/openshift_alerting/heatmaps/multi_",i,".png"),
         p,
         width = 10,height = 5)
  
  
}



#sidebar proportions
df_long %>%
  left_join(area_key) %>%
  filter(area=="Sidebar") %>%
  group_by(alert_name) %>%
  summarize(total=n()) %>%
  right_join(df_long,by="alert_name") %>%
  left_join(area_key) %>%
  filter(area=="Sidebar") %>%
  group_by(click,total,alert_name) %>%
  count() %>%
  mutate(
    count = n,
    n=total, #rename
    prop = count / total, #get cis
    prop = count / n, #exact proportion from succesess/trials
    laplace = (count + 1) / (n + 2), #laplace point estimate
    p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval)), #adjust p for wald calculation
    n_adj = n + (zval * zval), #adjust n for wald calculation
    marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj), #wald margin value
    lowerci = p_adj - marg, #lower wald ci
    lowerci = ifelse(lowerci <= 0, 0, lowerci), #keep lower ci above 0
    upperci = p_adj + marg, #upper wald ci
    upperci = ifelse(upperci >= 1, 1, upperci)) %>% #keep upper ci below 1
  arrange(-prop) %>%
  na.omit() %>%
  ggplot(aes(y=prop,x= click,fill=click)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="black",position="dodge",width=.5) + #add adjusted wald CIs
  ggthemes::theme_tufte(base_family = "sans") + 
  theme(
    axis.text.x = element_text(angle = 45,hjust = 1),
    axis.title.x   = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(~alert_name) +
  labs(y="Proportion")

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/sidebar_clicks.png",width = 10.05,height = 6)



#conf with error
df_long %>%
  group_by(alert_name) %>%
  summarise(conf_avg=mean(conf),conf_sd=sd(conf),se=sd*sqrt(nrow(dflo))*1.96)

