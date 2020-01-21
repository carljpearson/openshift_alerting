library(tidyverse)

data <- read_csv("/Users/carlpearson/Documents/r_github/openshift_alerting/alert_1_15_col.csv",
                 col_names = T)

#removing bad responses
df <- data[3:nrow(data),]


df <- df %>% 
  filter(Status=="IP Address",
         role!="test") %>%
  select(id=ResponseId,
         rh:FL_14_DO) %>%
  mutate(id=as.numeric(as.factor(id))) %>%
  separate(exp, into = c("exp","drop_me"),sep=" – ") %>%
  select(-drop_me) 

#recording factors
df$exp <- factor(df$exp, levels = c("None","Basic","Intermediate","Advanced","Expert"))

df$use <- factor(df$use, levels = c("Less than once","1-2 times", "3-5 times", "6-10 times","11+ times", "I'm not sure"))

#creating long format df
df_long <- df %>%
  select(id,
         contains("-conf"),
         contains("-click"),
         contains("Last")
         #-contains("_open"),
         #-contains("_x"),
        # -contains("_y")
         ) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  na.omit() %>%
  separate(variable,into=c("alert_number","variable"),sep="-") %>%
  mutate(variable = gsub(" Click","",variable),
         variable = gsub("time_Page Submit","submit",variable),
         variable = gsub("time_Click Count","count",variable)
         ) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(
    conf=case_when(
      conf == "Very confident" ~ 7,
      conf == 6 ~ 6,
      conf == 5 ~ 5,
      conf == "Neutral" ~ 4,
      conf == 3 ~ 3,
      conf == 2 ~ 2,
      conf == "Very unconfident" ~ 1
                          ),
    conf=as.numeric(as.character(conf)),
    alert_number = paste0("alert_",alert_number),
    click_time_Last=as.numeric(as.character(click_time_Last)),
    click_time_First=as.numeric(as.character(click_time_First)),
    click_1_y=as.numeric(click_1_y),
    click_1_x=as.numeric(click_1_x),
    conf_cat = 
      case_when(
      conf == 7 ~ "high",
      conf == 6 ~ "high",
      conf == 5 ~ "mid",
      conf == 4 ~ "mid",
      conf == 3 ~ "lo",
      conf == 2 ~ "lo",
      conf == 1 ~ "lo"),
    conf_cat = factor(conf_cat, levels = c("lo","mid","high"))
    )

#creating higher level categories for heatmap
area_key <- tibble(click=c("Home","Dashboards","Projects","Search","Explore","Events","Catalog","Workloads","Networking","Storage","Builds","Monitoring","Compute","Administration",
                           "Inv - Projects","Inv - Nodes","Inv - Pods","Inv - VMs","Inv - Storage Classes","Inv - PVCs","Inv - Disks",
                           "View Events", "Events Activity",
                           "u- CPU","u- Memory", "u- Storage", "u- Network Transfer", "u-Pod count",
                           "Control Plane","Operators","Alert text","View details",
                           "Burger","Grid button","Help",
                           "Storage Tab",
                           "OCM",
                           "Other"
                           ),
                   area= c(rep("Sidebar",14),
                           rep("Inventory",7),
                           rep("Activity",2),
                           rep("Utilization",5),
                           rep("Status",4),
                           rep("Masthead",3),
                           "Storage tab",
                            "OCM",
                           "Other"
                           
                           )
                   )

df_long <- df %>% select(id,exp) %>% right_join(df_long)

#reading in alert names
alert_name_key <- read_csv("/Users/carlpearson/Documents/r_github/openshift_alerting/alert_name_key.csv",
                 col_names = T)

df_long <- df_long %>% left_join(alert_name_key)

#l1 analysis

#expertise
df %>%
  ggplot(aes(exp)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2),size=7) +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Expertise") +
  theme(
        axis.title.x=element_blank(),
        axis.text.x= element_text(size=15),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/exp.png",bg="transparent",width = 8,height = 6)

#use
df %>%
  ggplot(aes(use)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2),size=7) +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Weekly Use") +
  theme(
    axis.text.x= element_text(size=15),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/use.png",bg="transparent",width = 8,height = 6)

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
    value = factor(value, levels = c("Have not used","Used in demo","Little experience","Some experience","Lots of experience"))
    ) %>%
    ggplot(aes(name,fill=value)) +
    geom_bar() +
    geom_text(stat = 'count',aes(label =..count.., y=..count..),size=7,position = position_stack(vjust = .5),color="#585858") +
    ggthemes::theme_tufte(base_family="sans",base_size = 15) +
    labs(y="Count",
         x="Version",
         fill="Use") +
    theme(
      axis.text.x= element_text(size=15),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())

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
  filter(num2!=5) %>%
  ggplot(aes(env,fill=reso)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., y=..count..),size=7,position = position_stack(vjust = .5),color="#585858") +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Do you have to resolve alerts?",
       fill="Use",
       caption="Customers only") +
  coord_flip()
  

#channel
df %>%
  separate(channel,into = paste0("channel_",1:5),sep=",") %>%
  select(id,contains("channel_"),use) %>%
  pivot_longer(cols=contains("channel_")) %>%
  rename(Weekly_Use = use) %>%
  na.omit(value) %>%
  ggplot((aes(value)),fill=Weekly_Use) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., y=..count..+1),size=7) +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Version",
       fill="Use",
       caption="Customers only") +
  theme(
    axis.text.x=element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x=element_blank()) +
  coord_flip()
  
  


#analysis





df_long %>%
  group_by(alert_name) %>%
  summarise(conf_avg=mean(conf),conf_sd=sd(conf))
df_long %>%
  select(conf,click_time_Last,click_count,alert_name) %>%
  psych::describeBy(group = "alert_name") 

df_long_mod <- df_long %>% filter(exp != "None")

#confidence model
mod_conf <- lmerTest::lmer(conf ~ alert_name  + (1|id),
               data=df_long_mod)

sjPlot::tab_model(mod_conf)
sjPlot::plot_model(mod_conf,type="eff") 

#confidence + experience model
mod_conf_exp <- lmerTest::lmer(conf ~ exp * alert_name + (1|id),
                           data=df_long)

sjPlot::tab_model(mod_conf,mod_conf_exp)
sjPlot::plot_model(mod_conf_exp,type="int",grid=T,colors = rep(c("#ee0000","#000000"))) + 
  ggthemes::theme_tufte(base_family = "sans") + geom_line() +
  labs(title = "Confidence values",
       y="Confidence",
       x="Experience levels")



#click location 

df_long %>%
  group_by(exp) %>%
  summarize(total=n()) %>%
  right_join(df_long,by="exp") %>%
  group_by(exp,click,total) %>%
  count() %>%
  mutate(prop=round(n/total,2)
           ) %>%
  arrange(-prop) %>%
  na.omit() %>%
  ggplot(aes(exp, click, fill= prop)) + 
  geom_tile() +
  geom_text(aes(exp, click,label=paste0("n=",n,", ","p=",prop)),color="white") +
  ggthemes::theme_tufte(base_family = "sans")




images <- as.data.frame(list.files("/Users/carlpearson/Documents/r_github/openshift_alerting/img/"))
colnames(images) <- "images"
images <- images %>%
  mutate(path="/Users/carlpearson/Documents/r_github/openshift_alerting/img/") %>%
  separate(col = images, into = c("number","drop"),remove = F,sep="-") %>%
  mutate(number=as.numeric(number)) %>%
  arrange(number) %>%
  select(images,path) %>%
  unite(img,c("path","images"),sep="")

img_p <- png::readPNG(images[2,1])

df_long %>%
 filter(alert_number=="alert_2") %>%
  mutate(
    click_1_y = dim(img_p)[1] - click_1_y
  ) %>%
  ggplot(aes(click_1_x,click_1_y))  + 
  annotation_raster(img_p, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  stat_density2d(geom = "polygon", aes(fill=..level..),alpha=.1) + 
  geom_point(size=2,shape=10)+
  scale_fill_gradient(low="green",high="red") + 
  scale_x_continuous(limits=c(0,dim(img_p)[2]),expand=c(0,0))+
  scale_y_continuous(limits=c(0,dim(img_p)[1]),expand=c(0,0))+
  coord_fixed() +
  theme_void() 

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


#experience heatmap
img_p <- png::readPNG(images[1,1])

df_long %>%
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


ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/heatmaps/experience.png",width = 10.05,height = 6,bg="transparent")


img_p <- png::readPNG(images[1,1])

df_long %>%
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
  facet_wrap(~conf_cat,ncol=2)

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/heatmaps/conf.png",width = 10.05,height = 6,bg="transparent")

zval=1.64

#area proportions
df_long %>%
  group_by(alert_name) %>%
  summarize(total=n()) %>%
  right_join(df_long,by="alert_name") %>%
  left_join(area_key) %>%
  group_by(area,total,alert_name) %>%
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
  ggplot(aes(y=prop,x= area, fill= area)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="darkgray",position="dodge") + #add adjusted wald CIs
  ggthemes::theme_tufte(base_family = "sans") + 
  theme(
    axis.text.x = element_text(angle = 45,hjust = 1)
    
  ) +
  facet_wrap(~alert_name)




#loop for combo
for(i in 1:11) {
  
  img_p <- png::readPNG(images[i,1])
  
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
    geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="darkgray",position="dodge") + #add adjusted wald CIs
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



