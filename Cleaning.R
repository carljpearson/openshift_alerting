library(tidyverse)

data <- read_csv("/Users/carlpearson/Documents/r_github/openshift_alerting/do_not_upload/alert_data_final.csv",
                 col_names = T)

#removing bad responses
df <- data[3:nrow(data),]

#getting relevant variables
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

df$use <- factor(df$use, levels = c("I'm not sure","Less than once","1-2 times", "3-5 times", "6-10 times","11+ times"))




#write dataframe
#write_csv(df,"/Users/carlpearson/Documents/r_github/openshift_alerting/do_not_upload/df_clean.csv",      col_names = T)

#creating long format df with nested outcome variables
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


long_open <- df %>%
  select(id,
         contains("conf"),
         contains("click"),
         contains("Last")
         #-contains("_open"),
         #-contains("_x"),
         # -contains("_y")
  ) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  separate(variable,into=c("alert_number","variable"),sep="-") %>%
  pivot_wider(names_from = "variable",values_from = "value") %>%
  left_join(area_key) %>%
  left_join(alert_name_key) 

long_open <- long_open[!is.na(long_open$conf_open),]

write_csv(long_open,"/Users/carlpearson/Documents/r_github/openshift_alerting/do_not_upload/open.csv")

#reading in alert names
alert_name_key <- read_csv("/Users/carlpearson/Documents/r_github/openshift_alerting/alert_name_key.csv",
                 col_names = T)

df_long <- df_long %>% left_join(alert_name_key)


df_long_region <- df_long %>%
  group_by(alert_name) %>%
  summarize(total=n()) %>%
  right_join(df_long,by="alert_name") %>%
  left_join(area_key)

df %>%
  select(id,rh,role,exp,use,contains("conf"),alert_name,alert_number,area) %>%
  pivot_longer()


left_join(alert_name_key) %>%
  left_join(area_key) %>%

#l1 analysis

#expertise
df %>%
  ggplot(aes(exp,fill=exp)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2),size=7) +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(y="Count",
       x="Expertise") +
  theme(
        axis.title.x=element_blank(),
        axis.text.x= element_text(size=15),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none" )

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/exp.png",bg="transparent",width = 8,height = 6)

#use
df %>%
  ggplot(aes(use,fill=use)) +
  geom_bar() +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2),size=7) +
  ggthemes::theme_tufte(base_family="sans",base_size = 15) +
  labs(y="Count",
       x="Weekly Use") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(
    axis.text.x= element_text(size=12),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none")

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

  ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/vers.png",bg="transparent",width = 8,height = 6)
  
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

#channel by response env

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
  mutate( #env=str_wrap(env,width=20),
         reso=factor(reso, levels = c("Unsure","No","Sometimes","Often"))
         ) %>% 
  select(-num2) %>%
  pivot_wider(names_from = env,values_from = reso) -> p.custom.1
  #right_join(df) %>%
  
  df %>%
  separate(channel,into = paste0("channel_",1:5),sep=",") %>%
  select(id,contains("channel_"),id,rh) %>%
  pivot_longer(cols=contains("channel_")) %>%
  na.omit(value) %>%
  mutate(value=str_wrap(value,30)) %>% 
  mutate(name=gsub("channel_","",name))
  pivot_wider()
  -> p.custom.2
    
left_join(p.custom.1,p.custom.2,by="id")

  ggplot((aes(value,fill=`My company's production cluster`))) +
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
  





df_long %>%
  ggplot(aes(x=alert_name,y=conf ,fill=alert_name)) +
  geom_violin()+
  ggthemes::theme_tufte(base_family = "sans",base_size = 15) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) + 
  coord_flip(ylim=c(1,7)) +
  labs(x="Confidence")

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/conf_viol.png",width = 12,height = 8)


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

#version heatmap

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


#area proportions overall
df_long %>%
  group_by(alert_name) %>%
  summarize(total=n()) %>%
  right_join(df_long,by="alert_name") %>%
  left_join(area_key) %>%
  group_by(area,total,exp) %>%
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
  facet_wrap(~exp)

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/click_area_all.png",width = 10.05,height = 6,bg="transparent")


#click region by confidence

df_long %>%
  left_join(area_key) %>%
  group_by(area) %>%
  summarise(conf.avg=mean(conf),sd=sd(conf,na.rm = T),n=n(),se= sd/sqrt(n),marg=se*1.64) %>%
  ggplot(aes(x=area,y=conf.avg,fill=area)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymax=conf.avg+marg,ymin=conf.avg-marg))

df_long %>%
  left_join(area_key) %>%
  group_by(area,conf) %>%
  summarise(n=n()) %>%
  group_by(area) %>%
  mutate(total=sum(n),prop=n/total,scale(prop)) %>%
  ggplot(aes(x=area,y=conf,fill=prop)) +
  geom_tile()


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
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="darkgray",position="dodge") + #add adjusted wald CIs
  ggthemes::theme_tufte(base_family = "sans") + 
  theme(
    axis.text.x = element_text(angle = 45,hjust = 1),
    axis.title.x   = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(~alert_name) +
  labs(y="Proportion")

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/sidebar_clicks.png",width = 10.05,height = 6)
