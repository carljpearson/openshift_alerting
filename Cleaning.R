library(tidyverse)

data <- read_csv("/Users/carlpearson/Documents/r_github/openshift_alerting/alert_1_7_col.csv",
                 col_names = T)

df <- data[3:nrow(data),]

df <- df %>% 
  filter(Status=="IP Address",
         role!="test") %>%
  select(id=ResponseId,
         rh:FL_14_DO) %>%
  mutate(id=as.numeric(as.factor(id))) %>%
  separate(exp, into = c("exp","drop_me"),sep=" – ") %>%
  select(-drop_me) 


df$exp <- factor(df$exp, levels = c("None","Basic","Intermediate","Advanced","Expert"))

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
    click_time_Last=as.numeric(as.character(click_time_Last)),
    click_time_First=as.numeric(as.character(click_time_First))
    )



#df_long$click[df_long$click=="Events" & df_long$click_1_x > 500] <- "Events Activity"

#df_long$click <- if(df_long$click=="Events" & df_long$click_1_x > 500){"Events Activity"} else {df_long$click}


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

df_long %>%
  group_by(exp) %>%
  summarize(total=n()) %>%
  right_join(df_long,by="exp") %>%
  left_join(area_key) %>%
  group_by(exp,area,total) %>%
  count() %>%
  mutate(prop=round(n/total,2)) %>%
  arrange(-prop) %>%
  na.omit() %>%
  ggplot(aes(exp, area, fill= prop)) + 
  geom_tile() +
  geom_text(aes(exp, area,label=paste0("n=",n,", ","p=",prop)),color="white") +
  ggthemes::theme_tufte(base_family = "sans")


df_long %>%
  mutate(click_1_y=as.numeric(click_1_y),
         click_1_x=as.numeric(click_1_x)
         ) %>%
  ggplot(aes(y=-click_1_y,x=click_1_x,color=exp)) +
  geom_point() +
  scale_color_brewer(type="seq")
  
  
  
  