library(tidyverse)

#data key
alert_name_key <- read_csv("/Users/carlpearson/Documents/r_github/openshift_alerting/alert_name_key.csv",
                           col_names = T)
#main data
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
  mutate( #recode variables and make proper format
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


#create df for open response coding in
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

long_open <- long_open[!is.na(long_open$conf_open),] #

#write_csv(long_open,"/Users/carlpearson/Documents/r_github/openshift_alerting/do_not_upload/open.csv")

#reading in alert names


df_long <- df_long %>% left_join(alert_name_key)


df_long_region <- df_long %>%
  group_by(alert_name) %>%
  summarize(total=n()) %>%
  right_join(df_long,by="alert_name") %>%
  left_join(area_key)



