#analysis

df_long %>%
  group_by(alert_name) %>%
  summarise(conf_avg=mean(conf),conf_sd=sd(conf))
df_long %>%
  select(conf,click_time_Last,click_count,alert_name) %>%
  psych::describeBy(group = "alert_name") 


df_long_mod <- df_long %>% filter(exp != "None")

#confidence model
mod_conf <- lmerTest::lmer(conf ~ alert_name   + (1|id),
                           data=df_long_mod)

sjPlot::tab_model(mod_conf) -> test
sjPlot::plot_model(mod_conf, type="eff",wrap.labels = 10) 

df_long %>%
  ggplot(aes(y=conf,x=alert_name,fill=alert_name)) +
  geom_bar(identity=stat_function(fun="mean"))

#confidence + experience model
mod_conf_exp <- lmerTest::lmer(conf ~ exp * alert_name + (1|id),
                               data=df_long)

sjPlot::tab_model(mod_conf,mod_conf_exp)
sjPlot::plot_model(mod_conf_exp,type="int",grid=T,colors = rep(c("#ee0000","#000000"))) + 
  ggthemes::theme_tufte(base_family = "sans") + geom_line() +
  labs(title = "Confidence values",
       y="Confidence",
       x="Experience levels")

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/mod_conf_exp.png",width = 12,height = 8)

#confidence + region

mod_conf_area <- lmerTest::lmer(conf ~  area + (1|id),
                                data=df_long_region)

sjPlot::tab_model(mod_conf,mod_conf_area)
sjPlot::plot_model(mod_conf_area,type="eff") 

ggsave("/Users/carlpearson/Documents/r_github/openshift_alerting/plots/mod_conf_exp.png",width = 12,height = 8)