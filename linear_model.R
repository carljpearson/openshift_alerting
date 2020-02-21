#analysis

df_long %>%
  group_by(alert_name) %>%
  summarise(conf_avg=mean(conf),conf_sd=sd(conf))
df_long %>%
  select(conf,click_time_Last,click_count,alert_name) %>%
  psych::describeBy(group = "alert_name") 



#confidence model
mod_conf <- lmerTest::lmer(conf ~ alert_name   + (1|id),
                           data=df_long_mod)

sjPlot::tab_model(mod_conf) -> test
sjPlot::plot_model(mod_conf, type="eff",wrap.labels = 10) 



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

#