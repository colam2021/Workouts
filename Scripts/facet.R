

test1 <- df_gp1 %>% gather(wkout, value, 
                             c(avgIntensity, avgDist))

ggplot(test1) +
  geom_col(mapping = aes(x=days, y=value)) + 
  facet_wrap(~wkout)
