######################### PRELIMINARIES #########################

# Author: Lewis Elliott
# Title: BIS - Bluespace distance paper
# Project: BlueHealth
# Created: 20190711
# Last updated: 20190917

rm(list=ls()) # removes all objects
pacman::p_unload("all") # detaches all packages
setwd("U:/20160301_BH/20160301_Survey/20170713_Data/Papers/20190711_Proximity") # sets working directory
load("U:/20160301_BH/20160301_Survey/20170713_Data/Final Datasets/20190306_bis.RData") # loads bis
pacman::p_load(tidyverse,glue,mgcv,sjstats,maps,wesanderson,cowplot,lme4,broom.mixed)
bis <- as_tibble(bis)
glimpse(bis)

######################### RESCALING WEIGHTS FOR LATER USE IN REGRESSION #########################

# creating a cluster variable

bis <- bis %>%
  mutate(cluster = if_else(country=="California"|country=="Canada"|country=="Estonia"|country=="Australia"|country=="Hong Kong", 
                           as.character(country), as.character(region)),
         cluster = glue("{cluster} {age_sex}") %>% as_factor())
fct_count(bis$cluster)

# redoing cluster for California as their grouping structure differed

bis <- bis %>%
  mutate(cluster = if_else(cluster=="California Male 30-39"|cluster=="California Male 40-49"|cluster=="California Male 50-59",
                           "California Male 30-64",
                           if_else(cluster=="California Male 60 and above", 
                                   "California Male 65 and above",
                                   if_else(cluster=="California Female 30-39"|cluster=="California Female 40-49"|cluster=="California Female 50-59",
                                           "California Female 30-64",
                                           if_else(cluster=="California Female 60 and above", 
                                                   "California Female 65 and above",
                                                   as.character(cluster))))))

bis$cluster <- factor(bis$cluster)

# scaling weights by country

bis <- bis %>%
  group_by(country) %>% # performs all following operations by country
  scale_weights(cluster,weight_var) %>%
  ungroup()

# new survey weights are found under svywght_a and svywght_b

summary(bis$svywght_a)
summary(bis$svywght_b)

######################### CREATING OUTCOME VARIABLES #########################

# creating weekly variables for each environment

weekly_yes <- function(item){fct_recode(item,
                                        'No'="Not at all in the last four weeks",
                                        'No'="Once or twice in the last four weeks",
                                        'Yes'="Once a week",
                                        'Yes'="Several times a week")}
vars <- bis %>% select(local_park_4wk:sea_4wk) %>% colnames()
vars <- set_names(vars,glue("{vars}_weekly"))
bis <- bis %>% mutate_at(.,vars,weekly_yes)
bis %>% select(local_park_4wk_weekly:sea_4wk_weekly) %>% purrr::map(~fct_count(.)) # looks good

rm(weekly_yes,vars) # removing functions and temporary character variables

# combining coastal blue space and riverside blue space categories

bis <- bis %>%
  mutate(coast_weekly = factor(if_else(esplanade_4wk_weekly=="Yes"|pier_4wk_weekly=="Yes"|harbour_4wk_weekly=="Yes"|
                                         beach_4wk_weekly=="Yes"|rocky_shore_4wk_weekly=="Yes"|cliff_4wk_weekly=="Yes"|
                                         lagoon_4wk_weekly=="Yes"|sea_4wk_weekly=="Yes",
                                       "Yes","No"),levels=c("No","Yes")),
         river_weekly = factor(if_else(urban_river_4wk_weekly=="Yes"|rural_river_4wk_weekly=="Yes",
                                       "Yes","No"), levels=c("No","Yes")),
         lake_weekly = lake_4wk_weekly)

bis %>% select(coast_weekly:lake_weekly) %>% purrr::map(~fct_count(.)) # looking good, and n's not too bad either

######################### CREATING COVARIATES #########################

bis <- bis %>% 
  mutate(
    # work status
    work_new = fct_recode(work_status,
                          'Employed'="In paid work (or away temporarily) (employee, self-employed, working for your family business)",
                          'Unemployed'="Unemployed and actively looking for a job",
                          'Unemployed'="Unemployed, wanting a job but not actively looking for a job",
                          'In education'="In education, (not paid for by employer) even if on vacation",
                          'Housework'="Doing housework, looking after children, or other persons",
                          'Retired'="Retired",
                          'Disabled'="Permanently sick or disabled",
                          'Other/unsure'="In community or military service",
                          'Other/unsure'="Other",
                          'Other/unsure'="Do not know"),
    # education
    edu_new = fct_recode(education,
                         'Primary'="Did not complete primary education",
                         'Primary'="Completed primary education",
                         'Secondary'="Completed secondary/further education (up to 18 years of age)",
                         'University'="Completed higher education (e.g. university degree or higher)"),
    # financial strain
    financial_strain_2cats = factor(if_else(income_perceived=="Coping on present income"|income_perceived=="Living comfortably on present income",
                                            "Coping","Not coping"),levels=c("Coping","Not coping")),
    # urban
    urban = as_factor(if_else(pop_dens_1km>=150, "Urban","Rural")), # NOTE THIS INCLUDES -9999.00s AS RURAL. ALSO EXTRA 9 PEOPLE OUTSIDE OF DATASET BOUNDARIES.
    # season
    season = factor(if_else(country=="Australia",
                            fct_recode(wave,'Autumn'="Jun-17",'Winter'="Sep-17",'Spring'="Dec-17",'Summer'="Mar-18"), # to account for hemisphere
                            fct_recode(wave,'Spring'="Jun-17",'Summer'="Sep-17",'Autumn'="Dec-17",'Winter'="Mar-18")),
                    levels=c("Spring","Summer","Autumn","Winter")),
    # new country
    country_new = fct_recode(country, 'Queensland, AU'="Australia",'California, US'="California") %>% fct_relevel("Queensland, AU",after=14)
  )

######################### CREATING AN ESTIMATION SAMPLE #########################

bis2 <- bis %>%
  filter(flag_homes=="Reliable" & 
           flag_homes_incongruent=="Congruent" & 
           flag_times=="Include" & 
           flag_straightliner=="Include") %>%
  filter_at(vars(id, country_new,
                 coast_weekly, river_weekly, lake_weekly,
                 dist_coast_km,
                 svywght_a, svywght_b,
                 home_latitude, home_longitude),
            all_vars(!is.na(.))) %>%
  select(id, country_new,
         coast_weekly, river_weekly, lake_weekly,
         dist_coast_km, dist_river_km, dist_lake_km,
         svywght_a, svywght_b,
         home_latitude, home_longitude)

######################### MAP OF LOCATIONS #########################

pal <- wes_palette("GrandBudapest2", 3, "discrete")
pal_map <- wes_palette("Zissou1", 1, "discrete")

# Spain

spain <- map_data("world", region="Spain")
spain_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=spain, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(spain$lat)-2 & 
                                    home_latitude<max(spain$lat) &
                                    home_longitude>min(spain$long) &
                                    home_longitude<max(spain$long) &
                                    country_new=="Spain"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
      ggtitle("Spain")

# Portugal

portugal <- map_data("world", region="Portugal")

portugal_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=portugal, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(portugal$lat) & 
                                    home_latitude<max(portugal$lat) &
                                    home_longitude>min(portugal$long) &
                                    home_longitude<max(portugal$long) &
                                    country_new=="Portugal"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Portugal")

# France

france <- map_data("world", region="France")
france_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=france, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(france$lat) & 
                                    home_latitude<max(france$lat) &
                                    home_longitude>min(france$long) &
                                    home_longitude<max(france$long) &
                                    country_new=="France"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("France")

# Ireland

ireland <- map_data("world", region="Ireland")
ireland_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=ireland, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(ireland$lat) & 
                                    home_latitude<max(ireland$lat) &
                                    home_longitude>min(ireland$long) &
                                    home_longitude<max(ireland$long) &
                                    country_new=="Ireland"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Ireland")

# United Kingdom

uk <- map_data("world", region="UK")

uk_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=uk, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(uk$lat) & 
                                    home_latitude<max(uk$lat) &
                                    home_longitude>min(uk$long) &
                                    home_longitude<max(uk$long) &
                                    country_new=="United Kingdom"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("United Kingdom")

# Netherlands

netherlands <- map_data("world", region="Netherlands")
netherlands_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=netherlands, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(netherlands$lat) & 
                                    home_latitude<max(netherlands$lat) &
                                    home_longitude>min(netherlands$long) &
                                    home_longitude<max(netherlands$long) &
                                    country_new=="Netherlands"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Netherlands")

# Germany

germany <- map_data("world", region="Germany")
germany_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=germany, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(germany$lat) & 
                                    home_latitude<max(germany$lat) &
                                    home_longitude>min(germany$long) &
                                    home_longitude<max(germany$long) &
                                    country_new=="Germany"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Germany")

# Italy

italy <- map_data("world", region="Italy")
italy_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=italy, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(italy$lat) & 
                                    home_latitude<max(italy$lat) &
                                    home_longitude>min(italy$long) &
                                    home_longitude<max(italy$long) &
                                    country_new=="Italy"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Italy")

# Czech Republic

czech_republic <- map_data("world", region="Czech Republic")
czech_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=czech_republic, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(czech_republic$lat) & 
                                    home_latitude<max(czech_republic$lat) &
                                    home_longitude>min(czech_republic$long) &
                                    home_longitude<max(czech_republic$long) &
                                    country_new=="Czech Republic"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Czech Republic")

# Sweden

sweden <- map_data("world", region="Sweden")
sweden_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=sweden, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(sweden$lat) & 
                                    home_latitude<max(sweden$lat) &
                                    home_longitude>min(sweden$long) &
                                    home_longitude<max(sweden$long) &
                                    country_new=="Sweden"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Sweden")

# Finland

finland <- map_data("world", region="Finland")
finland_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=finland, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(finland$lat) & 
                                    home_latitude<max(finland$lat) &
                                    home_longitude>min(finland$long) &
                                    home_longitude<max(finland$long) &
                                    country_new=="Finland"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Finland")

# Bulgaria

bulgaria <- map_data("world", region="Bulgaria")
bulgaria_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=bulgaria, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(bulgaria$lat) & 
                                    home_latitude<max(bulgaria$lat) &
                                    home_longitude>min(bulgaria$long) &
                                    home_longitude<max(bulgaria$long) &
                                    country_new=="Bulgaria"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Bulgaria")

# Greece

greece <- map_data("world", region="Greece")
greece_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=greece, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(greece$lat) & 
                                    home_latitude<max(greece$lat) &
                                    home_longitude>min(greece$long) &
                                    home_longitude<max(greece$long) &
                                    country_new=="Greece"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Greece")

# Estonia

estonia <- map_data("world", region="Estonia")
estonia_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=estonia, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(estonia$lat) & 
                                    home_latitude<max(estonia$lat) &
                                    home_longitude>min(estonia$long) &
                                    home_longitude<max(estonia$long) &
                                    country_new=="Estonia"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Estonia")

# Australia

australia <- map_data("world", region="Australia")
australia_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=australia, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(australia$lat) & 
                                    home_latitude<max(australia$lat) &
                                    home_longitude>min(australia$long) &
                                    home_longitude<max(australia$long) &
                                    country_new=="Queensland, AU"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ylim(-45,-9) +
  ggtitle("Queensland, AU")

# Hong Kong

pacman::p_load(sp)
hk <- readRDS("hk.rds")
hk <- fortify(hk)
hk_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=hk, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(hk$lat) & 
                                    home_latitude<22.555 &
                                    home_longitude>min(hk$long) &
                                    home_longitude<max(hk$long) &
                                    country_new=="Hong Kong"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Hong Kong")
pacman::p_unload(sp)

# Canada

canada <- map_data("world", region="Canada")
canada_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=canada, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(canada$lat) & 
                                    home_latitude<max(canada$lat) &
                                    home_longitude>min(canada$long) &
                                    home_longitude<max(canada$long) &
                                    country_new=="Canada"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("Canada")

# California

california <- map_data("state", region="California")
california_plot <- ggplot() + 
  coord_fixed(1.3) +
  geom_polygon(data=california, aes(x=long, y=lat, group=group), 
               fill = "light grey", color = "black") + 
  geom_point(data=bis2 %>% filter(home_latitude>min(california$lat) & 
                                    home_latitude<max(california$lat) &
                                    home_longitude>min(california$long) &
                                    home_longitude<max(california$long) &
                                    country_new=="California, US"), aes(x=home_longitude,y=home_latitude), colour=pal_map, size=0.75) +
  theme(panel.background = element_blank()) +
  ggtitle("California, US")

# arranging into one figure

map_plot <- plot_grid(bulgaria_plot, california_plot, canada_plot, czech_plot, estonia_plot, finland_plot, france_plot,
          germany_plot, greece_plot, hk_plot, ireland_plot, italy_plot, netherlands_plot, portugal_plot,
          australia_plot, spain_plot, sweden_plot, uk_plot,
          nrow=3, ncol=6)
map_plot

pdf("Figure 1.pdf", width=18, height=10)
map_plot
dev.off()
dev.off()
dev.off()

# clearing up workspace

rm(australia, australia_plot, bulgaria, bulgaria_plot, california, california_plot, canada, canada_plot, 
   czech_republic, czech_plot, estonia, estonia_plot, finland, finland_plot, france, france_plot, germany, germany_plot, 
   greece, greece_plot, hk, hk_plot, ireland, ireland_plot, italy, italy_plot, netherlands, netherlands_plot, portugal, portugal_plot, spain, spain_plot, 
   sweden, sweden_plot, uk, uk_plot)

######################### BASIC DESCRIPTIVES AND DISTRIBUTIONS #########################

# eliminating Mellila people for ECRINS data

  # extremes

  bis2 %>%
    filter(dist_lake_km>=100) %>%
    view() # One in Menorca, and one in Melilla: 10219, 10268

  bis2 %>%
    filter(dist_coast_km>=500) %>%
    view() # Cezchs, Canadians, and Australians
  
bis2 <- bis2 %>%
  mutate(dist_lake_km = if_else(dist_lake_km<=100, dist_lake_km, NULL))

bis2 %>%
  filter(dist_lake_km>60) %>%
  view()

# ranges

round(range(bis2$dist_coast_km,na.rm = T),3)
round(range(bis2$dist_river_km,na.rm = T),3)
round(range(bis2$dist_lake_km,na.rm = T),3)

distr_untr <- bis2 %>%
  gather(key="environment", value="distance", dist_coast_km:dist_lake_km) %>%
  mutate(environment=fct_recode(environment, 'Coasts'='dist_coast_km',
                                'Rivers'='dist_river_km',
                                'Lakes'='dist_lake_km') %>%
           factor(levels=c("Coasts","Lakes","Rivers"))) %>%
  ggplot(aes(x=distance, group=environment, fill=environment, colour=environment)) +
  geom_density(aes(y=..count..), alpha=0.5) +
  scale_fill_manual(values=pal) +
  scale_colour_manual(values=pal) +
  facet_wrap(~environment, scales="free") +
  scale_x_continuous(name="Residential distance (km)") +
  scale_y_continuous(name="Count") +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45,hjust=1))

pdf("Figure 2.pdf", width=10, height=4)
distr_untr
dev.off()
dev.off()
dev.off()

######################### MODELLING GAMS #########################

# a few zero weights exist - adding a tiny constant

bis2 <- bis2 %>%
  mutate(svywght_a_new=if_else(svywght_a==0, 0.0000000001, svywght_a))
summary(bis2$svywght_a_new)

# random intercept models

coast_fit <- gam(coast_weekly ~ s(dist_coast_km) + s(country_new,bs="re"),
                       data=bis2, family="binomial", weights=svywght_a_new, method="ML")

river_fit <- gam(river_weekly ~ s(dist_river_km) + s(country_new,bs="re"),
                       data=bis2, family="binomial", weights=svywght_a_new, method="ML")

lake_fit <- gam(lake_weekly ~ s(dist_lake_km) + s(country_new,bs="re"),
                      data=bis2, family="binomial", weights=svywght_a_new, method="ML")

# random intercept and slope models

coast_fit_slope <- gam(coast_weekly ~ s(dist_coast_km) + s(country_new,bs="re") + s(country_new,dist_coast_km,bs="re",m=1),
                 data=bis2, family="binomial", weights=svywght_a_new, method="ML")

river_fit_slope <- gam(river_weekly ~ s(dist_river_km) + s(country_new,bs="re") + s(country_new,dist_river_km,bs="re",m=1),
                 data=bis2, family="binomial", weights=svywght_a_new, method="ML")

lake_fit_slope <- gam(lake_weekly ~ s(dist_lake_km) + s(country_new,bs="re") + s(country_new,dist_lake_km,bs="re",m=1),
                data=bis2, family="binomial", weights=svywght_a_new, method="ML")

# ascertaining differences between models with and without random slopes

anova(coast_fit,coast_fit_slope,test="LRT")
AIC(coast_fit)
AIC(coast_fit_slope)
anova(river_fit,river_fit_slope,test="LRT")
AIC(river_fit)
AIC(river_fit_slope)
anova(lake_fit,lake_fit_slope,test="LRT")
AIC(lake_fit)
AIC(lake_fit_slope)

# new data for plotting properly over sensible distances

new_data_coast <- tibble(dist_coast_km = rep(seq(0, 100, 0.1),18),
                         country_new = rep(levels(bis2$country_new),1001))
bis2 <- bis2 %>%
  mutate(ecrins_countries = if_else(country_new=="Bulgaria"|country_new=="Czech Republic"|country_new=="Estonia"|country_new=="Finland"|
                                      country_new=="France"|country_new=="Germany"|country_new=="Greece"|country_new=="Ireland"|
                                      country_new=="Italy"|country_new=="Netherlands"|country_new=="Portugal"|country_new=="Spain"|
                                      country_new=="Sweden"|country_new=="United Kingdom", country_new, NULL),
         ecrins_countries = fct_drop(ecrins_countries)) # for conditional selection below
levels(bis2$ecrins_countries)

new_data_river <- tibble(dist_river_km = rep(seq(0, 10, 0.01),14),
                         country_new = rep(levels(bis2$ecrins_countries),1001))

new_data_lake <- tibble(dist_lake_km = rep(seq(0, 20, 0.1),14),
                        country_new = rep(levels(bis2$ecrins_countries),201))

# getting predicted values from regressions and augmenting them with data needed for plotting

coast_pred <- 
  predict(coast_fit_slope,
          new_data_coast,
          type="link",
          se.fit=TRUE, 
          exclude=c("s(country_new)","s(country_new,dist_coast_km)")) %>%
  as_tibble() %>%
  mutate(distance = rep(seq(0, 100, 0.1),18),
         country_new = rep(levels(bis2$country_new),1001),
         lci=coast_fit_slope$family$linkinv(fit-1.96*se.fit),
         estimate=coast_fit_slope$family$linkinv(fit),
         uci=coast_fit_slope$family$linkinv(fit+1.96*se.fit),
         environment="Coasts")

coast_pred_countries <- 
  predict(coast_fit_slope,
          new_data_coast,
          type="link",
          se.fit=TRUE) %>%
  as_tibble() %>%
  mutate(distance = rep(seq(0, 100, 0.1),18),
         country_new = rep(levels(bis2$country_new),1001),
         lci=coast_fit_slope$family$linkinv(fit-1.96*se.fit),
         estimate=coast_fit_slope$family$linkinv(fit),
         uci=coast_fit_slope$family$linkinv(fit+1.96*se.fit),
         environment="Coasts")

lake_pred <- 
  predict(lake_fit_slope,
          new_data_lake,
          type="link",
          se.fit=TRUE, 
          exclude=c("s(country_new)","s(country_new,dist_lake_km)")) %>%
  as_tibble() %>%
  mutate(distance =  rep(seq(0, 20, 0.1),14),
         country_new = rep(levels(bis2$ecrins_countries),201),
         lci=lake_fit_slope$family$linkinv(fit-1.96*se.fit),
         estimate=lake_fit_slope$family$linkinv(fit),
         uci=lake_fit_slope$family$linkinv(fit+1.96*se.fit),
         environment="Lakes")

lake_pred_countries <- 
  predict(lake_fit_slope,
          new_data_lake,
          type="link",
          se.fit=TRUE) %>%
  as_tibble() %>%
  mutate(distance = rep(seq(0, 20, 0.1),14),
         country_new = rep(levels(bis2$ecrins_countries),201),
         lci=lake_fit_slope$family$linkinv(fit-1.96*se.fit),
         estimate=lake_fit_slope$family$linkinv(fit),
         uci=lake_fit_slope$family$linkinv(fit+1.96*se.fit),
         environment="Lakes")

river_pred <- 
  predict(river_fit_slope,
          new_data_river,
          type="link",
          se.fit=TRUE, 
          exclude=c("s(country_new)","s(country_new,dist_river_km)")) %>%
  as_tibble() %>%
  mutate(distance = rep(seq(0, 10, 0.01),14),
         country_new = rep(levels(bis2$ecrins_countries),1001),
         lci=river_fit_slope$family$linkinv(fit-1.96*se.fit),
         estimate=river_fit_slope$family$linkinv(fit),
         uci=river_fit_slope$family$linkinv(fit+1.96*se.fit),
         environment="Rivers")

river_pred_countries <- 
  predict(river_fit_slope,
          new_data_river,
          type="link",
          se.fit=TRUE) %>%
  as_tibble() %>%
  mutate(distance = rep(seq(0, 10, 0.01),14),
         country_new = rep(levels(bis2$ecrins_countries),1001),
         lci=river_fit_slope$family$linkinv(fit-1.96*se.fit),
         estimate=river_fit_slope$family$linkinv(fit),
         uci=river_fit_slope$family$linkinv(fit+1.96*se.fit),
         environment="Rivers")

new_data_sensible <- bind_rows(coast_pred,lake_pred,river_pred)
new_data_sensible_countries <- bind_rows(coast_pred_countries,lake_pred_countries,river_pred_countries)

# plotting the predicted values from the regressions

distance_sense <- ggplot(new_data_sensible,aes(y=estimate,ymin=lci,ymax=uci,x=distance,group=environment,colour=environment,fill=environment)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  scale_colour_manual(values=pal) +
  scale_fill_manual(values=pal) +
  geom_vline(data=new_data_sensible %>% filter(environment=="Coasts"), aes(xintercept=1), colour=pal[1]) +
  geom_vline(data=new_data_sensible %>% filter(environment=="Coasts"), aes(xintercept=5), colour=pal[1]) +
  geom_vline(data=new_data_sensible %>% filter(environment=="Coasts"), aes(xintercept=25), colour=pal[1]) +
  geom_vline(data=new_data_sensible %>% filter(environment=="Coasts"), aes(xintercept=50), colour=pal[1]) +
  geom_vline(data=new_data_sensible %>% filter(environment=="Lakes"), aes(xintercept=1), colour=pal[2]) +
  geom_vline(data=new_data_sensible %>% filter(environment=="Lakes"), aes(xintercept=5), colour=pal[2]) +
  geom_vline(data=new_data_sensible %>% filter(environment=="Rivers"), aes(xintercept=1), colour=pal[3]) +
  geom_vline(data=new_data_sensible %>% filter(environment=="Rivers"), aes(xintercept=2.5), colour=pal[3]) +
  facet_wrap(~environment, scales="free_x") +
  scale_x_continuous(name="Residential distance (km)") +
  scale_y_continuous(name="Probability of visiting at least weekly",limits=c(0,1)) +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45,hjust=1))
distance_sense

strat_coast <- ggplot(new_data_sensible_countries %>% filter(environment=="Coasts" & country_new!="Czech Republic"),
                      aes(y=estimate,ymin=lci,ymax=uci,x=distance,colour=pal[1],fill=pal[1])) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  scale_colour_manual(values=pal[1]) +
  scale_fill_manual(values=pal[1]) +
  geom_vline(data=new_data_sensible_countries %>% filter(environment=="Coasts" & country_new!="Czech Republic"), aes(xintercept=1), colour=pal[1]) +
  geom_vline(data=new_data_sensible_countries %>% filter(environment=="Coasts" & country_new!="Czech Republic"), aes(xintercept=5), colour=pal[1]) +
  geom_vline(data=new_data_sensible_countries %>% filter(environment=="Coasts" & country_new!="Czech Republic"), aes(xintercept=25), colour=pal[1]) +
  geom_vline(data=new_data_sensible_countries %>% filter(environment=="Coasts" & country_new!="Czech Republic"), aes(xintercept=50), colour=pal[1]) +
  scale_x_continuous(name="Residential distance to the coast (km)") +
  scale_y_continuous(name="Probability of visiting the coast at least weekly") +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45,hjust=1)) +
  facet_wrap(~country_new,nrow=4)

strat_lake <- ggplot(new_data_sensible_countries %>% filter(environment=="Lakes"),
                     aes(y=estimate,ymin=lci,ymax=uci,x=distance,colour=pal[2],fill=pal[2])) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  scale_colour_manual(values=pal[2]) +
  scale_fill_manual(values=pal[2]) +
  geom_vline(data=new_data_sensible_countries %>% filter(environment=="Lakes"), aes(xintercept=1), colour=pal[2]) +
  geom_vline(data=new_data_sensible_countries %>% filter(environment=="Lakes"), aes(xintercept=5), colour=pal[2]) +
  scale_x_continuous(name="Residential distance to nearest lake (km)") +
  scale_y_continuous(name="Probability of visiting a lake at least weekly") +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45,hjust=1)) +
  facet_wrap(~country_new,nrow=3)

strat_river <- ggplot(new_data_sensible_countries %>% filter(environment=="Rivers"),
                      aes(y=estimate,ymin=lci,ymax=uci,x=distance,colour=pal[3],fill=pal[3])) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  scale_colour_manual(values=pal[3]) +
  scale_fill_manual(values=pal[3]) +
  geom_vline(data=new_data_sensible_countries %>% filter(environment=="Rivers"), aes(xintercept=1), colour=pal[3]) +
  geom_vline(data=new_data_sensible_countries %>% filter(environment=="Rivers"), aes(xintercept=2.5), colour=pal[3]) +
  scale_x_continuous(name="Residential distance to nearest river (km)") +
  scale_y_continuous(name="Probability of visiting a river at least weekly") +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45,hjust=1)) +
  facet_wrap(~country_new,nrow=3)

strats <- plot_grid(strat_coast,strat_lake,strat_river,nrow=3)

pdf("Supplementary Figure 1_slopes.pdf", width=10, height=14)
strats
dev.off()
dev.off()
dev.off()

rm(strat_coast,strat_lake,strat_river)

# and larger distances

new_data_coast <- tibble(dist_coast_km = rep(seq(0, 1192, 0.1),18),
                         country_new = rep(levels(bis2$country_new),11921))
new_data_lake <- tibble(dist_lake_km = rep(seq(0, 70, 0.1),14),
                        country_new = rep(levels(bis2$ecrins_countries),701))
new_data_river <- tibble(dist_river_km = rep(seq(0, 20, 0.01),14),
                         country_new = rep(levels(bis2$ecrins_countries),2001))

coast_pred_full <- 
  predict(coast_fit_slope,
          new_data_coast,
          type="link",
          se.fit=TRUE, 
          exclude=c("s(country_new)","s(country_new,dist_coast_km)")) %>%
  as_tibble() %>%
  mutate(distance = rep(seq(0, 1192, 0.1),18),
         country_new = rep(levels(bis2$country_new),11921),
         lci=coast_fit_slope$family$linkinv(fit-1.96*se.fit),
         estimate=coast_fit_slope$family$linkinv(fit),
         uci=coast_fit_slope$family$linkinv(fit+1.96*se.fit),
         environment="Coasts")

lake_pred_full <- 
  predict(lake_fit_slope,
          new_data_lake,
          type="link",
          se.fit=TRUE, 
          exclude=c("s(country_new)","s(country_new,dist_lake_km)")) %>%
  as_tibble() %>%
  mutate(distance =  rep(seq(0, 70, 0.1),14),
         country_new = rep(levels(bis2$ecrins_countries),701),
         lci=lake_fit_slope$family$linkinv(fit-1.96*se.fit),
         estimate=lake_fit_slope$family$linkinv(fit),
         uci=lake_fit_slope$family$linkinv(fit+1.96*se.fit),
         environment="Lakes")

river_pred_full <- 
  predict(river_fit_slope,
          new_data_river,
          type="link",
          se.fit=TRUE, 
          exclude=c("s(country_new)","s(country_new,dist_river_km)")) %>%
  as_tibble() %>%
  mutate(distance = rep(seq(0, 20, 0.01),14),
         country_new = rep(levels(bis2$ecrins_countries),2001),
         lci=river_fit_slope$family$linkinv(fit-1.96*se.fit),
         estimate=river_fit_slope$family$linkinv(fit),
         uci=river_fit_slope$family$linkinv(fit+1.96*se.fit),
         environment="Rivers")

new_data_full <- bind_rows(coast_pred_full,lake_pred_full,river_pred_full)


distance_range <- ggplot(new_data_full,
                         aes(y=estimate,ymin=lci,ymax=uci,x=distance,group=environment,colour=environment,fill=environment)) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  scale_colour_manual(values=pal) +
  scale_fill_manual(values=pal) +
  facet_wrap(~environment, scales="free_x") +
  scale_x_continuous(name="Residential distance (km)") +
  scale_y_continuous(name="Probability of visiting at least weekly") +
  theme(legend.position = "none",
        axis.text.x=element_text(angle=45,hjust=1))

pdf("Figure 3.pdf", width=10, height=4)
distance_sense
dev.off()
dev.off()
dev.off()

pdf("Supplementary Figure 2.pdf", width=10, height=4)
distance_range
dev.off()
dev.off()
dev.off()

# clearing up workspace

rm(coast_pred,coast_pred_countries,coast_pred_full,lake_pred,lake_pred_countries,lake_pred_full,river_pred,river_pred_countries,river_pred_full,
   new_data_coast,new_data_lake,new_data_river,new_data_sensible,new_data_sensible_countries,new_data_full)

######################### NOW MIXED MODELS #########################

# first adding categorical variables to bis2

bis2 <- bis2 %>%
  mutate(coast_cats = factor(if_else(dist_coast_km>=0 & dist_coast_km<=1, "0-1km",
                                     if_else(dist_coast_km>1 & dist_coast_km<=5, ">1-5km",
                                             if_else(dist_coast_km>5 & dist_coast_km<=25, ">5-25km",
                                                     if_else(dist_coast_km>25 & dist_coast_km<=50, ">25-50km", ">50km")))),
                             levels=c("0-1km",">1-5km",">5-25km",">25-50km",">50km")),
         river_cats = factor(if_else(dist_river_km>=0 & dist_river_km<=1, "0-1km",
                                     if_else(dist_river_km>1 & dist_river_km<=2.5, ">1-2.5km", ">2.5km")),
                             levels=c("0-1km",">1-2.5km",">2.5km")),
         lake_cats = factor(if_else(dist_lake_km>=0 & dist_lake_km<=1, "0-1km",
                                    if_else(dist_lake_km>1 & dist_lake_km<=5, ">1-5km", ">5km")),
                            levels=c("0-1km",">1-5km",">5km")))

# next, forming binomial random intercept models

coast_fit_glmer <- glmer(coast_weekly ~ fct_rev(coast_cats) + (fct_rev(coast_cats)|country_new),
                         data=bis2, family="binomial", weights=svywght_a_new, 
                         control=glmerControl(calc.derivs=F, optCtrl = list(maxfun=200000)), nAGQ = 1)
river_fit_glmer <- glmer(river_weekly ~ fct_rev(river_cats) + (fct_rev(river_cats)|country_new),
                         data=bis2, family="binomial", weights=svywght_a_new, 
                         control=glmerControl(calc.derivs=F, optCtrl = list(maxfun=200000)), nAGQ = 1)
lake_fit_glmer <- glmer(lake_weekly ~ fct_rev(lake_cats) + (fct_rev(lake_cats)|country_new),
                        data=bis2, family="binomial", weights=svywght_a_new, 
                        control=glmerControl(calc.derivs=F, optCtrl = list(maxfun=200000)), nAGQ = 1)

# and summarising

pacman::p_load(sjPlot)
tab_model(coast_fit_glmer)
tab_model(lake_fit_glmer)
tab_model(river_fit_glmer)

# summarising previous gams for supplementary materials
summary(coast_fit_slope,digits=10) # for effective degrees of freedom
tidy(coast_fit_slope) %>% view()
tab_model(coast_fit_slope) # largely unhelpful but gives Tjur's R2
gam.vcomp(coast_fit_slope) # sds of random effects

summary(lake_fit_slope,digits=10)
tidy(lake_fit_slope) %>% view()
tab_model(lake_fit_slope)
gam.vcomp(lake_fit_slope)[[2]]^2
gam.vcomp(lake_fit_slope)[[3]]^2

summary(river_fit_slope)
tidy(river_fit_slope) %>% view()
tab_model(river_fit_slope)
gam.vcomp(river_fit_slope)[[2]]^2
gam.vcomp(river_fit_slope)[[3]]^2

# some descriptives in response to reviewer 2

fct_count(bis2$coast_cats) %>% mutate(perc=n/sum(n))
fct_count(bis2$lake_cats) %>% slice(1:3) %>% mutate(perc=n/sum(n))
fct_count(bis2$river_cats) %>% slice(1:3) %>% mutate(perc=n/sum(n))

bis2 %>%
  group_by(country_new, coast_cats) %>%
  count() %>%
  pivot_wider(names_from = coast_cats, values_from = n) %>%
  bind_cols(bis2 %>%
              group_by(country_new, lake_cats) %>%
              count() %>%
              pivot_wider(names_from = lake_cats, values_from = n)) %>%
  bind_cols(bis2 %>%
              group_by(country_new, river_cats) %>%
              count() %>%
              pivot_wider(names_from = river_cats, values_from = n)) %>%
  select(-`NA`, -NA1, -country_new1, -country_new2) -> des_by_country

write.table(des_by_country, "clipboard", sep="\t")

######################### END OF SCRIPT #########################