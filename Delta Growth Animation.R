library(tidyverse)
library(gganimate)
library(hrbrthemes)

voc_date_list <- list.files(path="Data/variants",pattern="voc-dates *", full.names=TRUE)
voc_date <- read_csv(voc_date_list[44],
                     col_types = cols(`DATE` = col_date(format = "%Y-%m-%d")))

delta_anim_data <- NULL
for(i in 44:length(voc_date_list)) {
  
  temp_date <- str_replace(voc_date_list[i], "Data/variants/voc-dates ","")
  temp_date <- str_replace(temp_date, ".csv","")
  temp_date <- as.Date(temp_date)
  
  temp_data <- read_csv(voc_date_list[i],
                        col_types = cols(`DATE` = col_date(format = "%Y-%m-%d")))
  
  temp_data <- temp_data %>%
    mutate(KEY_FRAME = i-43) %>%
    select(KEY_FRAME, DATE, B1617) %>%
    filter(DATE >= as.Date("2021-04-08")) %>%
    mutate(KEY_DATE = temp_date)
  
  delta_anim_data <- rbind(delta_anim_data, temp_data)
  
}

delta_anim_data <- delta_anim_data %>%
  group_by(DATE) %>%
  mutate(B_DIFF = B1617 - lag(B1617)) %>%
  replace_na(list(B_DIFF = 0)) %>%
  mutate(B_OLD = B1617 - B_DIFF)

delta_anim_data_long <- delta_anim_data %>%
  select(KEY_FRAME, KEY_DATE, B_OLD, B_DIFF) %>%
  pivot_longer(cols=c("B_OLD", "B_DIFF"), names_to="TYPE", values_to="COUNT") %>%
  filter(COUNT > 0)

delta_animation <- delta_anim_data_long %>%
  ggplot(aes(x=DATE, y=COUNT, group=KEY_FRAME)) +
  geom_col(aes(color=TYPE, fill=TYPE)) +
  scale_color_manual(values=c("#FFFFFF","#987CA6"), labels=c("New cases","Existing cases")) +
  scale_fill_manual(values=c("#FFFFFF","#987CA6"), labels=c("New cases","Existing cases")) +
  theme_ft_rc() +
  labs(title="How Delta Cases Grow in Alberta",
       subtitle="Daily additions and removals of B.1.617 Delta cases",
       x="Date", y="Daily case count") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  transition_states(states = KEY_FRAME, transition_length = 2, state_length=1) +
  enter_fade()

delta_final <- animate(delta_animation, fps=24, nframes=528, end_pause=48,
        width=1280, height=720, units="px", res=144
        )

anim_save(filename="delta-growth.gif",animation=delta_final)