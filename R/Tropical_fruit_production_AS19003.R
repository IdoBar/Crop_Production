devtools::source_gist("7f63547158ecdbacf31b54a58af0d1cc", filename = "util.R")
# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer", "dgrtwo/fuzzyjoin"), INSTALL_opts = "--no-multiarch")

pacman::p_load(tidyverse, readxl, snakecase, janitor, scales,
               paletteer, cowplot, showtext)


# Data Source: https://www.horticulture.com.au/growers/help-your-business-grow/research-reports-publications-fact-sheets-and-more/grower-resources/ha18002-assets/australian-horticulture-statistics-handbook
# read data
crop_data <- read_excel("data/tropical_fruit_production_value.xlsx") %>% 
  mutate(State=factor(State, levels = c("QLD", "NSW", "VIC", "NT",  "WA", "TAS", "SA")))

crop_data %>% filter(Crop=="Papaya", !is.na(Yield)) %>% 
  mutate(yield_prop=Yield/sum(Yield)) 
  

state_cols <- levels(crop_data$State) %>% 
  set_names(c("#741128", "#88CFEC", "#DBEBE7", "#C35E00", "#FFD800", "#0645AD", "#FF0000"), .)

crop_sum <- crop_data %>% group_by(Crop) %>% 
  summarise(Yield=sum(Yield, na.rm=TRUE), Value=sum(Value, na.rm=TRUE)) %>%
  arrange(desc(Value)) 

crop_sum %>% 
  adorn_totals() %>% 
  write_xlsx(., "crop_production_value.xlsx", sheet = "summary", overwritesheet = TRUE)

# plot data
pale_theme <- ggthemr::ggthemr(palette = "pale", set_theme = FALSE, text_size = 16)
ggplot(crop_data %>% mutate(Crop=factor(Crop, levels = rev(crop_sum$Crop))), 
             aes(x = Crop,  y = Yield/1000)) +
  geom_bar(mapping = aes(fill = State), stat = "identity", width=0.4) + 
  geom_point(data = crop_sum, mapping = aes(y=Value/4e6), size=4, shape = 24, fill = "lightskyblue") +
  # scale_shape_manual(values = 21:24) + 
  scale_y_continuous(name = "Yield (kt)", expand = expansion(mult = c(0, .1)),
                     labels = comma, sec.axis = sec_axis( trans=~.*4, name="Value (M$)")) +
  scale_fill_manual(values = state_cols) +
  # scale_fill_paletteer_d("RColorBrewer::Set1", drop=FALSE) + 
  labs(title = "Production of Papaya, Strawberry, Mango and Pineapple", # subtitle = "Data from the ABS (2019-2020)", 
       caption = "Source: ABS Agricultural Commodities, Australia (2019-2020)")  +
  pale_theme$theme
  # theme_cowplot(16)
ggsave("plots/tropical_fruits_production_value.pdf", width=7, height = 5)

gg +  scale_fill_paletteer_d("rcartocolor::Bold", drop=FALSE) +
  scale_x_date(NULL, breaks = breaks_width("2 month"),
               labels = label_date_short()) +
  guides(color = guide_legend(override.aes = list(size = 1, 
                                                  linetype=c(3, rep(1, nlevels(comparative_vacs_data$Country)-1)))) ) +
  # labs(colour = "Country") + 
  # theme_bw()
  theme_cowplot(16) +
  background_grid(major = "y", minor = "y") 
# theme(
#   plot.caption = element_markdown(size = 11)
# )

#+ theme(panel.grid.minor = element_blank())
# try a different theme
library(showtext)
font_add_google("Ubuntu", "ubuntu")
showtext_auto()
gg + scale_color_paletteer_d("ggthemes::calc", , drop=FALSE) + # rcartocolor::Bold
  scale_x_date(name=NULL, breaks = breaks_width("1 month"),
               labels = label_date_short(), #label_date(format = "%b"),
               expand = expansion(mult = 0.01)) +
  # guides(color = guide_legend(override.aes = list(size = 1) ) ) +
  guides(color = guide_legend(override.aes = list(size = 1, 
                                                  linetype=c(3, rep(1, nlevels(comparative_vacs_data$Country)-1)))) ) +
  theme_cowplot(16) +
  background_grid(major = "y", minor = "y") +
  theme(text = element_text(family = "ubuntu"),
        axis.title.y.left = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.y.right = element_text(margin = unit(c(0, 0, 0, 3), "mm")))


