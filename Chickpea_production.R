# load custom functions from github
devtools::source_gist("7f63547158ecdbacf31b54a58af0d1cc", filename = "util.R")

# install dependencies
# gganimate requires gifski to render gifs, which first requires rust, see installation instructions: https://github.com/r-rust/hellorust#installation
# or download from this link: https://gif.ski/ and put gifski.exe in a folder in your Path
# devtools::install_github("r-rust/gifski")
# load packages we're going to use
# BiocManager::install("qvalue", update = FALSE)
pacman::p_load(tidyverse, paletteer, janitor, ggtext) # quadprog, raster, mvtnorm, dartR
clean_theme <-  ggthemr::ggthemr("flat", layout = "clean", 
                                 text_size = 14, set_theme = FALSE)

#### Plot production ####
# prepare ABARES data
# download from http://data.daff.gov.au/data/warehouse/aucrpd9abcc003/aucrpd9aba_20180911_Z0Srg/AustCropRrt20180911_StateCropData_v1.0.0.xlsx and edit to combine data from all states
# see also https://www.awe.gov.au/abares/research-topics/agricultural-outlook/data#2021
states <- set_names(c("QLD", "NSW", "VIC", "SA", "WA"), c("Queensland", "New South Wales", "Victoria", "South Australia", "Western Australia"))

state_cols <- paletteer_d("tidyquant::tq_dark", n = length(states)) %>% as.character() %>% setNames(c("WA", "QLD", "VIC", "SA", "NSW")) %>% 
  .[rev(states)]

prod_data <- readxl::read_excel("data/Chickpea_production_1989_2022.xlsx", sheet = "Chickpea_prod_updated") %>% 
  tidyr::pivot_longer(where(is.double), names_to = "Period", values_to = "value") %>% 
  tidyr::pivot_wider(names_from = Measure, values_from = value) %>% 
  mutate(Period=sub(" (\\w)$", "(\\1)", Period)) %>% 
  mutate(State_abbr=factor(states[State], levels = rev(states)),
         State=factor(State, levels = names(rev(states))))

period <- unique(prod_data$Period)
period_breaks <- period[seq_along(period)%%2==!(length(period)%%2==0)]
prod_data %>% group_by(Period) %>% summarise(sum(Production))

# plot stacked bar graph
ggplot(prod_data, aes(x=Period, y=Production, fill=State_abbr, group=State_abbr)) +
  # geom_line(size=1) +
  geom_bar(stat = "identity", width = 0.5) +
  # geom_line(size=1.5) +
  # scale_fill_paletteer_d("tidyquant::tq_dark") + 
  # scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_fill_manual(values = state_cols) +
  scale_x_discrete(breaks=period_breaks,
                   guide = guide_axis(check.overlap = TRUE,
                                      angle = -45)) +
  scale_y_continuous(expand = expansion(add = c(0, 200)), labels = scales::comma) + # ggsci, category10_d3 ; ggthemes, wsj_colors6 ; "RColorBrewer::Set1"
  # coord_cartesian(ylim = c(-25, 2200)) + 
  labs(x="Reporting Period", y="Production (kt)", 
       fill="State",
       caption = "Data source: [Agricultural commodities and trade data](https://www.awe.gov.au/abares/research-topics/agricultural-outlook/data#australian-crop-report-data)\nDepartment of Agriculture, Water and the Environment, Australian Bureau of Agricultural and Resource Economics and Sciences (ABARES)") +
  # coord_flip(expand = FALSE, ylim = c(-25, 2200)) +
  clean_theme$theme +
  # plot_theme() + 
  theme(#axis.text.x = element_text(angle = -45, hjust = 0),
        plot.caption = element_text(hjust = 0, size=rel(0.7)))
  
ggsave("plots/Chickpea_production_bars_1989_2022.pdf", height = 7, width = 10)

# plot line graph
ggplot(prod_data, aes(x=Period, y=Production, colour=State, fill=State, group=State, shape=State)) +
  geom_line(size=1) + geom_point(size=4) + scale_shape_manual(values=21:25) +
  scale_colour_paletteer_d(RColorBrewer, Set1) +
  scale_fill_paletteer_d(RColorBrewer, Set1) + scale_y_continuous(expand = expand_scale(add = c(50, 200))) + # ggsci, category10_d3 ; ggthemes, wsj_colors6 
  # coord_cartesian(ylim = c(-25, 2200)) + 
  labs(x="ABARES Reporting Period", y="Production (kt)") +
  # coord_flip(expand = FALSE, ylim = c(-25, 2200)) +
  plot_theme() + theme(axis.text.x = element_text(angle = -45, hjust = 0))

ggsave("plots/Chickpea_production_ABARES_line.pdf", height = 6, width = 8)

price_data <- readxl::read_excel("data/Chickpea_production_2003_2019.xlsx", sheet = "Chickpea_prices")
# plot line graph
ggplot(price_data, aes(x=Quarter, y=Price, group=1)) +
  geom_line(size=4, colour="grey30")  + #+ geom_point(size=4, colour="grey10", pch=12) 
  # coord_cartesian(ylim = c(-25, 2200)) + 
  labs(x="ABARES Reporting Period", y="Price (A$/t)") +
  # coord_flip(expand = FALSE, ylim = c(-25, 2200)) +
  plot_theme() + theme(axis.text.x = element_text(angle = -45, hjust = 0), panel.grid = element_blank(), panel.background = element_blank())

ggsave("output/plots/Chickpea_prices_ABARES_line.pdf", height = 8, width = 12)
