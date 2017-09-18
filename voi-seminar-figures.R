library(dplyr)
library(ggplot2)
library(raster)
library(rgdal)
library(scales)
library(viridis)
library(voiConsPlan)

load("~/voiConsPlan/voiConsPlan.Rdata")

ggsave(
  "HunterSpeciesModels.svg",
  gg_raster(models, spp_of_interest, 2) +
  scale_fill_viridis(na.value = NA, name = expression(bar(italic(K))^"max")) +
  guides(fill = guide_colourbar(barheight = 10)) +
  theme_bw(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    axis.text  = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "italic"),
    strip.background = element_rect(fill = "white")
  ),
  "svg",
  "voi-seminar_files/figure-html", 
  width = 7, 
  height = 5,
  units = "in",
  system_fonts = list(serif = "Droid Serif")
)

ggsave(
  "HunterPlan1.svg",
  gg_raster(plans_boot_max, highlight = c(0, 1)) +
  scale_fill_viridis(na.value = NA, name = "Rank") +
  guides(fill = guide_colourbar(barheight = 10, label = FALSE)) +
  ggtitle("Hunter Valley Priority Map", subtitle = "All grid cells") +
  theme_dark(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ),
  "svg",
  "voi-seminar_files/figure-html", 
  width = 7, 
  height = 5,
  units = "in",
  system_fonts = list(serif = "Droid Serif")
)

ggsave(
  "HunterPlan2.svg",
  gg_raster(plans_boot_max, highlight = c(prop_loss, 1)) +
  scale_fill_viridis(na.value = NA, name = "Rank") +
  guides(fill = guide_colourbar(barheight = 10, label = FALSE)) +
  ggtitle("Hunter Valley Priority Map", subtitle = "Top 20%") +
  theme_dark(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ),
  "svg",
  "voi-seminar_files/figure-html", 
  width = 7, 
  height = 5,
  units = "in",
  system_fonts = list(serif = "Droid Serif")
)

plans_boot_df2 =
  sapply(plans_boot, values) %>%
  apply(1L, function(x) c(mean = mean(x), min = min(x), max = max(x))) %>%
  t %>% data.frame %>% 
  mutate(hl = case_when(max < prop_loss ~ 0, min > prop_loss ~ 2, TRUE ~ 1))

ggsave(
  "HunterPlanBoot.svg",
  na.omit(plans_boot_df2) %>%
  arrange(mean) %>%
  mutate(x = seq(0, 1, length.out = n())) %>%
  slice(floor(seq.int(1, n(), length.out = 10000))) %>%
  ggplot +
  geom_hline(aes(yintercept = prop_loss)) +
  geom_linerange(aes(x = x, ymin = min, ymax = max, col = factor(hl))) +
  scale_colour_manual(values = viridis(3, .1, 0, 1), guide = "none") +
  xlab("Grid Cells") +
  ylab("Ranking") +
  theme_bw(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ),
  "svg",
  "voi-seminar_files/figure-html", 
  width = 7, 
  height = 5,
  units = "in",
  system_fonts = list(serif = "Droid Serif")
)

ggsave(
  "HunterPlanBootIC.svg",
  na.omit(plans_boot_df2) %>%
  arrange(mean) %>%
  mutate(x = seq(0, 1, length.out = n())) %>%
  slice(floor(seq.int(1, n(), length.out = 10000))) %>%
  ggplot +
  geom_hline(aes(yintercept = prop_loss)) +
  geom_linerange(aes(x = x, ymin = min, ymax = max, col = factor(hl))) +
  scale_colour_manual(values = c("#DEB88766", "#F8F8FF66", "#FF000066"), guide = "none") +
  xlab("Grid Cells") +
  ylab("Ranking") +
  theme_dark(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ),
  "svg",
  "voi-seminar_files/figure-html", 
  width = 7, 
  height = 5,
  units = "in",
  system_fonts = list(serif = "Droid Serif")
)

lapply(
  1:3,
  function(x) {
    ggsave(
      sprintf("HunterPlanZones%s.svg", x),
      gg_raster(plans_boot_max, highlight = plans_boot_df2$hl == x - 1) +
      scale_fill_viridis(na.value = NA, name = "Rank") +
      guides(fill = guide_colourbar(barheight = 10, label = FALSE)) +
      theme_dark(base_size = 14) +
      theme(
        text = element_text(family = "serif"),
        axis.text = element_blank(), 
        axis.title = element_blank()
      ),
      "svg",
      "voi-seminar_files/figure-html", 
      width = 7, 
      height = 5,
      units = "in",
      system_fonts = list(serif = "Droid Serif")
    )
  }
)

pdftools::pdf_render_page(
  "http://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0059662&type=printable",
  page = 1, dpi = 96, numeric = FALSE) %>%
magick::image_read() %>%
magick::image_chop("x800+0+200") %>%
magick::image_crop("700x254+50+0") %>%
magick::image_resize("1000x") %>%
magick::image_write("voi-seminar_files/figure-html/hermoso.png", "png")

pdftools::pdf_render_page(
  "http://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0059662&type=printable",
  page = 6, dpi = 180, numeric = FALSE) %>%
magick::image_read() %>%
magick::image_crop("400x600+150+1100") %>%
magick::image_resize("300x300!") %>%
magick::image_write("voi-seminar_files/figure-html/hermoso-plot.png", "png")

pdftools::pdf_render_page(
  "https://www.researchgate.net/profile/James_Lyons8/publication/216770891_Which_uncertainty_Using_expert_elicitation_and_expcetd_value_of_information_to_design_an_adaptive_program/links/568ee19f08aef987e567e367.pdf",
  page = 3, dpi = 96, numeric = FALSE) %>%
magick::image_read() %>%
magick::image_crop("720x270+20+120") %>%
magick::image_resize("1000x") %>%
magick::image_write("voi-seminar_files/figure-html/runge.png", "png")

pdftools::pdf_render_page(
  "https://www.researchgate.net/profile/James_Lyons8/publication/216770891_Which_uncertainty_Using_expert_elicitation_and_expcetd_value_of_information_to_design_an_adaptive_program/links/568ee19f08aef987e567e367.pdf",
  page = 9, dpi = 180, numeric = FALSE) %>%
magick::image_read() %>%
magick::image_crop("600x420+100+1150") %>%
magick::image_resize("300x300!") %>%
magick::image_write("voi-seminar_files/figure-html/runge-plot.png", "png")


