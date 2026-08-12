suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(grid)
  library(stringr)
  library(scales)
})

SBK_FONT <- "Arial"

sbk_palette <- c(
  black = "#000000",
  dark_text = "#222222",
  dark_navy = "#264653",
  muted_blue = "#457B9D",
  teal = "#2A9D8F",
  muted_purple = "#7567B8",
  coral = "#E76F51",
  amber = "#D8A73C",
  light_blue_gray = "#DDE6ED",
  light_gray = "#F4F6F8",
  mid_gray = "#AAB4BE",
  zero = "#D5DCE3"
)

relation_palette <- c(
  RESPONDS_TO = sbk_palette[["teal"]],
  TARGETS = sbk_palette[["teal"]],
  DELIVERS = sbk_palette[["muted_blue"]],
  RELEASES = sbk_palette[["muted_blue"]],
  MODULATES = sbk_palette[["muted_purple"]],
  PROMOTES = sbk_palette[["coral"]],
  VALIDATED_AT = sbk_palette[["dark_navy"]],
  COMPOSED_OF = "#6F8794",
  FABRICATED_BY = "#8A7F73",
  USES = "#7C8B76"
)

node_palette <- c(
  MaterialPlatform = sbk_palette[["dark_navy"]],
  MaterialComposition = "#7B94A2",
  FabricationStrategy = "#8A7F73",
  DesignMode = "#7C8B76",
  Stimulus = sbk_palette[["teal"]],
  ResponsiveBehavior = sbk_palette[["teal"]],
  TherapeuticCargo = sbk_palette[["muted_blue"]],
  TargetCell = "#3A9E90",
  SenescenceEvidence = sbk_palette[["amber"]],
  Mechanism = sbk_palette[["muted_purple"]],
  DiseaseModel = "#B88958",
  RegenerativeEndpoint = sbk_palette[["coral"]],
  ValidationStage = sbk_palette[["dark_navy"]],
  Study = sbk_palette[["mid_gray"]]
)

theme_sbk <- function(base_size = 8.2) {
  theme_classic(base_size = base_size, base_family = SBK_FONT) +
    theme(
      text = element_text(family = SBK_FONT, colour = sbk_palette[["dark_text"]]),
      axis.title = element_text(family = SBK_FONT, face = "bold", size = base_size + 0.2,
                                colour = sbk_palette[["black"]]),
      axis.text = element_text(family = SBK_FONT, face = "plain", size = base_size,
                               colour = sbk_palette[["dark_text"]]),
      axis.line = element_line(linewidth = 0.35, colour = sbk_palette[["black"]]),
      axis.ticks = element_line(linewidth = 0.35, colour = sbk_palette[["black"]]),
      axis.ticks.length = unit(1.4, "mm"),
      legend.title = element_text(family = SBK_FONT, face = "plain", size = base_size),
      legend.text = element_text(family = SBK_FONT, face = "plain", size = base_size - 0.2),
      legend.key.height = unit(3.4, "mm"),
      legend.key.width = unit(4.2, "mm"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(family = SBK_FONT, face = "bold", size = base_size + 0.2,
                                colour = sbk_palette[["black"]]),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_text(family = SBK_FONT, face = "plain", size = base_size - 0.6,
                                  colour = sbk_palette[["dark_text"]]),
      plot.tag = element_text(family = SBK_FONT, face = "bold", size = 17,
                              colour = sbk_palette[["black"]]),
      plot.tag.position = c(0.002, 0.995),
      plot.margin = margin(7, 8, 6, 9, unit = "pt"),
      panel.grid = element_blank()
    )
}

theme_sbk_void <- function(base_size = 8.2) {
  theme_void(base_size = base_size, base_family = SBK_FONT) +
    theme(
      text = element_text(family = SBK_FONT, colour = sbk_palette[["dark_text"]]),
      legend.title = element_text(family = SBK_FONT, face = "plain", size = base_size),
      legend.text = element_text(family = SBK_FONT, face = "plain", size = base_size - 0.2),
      plot.tag = element_text(family = SBK_FONT, face = "bold", size = 17,
                              colour = sbk_palette[["black"]]),
      plot.tag.position = c(0.002, 0.995),
      plot.margin = margin(7, 8, 6, 9, unit = "pt")
    )
}

add_panel_tag <- function(plot, tag) {
  plot + labs(tag = tag) + theme(plot.tag.position = c(0.002, 0.995))
}

wrap_sbk <- function(x, width = 28) {
  str_wrap(as.character(x), width = width)
}

save_pdf_sbk <- function(plot, filename, width_mm, height_mm) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  grDevices::cairo_pdf(
    filename = filename,
    width = width_mm / 25.4,
    height = height_mm / 25.4,
    family = SBK_FONT,
    onefile = TRUE,
    bg = "white"
  )
  print(plot)
  grDevices::dev.off()
  invisible(filename)
}

save_preview_sbk <- function(plot, filename, width_mm, height_mm, dpi = 180) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(
    filename = filename,
    width = width_mm / 25.4,
    height = height_mm / 25.4,
    units = "in",
    res = dpi,
    type = "cairo"
  )
  print(plot)
  grDevices::dev.off()
  invisible(filename)
}

theme_set(theme_sbk())
