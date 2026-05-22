library(ggplot2)

knitr::opts_chunk$set(
  dev = "ragg_png",
  dpi = 160,
  fig.width = 10.5,
  fig.height = 5.4,
  out.width = "100%"
)

bmaa_font_zh <- "Noto Sans TC"
bmaa_font_en <- "Noto Sans"
bmaa_text_family <- bmaa_font_zh

if (requireNamespace("systemfonts", quietly = TRUE)) {
  register_bmaa_font <- function(name, plain, bold) {
    tryCatch(
      systemfonts::register_font(
        name = name,
        plain = plain,
        bold = bold
      ),
      error = function(e) {
        if (!grepl("already exists", conditionMessage(e), fixed = TRUE)) {
          warning(conditionMessage(e), call. = FALSE)
        }
      }
    )
  }

  if (!bmaa_font_zh %in% systemfonts::system_fonts()$family) {
    register_bmaa_font(
      name = bmaa_font_zh,
      plain = "/Users/juyijen/Library/Fonts/NotoSansTC-Regular.otf",
      bold = "/Users/juyijen/Library/Fonts/NotoSansTC-Bold.otf"
    )
  }

  if (!bmaa_font_en %in% systemfonts::system_fonts()$family) {
    register_bmaa_font(
      name = bmaa_font_en,
      plain = "/Users/juyijen/Library/Fonts/NotoSans-Regular.ttf",
      bold = "/Users/juyijen/Library/Fonts/NotoSans-Bold.ttf"
    )
  }
}

bmaa_palette <- c(
  "#0072B2", "#D55E00", "#009E73", "#CC79A7",
  "#E69F00", "#56B4E9", "#F0E442", "#000000",
  "#8C564B", "#4D4D4D", "#66A61E", "#A6761D"
)

theme_bmaa <- function(base_size = 15) {
  theme_minimal(base_size = base_size, base_family = bmaa_text_family) +
    theme(
      text = element_text(family = bmaa_text_family, color = "#203030"),
      plot.title = element_text(family = bmaa_text_family, face = "bold", size = base_size + 4),
      plot.subtitle = element_text(family = bmaa_text_family, size = base_size - 1),
      axis.title = element_text(family = bmaa_text_family, face = "bold"),
      axis.text = element_text(family = bmaa_text_family),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(family = bmaa_text_family),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(family = bmaa_text_family, face = "bold", size = base_size - 1)
    )
}

bmaa_line_plot <- function(dat, title, ylab = "數值", subtitle = NULL) {
  dat$time <- factor(dat$time, levels = unique(dat$time))
  ggplot(dat, aes(time, value, group = group, color = group)) +
    geom_line(linewidth = 1.15) +
    geom_point(size = 2.7) +
    scale_color_manual(values = rep(bmaa_palette, length.out = length(unique(dat$group)))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab) +
    theme_bmaa()
}

bmaa_facet_line_plot <- function(dat, title, ylab = "數值", subtitle = NULL, ncol = NULL) {
  dat$time <- factor(dat$time, levels = unique(dat$time))
  dat$outcome <- factor(dat$outcome, levels = unique(dat$outcome))
  ggplot(dat, aes(time, value, group = group, color = group)) +
    geom_line(linewidth = 1.15) +
    geom_point(size = 2.7) +
    facet_wrap(~ outcome, scales = "free_y", ncol = ncol) +
    scale_color_manual(values = rep(bmaa_palette, length.out = length(unique(dat$group)))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab) +
    theme_bmaa()
}

bmaa_bar_plot <- function(dat, title, ylab = "數值", subtitle = NULL) {
  dat$outcome <- factor(dat$outcome, levels = unique(dat$outcome))
  ggplot(dat, aes(outcome, value, fill = group)) +
    geom_col(position = position_dodge(width = .75), width = .68) +
    scale_fill_manual(values = rep(bmaa_palette, length.out = length(unique(dat$group)))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab) +
    theme_bmaa() +
    theme(axis.text.x = element_text(family = bmaa_text_family, angle = 15, hjust = 1))
}
