source("R/bmaa_plot_theme.R")

dir.create("assets/study-covers-en", recursive = TRUE, showWarnings = FALSE)

bmaa_text_family <- bmaa_font_en

save_cover <- function(plot, filename, width = 9.6, height = 5.4) {
  ggplot2::ggsave(
    filename = file.path("assets/study-covers-en", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 180,
    bg = "white"
  )
}

cover_theme <- function(plot) {
  plot +
    theme(
      text = element_text(family = bmaa_text_family),
      plot.title = element_text(family = bmaa_text_family, size = 24),
      plot.subtitle = element_text(family = bmaa_text_family, size = 16),
      legend.text = element_text(family = bmaa_text_family, size = 15),
      axis.title = element_text(family = bmaa_text_family, size = 15),
      axis.text = element_text(family = bmaa_text_family, size = 14),
      strip.text = element_text(family = bmaa_text_family)
    )
}

dat <- data.frame(
  outcome = rep(c("Body awareness (BAAI)", "Mental health (AMHS)", "Life satisfaction (SWLS)", "Alexithymia (TAS-20)"), each = 4),
  group = rep(c("BMAA", "BMAA", "Control", "Control"), 4),
  time = rep(c("Pretest", "Posttest", "Pretest", "Posttest"), 4),
  value = c(
    92.74, 99.87, 95.74, 94.85,
    94.08, 101.00, 101.70, 99.44,
    23.82, 25.26, 25.37, 25.26,
    47.32, 43.13, 51.44, 50.81
  )
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "Mental health (AMHS)"), "Mental Health Increased", ylab = "AMHS mean score")),
  "2026-tien-healthcare-students.png"
)

dat <- data.frame(
  outcome = rep(c("Depression (BDI-II)", "Negative agency (SoNA)"), each = 4),
  group = rep(c("BMAA", "BMAA", "Active control", "Active control"), 2),
  time = rep(c("Pretest", "Posttest", "Pretest", "Posttest"), 2),
  value = c(15.62, 7.89, 11.00, 11.95, 2.96, 2.40, 2.55, 2.66)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "Depression (BDI-II)"), "Depression Decreased", ylab = "BDI-II mean score")),
  "2026-lien-depression-agency.png"
)

dat <- data.frame(
  outcome = rep(c("Nightmare distress (NDQ)", "Nightmare effects (NES)", "Sleep quality (PSQI)"), each = 6),
  group = rep(c("BMAA-MILD", "BMAA-MILD", "BMAA-MILD", "Control", "Control", "Control"), 3),
  time = rep(c("Pretest", "Midtest", "Posttest", "Pretest", "Midtest", "Posttest"), 3),
  value = c(
    23.52, 22.81, 16.76, 25.43, 24.70, 23.39,
    10.29, 10.95, 7.29, 13.83, 13.09, 11.78,
    8.57, 7.67, 6.10, 9.35, 9.78, 9.74
  )
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "Nightmare distress (NDQ)"), "Nightmare Distress Decreased", ylab = "NDQ mean score")),
  "2024-chiu-nightmare-lucid-dream.png"
)

dat <- data.frame(
  outcome = rep(c("CIAS total", "Core internet-addiction symptoms"), each = 4),
  group = rep(c("BMAA", "BMAA", "Control", "Control"), 2),
  time = rep(c("Pretest", "Posttest", "Pretest", "Posttest"), 2),
  value = c(74.84, 62.37, 75.35, 71.35, 41.79, 35.26, 42.90, 40.65)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "CIAS total"), "Internet Addiction Tendency Decreased", ylab = "CIAS mean score")),
  "2024-mou-internet-addiction.png"
)

dat <- data.frame(
  outcome = rep(c("Interoception (MAIA-C)", "Alexithymia (TAS-20)", "Low-intensity expression recognition"), each = 4),
  group = rep(c("BMAA", "BMAA", "Control", "Control"), 3),
  time = rep(c("Pretest", "Posttest", "Pretest", "Posttest"), 3),
  value = c(81.12, 96.65, 80.73, 82.18, 58.71, 50.53, 56.36, 55.91, 40.44, 45.51, 40.22, 40.40)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "Low-intensity expression recognition"), "Expression Recognition", ylab = "Mean score")),
  "2024-yang-autistic-traits.png"
)

dat <- data.frame(
  outcome = c("Visual sustained attention", "Auditory sustained attention", "Balance"),
  group = "Improved cases",
  value = c(2, 1, 4)
)
save_cover(
  cover_theme(bmaa_bar_plot(dat, "Six-Day Camp-Based BMAA: Improvement Summary", ylab = "Number improved (out of 5)")),
  "2023-yeh-camp-attention-balance.png"
)

dat <- data.frame(
  outcome = c("Pretest", "Posttest"),
  group = "Incongruent - congruent",
  value = c(3173.46 - 1551.94, 1765.30 - 2566.15)
)
save_cover(
  cover_theme(bmaa_bar_plot(dat, "Stereotype Interference", ylab = "Rereading-time difference (ms)")),
  "2023-hung-gender-stereotype.png"
)

dat <- data.frame(
  outcome = rep(c("Perceived stress (PSS)", "Resilience (BRS)", "PANAS negative affect"), each = 2),
  group = rep("Teacher BMAA", 6),
  time = rep(c("Pretest", "Posttest"), 3),
  value = c(16.00, 10.28, 3.33, 3.83, 2.26, 1.65)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "Perceived stress (PSS)"), "Teachers' Perceived Stress Decreased", ylab = "PSS mean score")),
  "2020-weng-teachers-stress-resilience.png"
)

dat <- data.frame(
  outcome = rep(c("Depression", "Emotion-regulation difficulty", "Body awareness"), each = 4),
  group = rep(c("BMAA-S", "BMAA-S", "Control", "Control"), 3),
  time = rep(c("Pretest", "Posttest", "Pretest", "Posttest"), 3),
  value = c(16.54, 7.36, 15.04, 13.43, 53.39, 42.46, 54.86, 55.46, 78.77, 100.39, 81.04, 83.34)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "Depression"), "Depression Decreased", ylab = "BDI mean score")),
  "2020-hsu-emotion-regulation.png"
)

dat <- data.frame(
  outcome = rep(c("Seeking help", "Reappraisal", "Breathing relaxation"), each = 4),
  group = rep(c("BMAA-C", "BMAA-C", "Control", "Control"), 3),
  time = rep(c("Pretest", "Posttest", "Pretest", "Posttest"), 3),
  value = c(0.85, 0.53, 0.74, 1.13, 0.74, 1.32, 1.26, 1.35, 0.50, 1.41, 0.84, 1.03)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "Breathing relaxation"), "Breathing Relaxation Strategy Increased", ylab = "Strategy-use count")),
  "2019-lee-feel-your-body.png"
)

dat <- data.frame(
  outcome = rep(c("Working-memory span", "Positive emotion regulation", "Negative emotion regulation"), each = 4),
  group = rep(c("BMAA-C", "BMAA-C", "Integrated activity", "Integrated activity"), 3),
  time = rep(c("Pretest", "Posttest", "Pretest", "Posttest"), 3),
  value = c(37.44, 42.00, 39.08, 40.08, 29.24, 29.84, 30.58, 32.83, 7.88, 8.04, 8.08, 7.17)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "Working-memory span"), "Working-Memory Span Increased", ylab = "Mean score")),
  "2018-chang-children-executive-emotion.png"
)

dat <- data.frame(
  outcome = rep(c("Working-memory improvement", "RTCV improvement"), each = 2),
  group = rep(c("BMAA-C", "Control"), 2),
  value = c(0.14, 0.04, -0.06, 0.11)
)
save_cover(
  cover_theme(bmaa_bar_plot(dat, "Cognitive Improvement after BMAA-C", ylab = "Improvement rate", subtitle = "Lower RTCV indicates more stable responding")),
  "2017-lee-body-mind-interaction.png"
)

dat <- data.frame(
  outcome = rep(c("Working-memory capacity (OSPAN)", "Attention switching cost"), each = 6),
  group = rep(c("BMAA", "Chan meditation", "Waitlist control"), each = 2, times = 2),
  time = rep(c("Pretest", "Posttest"), 6),
  value = c(
    3.75, 4.58, 3.52, 3.92, 3.65, 3.98,
    218.24, 195.99, 232.30, 153.04, 314.53, 278.93
  )
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "Working-memory capacity (OSPAN)"), "Working-Memory Capacity Increased", ylab = "OSPAN mean score")),
  "2016-teng-confucius.png"
)
