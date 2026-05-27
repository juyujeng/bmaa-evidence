source("R/bmaa_plot_theme.R")

dir.create("assets/study-covers", recursive = TRUE, showWarnings = FALSE)

save_cover <- function(plot, filename, width = 9.6, height = 5.4) {
  ggplot2::ggsave(
    filename = file.path("assets/study-covers", filename),
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
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 16),
      legend.text = element_text(size = 15),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 14)
    )
}

dat <- data.frame(
  outcome = rep(c("身體覺察 BAAI", "心理健康 AMHS", "生活滿意度 SWLS", "述情困難 TAS-20"), each = 4),
  group = rep(c("BMAA", "BMAA", "控制組", "控制組"), 4),
  time = rep(c("前測", "後測", "前測", "後測"), 4),
  value = c(
    92.74, 99.87, 95.74, 94.85,
    94.08, 101.00, 101.70, 99.44,
    23.82, 25.26, 25.37, 25.26,
    47.32, 43.13, 51.44, 50.81
  )
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "心理健康 AMHS"), "心理健康提升", ylab = "AMHS 平均分數")),
  "2026-tien-healthcare-students.png"
)

dat <- data.frame(
  outcome = rep(c("憂鬱傾向", "負向能動感"), each = 4),
  group = rep(c("BMAA", "BMAA", "主動控制組", "主動控制組"), 2),
  time = rep(c("前測", "後測", "前測", "後測"), 2),
  value = c(15.62, 7.89, 11.00, 11.95, 2.96, 2.40, 2.55, 2.66)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "憂鬱傾向"), "憂鬱傾向下降", ylab = "BDI-II 平均分數")),
  "2026-lien-depression-agency.png"
)

dat <- data.frame(
  outcome = rep(c("惡夢困擾 NDQ", "惡夢影響 NES", "睡眠品質 PSQI"), each = 6),
  group = rep(c("BMAA-MILD", "BMAA-MILD", "BMAA-MILD", "控制組", "控制組", "控制組"), 3),
  time = rep(c("前測", "中測", "後測", "前測", "中測", "後測"), 3),
  value = c(
    23.52, 22.81, 16.76, 25.43, 24.70, 23.39,
    10.29, 10.95, 7.29, 13.83, 13.09, 11.78,
    8.57, 7.67, 6.10, 9.35, 9.78, 9.74
  )
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "惡夢困擾 NDQ"), "惡夢困擾下降", ylab = "NDQ 平均分數")),
  "2024-chiu-nightmare-lucid-dream.png"
)

dat <- data.frame(
  outcome = rep(c("CIAS 總分", "網路成癮核心症狀"), each = 4),
  group = rep(c("BMAA", "BMAA", "控制組", "控制組"), 2),
  time = rep(c("前測", "後測", "前測", "後測"), 2),
  value = c(74.84, 62.37, 75.35, 71.35, 41.79, 35.26, 42.90, 40.65)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "CIAS 總分"), "網路成癮傾向下降", ylab = "CIAS 平均分數")),
  "2024-mou-internet-addiction.png"
)

dat <- data.frame(
  outcome = rep(c("內感覺 MAIA總分", "述情障礙 TAS-20", "低強度表情辨識"), each = 4),
  group = rep(c("BMAA", "BMAA", "控制組", "控制組"), 3),
  time = rep(c("前測", "後測", "前測", "後測"), 3),
  value = c(81.12, 96.65, 80.73, 82.18, 58.71, 50.53, 56.36, 55.91, 40.44, 45.51, 40.22, 40.40)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "低強度表情辨識"), "表情辨識能力", ylab = "平均分數")),
  "2024-yang-autistic-traits.png"
)

dat <- data.frame(
  outcome = c("視覺持續注意力", "聽覺持續注意力", "平衡能力"),
  group = "改善人數",
  value = c(2, 1, 4)
)
save_cover(
  cover_theme(bmaa_bar_plot(dat, "六天營隊式 BMAA 的個案改善摘要", ylab = "改善人數（共 5 人）")),
  "2023-yeh-camp-attention-balance.png"
)

dat <- data.frame(
  outcome = c("前測", "後測"),
  group = "不一致 - 一致",
  value = c(3173.46 - 1551.94, 1765.30 - 2566.15)
)
save_cover(
  cover_theme(bmaa_bar_plot(dat, "刻板印象干擾程度", ylab = "再閱讀時間差異（毫秒）")),
  "2023-hung-gender-stereotype.png"
)

dat <- data.frame(
  outcome = rep(c("壓力知覺 PSS", "心理韌性 BRS", "PANAS 負向情緒"), each = 2),
  group = rep("教師 BMAA", 6),
  time = rep(c("前測", "後測"), 3),
  value = c(16.00, 10.28, 3.33, 3.83, 2.26, 1.65)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "壓力知覺 PSS"), "教師壓力知覺下降", ylab = "PSS 平均分數")),
  "2020-weng-teachers-stress-resilience.png"
)

dat <- data.frame(
  outcome = rep(c("憂鬱", "情緒調節困難", "身體覺察"), each = 4),
  group = rep(c("BMAA-S", "BMAA-S", "控制組", "控制組"), 3),
  time = rep(c("前測", "後測", "前測", "後測"), 3),
  value = c(16.54, 7.36, 15.04, 13.43, 53.39, 42.46, 54.86, 55.46, 78.77, 100.39, 81.04, 83.34)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "憂鬱"), "憂鬱程度下降", ylab = "BDI 平均分數")),
  "2020-hsu-emotion-regulation.png"
)

dat <- data.frame(
  outcome = rep(c("尋求協助策略", "再評價策略", "呼吸放鬆策略"), each = 4),
  group = rep(c("BMAA-C", "BMAA-C", "控制組", "控制組"), 3),
  time = rep(c("前測", "後測", "前測", "後測"), 3),
  value = c(0.85, 0.53, 0.74, 1.13, 0.74, 1.32, 1.26, 1.35, 0.50, 1.41, 0.84, 1.03)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "呼吸放鬆策略"), "呼吸放鬆策略增加", ylab = "策略使用次數")),
  "2019-lee-feel-your-body.png"
)

dat <- data.frame(
  outcome = rep(c("工作記憶廣度", "正向情緒調節", "負向情緒調節"), each = 4),
  group = rep(c("BMAA-C", "BMAA-C", "綜合活動", "綜合活動"), 3),
  time = rep(c("前測", "後測", "前測", "後測"), 3),
  value = c(37.44, 42.00, 39.08, 40.08, 29.24, 29.84, 30.58, 32.83, 7.88, 8.04, 8.08, 7.17)
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "工作記憶廣度"), "工作記憶廣度提升", ylab = "平均分數")),
  "2018-chang-children-executive-emotion.png"
)

dat <- data.frame(
  outcome = rep(c("工作記憶改善率", "RTCV 改善率"), each = 2),
  group = rep(c("BMAA-C", "控制組"), 2),
  value = c(0.14, 0.04, -0.06, 0.11)
)
save_cover(
  cover_theme(bmaa_bar_plot(dat, "BMAA-C 的認知改善率", ylab = "改善率", subtitle = "RTCV 越低代表反應穩定度越佳")),
  "2017-lee-body-mind-interaction.png"
)

dat <- data.frame(
  outcome = rep(c("工作記憶容量（OSPAN）", "注意力轉換成本"), each = 6),
  group = rep(c("BMAA", "禪修", "等待控制"), each = 2, times = 2),
  time = rep(c("前測", "後測"), 6),
  value = c(
    3.75, 4.58, 3.52, 3.92, 3.65, 3.98,
    218.24, 195.99, 232.30, 153.04, 314.53, 278.93
  )
)
save_cover(
  cover_theme(bmaa_line_plot(subset(dat, outcome == "工作記憶容量（OSPAN）"), "工作記憶容量提升", ylab = "OSPAN 平均分數")),
  "2016-teng-confucius.png"
)
