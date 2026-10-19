# Source: 李政霆 (2025), Tables 3-5, printed pp. 28-30 (PDF pp. 36-38).
# Values are unadjusted item means, ordered BMAA pre/post, control pre/post.
# The external-action means follow Table 4; the paragraph above that table
# repeats the MAIA noticing values from Table 3 instead.
li_study_means <- data.frame(
  outcome = c(
    "MAIA 注意", "MAIA 不擔心", "MAIA 不分心", "MAIA 情緒覺察",
    "MAIA 自我調節", "MAIA 注意力調節", "MAIA 身體傾聽", "MAIA 信任",
    "身體節律覺察", "覺察當下", "外在行動能動感", "內在調節能動感",
    "自我接納", "正向關係", "自主性", "掌握生活環境", "人生目標",
    "自我成長", "安適幸福感", "生活滿意度", "正向情緒", "負向情緒"
  ),
  bmaa_pre = c(
    3.16, 1.50, 2.02, 3.31, 2.90, 2.50, 2.64, 3.03, 4.20, 3.54, 4.96,
    3.18, 3.97, 4.52, 3.94, 4.13, 4.29, 5.29, 2.77, 4.16, 3.22, 2.73
  ),
  bmaa_post = c(
    3.77, 2.09, 2.41, 3.82, 3.74, 3.33, 3.51, 3.54, 4.78, 3.96, 5.17,
    4.14, 4.24, 4.54, 4.24, 4.14, 4.40, 5.43, 3.11, 4.32, 3.27, 2.58
  ),
  control_pre = c(
    3.28, 1.67, 1.87, 3.20, 2.55, 2.52, 2.67, 2.93, 4.01, 3.49, 4.94,
    3.19, 3.67, 4.43, 3.84, 4.15, 4.32, 5.30, 2.70, 3.89, 2.94, 2.67
  ),
  control_post = c(
    3.27, 1.56, 1.76, 3.28, 2.72, 2.66, 2.76, 3.02, 4.24, 3.57, 4.77,
    3.29, 3.60, 4.54, 4.08, 4.08, 4.21, 5.35, 2.50, 3.84, 2.77, 2.80
  ),
  source_table = c(rep(3, 10), rep(4, 2), rep(5, 10)),
  stringsAsFactors = FALSE
)

li_study_plot <- function(outcomes, title, labels = outcomes, ncol = 2) {
  selected <- li_study_means[match(outcomes, li_study_means$outcome), ]
  stopifnot(!anyNA(selected), length(labels) == nrow(selected))
  values <- as.matrix(selected[c("bmaa_pre", "bmaa_post", "control_pre", "control_post")])
  dat <- data.frame(
    outcome = rep(labels, each = 4),
    group = rep(c("BMAA 組", "BMAA 組", "等候組", "等候組"), nrow(selected)),
    time = rep(c("前測", "後測", "前測", "後測"), nrow(selected)),
    value = as.vector(t(values))
  )

  bmaa_facet_line_plot(dat, title, ylab = "平均分數", ncol = ncol) +
    geom_text(
      aes(label = sprintf("%.2f", value), vjust = ifelse(group == "BMAA 組", -0.8, 1.6)),
      family = bmaa_text_family,
      size = 3.8,
      show.legend = FALSE
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.22, 0.25))) +
    theme(strip.text = element_text(size = 13))
}

li_wellbeing_plot <- function() {
  li_study_plot(
    c("安適幸福感", "自我接納"),
    "幸福感的變化：內心安定與自我接納",
    labels = c(
      "安適幸福感\n（內心安定、自在的程度）",
      "心理幸福感中的自我接納\n（接受自己的優點與缺點）"
    )
  )
}
