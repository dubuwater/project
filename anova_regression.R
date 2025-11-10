#anova_regression.R
pkgs <- c("readr","dplyr","car","rstatix","effectsize","ggplot2","Hmisc")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, quiet=TRUE)
lapply(pkgs, library, character.only = TRUE)

df <- read_csv("oecd_resource_performance_2020_2024_template.csv", locale = locale(encoding = "CP949"))
df$group <- enc2utf8(df$group)
df$country <- enc2utf8(df$country)

df <- df %>%
  mutate(
    group = trimws(group),
    group = case_when(
      group %in% c("북미", "남미", "오세아니아") ~ "기타",
      TRUE ~ group
    ),
    group = factor(group, levels = c("아시아", "유럽", "기타"))
  )

# NA 여부 검증
table(df$group, useNA = "ifany")

message("결측치 요약:\n"); print(sapply(df[,c("resource","performance")], function(x) sum(is.na(x) | x=="")))

# 2024 ANOVA
df24 <- df %>% filter(year == 2024) %>% filter(!is.na(performance))
stopifnot(n_distinct(df24$group) >= 3)

fit_aov <- aov(performance ~ group, data = df24)
cat("\n=== [ANOVA: 2024 성과 ~ 그룹] ===\n"); print(summary(fit_aov))

cat("\n[Levene 등분산성]\n"); print(car::leveneTest(performance ~ group, data = df24))
cat("\n[Shapiro 잔차 정규성]\n"); print(shapiro.test(residuals(fit_aov)))

cat("\n[Tukey HSD]\n"); print(TukeyHSD(fit_aov))
cat("\n[Welch ANOVA]\n"); print(oneway.test(performance ~ group, data = df24, var.equal = FALSE))
cat("\n[Games-Howell]\n"); print(df24 %>% games_howell_test(performance ~ group))

cat("\n[효과크기 eta^2]\n"); print(effectsize::eta_squared(fit_aov, partial = TRUE))

# 폰트 세팅 (한 번만 실행하면 됨)
library(showtext)
font_add_google("Noto Sans KR", "noto")
showtext_auto()

# 그래프 예시
p1 <- ggplot(df24, aes(x = group, y = performance)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "#0072B2") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(
    title = "대륙별 성과 비교 (2024)",
    x = "대륙 그룹",
    y = "성과 지수"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "noto", color = "#333333"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p1)

# 2024 회귀
df24_reg <- df24 %>% filter(!is.na(resource))
fit_lm <- lm(performance ~ resource, data = df24_reg)
cat("\n=== [Regression: 2024 성과 ~ 자원] ===\n"); print(summary(fit_lm))

p2 <- ggplot(df24_reg, aes(resource, performance)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE) +
  labs(title = "자원 점수와 성과의 관계 (2024)",
      x = "자원 점수", y = "성과 점수") +
  theme_minimal(base_size = 13)
print(p2)

#전체연도 회귀: 성과 ~ 자원 + 연도
df_all <- df %>% filter(!is.na(performance), !is.na(resource))
fit_lm_fe <- lm(performance ~ resource + factor(year), data = df_all)
cat("\n=== [Regression (All Years): 성과 ~ 자원 + 연도 고정효과] ===\n"); print(summary(fit_lm_fe))