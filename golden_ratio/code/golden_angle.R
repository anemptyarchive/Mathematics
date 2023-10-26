
# 黄金角 ---------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 黄金角の定義 ------------------------------------------------------------------

### ・設定 -----

# 半径を指定
r <- 2

# 黄金数を計算
phi <- (1 + sqrt(5)) * 0.5

# 黄金数の逆数を計算
recip_phi <- 1 / phi

# 円周の長さを計算
ab <- 2 * pi * r
#ab <- a

# 円弧の長さを計算
a <- ab * recip_phi
b <- ab * (1 - recip_phi)

# ラジアン(弧度法の角度)を計算
alpha <- 2*pi * recip_phi
beta  <- 2*pi * (1 - recip_phi)

# 半径を計算
r <- ab / (2 * pi)


# 円周の座標を作成
circle_df <- tibble::tibble(
  theta = seq(from = 0, to = 2*pi, length.out = 901), # ラジアン
  x     = r * cos(theta), 
  y     = r * sin(theta), 
  arc_label = dplyr::if_else(
    theta <= alpha, true = "a", false = "b"
  ) # 円弧ラベル
)


### ・円周で可視化 -----

# 角度線の座標を作成
radius_df <- tibble::tibble(
  x = c(r, r * cos(2*pi * recip_phi)), 
  y = c(0, r * sin(2*pi * recip_phi))
)

# 辺ラベルの座標を作成
d <- 0.1
label_df <- dplyr::bind_rows(
  # 半径ラベル
  tibble::tibble(
    edge_label = "r", 
    x = r * 0.5, 
    y = 0, 
    h = 0.5, 
    v = -0.5
  ), 
  # 円弧ラベル
  tibble::tibble(
    edge_label = c("a", "b"), 
    theta = pi * c(recip_phi, -(1 - recip_phi)), 
    x     = (r - d) * cos(theta), 
    y     = (r - d) * sin(theta), 
    h = 0.5, 
    v = 0.5
  )
)


# 角度マークの座標を作成
d_a <- 0.1
d_b <- 0.2
angle_arc_df <- dplyr::bind_rows(
  tibble::tibble(
    theta = seq(from = 0, to = alpha, length.out = 100), 
    x     = d_a * cos(theta), 
    y     = d_a * sin(theta), 
    arc_label = "a"
  ), 
  tibble::tibble(
    theta = seq(from = 0, to = -beta, length.out = 100), 
    x     = d_b * cos(theta), 
    y     = d_b * sin(theta), 
    arc_label = "b"
  )
)

# 角度ラベルの座標を作成
d_a <- 0.2
d_b <- 0.3
angle_label_df <- dplyr::bind_rows(
  tibble::tibble(
    theta = 0.5 * alpha, 
    x     = d_a * cos(theta), 
    y     = d_a * sin(theta), 
    arc_label  = "a", 
    angle_label = "alpha"
  ), 
  tibble::tibble(
    theta = -0.5 * beta, 
    x     = d_b * cos(theta), 
    y     = d_b * sin(theta), 
    arc_label  = "b", 
    angle_label = "beta"
  )
)


# 半円における目盛数(分母の値)を指定
denom <- 6

# 角度目盛線の座標を作成
angle_axis_df <- tibble::tibble(
  i     = 0:(2*denom-1),  # 目盛位置番号(分子の値)
  theta = i / denom * pi, # 目盛値
  x     = r * cos(theta), 
  y     = r * sin(theta), 
  rad_label = paste0("frac(", i, ", ", denom, ") ~ pi"), # 角度ラベル
  h = 1 - (x/r * 0.5 + 0.5), # ラベル位置の調整値
  v = 1 - (y/r * 0.5 + 0.5)  # ラベル位置の調整値
)

# ラベル用の文字列を作成
def_label <- paste0(
  "list(", 
  "phi == frac(1 + sqrt(5), 2), ", 
  "a + b == 2 * pi * r, ", 
  "a + b : a == 1 : frac(1, phi), ", 
  "a + b : b == 1 : 1 - frac(1, phi), ", 
  "alpha == frac(a, r), ", 
  "beta == frac(b, r), ", 
  "theta*degree == frac(180*degree, pi) ~ theta", 
  ")"
)
var_label <- paste0(
  "list(", 
  "r == ", round(r, digits = 2), ", ", 
  "a == ", round(a, digits = 2), ", ", 
  "b == ", round(b, digits = 2), ", ", 
  "alpha == ", round(alpha/pi, digits = 2), " * pi, ", 
  "beta == ", round(beta/pi, digits = 2), " * pi, ", 
  "alpha*degree == ", round(alpha/pi*180, digits = 1), "*degree, ", 
  "beta*degree == ", round(beta/pi*180, digits = 1), "*degree", 
  ")"
)

# 円周上で黄金角を作図
add_size <- 0.5
ggplot() + 
  geom_segment(data = angle_axis_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = factor(i)), 
               color = "gray", linewidth = 1, linetype = "dotted") + # 角度目盛線
  geom_text(data = angle_axis_df, 
            mapping = aes(x = x, y = y, label = rad_label, hjust = h, vjust = v), 
            parse = TRUE, size = 4) + # 角度目盛ラベル
  geom_segment(data = radius_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y), 
               linewidth = 1) + # 半径線
  geom_path(data = angle_arc_df, 
            mapping = aes(x = x, y = y, group = arc_label), 
            linewidth = 0.5) + # 角度マーク
  geom_text(data = angle_label_df, 
            mapping = aes(x = x, y = y, label = angle_label), 
            parse = TRUE, size = 8) + # 角度ラベル
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y, color = arc_label), 
            linewidth = 1) + # 円周
  geom_text(data = label_df, 
            mapping = aes(x = x, y = y, label = edge_label, hjust = h, vjust = v), 
            parse = TRUE, size = 8) + # 辺ラベル
  geom_label(mapping = aes(x = -Inf, y = Inf, label = var_label), 
             parse = TRUE, hjust = 0, vjust = 1, label.r = unit(0, units = "pt"), 
             alpha = 0.5) + # 変数ラベル
  coord_fixed(ratio = 1, 
              xlim = c(-r-add_size, r+add_size), 
              ylim = c(-r-add_size, r+add_size)) + 
  labs(title = "golden angle", 
       subtitle = parse(text = def_label), 
       color = "arc", 
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))


### ・線分で可視化 -----

# 辺ラベルの座標を作成
d <- 0.1
label_df <- tibble::tibble(
  edge_label = c("ab", "a", "b"), 
  theta = pi * c(1, recip_phi, 2*recip_phi+(1 - recip_phi)), 
  x     = r * theta, 
  h = 0.5, 
  v = c(-0.5, 1.5, 1.5)
)


# 半円における目盛数(分母の値)を指定
denom <- 6

# 角度目盛線の座標を作成
angle_axis_df <- tibble::tibble(
  i     = 0:(2*denom),    # 目盛位置番号(分子の値)
  theta = i / denom * pi, # 目盛値
  x     = r * theta, 
  rad_label = paste0("frac(", i, ", ", denom, ") ~ pi * r") # 角度ラベル
)


# ラベル用の文字列を作成
def_label <- paste0(
  "list(", 
  "phi == frac(1 + sqrt(5), 2), ", 
  "a + b == 2 * pi * r, ", 
  "a + b : a == phi : 1, ", 
  "a + b : b == phi : phi - 1, ", 
  ")"
)
var_label <- paste0(
  "list(", 
  "r == ", round(r, digits = 2), ", ", 
  "a + b == ", round(ab, digits = 2), ", ", 
  "a == ", round(a, digits = 2), ", ", 
  "b == ", round(b, digits = 2), 
  ")"
)

# 線分上で黄金比を作図
ggplot() + 
  geom_vline(data = angle_axis_df, 
             mapping = aes(xintercept = x), 
             color = "gray", linewidth = 1, linetype = "dotted") + # 角度目盛線
  geom_line(data = circle_df, 
            mapping = aes(x = r*theta, y = 0, color = arc_label), 
            linewidth = 1) + # 円周
  geom_text(data = label_df, 
            mapping = aes(x = r*theta, y = 0, label = edge_label, hjust = h, vjust = v), 
            parse = TRUE, size = 8) + # 辺ラベル
  geom_label(mapping = aes(x = -Inf, y = Inf, label = var_label), 
             parse = TRUE, hjust = 0, vjust = 1, label.r = unit(0, units = "pt"), 
             alpha = 0.5) + # 変数ラベル
  scale_x_continuous(sec.axis = sec_axis(trans = ~., 
                                         breaks = angle_axis_df[["x"]], 
                                         labels = parse(text = angle_axis_df[["rad_label"]]))) + # 角度目盛ラベル
  scale_y_continuous(breaks = NULL) + 
  labs(title = "golden ratio", 
       subtitle = parse(text = def_label), 
       color = "arc", 
       x = expression(x == r * theta), 
         y = "")


# 黄金角の性質 ------------------------------------------------------------------

### ・設定 -----

# 半径を指定
r <- 2

# 黄金数を計算
phi <- (1 + sqrt(5)) * 0.5

# 黄金角を計算
beta <- 2*pi * (1 - 1/phi)


# 円周の座標を作成
circle_df <- tibble::tibble(
  theta = seq(from = 0, to = 2*pi, length.out = 901), # ラジアン
  x     = r * cos(theta), 
  y     = r * sin(theta)
)

# 半円における目盛数(分母の値)を指定
denom <- 6

# 角度目盛線の座標を作成
angle_axis_df <- tibble::tibble(
  i     = 0:(2*denom-1),  # 目盛位置番号(分子の値)
  theta = i / denom * pi, # 目盛値
  x     = r * cos(theta), 
  y     = r * sin(theta)
)


### ・黄金角と点の分布の関係 -----

# 点数を指定
n <- 300

# 円周上の点の座標を作成
point_df <- tibble::tibble(
  i     = 1:n, # 点番号
  theta = (i - 1) * beta, # ラジアン
  x     = r * cos(theta), 
  y     = r * sin(theta)
)


# ラベル用の文字列を作成
var_label <- paste0(
  "list(", 
  "r == ", r, ", ", 
  "theta == i * beta ~~ (i == list(1, ldots, n)), ", 
  "n == ", n, ", ", 
  "beta == ", round(beta/pi, digits = 2), " * pi, ", 
  "beta*degree == ", round(beta/pi*180, digits = 2), "*degree", 
  ")"
)

# 円周上の点を作図
ggplot() + 
  geom_segment(data = angle_axis_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = factor(i)), 
               color = "white") + # 角度目盛線
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y), 
            linewidth = 1) + # 円周
  geom_point(data = point_df, 
             mapping = aes(x = x, y = y, color = i), 
             size = 2.5) + # 円周上の点
  geom_path(data = point_df, 
            mapping = aes(x = x, y = y, color = i), 
            linewidth = 0.5) + # 円周上の点を結ぶ線分
  coord_fixed(ratio = 1) + 
  labs(title = "golden angle", 
       subtitle = parse(text = var_label), 
       color = expression(i),
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))


### ・角度と座標の関係 -----

# 点数を指定
n <- 10

# 点間の補完数を指定
inter_num <- 10

# 点間を補完して変化する点の座標を作成
anim_point_df <- tibble::tibble(
  i     = 1:n, # 点番号
  theta = (i - 1) * beta # ラジアン
) |> 
  dplyr::reframe(
    theta = seq(from = theta, to = theta+beta, length.out = inter_num+1)[1:inter_num], .by = dplyr::everything()
  ) |> # 次点間のラジアンを作成
  dplyr::mutate(
    inter_id = dplyr::row_number(), .by = i
  ) |> # 補完番号を割当
  dplyr::mutate(
    frame_i = (i - 1) * inter_num + inter_id, 
    x = r * cos(theta), 
    y = r * sin(theta), 
    var_label = paste0(
      "list(", 
      "r == ", r, ", ", 
      "theta == i * beta ~~ (i == list(1, ldots, n)), ", 
      "n == ", n, ", ", 
      "beta == ", round(beta/pi, digits = 2), " * pi, ", 
      "beta*degree == ", round(beta/pi*180, digits = 2), "*degree, ", 
      "i == ", i, ", ", 
      "theta == ", round(theta/pi, digits = 2), " * pi", 
      ")"
    )
  ) |> # フレーム番号を補完フレームに対応
  dplyr::arrange(frame_i)

# 黄金角分遅れて変化する点の座標を作成
delay_point_df <- anim_point_df |> 
  dplyr::mutate(
    theta = theta - beta, # 黄金数分戻す
    x     = r * cos(theta), 
    y     = r * sin(theta)
  )

# 前の点の座標を作成
pre_point_df <- tibble::tibble(
  i     = 1:n, 
  theta = (i - 1) * beta, 
  x     = r * cos(theta), 
  y     = r * sin(theta)
) |> 
  tidyr::uncount(
    weights = inter_num, .id = "inter_id"
  ) |> # 補完フレーム分の点を複製
  dplyr::mutate(
    frame_i = (i - 1) * inter_num + inter_id
  ) |> # フレーム番号を補完フレームに対応
  dplyr::arrange(frame_i)

# 過去の点の座標を作成
trace_point_df <- tibble::tibble(
  i     = 1:n, 
  theta = (i - 1) * beta, 
  x     = r * cos(theta), 
  y     = r * sin(theta)
) |> 
  dplyr::reframe(
    frame_i = i:n, .by = dplyr::everything()
  ) |> # 過去フレームの点を複製
  tidyr::uncount(
    weights = inter_num, .id = "inter_id"
  ) |> # 補完フレーム分の点を複製
  dplyr::mutate(
    frame_i = (frame_i - 1) * inter_num + inter_id
  ) |> # フレーム番号を補完フレームに対応
  dplyr::arrange(frame_i)

# 黄金角ラベルの座標を作成
d <- 0.15
angle_label_df <- anim_point_df |> 
  dplyr::mutate(
    theta = theta - 0.5*beta, # 黄金数の半分戻す
    x     = d * cos(theta), 
    y     = d * sin(theta)
  )

# 黄金角マークの座標を作成
d <- 0.1
angle_arc_df <- tibble::tibble(
  i     = 1:n, 
  theta = (i - 1) * beta
) |> 
  dplyr::reframe(
    theta = seq(from = theta, to = theta+beta, length.out = inter_num+1)[1:inter_num], .by = dplyr::everything()
  ) |> # 次点間のラジアンを作成
  dplyr::mutate(
    inter_id = dplyr::row_number(), .by = i
  ) |> # 補完番号を割当
  dplyr::mutate(
    frame_i = (i - 1) * inter_num + inter_id
  ) |> # フレーム番号を補完フレームに対応
  dplyr::reframe(
    theta = seq(from = theta, to = theta-beta, length.out = 100), .by = dplyr::everything()
  ) |> # 前点間のラジアンを作成
  dplyr::mutate(
    x = d * cos(theta), 
    y = d * sin(theta)
  ) |> 
  dplyr::arrange(frame_i)


# アニメーションを作図
anim <- ggplot() + 
  geom_segment(data = angle_axis_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = factor(i)), 
               color = "white") + # 角度目盛線
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y), 
            linewidth = 1) + # 円周
  geom_path(data = trace_point_df,
            mapping = aes(x = x, y = y, color = i),
            linewidth = 0.5) + # 過去の点間の線分
  geom_point(data = trace_point_df,
             mapping = aes(x = x, y = y, color = i),
             size = 4) + # 過去の点
  geom_path(data = angle_arc_df, 
            mapping = aes(x = x, y = y)) + # 黄金角マーク
  geom_text(data = angle_label_df, 
            mapping = aes(x = x, y = y), 
            label = "b", size = 6) + # 黄金角ラベル
  geom_segment(data = pre_point_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y), 
               linewidth = 1) + # 前点との半径線
  geom_segment(data = delay_point_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y), 
               linewidth = 1, linetype = "dashed") + # 遅れて変化する点との半径線
  geom_segment(data = anim_point_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y), 
               linewidth = 1) + # 変化する点との半径線
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y, color = i), 
             size = 6) + # 変化する点
  geom_text(data = anim_point_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル
  gganimate::transition_manual(frames = frame_i) + # 
  coord_fixed(ratio = 1, clip = "off") + 
  labs(title = "golden angle", 
       subtitle = "", # (変数ラベルの表示用)
       color = expression(i),
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# gif画像を作成
gganimate::animate(plot = anim, nframes = n*inter_num, fps = 10, width = 800, height = 800)


### ・点の数と座標の関係 -----

# 点数(フレーム数)を指定
n <- 100

# 円周上の点の座標を作成
trace_point_df <- tibble::tibble(
  i     = 1:n, # 点番号
  theta = (i - 1) * beta, 
  x     = r * cos(theta), 
  y     = r * sin(theta)
) |> 
  dplyr::reframe(
    frame_i = i:n, .by = dplyr::everything()
  ) |> # 過去フレームの点を複製
  dplyr::arrange(frame_i)

# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  i     = 1:n, 
  theta = (i - 1) * beta, 
  var_label = paste0(
    "list(", 
    "r == ", r, ", ", 
    "theta == i * beta ~~ (i == list(1, ldots, n)), ", 
    "n == ", n, ", ", 
    "beta == ", round(beta/pi, digits = 2), " * pi, ", 
    "beta*degree == ", round(beta/pi*180, digits = 2), "*degree, ", 
    "i == ", i, ", ", 
    "theta == ", round(theta/pi, digits = 2), " * pi", 
    ")"
  ), 
  frame_i = i
)


# 円周上の点のアニメーションを作図
anim <- ggplot() + 
  geom_segment(data = angle_axis_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = factor(i)), 
               color = "white") + # 角度目盛線
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y), 
            linewidth = 1) + # 円周
  geom_point(data = trace_point_df, 
             mapping = aes(x = x, y = y, color = i), 
             size = 4) + # 円周上の点
  geom_path(data = trace_point_df, 
            mapping = aes(x = x, y = y, color = i), 
            linewidth = 0.5) + # 円周上の点を結ぶ線分
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル
  gganimate::transition_manual(frames = frame_i) + # 円周上の点を結ぶ線分
  coord_fixed(ratio = 1, clip = "off") + 
  labs(title = "golden angle", 
       subtitle = "", # (変数ラベルの表示用)
       color = expression(i),
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# gif画像を作成
gganimate::animate(plot = anim, nframes = n, fps = 10, width = 800, height = 800)


### ・角度と点の分布の関係 -----

# フレーム数を指定
frame_num <- 300

# 角度を作成
beta_deg_vals <- seq(from = 0, to = 360, length.out = frame_num+1)[1:frame_num]
beta_rad_vals <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]
beta_rad_vals <- beta_deg_vals/180 * pi
beta_deg_vals <- beta_rad_vals/pi * 180

# 点数を指定
n <- 100

# 円周上の点の座標を作成
anim_point_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  i       = 1:n # 点番号
) |> # フレームごとに点を複製
  dplyr::mutate(
    beta  = beta_rad_vals[frame_i], 
    theta = (i - 1) * beta, 
    x     = r * cos(theta), 
    y     = r * sin(theta)
  )

# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  beta_rad = beta_rad_vals, 
  beta_deg = beta_deg_vals, 
  var_label = paste0(
    "list(", 
    "r == ", r, ", ", 
    "theta == i * beta ~~ (i == list(1, ldots, n)), ", 
    "n == ", n, ", ", 
    "beta == ", round(beta_rad/pi, digits = 2), " * pi, ", 
    "beta*degree == ", round(beta_deg, digits = 2), "*degree", 
    ")"
  )
)


# 円周上の点のアニメーションを作図
anim <- ggplot() + 
  geom_segment(data = angle_axis_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = factor(i)), 
               color = "white") + # 角度目盛線
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y), 
            linewidth = 1) + # 円周
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y, color = i), 
             size = 4) + # 円周上の点
  geom_path(data = anim_point_df, 
            mapping = aes(x = x, y = y, color = i), 
            linewidth = 0.5) + # 円周上の点を結ぶ線分
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル
  gganimate::transition_manual(frames = frame_i) + # 円周上の点を結ぶ線分
  coord_fixed(ratio = 1, clip = "off") + 
  labs(title = "golden angle", 
       subtitle = "", # (変数ラベルの表示用)
       color = expression(i),
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# gif画像を作成
gganimate::animate(plot = anim, nframes = n, fps = 10, width = 800, height = 800)


# 黄金角と点の分布とsin・cos曲線の関係 ----------------------------------------------------

# 一時ファイルの書き出し先を指定
dir_path <- "golden_ratio/figure/tmp_folder"


# フレーム数を指定
frame_num <- 90

# 角度の範囲を指定
beta_vals <- seq(from = 0, to = pi, length.out = frame_num+1)[1:frame_num]

# 半径を指定
r <- 2

# 点数を指定
n <- 6

# 周回数を確認:(円周用の点数用)
(n - 1)*max(beta_vals) /2/pi


# ノルム軸線(円周)の座標を計算
norm_axis_df <- tibble::tibble(
  theta = seq(from = 0, to = 2*pi, length.out = 361), 
  x     = r * cos(theta), 
  y     = r * sin(theta)
)

# 半円における目盛数(分母の値)を指定
denom <- 6

# 角度目盛線の座標を作成
angle_axis_df <- tibble::tibble(
  i     = 0:(2*denom-1),  # 目盛位置番号(分子の値)
  theta = i / denom * pi, # 目盛値
  x     = r * cos(theta), 
  y     = r * sin(theta)
)

# 半周期における目盛数を指定
line_num <- 1

# ラジアン軸目盛ラベル用の文字列を作成
theta_min <- 0
theta_max <- (n - 1) * max(beta_vals)
rad_break_vec <- seq(from = theta_min, to = ceiling(theta_max/pi)*pi, by = pi/line_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# グラフサイズを指定:(x・y軸に対してθ軸が過大な場合)
axis_size <- 5

# 角度ごとにグラフを書き出し
for(j in 1:frame_num) {
  
  # 角度を取得
  beta <- beta_vals[j]
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    i     = 1:n, # 点番号(離散値)
    theta = (i - 1) * beta, 
    x     = r * cos(theta), 
    y     = r * sin(theta)
  )
  
  # 円周の座標を作成
  circle_df <- tibble::tibble(
    i     = seq(from = 1, to = n, length.out = 2001), # 点番号(連続値)
    theta = (i - 1) * beta, 
    x     = r * cos(theta), 
    y     = r * sin(theta)
  )
  
  
  # ラベル用の文字列を作成
  fnc_label <- paste0(
    "list(", 
    "r == ", r, ", ", 
    "n == ", n, ", ", 
    "beta == ", round(beta/pi, digits = 2), " * pi", 
    ")"
  )
  
  # 円周上の点を作図
  circle_graph <- ggplot() + 
    geom_path(data = norm_axis_df, 
              mapping = aes(x = x, y = y, group = r), 
              color = "white") + # ノルム軸線
    geom_segment(data = angle_axis_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, group = factor(i)), 
                 color = "white") + # 角度目盛線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_vline(data = point_df, 
               mapping = aes(xintercept = x, color = i), 
               linetype = "dotted") + # x軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y, color = i), 
               linetype = "dotted") + # y軸の補助線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y, color = i), 
              linewidth = 1) + # 円周
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y, color = i), 
               size = 4) + # 円周上の点
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    #theme(legend.position = "left") + 
    labs(title = "golden angle", 
         subtitle = parse(text = fnc_label), 
         color = expression(i),
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  
  # sin曲線を作図
  sin_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・y軸線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y, color = i), 
               linetype = "dotted") + # y軸の補助線
    geom_line(data = circle_df, 
              mapping = aes(x = theta, y = y, color = i), 
              linewidth = 1) + # 曲線
    geom_point(data = point_df, 
               mapping = aes(x = theta, y = y, color = i), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # θ軸目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(theta_min, theta_max), 
                ylim = c(-axis_size, axis_size)) + 
    #theme(legend.position = "none") + 
    labs(title = "sine curve", 
         color = expression(i), 
         x = expression(theta), 
         y = expression(r ~ sin~theta))
  
  
  # cos曲線を作図
  cos_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・θ軸線
    geom_vline(data = point_df, 
               mapping = aes(xintercept = x, color = i), 
               linetype = "dotted") + # x軸の補助線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = theta, color = i), 
              linewidth = 1) + # 曲線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = theta, color = i), 
               size = 4) + # 曲線上の点
    scale_y_reverse(breaks = rad_break_vec,
                    labels = parse(text = rad_label_vec)) + # θ軸目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(theta_max, theta_min)) + 
    #theme(legend.position = "none") + 
    labs(title = "cosine curve", 
         color = expression(i), 
         x = expression(r ~ cos~theta), 
         y = expression(theta))
  
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    circle_graph, sin_graph, 
    cos_graph, 
    nrow = 2, ncol = 2, 
    guides = "collect"
  ) & 
    theme(legend.position = "left")
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(j, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 1200, height = 1200, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", j, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "golden_ratio/figure/golden_angle/curves_angle.gif", delay = 1/10) -> tmp_path # gifファイル書出


