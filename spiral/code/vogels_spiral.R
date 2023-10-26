
# 黄金角を用いたフェルマーの螺旋 -------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# チェック用
library(ggplot2)


# 黄金角による螺旋の作図 -------------------------------------------------------------

### ・作図 -----

# 点数を指定
n <- 100

# パラメータを指定
c <- 1

# 黄金角(ラジアン)を作成
b <- (3 - sqrt(5)) * pi


# 螺旋状の点の座標を作成
spiral_point_df <- tibble::tibble(
  i = 1:n, # 点番号
  t = i * b, # ラジアン
  r = c * sqrt(i), # ノルム
  x = r * cos(t), 
  y = r * sin(t)
)


# グラフサイズを設定
axis_size <- spiral_point_df |> 
  dplyr::pull(x, y) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size

# 目盛間隔を設定
step_val <- 2.5

# 目盛数を設定
if(axis_size%%step_val == 0) {
  
  # グラフサイズと最大目盛が一致する場合
  circle_num <- axis_size %/% step_val
} else {
  
  # グラフサイズと最大目盛が一致しない場合
  circle_num <- axis_size %/% step_val + 1
  
  # グラフサイズを拡大
  axis_size <- max(axis_size, circle_num*step_val)
}

# ノルム軸目盛線の座標を作成
coord_norm_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数(分母の値)を指定
denom <- 6

# 角度軸線の座標を作成
coord_angle_df <- tibble::tibble(
  i = 0:(2*denom-1),  # 目盛位置番号(分子の値)
  t = i / denom * pi, # 目盛値
  r = axis_size, # ノルム軸線の最大値
  x = r * cos(t), 
  y = r * sin(t), 
  t_label = paste0("frac(", i, ", ", denom, ")~pi"), # 角度ラベル
  h = 1 - (x/r * 0.5 + 0.5), # ラベル位置の調整値
  v = 1 - (y/r * 0.5 + 0.5)  # ラベル位置の調整値
)


# ラベル用の文字列を作成
fnc_label <- paste0(
  "list(", 
  "theta == i * beta, ", 
  "r == c * sqrt(i), ", 
  "(i == list(1, ldots, n)), ", 
  "n == ", n, ", ", 
  "beta == (3 - sqrt(5)) ~ pi, ", 
  "c == ", c, 
  ")"
)

# 螺旋状の点を作図
add_size <- 0.5
ggplot() + 
  geom_path(data = coord_norm_df, 
            mapping = aes(x = x, y = y, group = factor(r)), 
            color = "white") + # ノルム軸目盛線
  geom_segment(data = coord_angle_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = factor(i)), 
               color = "white") + # 角度軸目盛線
  geom_text(data = coord_angle_df, 
            mapping = aes(x = x, y = y, label = t_label, hjust = h, vjust = v), 
            parse = TRUE) + # 角度軸目盛ラベル
  geom_point(data = spiral_point_df, 
             mapping = aes(x = x, y = y, color = i), 
             size = 4) + # 螺旋状の点
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size-add_size, axis_size+add_size), 
              ylim = c(-axis_size-add_size, axis_size+add_size)) + 
  labs(title = "Fermat's spiral (golden angle)", 
       subtitle = parse(text = fnc_label), 
       color = expression(i),
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))


### ・点番号と角度と座標の関係 -----

# フレーム数を指定
frame_num <- 300

# 点番号の上限値を指定
n_upper <- 10

# 周回数を確認:(螺旋用の点数用)
n*b /2/pi


# 螺旋状の補助線の座標を作成
spiral_line_df <- tibble::tibble(
  i = seq(from = 0, to = n, length.out = 10001), # 点番号間を補完する連続値
  t = i * b, 
  r = c * sqrt(i), 
  x = r * cos(t), 
  y = r * sin(t)
)

# 螺旋上の点の座標を作成
anim_point_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  i = seq(from = 0, to = n_upper, length.out = frame_num), # 点番号間を補完する連続値
  t = i * b, 
  r = c * sqrt(i), 
  x = r * cos(t), # 螺旋上の点
  y = r * sin(t), # 螺旋上の点
  x_max = axis_size * cos(t), # ノルム軸線上の点
  y_max = axis_size * sin(t), # ノルム軸線上の点
  var_label = paste0(
    "list(", 
    "beta == ", round(b/pi, digits = 2), " * pi, ", 
    "c == ", c, ", ", 
    "i == ", round(i, digits = 2), ", ", 
    "theta == ", round(t/pi, digits = 2), " * pi, ", 
    "r == ", round(r, digits = 2), 
    ")"
  ) # 変数ラベル
)

# 角度用の補助線の座標を作成
anim_angle_oblique_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  i = seq(from = 0, to = n_upper, length.out = frame_num), # 点番号間を補完する連続値
  t = i * b, 
)


# 螺旋上の点のアニメーションを作図
add_size <- 0.5
anim <- ggplot() + 
  geom_path(data = coord_norm_df, 
            mapping = aes(x = x, y = y, group = factor(r)), 
            color = "white") + # ノルム軸目盛線
  geom_segment(data = coord_angle_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = factor(i)), 
               color = "white") + # 角度軸目盛線
  geom_text(data = coord_angle_df, 
            mapping = aes(x = x, y = y, label = t_label, hjust = h, vjust = v), 
            parse = TRUE) + # 角度軸目盛ラベル
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                             xend = c(Inf, 0), yend = c(0, Inf)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_path(data = spiral_line_df, 
            mapping = aes(x = x, y = y, color = i), 
            linewidth = 0.5) + # 螺旋状の補助線
  geom_point(data = spiral_point_df, 
             mapping = aes(x = x, y = y, color = i), 
             size = 4) + # 螺旋状の点
  geom_segment(data = anim_point_df,
               mapping = aes(x = 0, y = 0, xend = x_max, yend = y_max),
               linewidth = 1) + # 角度用の補助線
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y), 
             size = 5) + # 螺旋上の点
  geom_text(data = anim_point_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size-add_size, axis_size+add_size), 
              ylim = c(-axis_size-add_size, axis_size+add_size)) + 
  labs(title = "Fermat's spiral (golden angle)", 
       subtitle = "", # (変数ラベルの表示用の空行)
       color = expression(i),
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 600, height = 600, 
  renderer = gganimate::gifski_renderer()
)


### ・変数と中心からの距離の関係 -----

# 基準値を設定
t_min <- 0
t_max <- n * b

# ノルム軸目盛線の座標を作成
coord_radian_df <- tibble::tibble(
  t = seq(from = t_min, to = t_max, by = 2*pi), # 1周期間隔のラジアン
  i = t / b, 
  r = c * sqrt(i), 
  t_label = paste0(round(t/pi, digits = 1), " * pi")
)

# ラベル用の文字列を作成
fnc_label <- paste0(
  "list(", 
  "n == ", n, ", ", 
  "beta == ", round(b/pi, digits = 2), " * pi, ", 
  "c == ", c, 
  ")"
)

# 点番号(ラジアン)とノルムの関係を作図
ggplot() + 
  geom_line(data = spiral_line_df, 
            mapping = aes(x = t, y = r)) + # ラジアンとノルムの関係曲線
  geom_vline(data = coord_radian_df, 
             mapping = aes(xintercept = t), 
             linetype = "dotted") + # ラジアン軸目盛線
  geom_hline(data = coord_radian_df, 
             mapping = aes(yintercept = r), 
             linetype = "dotted") + # ノルム軸目盛線
  scale_x_continuous(sec.axis = sec_axis(trans = ~., 
                                         breaks = coord_radian_df[["t"]], 
                                         labels = parse(text = coord_radian_df[["t_label"]]))) + 
  labs(title = "Fermat's spiral (golden angle): norm", 
       subtitle = parse(text = fnc_label), 
       x = expression(theta), 
       y = expression(r == c ~ sqrt(frac(theta, beta))))
ggplot() + 
  geom_line(data = spiral_line_df, 
            mapping = aes(x = i, y = r), 
            linewidth = 1) + # 曲線
  geom_vline(data = coord_radian_df, 
             mapping = aes(xintercept = i), 
             linetype = "dotted") + # ラジアン軸目盛線
  geom_hline(data = coord_radian_df, 
             mapping = aes(yintercept = r), 
             linetype = "dotted") + # ノルム軸目盛線
  scale_x_continuous(sec.axis = sec_axis(trans = ~., 
                                         breaks = coord_radian_df[["i"]], 
                                         labels = parse(text = coord_radian_df[["t_label"]]), 
                                         name = expression(theta == i * beta))) + # ラジアン軸目盛ラベル
  labs(title = "Fermat's spiral (golden angle): norm", 
       subtitle = parse(text = fnc_label), 
       x = expression(i), 
       y = expression(list(r == c ~ sqrt(i), r == c ~ sqrt(frac(theta, beta)))))


# x軸の最大値を指定
x_max <- 10000

# 平方根を計算
sqrt_df <- tibble::tibble(
  x      = seq(from = 0, to = x_max, length.out = 1001), 
  sqrt_x = sqrt(x)
)

# 平方根を作図
ggplot() + 
  geom_line(data = sqrt_df, 
            mapping = aes(x = x, y = sqrt_x)) + # 曲線
  labs(title = "square root", 
       x = expression(x), 
       y = expression(sqrt(x)))


# パラメータと形状の関係 -------------------------------------------------------------------

# フレーム数を指定
frame_num <- 101

# パラメータの範囲を指定
c_vals <- seq(from = -5, to = 5, length.out = frame_num)

# 点数を指定
n <- 100

# 黄金角(ラジアン)を作成
b <- (3 - sqrt(5)) * pi

# 周回数を確認:(螺旋用の点数用)
n*b /2/pi


# 螺旋状の点の座標を作成
anim_spiral_point_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  i       = 1:n # 点番号(離散値)
) |> # フレームごとに点を複製
  dplyr::mutate(
    c = c_vals[frame_i], 
    t = i * b, 
    r = c * sqrt(i), 
    x = r * cos(t), 
    y = r * sin(t)
  )

# 螺旋状の補助線の座標を作成
anim_spiral_line_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, 
  i       = seq(from = 0, to = n, length.out = 10001) # 点番号(連続値)
) |> # フレームごとに螺旋用の点を複製
  dplyr::mutate(
    c = c_vals[frame_i], 
    t = i * b, 
    r = c * sqrt(i), 
    x = r * cos(t), 
    y = r * sin(t)
  )

# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  c       = c_vals, 
  param_label = paste0("c == ", round(c, digits = 2)) # パラメータラベル
)


# グラフサイズを指定
axis_size <- 20

# 目盛間隔を設定
step_val <- 5

# 目盛数を設定
circle_num <- (axis_size * sqrt(2)) %/% step_val

# ノルム軸目盛線の座標を作成
coord_norm_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数を指定
oblique_num <- 6

# 角度軸目盛線の座標を作成
coord_angle_df <- tibble::tibble(
  i = 0:(oblique_num-1),    # 目盛位置番号
  t = i / oblique_num * pi, # 目盛値
  a = tan(t) # ノルム軸目盛線の傾き
)


# ラベル用の文字列を作成
fnc_label <- paste0(
  "list(", 
  "theta == i * beta, ", 
  "r == c * sqrt(i), ", 
  "n == ", n, ", ", 
  "beta == ", round(b/pi, digits = 2), " * pi", 
  ")"
)

# 螺旋状の点のアニメーションを作図
anim <- ggplot() + 
  geom_path(data = coord_norm_df, 
            mapping = aes(x = x, y = y, group = factor(r)), 
            color = "white") + # ノルム軸目盛線
  geom_abline(data = coord_angle_df, 
              mapping = aes(slope = a, intercept = 0), 
              color = "white") + # 角度軸目盛線
  geom_path(data = anim_spiral_line_df,
            mapping = aes(x = x, y = y, color = i),
            linewidth = 0.5) + # 螺旋状の補助線
  geom_point(data = anim_spiral_point_df, 
             mapping = aes(x = x, y = y, color = i), 
             size = 4) + # 螺旋状の点
  geom_label(data = anim_label_df, 
             mapping = aes(x = -Inf, y = Inf, label = param_label), 
             parse = TRUE, hjust = 0, vjust = 1, label.r = unit(0, units = "pt"), 
             alpha = 0.5) + # パラメータラベル
  gganimate::transition_manual(frame = frame_i) + # フレーム切替
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "Fermat's spiral", 
       subtitle = parse(text = fnc_label), 
       color = expression(i),
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 600, height = 600, 
  renderer = gganimate::gifski_renderer()
)


# 黄金角と点の分布の関係 -------------------------------------------------------------

# フレーム数を指定
frame_num <- 100

# 点間の角度(ラジアン)の範囲を指定
b_vals <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# パラメータを指定
c <- 1

# 点数を指定
n <- 100

# 周回数を確認:(螺旋用の点数用)
n*max(b_vals) /2/pi


# 螺旋状の点の座標を作成
anim_spiral_point_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  i       = 1:n # 点番号(離散値)
) |> # フレームごとに点を複製
  dplyr::mutate(
    b = b_vals[frame_i], 
    t = i * b, 
    r = c * sqrt(i), 
    x = r * cos(t), 
    y = r * sin(t)
  )

# 螺旋の座標を作成
anim_spiral_line_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, 
  i       = seq(from = 0, to = n, length.out = 10001) # 点番号(連続値)
) |> # フレームごとに螺旋用の点を複製
  dplyr::mutate(
    b = b_vals[frame_i], 
    t = i * b, 
    r = c * sqrt(i), 
    x = r * cos(t), 
    y = r * sin(t)
  )

# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  b       = b_vals, 
  var_label = paste0(
    "list(", 
    "theta == i * beta, ", 
    "r == c * sqrt(i), ", 
    "n == ", n, ", ", 
    "beta == ", round(b/pi, digits = 2), " * pi, ", 
    "c == ", c, 
    ")"
  ) # 変数ラベル
)


# グラフサイズを設定
axis_size <- anim_spiral_line_df |> 
  dplyr::pull(x, y) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size

# 目盛間隔を設定
step_val <- 2.5

# 目盛数を設定
circle_num <- (axis_size * sqrt(2)) %/% step_val

# ノルム軸目盛線の座標を作成
coord_norm_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数を指定
oblique_num <- 6

# 角度軸目盛線の座標を作成
coord_angle_df <- tibble::tibble(
  i = 0:(oblique_num-1),    # 目盛位置番号
  t = i / oblique_num * pi, # 目盛値
  a = tan(t) # ノルム軸目盛線の傾き
)


# 螺旋状の点のアニメーションを作図
anim <- ggplot() + 
  geom_path(data = coord_norm_df, 
            mapping = aes(x = x, y = y, group = factor(r)), 
            color = "white") + # ノルム軸目盛線
  geom_abline(data = coord_angle_df, 
              mapping = aes(slope = a, intercept = 0), 
              color = "white") + # 角度軸目盛線
  geom_path(data = anim_spiral_line_df,
            mapping = aes(x = x, y = y, color = i),
            linewidth = 0.5) + # 螺旋状の補助線
  geom_point(data = anim_spiral_point_df, 
             mapping = aes(x = x, y = y, color = i), 
             size = 4) + # 螺旋状の点
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル
  gganimate::transition_manual(frame = frame_i) + # フレーム切替
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "Fermat's spiral", 
       subtitle = "", # (変数ラベルの表示用の空行)
       color = expression(i),
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 800, height = 800, 
  renderer = gganimate::gifski_renderer()
)


# 変数と螺旋とsin・cos曲線の関係 ----------------------------------------------------

# 一時ファイルの書き出し先を指定
dir_path <- "spiral/figure/tmp_folder"


# フレーム数を指定
frame_num <- 301

# 点数を指定
n <- 12

# 点番号(連続値)を作成
i_vals <- seq(from = 0, to = n, length.out = frame_num)

# パラメータを指定
c <- 1

# 黄金角(ラジアン)を作成
b <- (3 - sqrt(5)) * pi

# 周回数を確認:(螺旋用の点数用)
n*b /2/pi


# 螺旋状の点の座標を作成
spiral_point_df <- tibble::tibble(
  i = 1:n, # 点番号(離散値)
  t = i * b, 
  r = c * sqrt(i), 
  x = r * cos(t), 
  y = r * sin(t)
)

# 螺旋状の補助線の座標を作成
spiral_line_df <- tibble::tibble(
  i = seq(from = 0, to = n, length.out = 2001), # 点番号(連続値)
  t = i * b, 
  r = c * sqrt(i), 
  x = r * cos(t), 
  y = r * sin(t)
)


# グラフサイズを設定
axis_size <- c(spiral_line_df[["x"]], spiral_line_df[["y"]]) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size

# 目盛間隔を設定
step_val <- 1

# 目盛数を設定
circle_num <- (axis_size * sqrt(2)) %/% step_val

# ノルム軸目盛線の座標を作成
coord_norm_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数を指定
oblique_num <- 6

# 角度軸目盛線の座標を作成
coord_angle_df <- tibble::tibble(
  i = 0:(oblique_num-1), # 目盛位置番号
  t = i / oblique_num * pi, 
  a = tan(t) # ノルム軸線の傾き
)

# 半周期における目盛数を指定
line_num <- 1

# ラジアン軸目盛ラベル用の文字列を作成
t_min   <- 0
t_max   <- n * b
rad_break_vec <- seq(from = t_min, to = ceiling(t_max/pi)*pi, by = pi/line_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# 軸変換図のグリッド線の座標を作成
adapt_grid_df <- tidyr::expand_grid(
  r = seq(from = step_val, to = 2*axis_size, by = step_val), 
  t = seq(from = pi, to = 1.5*pi, length.out = 100)
) |> # グリッド線の数に応じてラジアンを複製
  dplyr::mutate(
    x = axis_size + r * cos(t), 
    y = axis_size + r * sin(t)
  )


# ラジアンごとにグラフを書き出し
for(j in 1:frame_num) {
  
  # 点番号(連続値)を取得
  i <- i_vals[j]
  
  # 係数を計算
  t <- i * b
  r <- c * sqrt(i)
  
  # 螺旋上の点の座標を作成
  point_df <- tibble::tibble(
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  
  # ラベル用の文字列を作成
  fnc_label <- paste0(
    "list(", 
    "beta == ", round(b/pi, digits = 2), " * pi, ", 
    "c == ", c, ", ", 
    "i == ", round(i, digits = 2), ", ", 
    "theta == ", round(t/pi, digits = 2), " * pi, ", 
    "r == ", round(r, digits = 2), 
    ")"
  )
  
  # 螺旋状の点を作図
  spiral_graph <- ggplot() + 
    geom_path(data = coord_norm_df, 
              mapping = aes(x = x, y = y, group = factor(r)), 
              color = "white") + # ノルム軸目盛線
    geom_abline(data = coord_angle_df, 
                mapping = aes(slope = a, intercept = 0), 
                color = "white") + # 角度軸目盛線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = spiral_line_df, 
              mapping = aes(x = x, y = y), 
              linewidth = 1) + # 螺旋状の補助線
    geom_point(data = spiral_point_df, 
               mapping = aes(x = x, y = y, color = i), 
               size = 4) + # 螺旋状の点
    geom_vline(data = point_df, 
               mapping = aes(xintercept = x), 
               color = "blue", linewidth = 1, linetype = "dotted") + # x軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y), 
               color = "red", linewidth = 1, linetype = "dotted") + # y軸の補助線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = 0), 
               color = "blue", size = 4) + # x軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = 0, y = y), 
               color = "red", size = 4) + # y軸線上の点
    geom_segment(data = point_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y), 
                 linewidth = 1) + # ノルム線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 螺旋上の点
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    #theme(legend.position = "left") + 
    labs(title = "Fermat's spiral (golden angle)", 
         subtitle = parse(text = fnc_label), 
         color = expression(i),
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  
  # ラベル用の文字列を作成
  sin_label <- paste0(
    "r ~ sin~theta == ", round(point_df[["y"]], digits = 2)
  )
  
  # sin曲線を作図
  sin_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・y軸線
    geom_line(data = spiral_line_df, 
              mapping = aes(x = t, y = y), 
              linewidth = 1) + # 曲線状の補助線
    geom_point(data = spiral_point_df, 
               mapping = aes(x = t, y = y, color = i), 
               size = 4) + # 曲線状の点
    geom_vline(mapping = aes(xintercept = t), 
               linewidth = 1, linetype = "dotted") + # θ軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y), 
               color = "red", linewidth = 1, linetype = "dotted") + # y軸の補助線
    geom_point(data = point_df, 
               mapping = aes(x = t, y = y), 
               color = "red", size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # θ軸目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(t_min, t_max), 
                ylim = c(-axis_size, axis_size)) + 
    #theme(legend.position = "none") + 
    labs(title = "sine curve", 
         subtitle = parse(text = sin_label), 
         color = expression(i), 
         x = expression(theta), 
         y = expression(r ~ sin~theta))
  
  
  # 軸変換曲線の座標を作成
  adapt_line_df <- tibble::tibble(
    rad = seq(from = pi, to = 1.5*pi, length.out = 100), 
    x = axis_size + abs(r * cos(t) - axis_size) * cos(rad), 
    y = axis_size + abs(r * cos(t) - axis_size) * sin(rad)
  )
  
  # 軸変換曲線を作図
  adapt_graph <- ggplot() + 
    geom_line(data = adapt_grid_df, 
              mapping = aes(x = x, y = y, group = r), 
              color = "white") + # グリッド線
    geom_line(data = adapt_line_df,
              mapping = aes(x = x, y = y),
              color = "blue", linewidth = 1, linetype = "dotted") + # x軸の補助線
    geom_point(data = point_df,
               mapping = aes(x = x, y = axis_size),
               color = "blue", size = 4) + # x軸線上の点:(変換前)
    geom_point(data = point_df,
               mapping = aes(x = axis_size, y = x),
               color = "blue", size = 4) + # x軸線上の点:(変換後)
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(x = "x", y = "x")
  
  
  # ラベル用の文字列を作成
  cos_label <- paste0(
    "r ~ cos~theta == ", round(point_df[["x"]], digits = 2)
  )
  
  # cos曲線を作図
  cos_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・θ軸線
    geom_line(data = spiral_line_df, 
              mapping = aes(x = t, y = x), 
              linewidth = 1) + # 曲線状の補助線
    geom_point(data = spiral_point_df, 
               mapping = aes(x = t, y = x, color = i), 
               size = 4) + # 曲線状の点
    geom_vline(mapping = aes(xintercept = t), 
               linewidth = 1, linetype = "dotted") + # θ軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = x), 
               color = "blue", linewidth = 1, linetype = "dotted") + # x軸の補助線
    geom_point(data = point_df, 
               mapping = aes(x = t, y = x), 
               color = "blue", size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # θ軸目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(t_min, t_max), 
                ylim = c(-axis_size, axis_size)) + 
    #theme(legend.position = "none") + 
    labs(title = "cosine curve", 
         subtitle = parse(text = cos_label), 
         color = expression(i), 
         x = expression(theta), 
         y = expression(r ~ cos~theta))
  
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    spiral_graph, sin_graph, 
    adapt_graph, cos_graph, 
    nrow = 2, ncol = 2, 
    guides = "collect"
  ) & 
    theme(legend.position = "left")
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(j, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 1500, height = 900, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", j, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "spiral/figure/fermats_golden_spiral/curves_variable.gif", delay = 1/30) -> tmp_path # gifファイル書出


# パラメータと螺旋とsin・cos曲線の関係 ----------------------------------------------------

# 一時ファイルの書き出し先を指定
dir_path <- "spiral/figure/tmp_folder"


# フレーム数を指定
frame_num <- 101

# パラメータの範囲を指定
c_vals <- seq(from = -5, to = 5, length.out = frame_num)

# 点数を指定
n <- 12

# 黄金角(ラジアン)を作成
b <- (3 - sqrt(5)) * pi

# 周回数を確認:(螺旋用の点数用)
n*b /2/pi


# グラフサイズを設定
axis_size <- c(
  max(abs(c_vals))*sqrt(1:n) * cos(1:n * b), 
  max(abs(c_vals))*sqrt(1:n) * sin(1:n * b)
) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size

# 目盛間隔を設定
step_val <- 5

# 目盛数を設定
circle_num <- (axis_size * sqrt(2)) %/% step_val

# ノルム軸目盛線の座標を作成
coord_norm_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数を指定
oblique_num <- 6

# 角度軸目盛線の座標を作成
coord_angle_df <- tibble::tibble(
  i = 0:(oblique_num-1), # 目盛位置番号
  t = i / oblique_num * pi, 
  a = tan(t) # ノルム軸線の傾き
)

# 半周期における目盛数を指定
line_num <- 1

# ラジアン軸目盛ラベル用の文字列を作成
t_min   <- 0
t_max   <- n * b
rad_break_vec <- seq(from = t_min, to = ceiling(t_max/pi)*pi, by = pi/line_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# パラメータごとにグラフを書き出し
for(j in 1:frame_num) {
  
  # パラメータを取得
  c <- c_vals[j]
  
  
  # 螺旋状の点の座標を作成
  spiral_point_df <- tibble::tibble(
    i = 1:n, # 点番号(離散値)
    t = i * b, 
    r = c * sqrt(i), 
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  # 螺旋状の補助線の座標を作成
  spiral_line_df <- tibble::tibble(
    i = seq(from = 0, to = n, length.out = 2001), # 点番号(連続値)
    t = i * b, 
    r = c * sqrt(i), 
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  
  # ラベル用の文字列を作成
  fnc_label <- paste0(
    "list(", 
    "theta == i * beta, ", 
    "r == c * sqrt(i), ", 
    "n == ", n, ", ", 
    "beta == ", round(b/pi, digits = 2), " * pi, ", 
    "c == ", round(c, digits = 2), 
    ")"
  )
  
  # 螺旋状の点を作図
  spiral_graph <- ggplot() + 
    geom_path(data = coord_norm_df, 
              mapping = aes(x = x, y = y, group = factor(r)), 
              color = "white") + # ノルム軸目盛線
    geom_abline(data = coord_angle_df, 
                mapping = aes(slope = a, intercept = 0), 
                color = "white") + # 角度軸目盛線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_vline(data = spiral_point_df, 
               mapping = aes(xintercept = x, color = i), 
               linetype = "dotted") + # x軸の補助線
    geom_hline(data = spiral_point_df, 
               mapping = aes(yintercept = y, color = i), 
               linetype = "dotted") + # y軸の補助線
    geom_path(data = spiral_line_df, 
              mapping = aes(x = x, y = y, color = i), 
              linewidth = 1) + # 螺旋状の補助線
    geom_point(data = spiral_point_df, 
               mapping = aes(x = x, y = y, color = i), 
               size = 4) + # 螺旋状の点
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    #theme(legend.position = "left") + 
    labs(title = "Fermat's spiral (golden angle)", 
         subtitle = parse(text = fnc_label), 
         color = expression(i),
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  
  # sin曲線を作図
  sin_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・y軸線
    geom_hline(data = spiral_point_df, 
               mapping = aes(yintercept = y, color = i), 
               linetype = "dotted") + # y軸の補助線
    geom_line(data = spiral_line_df, 
              mapping = aes(x = t, y = y, color = i), 
              linewidth = 1) + # 曲線状の補助線
    geom_point(data = spiral_point_df, 
               mapping = aes(x = t, y = y, color = i), 
               size = 4) + # 曲線状の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # θ軸目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(t_min, t_max), 
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
    geom_vline(data = spiral_point_df, 
               mapping = aes(xintercept = x, color = i), 
               linetype = "dotted") + # x軸の補助線
    geom_path(data = spiral_line_df, 
              mapping = aes(x = x, y = t, color = i), 
              linewidth = 1) + # 曲線状の補助線
    geom_point(data = spiral_point_df, 
               mapping = aes(x = x, y = t, color = i), 
               size = 4) + # 曲線状の点
    scale_y_reverse(breaks = rad_break_vec,
                    labels = parse(text = rad_label_vec)) + # θ軸目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(t_max, t_min)) + 
    #theme(legend.position = "none") + 
    labs(title = "cosine curve", 
         color = expression(i), 
         x = expression(r ~ cos~theta), 
         y = expression(theta))
  
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    spiral_graph, sin_graph, 
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
  magick::image_write_gif(path = "spiral/figure/fermats_golden_spiral/curves_param.gif", delay = 1/10) -> tmp_path # gifファイル書出


# 黄金角と螺旋とsin・cos曲線の関係 ----------------------------------------------------

# 一時ファイルの書き出し先を指定
dir_path <- "spiral/figure/tmp_folder"


# フレーム数を指定
frame_num <- 90

# 点間の角度(ラジアン)の範囲を指定
b_vals <- seq(from = 0, to = pi, length.out = frame_num+1)[1:frame_num]

# パラメータを指定
c <- 1

# 点数を指定
n <- 6

# 周回数を確認:(螺旋用の点数用)
n*max(b_vals) /2/pi


# グラフサイズを設定
axis_size <- c(
  c*sqrt(1:n) * cos(1:n * max(b_vals)), 
  c*sqrt(1:n) * sin(1:n * max(b_vals))
) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size

# 目盛間隔を設定
step_val <- 1

# 目盛数を設定
circle_num <- (axis_size * sqrt(2)) %/% step_val

# ノルム軸目盛線の座標を作成
coord_norm_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数を指定
oblique_num <- 6

# 角度軸目盛線の座標を作成
coord_angle_df <- tibble::tibble(
  i = 0:(oblique_num-1), # 目盛位置番号
  t = i / oblique_num * pi, 
  a = tan(t) # ノルム軸線の傾き
)

# 半周期における目盛数を指定
line_num <- 1

# ラジアン軸目盛ラベル用の文字列を作成
t_min   <- 0
t_max   <- n * max(b_vals)
rad_break_vec <- seq(from = t_min, to = ceiling(t_max/pi)*pi, by = pi/line_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# 角度ごとにグラフを書き出し
for(j in 1:frame_num) {
  
  # 角度を取得
  b <- b_vals[j]
  
  
  # 螺旋状の点の座標を作成
  spiral_point_df <- tibble::tibble(
    i = 1:n, # 点番号(離散値)
    t = i * b, 
    r = c * sqrt(i), 
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  # 螺旋状の補助線の座標を作成
  spiral_line_df <- tibble::tibble(
    i = seq(from = 0, to = n, length.out = 2001), # 点番号(連続値)
    t = i * b, 
    r = c * sqrt(i), 
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  
  # ラベル用の文字列を作成
  fnc_label <- paste0(
    "list(", 
    "theta == i * beta, ", 
    "r == c * sqrt(i), ", 
    "n == ", n, ", ", 
    "beta == ", round(b/pi, digits = 2), " * pi, ", 
    "c == ", c, 
    ")"
  )
  
  # 螺旋状の点を作図
  spiral_graph <- ggplot() + 
    geom_path(data = coord_norm_df, 
              mapping = aes(x = x, y = y, group = factor(r)), 
              color = "white") + # ノルム軸目盛線
    geom_abline(data = coord_angle_df, 
                mapping = aes(slope = a, intercept = 0), 
                color = "white") + # 角度軸目盛線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_vline(data = spiral_point_df, 
               mapping = aes(xintercept = x, color = i), 
               linetype = "dotted") + # x軸の補助線
    geom_hline(data = spiral_point_df, 
               mapping = aes(yintercept = y, color = i), 
               linetype = "dotted") + # y軸の補助線
    geom_path(data = spiral_line_df, 
              mapping = aes(x = x, y = y, color = i), 
              linewidth = 1) + # 螺旋状の補助線
    geom_point(data = spiral_point_df, 
               mapping = aes(x = x, y = y, color = i), 
               size = 4) + # 螺旋状の点
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    #theme(legend.position = "left") + 
    labs(title = "Fermat's spiral (golden angle)", 
         subtitle = parse(text = fnc_label), 
         color = expression(i),
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  
  # sin曲線を作図
  sin_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・y軸線
    geom_hline(data = spiral_point_df, 
               mapping = aes(yintercept = y, color = i), 
               linetype = "dotted") + # y軸の補助線
    geom_line(data = spiral_line_df, 
              mapping = aes(x = t, y = y, color = i), 
              linewidth = 1) + # 曲線状の補助線
    geom_point(data = spiral_point_df, 
               mapping = aes(x = t, y = y, color = i), 
               size = 4) + # 曲線状の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # θ軸目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(t_min, t_max), 
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
    geom_vline(data = spiral_point_df, 
               mapping = aes(xintercept = x, color = i), 
               linetype = "dotted") + # x軸の補助線
    geom_path(data = spiral_line_df, 
              mapping = aes(x = x, y = t, color = i), 
              linewidth = 1) + # 曲線状の補助線
    geom_point(data = spiral_point_df, 
               mapping = aes(x = x, y = t, color = i), 
               size = 4) + # 曲線状の点
    scale_y_reverse(breaks = rad_break_vec,
                    labels = parse(text = rad_label_vec)) + # θ軸目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(t_max, t_min)) + 
    #theme(legend.position = "none") + 
    labs(title = "cosine curve", 
         color = expression(i), 
         x = expression(r ~ cos~theta), 
         y = expression(theta))
  
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    spiral_graph, sin_graph, 
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
  magick::image_write_gif(path = "spiral/figure/fermats_golden_spiral/curves_angle.gif", delay = 1/10) -> tmp_path # gifファイル書出


