
# アルキメデスの螺旋 ---------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# チェック用
library(ggplot2)


# 螺旋の作図 -----------------------------------------------------------------

### ・作図 -----

# パラメータを指定
a <- 1
a <- 2.5

# 周回数を指定
lap_num <- 8

# 螺旋用のラジアン(弧度法の角度)を作成
t_vec <- seq(from = 0, to = lap_num*2*pi, length.out = 1000) # 正の範囲
t_vec <- seq(from = -lap_num*2*pi, to = 0, length.out = 1000) # 負の範囲
t_vec <- seq(from = -lap_num*pi, to = lap_num*pi, length.out = 1000) # 正と負の範囲

# 螺旋の座標を作成
spiral_df <- tibble::tibble(
  t = t_vec, # ラジアン
  r = a * t, # ノルム
  x = r * cos(t), # x座標
  y = r * sin(t), # y座標
  sign_t_flag = t >= 0 # 角度の正負
)


# グラフサイズを設定
axis_size <- c(spiral_df[["x"]], spiral_df[["y"]]) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size

# 目盛間隔を設定
step_val <- 12.5
step_val <- 25

# 軸線数を設定
if(axis_size%%step_val == 0) {
  
  # グラフサイズと最大目盛が一致する場合
  circle_num <- axis_size %/% step_val
} else {
  
  # グラフサイズと最大目盛が一致しない場合
  circle_num <- axis_size %/% step_val + 1
  
  # グラフサイズを拡大
  axis_size  <- max(axis_size, circle_num*step_val)
}

# 円形の軸線の座標を作成
coord_circle_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数(分母の値)を指定
denom <- 6

# 斜線の軸線(角度目盛ラベル)の座標を作成
coord_oblique_df <- tibble::tibble(
  i = seq(from = 0, to = 2*denom-1, by = 1), # 目盛位置番号(分子の値)
  t = i / denom * pi, 
  r = max(coord_circle_df[["r"]]), 
  x = r * cos(t), 
  y = r * sin(t), 
  t_label = paste0("frac(", i, ", ", denom, ")~pi"), # 角度ラベル
  h = 1 - (x/r * 0.5 + 0.5), 
  v = 1 - (y/r * 0.5 + 0.5)
)


# ラベル用の文字列を作成
fnc_label <- paste0(
  "list(", 
  "r == a * theta", ", ", 
  "a == ", a, ", ", 
  "theta == frac(i, n) ~ pi", 
  ")"
)

# 螺旋を作図
add_size <- 4
ggplot() + 
  geom_path(data = coord_circle_df, 
            mapping = aes(x = x, y = y, group = r), 
            color = "white") + # 円形の軸線
  geom_segment(data = coord_oblique_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = i), 
               color = "white") + # 斜線の軸線
  geom_text(data = coord_oblique_df, 
            mapping = aes(x = x, y = y, label = t_label, hjust = h, vjust = v), 
            parse = TRUE) + # 角度目盛ラベル
  geom_path(data = spiral_df, 
            mapping = aes(x = x, y = y), 
            linewidth = 1) + # 螺旋
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size-add_size, axis_size+add_size), 
              ylim = c(-axis_size-add_size, axis_size+add_size)) + 
  labs(title = "Archimedes' spiral", 
       subtitle = parse(text = fnc_label), 
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))


### ・角度の影響の可視化 -----

# フレーム数を指定
frame_num <- 300

# 曲線上の点用のラジアンを作成
t_vals <- seq(from = min(t_vec), to = max(t_vec), length.out = frame_num+1)[1:frame_num]

# 螺旋上の点の座標を作成
angle_point_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  t = t_vals, 
  r = a * t, 
  x = r * cos(t), 
  y = r * sin(t), 
  var_label = paste0(
    "list(", 
    "a == ", a, ", ", 
    "r == ", round(r, digits = 2), ", ", 
    "theta == ", round(t/pi, digits = 2), " * pi", 
    ")"
  ) # 変数ラベル
)

# 補助線上の点の座標を作成
angle_oblique_df <- dplyr::bind_rows(
  # 角度用の斜線
  tibble::tibble(
    frame_i = 1:frame_num, 
    t = t_vals, 
    r = a * t, 
    x = dplyr::if_else(r >= 0, true = axis_size*cos(t), false = -axis_size*cos(t)), 
    y = dplyr::if_else(r >= 0, true = axis_size*sin(t), false = -axis_size*sin(t)), 
    type = "origin"
  ), 
  # 反転した角度用の斜線
  tibble::tibble(
    frame_i = 1:frame_num, 
    t = t_vals, 
    r = a * t, 
    type = "negation"
  ) |> 
    dplyr::mutate(
      x = dplyr::if_else(r < 0, true = axis_size*cos(t), false = -axis_size*cos(t)), 
      y = dplyr::if_else(r < 0, true = axis_size*sin(t), false = -axis_size*sin(t))
    )
)

# 補助線上の点の座標を作成
r_min <- a * min(t_vec)
r_max <- a * max(t_vec)
point_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, 
  lap_i   = (-lap_num):(lap_num-1)
) |> 
  dplyr::mutate(
    t = t_vals[frame_i] %% (2*pi), # 単位円相当のラジアンに変換
    r = a * (lap_i*2*pi + t), 
    x = r * cos(t), 
    y = r * sin(t),   
    sign_t_flag = r/a >= 0 # 角度の正負
  ) |> 
  dplyr::filter(r >= r_min, r <= r_max) # 螺旋上の点を抽出


# 螺旋上の点のアニメーションを作図
add_size <- 4
anim <- ggplot() + 
  geom_path(data = coord_circle_df, 
            mapping = aes(x = x, y = y, group = r), 
            color = "white") + # 円形の軸線
  geom_segment(data = coord_oblique_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = i), 
               color = "white") + # 斜線の軸線
  geom_text(data = coord_oblique_df, 
            mapping = aes(x = x, y = y, label = t_label, hjust = h, vjust = v), 
            parse = TRUE) + # 角度目盛ラベル
  geom_path(data = spiral_df, 
            mapping = aes(x = x, y = y, color = sign_t_flag), 
            linewidth = 0.5) + # 螺旋
  geom_segment(data = angle_oblique_df,
               mapping = aes(x = 0, y = 0, xend = x, yend = y, linetype = type),
               linewidth = 1) + # 角度用の補助線
  geom_point(data = angle_point_df, 
             mapping = aes(x = x, y = y), 
             size = 5) + # 螺旋上の点
  geom_point(data = point_df, 
             mapping = aes(x = x, y = y, color = sign_t_flag), 
             size = 4, shape = "circle open") + # 角度用の補助線上の点
  geom_text(data = angle_point_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_color_manual(breaks = c("TRUE", "FALSE"), 
                     values = c("blue", "red"), 
                     labels = c(expression(theta >= 0), expression(theta < 0)), 
                     name = expression(sgn~theta)) + # 角度の正負
  scale_linetype_manual(breaks = c("origin", "negation"), 
                        values = c("solid", "dashed"), guide = "none") + # 角度の正負
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size-add_size, axis_size+add_size), 
              ylim = c(-axis_size-add_size, axis_size+add_size)) + 
  labs(title = "Archimedes' spiral", 
       subtitle = "", # (変数ラベルの表示用)
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 600, height = 600, 
  renderer = gganimate::gifski_renderer()
)


# パラメータの影響 -------------------------------------------------------------------

# フレーム数を指定
frame_num <- 101

# パラメータを指定
a_vals <- seq(from = -2, to = 2, length.out = frame_num)

# 周回数を指定
lap_num <- 8

# 点数を指定
point_num <- lap_num * 4 + 1

# 螺旋の座標を作成
anim_spiral_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, 
  t       = seq(from = -lap_num*pi, to = lap_num*pi, length.out = 1000)
) |> # フレームごとにラジアンを複製
  dplyr::mutate(
    a = a_vals[frame_i], 
    r = a * t, 
    x = r * cos(t), 
    y = r * sin(t)
  )

# 螺旋上の点の座標を作成
anim_point_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, 
  t       = seq(
    from = min(anim_spiral_df[["t"]]), 
    to   = max(anim_spiral_df[["t"]]), 
    length.out = point_num
  )
) |> # フレームごとにラジアンを複製
  dplyr::mutate(
    a = a_vals[frame_i], 
    r = a * t, 
    x = r * cos(t), 
    y = r * sin(t)
  )

# パラメータラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  a       = a_vals, 
  param_label = paste0(
    "list(", 
    "r == a * theta", ", ", 
    "a == ", round(a, digits = 2), 
    ")"
  ) # パラメータラベル
)


# グラフサイズを設定
axis_size <- c(anim_spiral_df[["x"]], anim_spiral_df[["y"]]) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size

# 目盛間隔を設定
step_val <- 12.5

# 軸線数を設定
circle_num <- axis_size %/% step_val

# 円形の軸線の座標を作成
coord_circle_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数(分母の値)を指定
denom <- 6

# 斜線の軸線(角度目盛ラベル)の座標を作成
coord_oblique_df <- tibble::tibble(
  i = seq(from = 0, to = 2*denom-1, by = 1), # 目盛位置番号(分子の値)
  t = i / denom * pi, 
  r = max(coord_circle_df[["r"]]), 
  x = r * cos(t), 
  y = r * sin(t), 
  t_label = paste0("frac(", i, ", ", denom, ")~pi"), # 角度ラベル
  h = 1 - (x/r * 0.5 + 0.5), 
  v = 1 - (y/r * 0.5 + 0.5)
)


# 螺旋のアニメーションを作図
anim <- ggplot() + 
  geom_path(data = coord_circle_df, 
            mapping = aes(x = x, y = y, group = r), 
            color = "white") + # 円形の軸線
  geom_segment(data = coord_oblique_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, group = i), 
               color = "white") + # 斜線の軸線
  geom_path(data = anim_spiral_df, 
            mapping = aes(x = x, y = y, color = t/pi), 
            linewidth = 1) + # 螺旋
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y, color = t/pi), 
             size = 4) + # 螺旋上の点
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = param_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # パラメータラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "Archimedes' spiral", 
       subtitle = "", # (パラメータラベルの表示用)
       color = expression(frac(theta, pi)), 
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 600, height = 600, 
  renderer = gganimate::gifski_renderer()
)


# 変数と螺旋とsin・cos関数の関係 ----------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "spiral/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# パラメータを指定
a <- 0.1

# 周回数を指定
lap_num <- 3

# ラジアンを作成
t_vec <- seq(from = 0*lap_num*2*pi, to = lap_num*2*pi, length.out = 1000) # 曲線用
t_vals <- seq(from = min(t_vec), to = max(t_vec), length.out = frame_num+1)[1:frame_num] # 曲線上の点用

# 螺旋の座標を作成
spiral_df <- tibble::tibble(
  t = t_vec, 
  r = a * t, 
  x = r * cos(t), 
  y = r * sin(t)
)


# グラフサイズを設定
axis_size <- c(spiral_df[["x"]], spiral_df[["y"]]) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size

# 目盛間隔を設定
step_val <- 0.5

# 軸線数を設定
circle_num <- axis_size %/% step_val

# 円形の軸線の座標を作成
coord_circle_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数(分母の値)を指定
denom <- 6

# 斜線の軸線(角度目盛ラベル)の座標を作成
coord_oblique_df <- tibble::tibble(
  i = seq(from = 0, to = 2*denom-1, by = 1), # 目盛位置番号(分子の値)
  t = i / denom * pi, 
  r = max(coord_circle_df[["r"]]), 
  x = r * cos(t), 
  y = r * sin(t), 
  h = 1 - (x/r * 0.5 + 0.5), 
  v = 1 - (y/r * 0.5 + 0.5)
)

# 目盛ラベル用の文字列を作成
t_min <- min(t_vec)
t_max <- max(t_vec)
numer_vec     <- seq(from = floor(t_min/pi*denom), to = ceiling(t_max/pi*denom), by = 1)
rad_break_vec <- numer_vec / denom * pi
rad_label_vec <- paste0(c("", "-")[(numer_vec < 0)+1], "frac(", abs(numer_vec), ", ", denom, ")~pi")


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
for(i in 1:frame_num) {
  
  # i番目のラジアンを取得
  t <- t_vals[i]
  
  # 係数を計算
  r <- a * t
  
  # 螺旋上の点の座標を作成
  point_df <- tibble::tibble(
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  
  # ラベル用の文字列を作成
  fnc_label <- paste0(
    "list(", 
    "r == a * theta", ", ", 
    "a == ", a, ", ", 
    "theta == ", round(t/pi, digits = 2), "*pi", 
    ")"
  )
  
  # 螺旋を作図
  spiral_graph <- ggplot() + 
    geom_path(data = coord_circle_df, 
              mapping = aes(x = x, y = y, group = r), 
              color = "white") + # 円形の軸線
    geom_segment(data = coord_oblique_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, group = i), 
                 color = "white") + # 斜線の軸線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = spiral_df, 
              mapping = aes(x = x, y = y), 
              linewidth = 1) + # 螺旋
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
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 曲線上の点
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "Archimedes' spiral", 
         subtitle = parse(text = fnc_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  
  # ラベル用の文字列を作成
  sin_label <- paste0(
    round(r, digits = 2), " ~ sin~theta == ", round(point_df[["y"]], digits = 2)
  )
  
  # sin曲線を作図
  sin_graph <- ggplot() + 
    geom_line(data = spiral_df, 
              mapping = aes(x = t, y = y), 
              linewidth = 1) + # 曲線
    geom_vline(mapping = aes(xintercept = t), 
               linewidth = 1, linetype = "dotted") + # ラジアン軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y), 
               color = "red", linewidth = 1, linetype = "dotted") + # y軸の補助線
    geom_point(data = point_df, 
               mapping = aes(x = t, y = y), 
               color = "red", size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # ラジアン目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(t_min, t_max), 
                ylim = c(-axis_size, axis_size)) + 
    theme(axis.text.y = element_text(size = 8)) + 
    labs(title = "sine curve", 
         subtitle = parse(text = sin_label), 
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
    round(r, digits = 2), " ~ cos~theta == ", round(point_df[["x"]], digits = 2)
  )
  
  # cos曲線を作図
  cos_graph <- ggplot() + 
    geom_line(data = spiral_df, 
              mapping = aes(x = t, y = x), 
              linewidth = 1) + # 曲線
    geom_vline(mapping = aes(xintercept = t), 
               linewidth = 1, linetype = "dotted") + # ラジアン軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = x), 
               color = "blue", linewidth = 1, linetype = "dotted") + # x軸の補助線
    geom_point(data = point_df, 
               mapping = aes(x = t, y = x), 
               color = "blue", size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # ラジアン目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(t_min, t_max), 
                ylim = c(-axis_size, axis_size)) + 
    theme(axis.text.y = element_text(size = 8)) + 
    labs(title = "cosine curve", 
         subtitle = parse(text = cos_label), 
         x = expression(theta), 
         y = expression(r ~ cos~theta))
  
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    spiral_graph, sin_graph, 
    adapt_graph, cos_graph, 
    nrow = 2, ncol = 2
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 2000, height = 1000, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読み込み
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "spiral/figure/archimedes/curves_var.gif", delay = 1/30) -> tmp_path # gifファイル書き出し


# パラメータと螺旋とsin・cos関数の関係 ----------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "spiral/figure/tmp_folder"


# フレーム数を指定
frame_num <- 101

# パラメータを指定
a_vals <- seq(from = -1, to = 1, length.out = frame_num)

# 周回数を指定
lap_num <- 2

# 点数を指定
point_num <- lap_num * 4 + 1

# ラジアンを作成
t_vec <- seq(from = -lap_num*pi, to = lap_num*pi, length.out = 1000) # 曲線用
t_vals <- seq(from = min(t_vec), to = max(t_vec), length.out = point_num) # 曲線上の点用


# グラフサイズを設定
axis_size <- c(
  max(abs(a_vals))*t_vec * cos(t_vec), 
  max(abs(a_vals))*t_vec * sin(t_vec)
) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size

# 目盛間隔を設定
step_val <- 2

# 軸線数を設定
circle_num <- axis_size %/% step_val

# 円形の軸線の座標を作成
coord_circle_df <- tidyr::expand_grid(
  r = 1:circle_num * step_val, 
  t = seq(from = 0, to = 2*pi, length.out = 361), 
) |> # 半径ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )

# 半円における目盛数(分母の値)を指定
denom <- 6

# 斜線の軸線(角度目盛ラベル)の座標を作成
coord_oblique_df <- tibble::tibble(
  i = seq(from = 0, to = 2*denom-1, by = 1), # 目盛位置番号(分子の値)
  t = i / denom * pi, 
  r = max(coord_circle_df[["r"]]), 
  x = r * cos(t), 
  y = r * sin(t), 
  h = 1 - (x/r * 0.5 + 0.5), 
  v = 1 - (y/r * 0.5 + 0.5)
)

# 目盛ラベル用の文字列を作成
t_min <- min(t_vec)
t_max <- max(t_vec)
numer_vec     <- seq(from = floor(t_min/pi*denom), to = ceiling(t_max/pi*denom), by = 1)
rad_break_vec <- numer_vec / denom * pi
rad_label_vec <- paste0(c("", "-")[(numer_vec < 0)+1], "frac(", abs(numer_vec), ", ", denom, ")~pi")


# パラメータごとにグラフを書き出し
for(i in 1:frame_num) {
  
  # i番目のパラメータを取得
  a <- a_vals[i]
  
  # 螺旋の座標を作成
  spiral_df <- tibble::tibble(
    t = t_vec, 
    r = a * t, 
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  # 螺旋上の点の座標を作成
  point_df <- tibble::tibble(
    t = t_vals, 
    r = a * t, 
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  
  # ラベル用の文字列を作成
  fnc_label <- paste0(
    "list(", 
    "r == a * theta", ", ", 
    "a == ", round(a, digits = 2), 
    ")"
  )
  
  # 螺旋を作図
  spiral_graph <- ggplot() + 
    geom_path(data = coord_circle_df, 
              mapping = aes(x = x, y = y, group = r), 
              color = "white") + # 円形の軸線
    geom_segment(data = coord_oblique_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, group = i), 
                 color = "white") + # 斜線の軸線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_vline(data = point_df, 
               mapping = aes(xintercept = x, color = t/pi), 
               linetype = "dotted") + # x軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y, color = t/pi), 
               linetype = "dotted") + # y軸の補助線
    geom_path(data = spiral_df, 
              mapping = aes(x = x, y = y, color = t/pi), 
              linewidth = 1) + # 螺旋
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y, color = t/pi), 
               size = 4) + # 螺旋上の点
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "Archimedes' spiral", 
         subtitle = parse(text = fnc_label), 
         color = expression(frac(theta, pi)), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  
  # sin曲線を作図
  sin_graph <- ggplot() + 
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y, color = t/pi), 
               linetype = "dotted") + # y軸の補助線
    geom_line(data = spiral_df, 
              mapping = aes(x = t, y = y, color = t/pi), 
              linewidth = 1) + # 曲線
    geom_point(data = point_df, 
               mapping = aes(x = t, y = y, color = t/pi), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # ラジアン目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(t_min, t_max), 
                ylim = c(-axis_size, axis_size)) + 
    theme(axis.text.x = element_text(size = 6)) + 
    labs(title = "sine curve", 
         color = expression(frac(theta, pi)), 
         x = expression(theta), 
         y = expression(r ~ sin~theta))
  
  
  # cos曲線を作図
  cos_graph <- ggplot() + 
    geom_vline(data = point_df, 
               mapping = aes(xintercept = x, color = t/pi), 
               linetype = "dotted") + # x軸の補助線
    geom_path(data = spiral_df, 
              mapping = aes(x = x, y = t, color = t/pi), 
              linewidth = 1) + # 曲線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = t, color = t/pi), 
               size = 4) + # 曲線上の点
    scale_y_reverse(breaks = rad_break_vec,
                    labels = parse(text = rad_label_vec)) + # ラジアン目盛ラベル
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(t_min, t_max)) + 
    theme(axis.text.y = element_text(size = 6)) + 
    labs(title = "cosine curve", 
         color = expression(frac(theta, pi)), 
         x = expression(r ~ cos~theta), 
         y = expression(theta))
  
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    spiral_graph, sin_graph, 
    cos_graph, 
    nrow = 2, ncol = 2, 
    guides = "collect"
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 1600, height = 1600, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読み込み
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "spiral/figure/archimedes/curves_param.gif", delay = 1/10) -> tmp_path # gifファイル書き出し


