
# sin関数の振幅の可視化 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 曲線の作図 -------------------------------------------------------------------

# 振幅パラメータを指定
A <- 2

# 変数(ラジアン)の範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)


# 曲線の座標を作成
curve_df <- tibble::tibble(
  theta = theta_vec, 
  sin_t = sin(theta), 
  f_t   = A * sin(theta)
)


# 範囲πにおける目盛数を指定
tick_num <- 1

# 目盛ラベルの範囲を設定:(π単位で切り捨て・切り上げ)
theta_lower <- floor(min(theta_vec) / pi) * pi
theta_upper <- ceiling(max(theta_vec) / pi) * pi

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = theta_lower, to = theta_upper, by = pi/tick_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# グラフサイズを指定
axis_size <- abs(A) + 1

# ラベル用の文字列を作成
param_label <- paste0("A == ", A)

# sin関数曲線を作図
ggplot() + 
  geom_line(data = curve_df, 
            mapping = aes(x = theta, y = sin_t, linetype = "sin"), 
            linewidth = 1) + # 元の曲線
  geom_line(data = curve_df, 
            mapping = aes(x = theta, y = f_t, linetype = "f"), 
            linewidth = 1) + # 変形した曲線
  scale_linetype_manual(breaks = c("f", "sin"), 
                        values = c("solid", "dotted"), 
                        labels = c(expression(A ~ sin~theta), expression(sin~theta)), 
                        name = "function") + # 凡例表示用
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 1) + # 図の体裁
  coord_fixed(ratio = 1, 
              ylim = c(-axis_size, axis_size)) + # 描画領域
  labs(title = "sine function: amplitude", 
       subtitle = parse(text = param_label), 
       x = expression(theta), 
       y = expression(f(theta)))


# パラメータと曲線の形状の関係 ------------------------------------------------------------------

# フレーム数を指定
frame_num <- 101

# 振幅パラメータの範囲を指定
A_vals <- seq(from = -4, to = 4, length.out = frame_num)

# ラジアンの範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)


# 曲線の座標を作成
anim_curve_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  theta   = theta_vec
) |> # フレームごとにラジアンを複製
  dplyr::mutate(
    A     = A_vals[frame_i], 
    sin_t = sin(theta), 
    f_t   = A * sin(theta)
  )

# 振幅の範囲の座標を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  A       = A_vals, 
  x       = 0, 
  y_from  = abs(A), 
  y_to    = -y_from, 
  y_med   = 0, 
  param_label = paste0("A == ", round(A, digits = 2))
)


# 範囲πにおける目盛数を指定
tick_num <- 1

# 目盛ラベルの範囲を設定:(π単位で切り捨て・切り上げ)
theta_lower <- floor(min(theta_vec) / pi) * pi
theta_upper <- ceiling(max(theta_vec) / pi) * pi

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = theta_lower, to = theta_upper, by = pi/tick_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# グラフサイズを指定
axis_size <- max(abs(A_vals))

# sin関数曲線を作図
anim <- ggplot() + 
  geom_line(data = anim_curve_df, 
            mapping = aes(x = theta, y = sin_t, linetype = "sin"), 
            linewidth = 1) + # 元の曲線
  geom_line(data = anim_curve_df, 
            mapping = aes(x = theta, y = f_t, linetype = "f"), 
            linewidth = 1) + # 変形した曲線
  geom_segment(data = anim_label_df, 
               mapping = aes(x = x, y = -1, xend = x, yend = 1), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "both"), 
               color = "blue", linewidth = 1) + # 元の曲線の振幅の範囲
  geom_segment(data = anim_label_df, 
               mapping = aes(x = x, y = y_from, xend = x, yend = y_to), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "both"), 
               color = "red", linewidth = 0.5) + # 変形した曲線の振幅の範囲
  geom_text(data = anim_label_df, 
            mapping = aes(x = x, y = 0), 
            label = "2", parse = TRUE, hjust = 1.5, 
            color = "blue", size = 6) + # 元の範囲ラベル
  geom_text(data = anim_label_df, 
            mapping = aes(x = x, y = y_med), 
            label = "2 * '|'*A*'|'", parse = TRUE, hjust = -0.2, 
            color = "red", size = 6) + # 変形した範囲ラベル
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = param_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # パラメータラベル:(subtitleの代用)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_linetype_manual(breaks = c("f", "sin"), 
                        values = c("solid", "dotted"), 
                        labels = c(expression(A ~ sin~theta), expression(sin~theta)), 
                        name = "function") + # 凡例表示用
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + 
  theme(legend.text.align = 1) + 
  coord_fixed(ratio = 1, clip = "off", 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "sine function: amplitude", 
       subtitle = "", # ラベル表示用の空行
       x = expression(theta), 
       y = expression(f(theta)))

# gif画像を作成
gganimate::animate(plot = anim, nframes = frame_num, fps = 10, width = 800, height = 600)


# 変数と円周と曲線の関係 ----------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# 振幅パラメータを指定
A <- 2

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -4*pi, to = 4*pi, length.out = frame_num+1)[1:frame_num]
theta_min  <- min(theta_vals)

# 曲線用のラジアンのサイズを指定
theta_size <- 2 * pi


# 範囲πにおける目盛数を指定
tick_num <- 6

# 角度軸線の座標を作成
rad_tick_df <- tibble::tibble(
  i = 0:(2*tick_num-1), # 目盛位置番号
  t = i/tick_num * pi, # ラジアン
  r = max(1, abs(A)), # 半径
  x = r * cos(t), 
  y = r * sin(t)
)


# 円周の座標を作成
circle_df <- tidyr::expand_grid(
  tibble::tibble(
    fnc = c("sin", "f"), 
    r   = c(1, abs(A)), # 半径
  ), 
  t = seq(from = 0, to = 2*pi, length.out = 361) # 1周期分のラジアン
) |> # 円周ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t)
  )


# グラフサイズを指定
axis_size <- abs(A) + 0.5

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 変数を取得
  theta <- theta_vals[i]
  
  # 円周・曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    fnc   = c("sin", "f"), 
    theta = c(theta, theta), 
    A     = c(1, A), 
    sin_t = A * sin(theta), 
    cos_t = A * cos(theta)
  )
  
  ## 単位円と関数の関係の作図処理
  
  # 半径線の終点の座標を作成
  radius_df <- dplyr::bind_rows(
    # x軸線上の線分
    tibble::tibble(
      fnc = "r", 
      x   = max(1, abs(A)), 
      y   = 0
    ), 
    # 円周上の点との線分
    fnc_point_df |> 
      dplyr::select(fnc, x = cos_t, y = sin_t)
  )
  
  # 角マークの座標を作成
  d  <- 0.2
  ds <- 0.005
  angle_mark_df <- tibble::tibble(
    t = seq(from = 0, to = theta, length.out = 600), 
    x = (d + ds*t) * cos(t), 
    y = (d + ds*t) * sin(t)
  )
  
  # 角ラベルの座標を作成
  d <- 0.35
  angle_label_df <- tibble::tibble(
    t = 0.5 * theta, 
    x = d * cos(t), 
    y = d * sin(t), 
    angle_label = "theta"
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("sin", "f"), 
    x_from = c(cos(theta), A*cos(theta)), 
    y_from = c(0, 0), 
    x_to   = x_from, 
    y_to   = c(sin(theta), A*sin(theta))
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("sin", "f"), 
    x   = c(cos(theta), A*cos(theta)), 
    y   = 0.5 * c(sin(theta), A*sin(theta)), 
    a   = 90, 
    h   = 0.5, 
    v   = 1, 
    fnc_label = c("sin~theta", "A ~ sin~theta")
  )
  
  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "cos~theta == ", round(cos(theta), digits = 2), ", ", 
    "sin~theta == ", round(sin(theta), digits = 2), 
    ")"
  )
  
  # 円周上の点を作図
  circle_graph <- ggplot() + 
    geom_segment(data = rad_tick_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y), 
                 color = "white") + # 角度軸目盛線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y, group = fnc), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linetype = fnc)) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label), 
              parse = TRUE) + # 角ラベル
    geom_segment(data = fnc_line_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = fnc), 
                 linewidth = 1) + # 関数直線
    geom_text(data = fnc_label_df, 
              mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                            angle = a, hjust = h, vjust = v), 
              parse = TRUE) + # 関数ラベル
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = -Inf, y = sin_t, xend = cos_t, yend = sin_t, color = fnc), 
                 linetype = "dotted") + # y軸の補助線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = cos_t, y = sin_t), 
               size = 4) + # 関数点
    scale_color_manual(breaks = c("f", "sin"), 
                       values = c("red", "blue")) + # 関数ごとに色分け
    scale_linetype_manual(breaks = c("r", "sin", "f"), 
                          values = c("solid", "solid", "dashed")) + # 関数ごとに線分け
    guides(color = "none", linetype = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = fnc_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## 関数曲線の作図処理
  
  # 曲線用のラジアンを作成
  theta_vec <- seq(from = max(theta_min, theta-theta_size), to = theta, length.out = 1000)
  
  # ラジアン軸目盛用の値を作成
  rad_break_vec <- seq(
    from = floor((theta-theta_size) / pi) * pi, 
    to   = ceiling(theta / pi) * pi, 
    by   = pi/tick_num
  )
  rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")
  
  # 曲線の座標を作成
  fnc_curve_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc = c("sin", "f"), 
      A   = c(1, A)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      sin_t = A * sin(theta)
    )
  
  # 関数ラベルを作成
  param_label <- paste0(
    "list(", 
    "A == ", round(A, digits = 2), ", ", 
    "theta == ", round(theta/pi, digits = 2), " * pi", 
    ")"
  )
  fnc_label_vec <- paste(
    c("A ~ sin~theta", "sin~theta"), 
    c(A*sin(theta), sin(theta)) |> 
      round(digits = 2), 
    sep = " == "
  )
  
  # 曲線上の点を作図
  curve_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = theta, y = sin_t, color = fnc), 
              linewidth = 1) + # 関数曲線
    geom_vline(xintercept = theta,
               linetype = "dotted") + # ラジアン軸の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = Inf, y = sin_t, xend = theta, yend = sin_t, color = fnc), 
                 linetype = "dotted") + # y軸の補助線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = theta, y = sin_t), 
               size = 4) + # 曲線上の点
    scale_color_manual(breaks = c("f", "sin"), 
                       values = c("red", "blue"), 
                       labels = parse(text = fnc_label_vec), 
                       name = "function") + # 関数ごとに色分け
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec), 
                       minor_breaks = FALSE) + # ラジアン軸目盛
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(theta-theta_size, theta), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "sine curve: amplitude", 
         subtitle = parse(text = param_label), 
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## グラフの書出処理
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_graph, circle_graph
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1000, height = 500, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, "/", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/sine/amplitude_curves_variable.gif", delay = 1/20) -> tmp_path # gifファイルを書出


# パラメータと円周と曲線の関係 ----------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 101

# 振幅パラメータの範囲を指定
A_vals <- seq(from = -4, to = 4, length.out = frame_num)

# 点用のラジアンを指定
theta <- 1/3 * pi

# 曲線用のラジアンの範囲を指定
theta_vec <- seq(from = 0, to = 2*pi, length.out = 1000)


# 範囲πにおける目盛数を指定
tick_num <- 6

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(
  from = floor(min(theta_vec) / pi) * pi, 
  to   = ceiling(max(theta_vec) / pi) * pi, 
  by   = pi/tick_num
)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# グラフサイズを指定
axis_size <- max(abs(A_vals))

# パラメータごとに作図
for(i in 1:frame_num) {
  
  # パラメータを取得
  A <- A_vals[i]
  
  # 円周・曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    fnc   = c("sin", "f"), 
    theta = c(theta, theta), 
    A     = c(1, A), 
    sin_t = A * sin(theta), 
    cos_t = A * cos(theta)
  )
  
  ## 単位円と関数の関係の作図処理
  
  # 円周の座標を作成
  circle_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc = c("sin", "f"), 
      r   = c(1, abs(A)), # 半径
    ), 
    t = seq(from = 0, to = 2*pi, length.out = 361) # 1周期分のラジアン
  ) |> # 円周ごとにラジアンを複製
    dplyr::mutate(
      x = r * cos(t), 
      y = r * sin(t)
    )
  
  # 半径線の終点の座標を作成
  radius_df <- dplyr::bind_rows(
    # x軸線上の線分
    tibble::tibble(
      fnc = "r", 
      x   = max(1, abs(A)), 
      y   = 0
    ), 
    # 円周上の点との線分
    fnc_point_df |> 
      dplyr::select(fnc, x = cos_t, y = sin_t)
  )
  
  # 角マークの座標を作成
  d  <- 0.2
  ds <- 0.005
  angle_mark_df <- tibble::tibble(
    t = seq(from = 0, to = theta, length.out = 600), 
    x = (d + ds*t) * cos(t), 
    y = (d + ds*t) * sin(t)
  )
  
  # 角ラベルの座標を作成
  d <- 0.3
  angle_label_df <- tibble::tibble(
    t = 0.5 * theta, 
    x = d * cos(t), 
    y = d * sin(t), 
    angle_label = "theta"
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("sin", "f"), 
    x_from = c(cos(theta), A*cos(theta)), 
    y_from = c(0, 0), 
    x_to   = x_from, 
    y_to   = c(sin(theta), A*sin(theta))
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("sin", "f"), 
    x   = c(cos(theta), A*cos(theta)), 
    y   = 0.5 * c(sin(theta), A*sin(theta)), 
    a   = 90, 
    h   = 0.5, 
    v   = 1, 
    fnc_label = c("sin~theta", "A ~ sin~theta")
  )
  
  # 角度軸線の座標を作成
  rad_tick_df <- tibble::tibble(
    i = 0:(2*tick_num-1), # 目盛位置番号
    t = i/tick_num * pi, # ラジアン
    r = max(1, abs(A)), # 半径
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "cos~theta == ", round(cos(theta), digits = 2), ", ", 
    "sin~theta == ", round(sin(theta), digits = 2), 
    ")"
  )
  
  # 円周上の点を作図
  circle_graph <- ggplot() + 
    geom_segment(data = rad_tick_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y), 
                 color = "white") + # 角度軸目盛線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y, group = r), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linetype = fnc)) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label), 
              parse = TRUE) + # 角ラベル
    geom_segment(data = fnc_line_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = fnc), 
                 linewidth = 1) + # 関数直線
    geom_text(data = fnc_label_df, 
              mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                            angle = a, hjust = h, vjust = v), 
              parse = TRUE) + # 関数ラベル
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = -Inf, y = sin_t, xend = cos_t, yend = sin_t, color = fnc), 
                 linetype = "dotted") + # y軸の補助線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = cos_t, y = sin_t), 
               size = 4) + # 関数点
    scale_color_manual(breaks = c("f", "sin"), 
                       values = c("red", "blue")) + # 関数ごとに色分け
    scale_linetype_manual(breaks = c("r", "sin", "f"), 
                          values = c("solid", "solid", "dashed")) + # 関数ごとに線分け
    guides(color = "none", linetype = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = fnc_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## 関数曲線の作図処理
  
  # 曲線の座標を作成
  fnc_curve_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc = c("sin", "f"), 
      A   = c(1, A)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      sin_t = A * sin(theta)
    )
  
  # 関数ラベルを作成
  param_label <- paste0(
    "list(", 
    "A == ", round(A, digits = 2), ", ", 
    "theta == ", round(theta/pi, digits = 2), " * pi", 
    ")"
  )
  fnc_label_vec <- paste(
    c("A ~ sin~theta", "sin~theta"), 
    c(A*sin(theta), sin(theta)) |> 
      round(digits = 2), 
    sep = " == "
  )
  
  # 曲線上の点を作図
  curve_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = theta, y = sin_t, color = fnc), 
              linewidth = 1) + # 関数曲線
    geom_vline(xintercept = theta,
               linetype = "dotted") + # ラジアン軸の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = Inf, y = sin_t, xend = theta, yend = sin_t, color = fnc), 
                 linetype = "dotted") + # y軸の補助線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = theta, y = sin_t), 
               size = 4) + # 曲線上の点
    scale_color_manual(breaks = c("f", "sin"), 
                       values = c("red", "blue"), 
                       labels = parse(text = fnc_label_vec), 
                       name = "function") + # 関数ごとに色分け
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec), 
                       minor_breaks = FALSE) + # ラジアン軸目盛
    coord_fixed(ratio = 1, 
                xlim = c(min(theta_vec), max(theta_vec)), 
                ylim = c(-axis_size, axis_size)) + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    labs(title = "sine curve: amplitude", 
         subtitle = parse(text = param_label), 
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## グラフの書出処理
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_graph, circle_graph
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1200, height = 800, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, "/", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/sine/amplitude_curves_param.gif", delay = 1/10) -> tmp_path # gifファイルを書出


