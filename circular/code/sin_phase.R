
# sin関数の位相の可視化 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 曲線の作図 -------------------------------------------------------------------

# 位相パラメータを指定
alpha <- 1/2 * pi

# 変数(ラジアン)の範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)


# 曲線の座標を作成
curve_df <- tibble::tibble(
  theta = theta_vec, 
  sin_t = sin(theta), 
  f_t   = sin(theta + alpha)
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
axis_size <- 2

# ラベル用の文字列を作成
param_label <- paste0(
  "alpha == ", round(alpha/pi, digits = 2), " * pi"
)

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
                        labels = c(expression(sin(theta + alpha)), expression(sin~theta)), 
                        name = "function") + # 凡例表示用
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁
  coord_fixed(ratio = 1, 
              ylim = c(-axis_size, axis_size)) + # 描画領域
  labs(title = "sine function: phase", 
       subtitle = parse(text = param_label), 
       x = expression(theta), 
       y = expression(f(theta)))


# パラメータと曲線の関係 ------------------------------------------------------------------

# フレーム数を指定
frame_num <- 100

# 位相パラメータの範囲を指定
alpha_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# ラジアンの範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)


# 曲線の座標を作成
anim_curve_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  theta   = theta_vec
) |> # フレームごとにラジアンを複製
  dplyr::mutate(
    alpha = alpha_vals[frame_i], 
    sin_t = sin(theta), 
    f_t   = sin(theta + alpha)
  )

# 位相の変化の範囲の座標を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  alpha   = alpha_vals, 
  x_from  = 0, 
  x_to    = -alpha, 
  x_med   = 0.5 * x_to, 
  y       = 0, 
  param_label = paste0("alpha == ", round(alpha/pi, digits = 2), " * pi")
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
axis_size <- 2

# sin関数曲線を作図
anim <- ggplot() + 
  geom_line(data = anim_curve_df, 
            mapping = aes(x = theta, y = sin_t, linetype = "sin"), 
            linewidth = 1) + # 元の曲線
  geom_line(data = anim_curve_df, 
            mapping = aes(x = theta, y = f_t, linetype = "f"), 
            linewidth = 1) + # 変形した曲線
  geom_segment(data = anim_label_df, 
               mapping = aes(x = x_from, y = 0, xend = x_to, yend = 0), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "both"), 
               color = "red", linewidth = 0.5) + # 位相の変化の範囲
  geom_text(data = anim_label_df, 
            mapping = aes(x = x_med, y = y), 
            label = "alpha", parse = TRUE, color = "red", vjust = -0.5, size = 6) + # 変形した範囲ラベル
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = param_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # パラメータラベル:(subtitleの代用)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_linetype_manual(breaks = c("f", "sin"), 
                        values = c("solid", "dotted"), 
                        labels = c(expression(sin(theta + alpha)), expression(sin~theta)), 
                        name = "function") + # 凡例表示用
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + 
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, clip = "off", 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "sine function: phase", 
       subtitle = "", # ラベル表示用の空行
       x = expression(theta), 
       y = expression(f(theta)))

# gif画像を作成
gganimate::animate(plot = anim, nframes = frame_num, fps = 10, width = 800, height = 400)


# 変数と円周と曲線の関係 ----------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# 位相パラメータを指定
alpha <- 1/2 * pi

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
  r = 1, # 半径
  x = r * cos(t), 
  y = r * sin(t)
)


# 単位円の座標を作成
unit_circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # 1周期分のラジアン
  x = cos(t), 
  y = sin(t)
)


# グラフサイズを指定
axis_size <- 2.5

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 変数を取得
  theta <- theta_vals[i]
  
  # 円周・曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    fnc   = c("sin", "f"), 
    theta = c(theta, theta), 
    alpha = c(0, alpha), 
    sin_t = sin(theta + alpha), 
    cos_t = cos(theta + alpha)
  )
  
  ## 単位円と関数の関係の作図処理
  
  # 半径線の終点の座標を作成
  radius_df <- dplyr::bind_rows(
    # x軸線上の線分
    tibble::tibble(
      fnc = "r", 
      x   = 1, 
      y   = 0
    ), 
    # 円周上の点との線分
    fnc_point_df |> 
      dplyr::select(fnc, x = cos_t, y = sin_t)
  )
  
  # 角マークの座標を作成
  dt  <- 0.2
  da  <- 0.3
  dta <- 0.4
  ds  <- 0.005
  angle_mark_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "sin", 
      t   = seq(from = 0, to = theta, length.out = 600), 
      x   = (dt + ds*t) * cos(t), 
      y   = (dt + ds*t) * sin(t)
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = seq(from = theta, to = theta+alpha, length.out = 600), 
      x   = da * cos(t), 
      y   = da * sin(t)
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = seq(from = 0, to = theta+alpha, length.out = 600), 
      x   = (dta + ds*t) * cos(t), 
      y   = (dta + ds*t) * sin(t)
    )
  )
  
  # 角ラベルの座標を作成
  dt  <- 0.1
  da  <- 0.45
  dta <- 0.55
  angle_label_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "sin", 
      t   = 0.5 * theta, 
      x   = dt * cos(t), 
      y   = dt * sin(t), 
      angle_label = "theta"
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = theta + 0.5*alpha, 
      x   = da * cos(t), 
      y   = da * sin(t), 
      angle_label = "alpha"
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = 0.5 * (theta + alpha), 
      x   = dta * cos(t), 
      y   = dta * sin(t), 
      angle_label = "theta + alpha"
    )
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("sin", "f"), 
    x_from = c(cos(theta), cos(theta+alpha)), 
    y_from = c(0, 0), 
    x_to   = x_from, 
    y_to   = c(sin(theta), sin(theta+alpha))
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("sin", "f"), 
    x   = c(cos(theta), cos(theta+alpha)), 
    y   = 0.5 * c(sin(theta), sin(theta+alpha)), 
    a   = 90, 
    h   = 0.5, 
    v   = 1, 
    fnc_label = c("sin~theta", "sin(theta + alpha)")
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
    geom_path(data = unit_circle_df, 
              mapping = aes(x = x, y = y), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linetype = fnc)) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y, color = fnc)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label, color = fnc), 
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
    scale_color_manual(breaks = c("f", "sin", "alpha"), 
                       values = c("red", "blue", "purple")) + # 関数ごとに色分け
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
  theta_max <- max(theta, theta+alpha)
  theta_vec <- seq(from = max(theta_min, theta_max-theta_size), to = theta_max, length.out = 1000)
  
  # ラジアン軸目盛用の値を作成
  rad_break_vec <- seq(
    from = floor((theta_max-theta_size) / pi) * pi, 
    to   = ceiling(theta_max / pi) * pi, 
    by   = pi/tick_num
  )
  rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")
  
  # 曲線の座標を作成
  fnc_curve_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc   = c("sin", "f"), 
      alpha = c(0, alpha)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      sin_t = sin(theta + alpha)
    )
  
  # 位相の変化の範囲の座標を作成
  phase_df <- tibble::tibble(
    x     = theta + alpha, 
    y     = sin(theta + alpha), 
    x_to  = theta, 
    x_med = theta + 0.5*alpha, 
    angle_lable = "alpha"
  )
  
  # 関数ラベルを作成
  param_label <- paste0(
    "list(", 
    "alpha == ", round(alpha/pi, digits = 2), " * pi, ", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "theta + alpha == ", round((theta + alpha)/pi, digits = 2), " * pi", 
    ")"
  )
  fnc_label_vec <- paste(
    c("sin(theta + alpha)", "sin~theta"), 
    c(sin(theta + alpha), sin(theta)) |> 
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
    geom_segment(data = phase_df, 
                 mapping = aes(x = x, y = y, xend = x_to, yend = y), 
                 color ="purple") + # 位相の変化の範囲
    geom_text(data = phase_df, 
              mapping = aes(x = x_med, y = y, label = angle_lable), 
              parse = TRUE, vjust = -0.5, color = "purple") + # 位相パラメータラベル
    geom_point(data = phase_df, 
               mapping = aes(x = x, y = y), 
               size = 4, shape = "circle open") + # 位相の変化の点
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
                xlim = c(theta_max-theta_size, theta_max), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "sine curve: phase", 
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
  magick::image_write_gif(path = "circular/figure/sine/phase_curves_variable.gif", delay = 1/20) -> tmp_path # gifファイルを書出


# パラメータと円周と曲線の関係 ----------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 100

# 位相パラメータの範囲を指定
alpha_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# 点用のラジアンを指定
theta <- 1/3 * pi

# 曲線用のラジアンの範囲を指定
theta_vec <- seq(from = 0, to = 2*pi, length.out = 1000)


# 範囲πにおける目盛数を指定
tick_num <- 6

# 角度軸線の座標を作成
rad_tick_df <- tibble::tibble(
  i = 0:(2*tick_num-1), # 目盛位置番号
  t = i/tick_num * pi, # ラジアン
  r = 1, # 半径
  x = r * cos(t), 
  y = r * sin(t)
)

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(
  from = floor(min(theta_vec) / pi) * pi, 
  to   = ceiling(max(theta_vec) / pi) * pi, 
  by   = pi/tick_num
)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# 単位円の座標を作成
unit_circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # 1周期分のラジアン
  x = cos(t), 
  y = sin(t)
)


# グラフサイズを指定
axis_size <- 1.5

# パラメータごとに作図
for(i in 1:frame_num) {
  
  # パラメータを取得
  alpha <- alpha_vals[i]
  
  # 円周・曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    fnc   = c("sin", "f"), 
    theta = c(theta, theta), 
    alpha = c(0, alpha), 
    sin_t = sin(theta + alpha), 
    cos_t = cos(theta + alpha)
  )
  
  ## 単位円と関数の関係の作図処理
  
  # 半径線の終点の座標を作成
  radius_df <- dplyr::bind_rows(
    # x軸線上の線分
    tibble::tibble(
      fnc = "r", 
      x   = 1, 
      y   = 0
    ), 
    # 円周上の点との線分
    fnc_point_df |> 
      dplyr::select(fnc, x = cos_t, y = sin_t)
  )
  
  # 角マークの座標を作成
  dt  <- 0.2
  da  <- 0.3
  dta <- 0.4
  ds  <- 0.005
  angle_mark_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "sin", 
      t   = seq(from = 0, to = theta, length.out = 600), 
      x   = (dt + ds*t) * cos(t), 
      y   = (dt + ds*t) * sin(t)
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = seq(from = theta, to = theta+alpha, length.out = 600), 
      x   = da * cos(t), 
      y   = da * sin(t)
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = seq(from = 0, to = theta+alpha, length.out = 600), 
      x   = (dta + ds*t) * cos(t), 
      y   = (dta + ds*t) * sin(t)
    )
  )
  
  # 角ラベルの座標を作成
  dt  <- 0.1
  da  <- 0.45
  dta <- 0.55
  angle_label_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "sin", 
      t   = 0.5 * theta, 
      x   = dt * cos(t), 
      y   = dt * sin(t), 
      angle_label = "theta"
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = theta + 0.5*alpha, 
      x   = da * cos(t), 
      y   = da * sin(t), 
      angle_label = "alpha"
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = 0.5 * (theta + alpha), 
      x   = dta * cos(t), 
      y   = dta * sin(t), 
      angle_label = "theta + alpha"
    )
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("sin", "f"), 
    x_from = c(cos(theta), cos(theta+alpha)), 
    y_from = c(0, 0), 
    x_to   = x_from, 
    y_to   = c(sin(theta), sin(theta+alpha))
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("sin", "f"), 
    x   = c(cos(theta), cos(theta+alpha)), 
    y   = 0.5 * c(sin(theta), sin(theta+alpha)), 
    a   = 90, 
    h   = 0.5, 
    v   = 1, 
    fnc_label = c("sin~theta", "sin(theta + alpha)")
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
    geom_path(data = unit_circle_df, 
              mapping = aes(x = x, y = y), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linetype = fnc)) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y, color = fnc)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label, color = fnc), 
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
    scale_color_manual(breaks = c("f", "sin", "alpha"), 
                       values = c("red", "blue", "purple")) + # 関数ごとに色分け
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
      fnc   = c("sin", "f"), 
      alpha = c(0, alpha)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      sin_t = sin(theta + alpha)
    )
  
  # 位相の変化の範囲の座標を作成
  phase_df <- tibble::tibble(
    x     = theta + alpha, 
    y     = sin(theta + alpha), 
    x_to  = theta, 
    x_med = theta + 0.5*alpha, 
    angle_lable = "alpha"
  )
  
  # 関数ラベルを作成
  param_label <- paste0(
    "list(", 
    "alpha == ", round(alpha/pi, digits = 2), " * pi, ", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "theta + alpha == ", round((theta + alpha)/pi, digits = 2), " * pi", 
    ")"
  )
  fnc_label_vec <- paste(
    c("tan(theta + alpha)", "tan~theta"), 
    c(tan(theta + alpha), tan(theta)) |> 
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
    geom_segment(data = phase_df,
                 mapping = aes(x = x, y = y, xend = x_to, yend = y),
                 color ="purple") + # 位相の変化の範囲
    geom_text(data = phase_df,
              mapping = aes(x = x_med, y = y, label = angle_lable),
              parse = TRUE, vjust = -0.5, color = "purple") + # 位相パラメータラベル
    geom_point(data = phase_df,
               mapping = aes(x = x, y = y),
               size = 4, shape = "circle open") + # 位相の変化の点
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
                xlim = c(min(theta_vec), max(theta_vec)), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "sine curve: phase", 
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
  magick::image_write_gif(path = "circular/figure/sine/phase_curves_param.gif", delay = 1/10) -> tmp_path # gifファイルを書出


