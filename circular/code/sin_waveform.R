
# sin関数の波形の可視化 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 曲線の作図 -------------------------------------------------------------------

# パラメータを指定
A     <- 4
a     <- 2
alpha <- 1/2 * pi
C     <- 1

# 変数(ラジアン)の範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)

# 曲線の座標を作成
curve_df <- tibble::tibble(
  theta = theta_vec, 
  sin_t = sin(theta), 
  f_t   = A * sin(a * theta + alpha) + C
)


# 範囲πにおける目盛数を指定
tick_num <- 1

# 目盛ラベルの範囲を設定:(π単位で切り捨て・切り上げ)
theta_lower <- floor(min(theta_vec) / pi) * pi
theta_upper <- ceiling(max(theta_vec) / pi) * pi

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = theta_lower, to = theta_upper, by = pi/tick_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# ラベル用の文字列を作成
param_label <- paste0(
  "list(", 
  "A == ", A, ", ", 
  "a == ", a, ", ", 
  "alpha == ", round(alpha/pi, digits = 2), " * pi", ", ", 
  "C == ", C, 
  ")"
)

# sin関数曲線を作図
ggplot() + 
  geom_line(data = curve_df, 
            mapping = aes(x = theta, y = sin_t, linetype = "sin"), 
            linewidth = 1) + # 元の曲線
  geom_line(data = curve_df, 
            mapping = aes(x = theta, y = f_t, linetype = "f"), 
            linewidth = 1) + # 変形した曲線
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  scale_linetype_manual(breaks = c("f", "sin"), 
                        values = c("solid", "dotted"), 
                        labels = c(expression(A~sin*(a*theta + alpha) + C), expression(sin~theta)), 
                        name = "function") + # 凡例表示用
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁
  coord_fixed(ratio = 1) + # 描画領域
  labs(title = "sine function: waveform", 
       subtitle = parse(text = param_label), 
       x = expression(theta), 
       y = expression(f(theta)))


# パラメータと曲線の形状の関係 ------------------------------------------------------------------

# フレーム数を指定
frame_num <- 101

# パラメータの範囲を指定
A_vals     <- seq(from = -1, to = 2, length.out = frame_num)
a_vals     <- seq(from = -2, to = 4, length.out = frame_num)
alpha_vals <- seq(from = -1*pi, to = 3*pi, length.out = frame_num)
C_vals     <- seq(from = 2, to = -4, length.out = frame_num)

# ラジアンの範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)


# 曲線の座標を作成
anim_curve_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  theta   = theta_vec
) |> # フレームごとにラジアンを複製
  dplyr::mutate(
    A     = A_vals[frame_i], 
    a     = a_vals[frame_i], 
    alpha = alpha_vals[frame_i], 
    C     = C_vals[frame_i], 
    sin_t = sin(theta), 
    f_t   = A * sin(a * theta + alpha) + C
  )

# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  A     = A_vals[frame_i], 
  a     = a_vals[frame_i], 
  alpha = alpha_vals[frame_i], 
  C     = C_vals[frame_i], 
  param_label = paste0(
    "list(", 
    "A == ", round(A, digits = 2), ", ", 
    "a == ", round(a, digits = 2), ", ", 
    "alpha == ", round(alpha/pi, digits = 2), " * pi, ", 
    "C == ", round(C, digits = 2), 
    ")"
  ), 
  theta = 0, 
  x     = theta, 
  y     = A * sin(a * theta + alpha) + C
)

# パラメータによる変化の範囲の座標を作成
anim_param_line_df <- dplyr::bind_rows(
  # 振幅パラメータ
  tibble::tibble(
    frame_i = 1:frame_num, 
    x_from  = 0, 
    y_from  = -abs(A_vals) + C_vals, 
    x_to    = x_from, 
    y_to    = -y_from, 
    param_name = "amplitude"
  ), 
  # 周期パラメータ
  tibble::tibble(
    frame_i = 1:frame_num, 
    x_from  = -alpha_vals / a_vals, 
    y_from  = C_vals, 
    x_to    = (2*pi - alpha_vals) / a_vals, 
    y_to    = y_from, 
    param_name = "period"
  ), 
  # 位相パラメータ
  tibble::tibble(
    frame_i = 1:frame_num, 
    x_from  = 0, 
    y_from  = C_vals, 
    x_to    = -alpha_vals / a_vals, 
    y_to    = y_from, 
    param_name = "phase"
  ), 
  # 切片パラメータ
  tibble::tibble(
    frame_i = 1:frame_num, 
    x_from  = -alpha_vals / a_vals, 
    y_from  = 0, 
    x_to    = x_from, 
    y_to    = C_vals, 
    param_name = "intercept"
  )
) |> 
  dplyr::mutate(
    param_name = factor(param_name, levels = c("amplitude", "period", "phase", "intercept")) # 描画順を指定
  )

# パラメータラベルの座標を作成
anim_param_label_df <- anim_param_line_df |> 
  dplyr::summarise(
    # 中点の座標を計算
    x = mean(c(x_from, x_to)), 
    y = mean(c(y_from, y_to)), 
    .by = c(frame_i, param_name)
  ) |> 
  dplyr::mutate(
    # ラベル用の値を指定
    param_label = dplyr::case_match(
      .x = param_name, 
      "amplitude" ~ "A", 
      "period"    ~ "frac(2*pi, a)", 
      "phase"     ~ "frac(alpha, a)", 
      "intercept" ~ "C"
    ), 
    h = dplyr::case_match(
      .x = param_name, 
      "amplitude" ~ -0.5, 
      "period"    ~ 0.5, 
      "phase"     ~ 0.5, 
      "intercept" ~ 1.5
    ), 
    v = dplyr::case_match(
      .x = param_name, 
      "amplitude" ~ 0.5, 
      "period"    ~ 1.2, 
      "phase"     ~ -0.2, 
      "intercept" ~ 0.5
    )
  )

# 原点の変化の座標を作成
anim_origin_df <- anim_param_line_df |> 
  dplyr::filter(param_name == "phase") |> # 原点の変化を抽出
  dplyr::select(frame_i, x_to, y_to)


# 範囲πにおける目盛数を指定
tick_num <- 1

# 目盛ラベルの範囲を設定:(π単位で切り捨て・切り上げ)
theta_lower <- floor(min(theta_vec) / pi) * pi
theta_upper <- ceiling(max(theta_vec) / pi) * pi

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = theta_lower, to = theta_upper, by = pi/tick_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# sin関数曲線を作図
anim <- ggplot() + 
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)),
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_line(data = anim_curve_df,
            mapping = aes(x = theta, y = sin_t, linetype = "sin"),
            linewidth = 1) + # 元の曲線
  geom_line(data = anim_curve_df,
            mapping = aes(x = theta, y = f_t, linetype = "f"),
            linewidth = 1) + # 変形した曲線
  geom_segment(data = anim_origin_df,
               mapping = aes(x = 0, y = 0, xend = x_to, yend = y_to),
               arrow = arrow(length = unit(10, units = "pt"), ends = "last"),
               linewidth = 0.5) + # 変化した原点
  geom_segment(data = anim_param_line_df,
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = param_name),
               arrow = arrow(length = unit(10, units = "pt"), ends = "both"),
               linewidth = 0.5) + # パラメータによる変化の範囲
  geom_text(data = anim_param_label_df,
            mapping = aes(x = x, y = y, label = param_label, color = param_name, hjust = h, vjust = v),
            parse = TRUE, size = 5, show.legend = FALSE) + # パラメータラベル
  geom_text(data = anim_label_df,
            mapping = aes(x = -Inf, y = Inf, label = param_label),
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  scale_linetype_manual(breaks = c("f", "sin"), 
                        values = c("solid", "dotted"), 
                        labels = c(expression(A~sin(a*theta + alpha) + C), expression(sin~theta)), 
                        name = "function") + # 凡例表示用
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + 
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(min(theta_vec), max(theta_vec))) + 
  labs(title = "sine function", 
       subtitle = "", # ラベル表示用の空行
       color = "parameter", 
       x = expression(theta), 
       y = expression(f(theta)))

# gif画像を作成
gganimate::animate(plot = anim, nframes = frame_num, fps = 10, width = 800, height = 600)


# 変数と円周と曲線の関係 ----------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# パラメータを指定
A     <- 2
a     <- 2
alpha <- -1/2 * pi
C     <- -3

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]
theta_min  <- min(theta_vals)

# 曲線用のラジアンのサイズを指定
theta_size <- 2 * pi


# 範囲πにおける目盛数を指定
tick_num <- 6

# 角度軸線の座標を作成
rad_tick_df <- tidyr::expand_grid(
  tibble::tibble(
    fnc = c("sin", "f"), 
    r   = c(1, abs(A)), # 半径
    y0  = c(0, C), # 中心のy座標
  ), 
  i = 0:(2*tick_num-1) # 目盛位置番号
) |> # 関数ごとにラジアンを複製
  dplyr::mutate(
    t = i/tick_num * pi, # ラジアン
    x = r * cos(t), 
    y = r * sin(t) + y0, 
    t_label = paste0("frac(", i, ", ", tick_num, ") ~ pi") # 角度ラベル
  )


# 円周の座標を作成
circle_df <- tidyr::expand_grid(
  tibble::tibble(
    fnc = c("sin", "f"), 
    r   = c(1, abs(A)), # 半径
    y0  = c(0, C) # 中心のy座標
  ), 
  t = seq(from = 0, to = 2*pi, length.out = 361) # 1周期分のラジアン
) |> # 円周ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t) + y0
  )


# グラフサイズを指定
axis_x_size <- abs(A) + 0.5
axis_y_min  <- min(-1, -abs(A)+C) - 0.5
axis_y_max  <- max(1, abs(A)+C) + 0.5

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 変数を取得
  theta <- theta_vals[i]
  
  # 円周・曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    fnc   = c("sin", "f", "g"), 
    theta = c(theta, theta, theta), 
    A     = c(1, A, A), 
    a     = c(1, a, a), 
    alpha = c(0, alpha, 0), 
    C     = c(0, C, C), 
    sin_t = A * sin(a * theta + alpha) + C, 
    cos_t = A * cos(a * theta + alpha)
  )
  
  ## 単位円と関数の関係の作図処理
  
  # 半径線の終点の座標を作成
  radius_df <- dplyr::bind_rows(
    # x軸線上の線分
    tibble::tibble(
      fnc = c("r", "r"), 
      y0  = c(0, C), 
      x   = c(1, abs(A)), 
      y   = y0, 
      line_type = c("main", "main")
    ), 
    # 振幅パラメータが負の場合用
    tibble::tibble(
      fnc   = c("f", "g"), 
      alpha = c(alpha, 0), 
      y0    = c(C, C), 
      x     = abs(A) * cos(a * theta + alpha), 
      y     = abs(A) * sin(a * theta + alpha) + C, 
      line_type = c("main", "sub")
    ), 
    # 円周上の点との線分
    fnc_point_df |> 
      dplyr::select(fnc, y0 = C, x = cos_t, y = sin_t) |> 
      dplyr::mutate(
        line_type = dplyr::if_else(fnc == "sin", true = "main", false = "rev")
      )
  )
  
  # 角マークの座標を作成
  d1 <- 0.15
  d2 <- 0.3
  d3 <- 0.2
  d4 <- 0.25
  angle_mark_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "sin", 
      t   = seq(from = 0, to = theta, length.out = 300), 
      x   = d1 * cos(t), 
      y   = d1 * sin(t)
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = seq(from = 0, to = a*theta+alpha, length.out = 300), 
      x   = d2 * cos(t), 
      y   = d2 * sin(t) + C
    ), 
    tibble::tibble(
      fnc = "g", 
      t   = seq(from = 0, to = a*theta, length.out = 300), 
      x   = d3 * cos(t), 
      y   = d3 * sin(t) + C
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = seq(from = a*theta, to = a*theta+alpha, length.out = 300), 
      x   = d4 * cos(t), 
      y   = d4 * sin(t) + C
    )
  )
  
  # 角ラベルの座標を作成
  d1 <- 0.25
  d2 <- 0.5
  d3 <- 0.1
  d4 <- 0.35
  angle_label_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "sin", 
      t   = 0.5 * theta, 
      x   = d1 * cos(t), 
      y   = d1 * sin(t), 
      angle_label = "theta"
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = 0.5 * (a*theta + alpha), 
      x   = d2 * cos(t), 
      y   = d2 * sin(t) + C, 
      angle_label = "a * theta + alpha"
    ), 
    tibble::tibble(
      fnc = "g", 
      t   = 0.5 * a*theta, 
      x   = d3 * cos(t), 
      y   = d3 * sin(t) + C, 
      angle_label = "a * theta"
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = a*theta + 0.5*alpha, 
      x   = d4 * cos(t), 
      y   = d4 * sin(t) + C, 
      angle_label = "alpha"
    )
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("sin", "f", "g"), 
    x_from = c(cos(theta), A*cos(a*theta + alpha), A*cos(a*theta)), 
    y_from = c(0, C, C), 
    x_to   = x_from, 
    y_to   = c(sin(theta), A*sin(a*theta + alpha) + C, A*sin(a*theta) + C), 
    line_type = c("main", "main", "sub")
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("sin", "f"), 
    x   = c(cos(theta), A*cos(a*theta + alpha)), 
    y   = c(0.5 * sin(theta), 0.5 * A*sin(a*theta + alpha) + C), 
    a   = 90, 
    h   = 0.5, 
    v   = 1, 
    fnc_label = c("sin~theta", "A ~ sin(a * theta + alpha)")
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
                 mapping = aes(x = 0, y = y0, xend = x, yend = y), 
                 color = "white") + # 角度軸目盛線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y, group = fnc), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = y0, xend = x, yend = y, linetype = line_type)) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y, color = fnc)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label, color = fnc), 
              parse = TRUE) + # 角ラベル
    geom_segment(data = fnc_line_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, linetype = line_type), 
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
    scale_color_manual(breaks = c("f", "g", "sin", "alpha"), 
                       values = c("red", "orange", "blue", "purple")) + # 関数ごとに色分け
    scale_linetype_manual(breaks = c("main", "sub", "rev"), 
                          values = c("solid", "twodash", "dashed"), 
                          guide = "none") + # 関数ごとに線分け
    guides(color = "none", linetype = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_x_size, axis_x_size), 
                ylim = c(axis_y_min, axis_y_max)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = fnc_label), 
         x = expression(x == x[0] + r ~ cos~theta), 
         y = expression(y == y[0] + r ~ sin~theta))
  
  ## 関数曲線の作図処理
  
  # 曲線用のラジアンを作成
  theta_max <- max(theta, theta+alpha/a)
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
      fnc   = c("sin", "f", "g"), 
      A     = c(1, A, A), 
      a     = c(1, a, a), 
      alpha = c(0, alpha, 0), 
      C     = c(0, C, C)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      sin_t = A * sin(a * theta + alpha) + C
    )
  
  # 位相の変化の範囲の座標を作成
  phase_df <- tibble::tibble(
    x     = theta + alpha/a, 
    y     = A * sin(a * theta + alpha) + C, 
    x_to  = theta, 
    x_med = theta + 0.5*alpha/a, 
    angle_lable = "frac(alpha, a)"
  )
  
  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "A == ", round(A, digits = 2), ", ", 
    "a == ", round(a, digits = 2), ", ", 
    "alpha == ", round(alpha/pi, digits = 2), " * pi, ", 
    "C == ", round(C, digits = 2), ", ", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "sin~theta == ", round(sin(theta), digits = 2), ", ", 
    "A ~ sin~theta == ", round(A*sin(theta), digits = 2), 
    ")"
  )
  
  # 曲線上の点を作図
  curve_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = theta, y = sin_t, color = fnc, linetype = fnc), 
              linewidth = 1) + # 関数曲線
    geom_vline(mapping = aes(xintercept = theta), 
                 linetype = "dotted") + # ラジアン軸の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = Inf, y = sin_t, xend = theta, yend = sin_t, color = fnc), 
                 linetype = "dotted") + # y軸の補助線
    geom_segment(data = phase_df,
                 mapping = aes(x = x, y = y, xend = x_to, yend = y),
                 color ="purple") + # 位相の変化の範囲
    geom_text(data = phase_df,
              mapping = aes(x = x_med, y = y, label = angle_lable),
              parse = TRUE, vjust = -0.2, color = "purple") + # 位相パラメータラベル
    geom_point(data = phase_df,
               mapping = aes(x = x, y = y),
               size = 4, shape = "circle open") + # 位相の変化の点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = theta, y = sin_t), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec), 
                       minor_breaks = FALSE) + # ラジアン軸目盛
    scale_color_manual(breaks = c("f", "g", "sin"), 
                       values = c("red", "orange", "blue"), 
                       labels = parse(text = c("A ~ sin(a * theta + alpha)", "A ~ sin(a * theta)", "sin~theta")), 
                       name = "function") + # 関数ごとに色分け
    scale_linetype_manual(breaks = c("sin", "f", "g"), 
                          values = c("solid", "solid", "dashed"), 
                          guide = "none") + # 関数ごとに線分け
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(theta_max-theta_size, theta_max), 
                ylim = c(axis_y_min, axis_y_max)) + 
    labs(title = "sine curve", 
         subtitle = parse(text = fnc_label), 
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## グラフの書出処理
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_graph, circle_graph, 
    guides = "collect"
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
  magick::image_write_gif(path = "circular/figure/sine/waveform_curves_variable.gif", delay = 1/30) -> tmp_path # gifファイル書出


# パラメータと円周と曲線の関係 ----------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 101

# パラメータの範囲を指定
A_vals     <- seq(from = -1, to = 2, length.out = frame_num)
a_vals     <- seq(from = -2, to = 4, length.out = frame_num)
alpha_vals <- seq(from = -1*pi, to = 3*pi, length.out = frame_num)
C_vals     <- seq(from = 2, to = -4, length.out = frame_num)

# 点用のラジアンを指定
theta <- 1/3 * pi

# 曲線用のラジアンの範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)


# 範囲πにおける目盛数を指定
tick_num <- 3

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(
  from = floor(min(theta_vec) / pi) * pi, 
  to   = ceiling(max(theta_vec) / pi) * pi, 
  by   = pi/tick_num
)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# グラフサイズを指定
axis_x_size <- max(abs(A_vals)) + 0.5
axis_y_min  <- min(-1, -abs(A_vals)+C_vals) - 0.5
axis_y_max  <- max(1, abs(A_vals)+C_vals) + 0.5

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # パラメータを取得
  A     <- A_vals[i]
  a     <- a_vals[i]
  alpha <- alpha_vals[i]
  C     <- C_vals[i]
  
  # 円周・曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    fnc   = c("sin", "f", "g"), 
    theta = c(theta, theta, theta), 
    A     = c(1, A, A), 
    a     = c(1, a, a), 
    alpha = c(0, alpha, 0), 
    C     = c(0, C, C), 
    sin_t = A * sin(a * theta + alpha) + C, 
    cos_t = A * cos(a * theta + alpha)
  )
  
  ## 単位円と関数の関係の作図処理
  
  # 円周の座標を作成
  circle_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc = c("sin", "f"), 
      r   = c(1, abs(A)), # 半径
      y0  = c(0, C) # 中心のy座標
    ), 
    t = seq(from = 0, to = 2*pi, length.out = 361) # 1周期分のラジアン
  ) |> # 円周ごとにラジアンを複製
    dplyr::mutate(
      x = r * cos(t), 
      y = r * sin(t) + y0
    )
  
  # 半径線の終点の座標を作成
  radius_df <- dplyr::bind_rows(
    # x軸線上の線分
    tibble::tibble(
      fnc = c("r", "r"), 
      y0  = c(0, C), 
      x   = c(1, abs(A)), 
      y   = y0, 
      line_type = c("main", "main")
    ), 
    # 振幅パラメータが負の場合用
    tibble::tibble(
      fnc   = c("f", "g"), 
      alpha = c(alpha, 0), 
      y0    = c(C, C), 
      x     = abs(A) * cos(a * theta + alpha), 
      y     = abs(A) * sin(a * theta + alpha) + C, 
      line_type = c("main", "sub")
    ), 
    # 円周上の点との線分
    fnc_point_df |> 
      dplyr::select(fnc, y0 = C, x = cos_t, y = sin_t) |> 
      dplyr::mutate(
        line_type = dplyr::if_else(fnc == "sin", true = "main", false = "rev")
      )
  )
  
  # 角マークの座標を作成
  d1 <- 0.15
  d2 <- 0.3
  d3 <- 0.2
  d4 <- 0.25
  angle_mark_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "sin", 
      t   = seq(from = 0, to = theta, length.out = 300), 
      x   = d1 * cos(t), 
      y   = d1 * sin(t)
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = seq(from = 0, to = a*theta+alpha, length.out = 300), 
      x   = d2 * cos(t), 
      y   = d2 * sin(t) + C
    ), 
    tibble::tibble(
      fnc = "g", 
      t   = seq(from = 0, to = a*theta, length.out = 300), 
      x   = d3 * cos(t), 
      y   = d3 * sin(t) + C
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = seq(from = a*theta, to = a*theta+alpha, length.out = 300), 
      x   = d4 * cos(t), 
      y   = d4 * sin(t) + C
    )
  )
  
  # 角ラベルの座標を作成
  d1 <- 0.25
  d2 <- 0.5
  d3 <- 0.1
  d4 <- 0.35
  angle_label_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "sin", 
      t   = 0.5 * theta, 
      x   = d1 * cos(t), 
      y   = d1 * sin(t), 
      angle_label = "theta"
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = 0.5 * (a*theta + alpha), 
      x   = d2 * cos(t), 
      y   = d2 * sin(t) + C, 
      angle_label = "a * theta + alpha"
    ), 
    tibble::tibble(
      fnc = "g", 
      t   = 0.5 * a*theta, 
      x   = d3 * cos(t), 
      y   = d3 * sin(t) + C, 
      angle_label = "a * theta"
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = a*theta + 0.5*alpha, 
      x   = d4 * cos(t), 
      y   = d4 * sin(t) + C, 
      angle_label = "alpha"
    )
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("sin", "f", "g"), 
    x_from = c(cos(theta), A*cos(a*theta + alpha), A*cos(a*theta)), 
    y_from = c(0, C, C), 
    x_to   = x_from, 
    y_to   = c(sin(theta), A*sin(a*theta + alpha) + C, A*sin(a*theta) + C), 
    line_type = c("main", "main", "sub")
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("sin", "f"), 
    x   = c(cos(theta), A*cos(a*theta + alpha)), 
    y   = c(0.5 * sin(theta), 0.5 * A*sin(a*theta + alpha) + C), 
    a   = 90, 
    h   = 0.5, 
    v   = 1, 
    fnc_label = c("sin~theta", "A ~ sin(a * theta + alpha)")
  )
  
  # 範囲πにおける目盛数を指定
  tick_num <- 6
  
  # 角度軸線の座標を作成
  rad_tick_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc = c("sin", "f"), 
      r   = c(1, abs(A)), # 半径
      y0  = c(0, C), # 中心のy座標
    ), 
    i = 0:(2*tick_num-1) # 目盛位置番号
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      t = i/tick_num * pi, # ラジアン
      x = r * cos(t), 
      y = r * sin(t) + y0, 
      t_label = paste0("frac(", i, ", ", tick_num, ") ~ pi") # 角度ラベル
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
                 mapping = aes(x = 0, y = y0, xend = x, yend = y), 
                 color = "white") + # 角度軸目盛線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y, group = fnc), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = y0, xend = x, yend = y, linetype = line_type)) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y, color = fnc)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label, color = fnc), 
              parse = TRUE) + # 角ラベル
    geom_segment(data = fnc_line_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, linetype = line_type), 
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
    scale_color_manual(breaks = c("f", "g", "sin", "alpha"), 
                       values = c("red", "orange", "blue", "purple")) + # 関数ごとに色分け
    scale_linetype_manual(breaks = c("main", "sub", "rev"), 
                          values = c("solid", "twodash", "dashed"), 
                          guide = "none") + # 関数ごとに線分け
    guides(color = "none", linetype = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_x_size, axis_x_size), 
                ylim = c(axis_y_min, axis_y_max)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = fnc_label), 
         x = expression(x == x[0] + r ~ cos~theta), 
         y = expression(y == y[0] + r ~ sin~theta))
  
  ## 関数曲線の作図処理
  
  # 曲線の座標を作成
  fnc_curve_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc   = c("sin", "f", "g"), 
      A     = c(1, A, A), 
      a     = c(1, a, a), 
      alpha = c(0, alpha, 0), 
      C     = c(0, C, C)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      sin_t = A * sin(a * theta + alpha) + C
    )
  
  # 位相の変化の範囲の座標を作成
  phase_df <- tibble::tibble(
    x     = theta + alpha/a, 
    y     = A * sin(a * theta + alpha) + C, 
    x_to  = theta, 
    x_med = theta + 0.5*alpha/a, 
    angle_lable = "frac(alpha, a)"
  )
  
  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "A == ", round(A, digits = 2), ", ", 
    "a == ", round(a, digits = 2), ", ", 
    "alpha == ", round(alpha/pi, digits = 2), " * pi, ", 
    "C == ", round(C, digits = 2), ", ", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "sin~theta == ", round(sin(theta), digits = 2), ", ", 
    "A ~ sin~theta == ", round(A*sin(theta), digits = 2), 
    ")"
  )
  
  # 曲線上の点を作図
  curve_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = theta, y = sin_t, color = fnc, linetype = fnc), 
              linewidth = 1) + # 関数曲線
    geom_vline(mapping = aes(xintercept = theta), 
               linetype = "dotted") + # ラジアン軸の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = Inf, y = sin_t, xend = theta, yend = sin_t, color = fnc), 
                 linetype = "dotted") + # y軸の補助線
    geom_segment(data = phase_df,
                 mapping = aes(x = x, y = y, xend = x_to, yend = y),
                 color ="purple") + # 位相の変化の範囲
    geom_text(data = phase_df,
              mapping = aes(x = x_med, y = y, label = angle_lable),
              parse = TRUE, vjust = -0.2, color = "purple") + # 位相パラメータラベル
    geom_point(data = phase_df,
               mapping = aes(x = x, y = y),
               size = 4, shape = "circle open") + # 位相の変化の点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = theta, y = sin_t), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec), 
                       minor_breaks = FALSE) + # ラジアン軸目盛
    scale_color_manual(breaks = c("f", "g", "sin"), 
                       values = c("red", "orange", "blue"), 
                       labels = parse(text = c("A ~ sin(a * theta + alpha)", "A ~ sin(a * theta)", "sin~theta")), 
                       name = "function") + # 関数ごとに色分け
    scale_linetype_manual(breaks = c("sin", "f", "g"), 
                          values = c("solid", "solid", "dashed"), 
                          guide = "none") + # 関数ごとに線分け
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(min(theta_vec), max(theta_vec)), 
                ylim = c(axis_y_min, axis_y_max)) + 
    labs(title = "sine curve", 
         subtitle = parse(text = fnc_label), 
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## グラフの書出処理
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_graph, circle_graph, 
    guides = "collect"
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1500, height = 800, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, "/", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/sine/waveform_curves_param.gif", delay = 1/10) -> tmp_path # gifファイル書出


# 変数とパラメータと円周と曲線の関係 ----------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# パラメータを指定
A_vals     <- c(1, 2.5, -2)
a_vals     <- c(1, -2, 0.5)
alpha_vals <- c(0, 1/2, 1/6) * pi
C_vals     <- c(0, 1.5, -3)

# 関数の数を設定
fnc_num <- length(a_vals)


# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]
theta_min  <- min(theta_vals)

# 曲線用のラジアンのサイズを指定
theta_size <- 4 * pi


# 範囲πにおける目盛数を指定
tick_num <- 6

# 角度軸線の座標を作成
rad_tick_df <- tidyr::expand_grid(
  tibble::tibble(
    fnc_i = 1:fnc_num, # 関数番号
    r     = abs(A_vals), # 半径
    y0    = C_vals, # 中心のy座標
  ), 
  i = 0:(2*tick_num-1) # 目盛位置番号
) |> # 関数ごとにラジアンを複製
  dplyr::mutate(
    t = i/tick_num * pi, # ラジアン
    x = r * cos(t), 
    y = r * sin(t) + y0, 
    t_label = paste0("frac(", i, ", ", tick_num, ") ~ pi") # 角度ラベル
  )


# 範囲πにおける目盛数を指定
tick_num <- 3

# 円周の座標を作成
circle_df <- tidyr::expand_grid(
  tibble::tibble(
    fnc_i = 1:fnc_num, 
    r     = abs(A_vals), # 半径
    y0    = C_vals # 中心のy座標
  ), 
  t = seq(from = 0, to = 2*pi, length.out = 361) # 1周期分のラジアン
) |> # 円周ごとにラジアンを複製
  dplyr::mutate(
    x = r * cos(t), 
    y = r * sin(t) + y0
  )


# グラフサイズを指定
axis_x_size <- max(abs(A_vals)) + 0.5
axis_y_min  <- min(-1, -abs(A_vals)+C_vals) - 0.5
axis_y_max  <- max(1, abs(A_vals)+C_vals) + 0.5

# ラベル用の文字列を作成
param_label_vec <- paste0(
  "list(", 
  "A[", 1:fnc_num, "] == ", round(A_vals, digits = 2), ", ", 
  "a[", 1:fnc_num, "] == ", round(a_vals, digits = 2), ", ", 
  "alpha[", 1:fnc_num, "] == ", round(alpha_vals/pi, digits = 2), " * pi, ", 
  "C[", 1:fnc_num, "] == ", round(C_vals, digits = 2), 
  ")"
) |> 
  (\(str) {parse(text = str)})() # 数式表示用

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 変数を取得
  theta <- theta_vals[i]
  
  # 円周・曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    fnc_i = 1:fnc_num, 
    theta = theta, 
    A     = A_vals, 
    a     = a_vals, 
    alpha = alpha_vals, 
    C     = C_vals, 
    sin_t = A * sin(a*theta + alpha) + C, 
    cos_t = A * cos(a*theta + alpha)
  )
  
  ## 単位円と関数の関係の作図処理
  
  # 半径線の終点の座標を作成
  radius_df <- dplyr::bind_rows(
    # 振幅パラメータが負の場合用
    tibble::tibble(
      fnc_i = 1:fnc_num, 
      y0    = C_vals, 
      x     = abs(A_vals) * cos(a_vals * theta + alpha_vals), 
      y     = abs(A_vals) * sin(a_vals * theta + alpha_vals) + C_vals, 
      line_type = "main", 
    ), 
    # 円周上の点との線分
    fnc_point_df |> 
      dplyr::select(fnc_i, y0 = C, x = cos_t, y = sin_t) |> 
      dplyr::mutate(
        line_type = "rev"
      )
  )
  
  # 角マークの座標を作成
  d <- 0.2
  angle_mark_df <- tibble::tibble(
    fnc_i = 1:fnc_num, 
    t_max = a_vals * theta + alpha_vals
  ) |> 
    dplyr::reframe(
      t = seq(from = 0, to = t_max, length.out = 300), .by = fnc_i
    ) |> # 関数ごとにラジアンを作成
    dplyr::mutate(
      x = d * cos(t), 
      y = d * sin(t) + C_vals[fnc_i]
    )
  
  # 角ラベルの座標を作成
  d <- 0.4
  angle_label_df <- tibble::tibble(
    fnc_i = 1:fnc_num, 
    t     = 0.5 * (a_vals * theta + alpha_vals),
    x     = d * cos(t), 
    y     = d * sin(t) + C_vals, 
    angle_label = paste0("theta[", fnc_i, "]")
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc_i  = 1:fnc_num, 
    x_from = A_vals * cos(a_vals * theta + alpha_vals), 
    y_from = C_vals, 
    x_to   = x_from, 
    y_to   = A_vals * sin(a_vals * theta + alpha_vals) + C_vals
  )

  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "theta[i] == a[i] * theta + alpha[i], ", 
    "cos~theta == ", round(cos(theta), digits = 2), ", ", 
    "sin~theta == ", round(sin(theta), digits = 2), 
    ")"
  )
  
  # 円周上の点を作図
  circle_graph <- ggplot() + 
    geom_segment(data = rad_tick_df, 
                 mapping = aes(x = 0, y = y0, xend = x, yend = y), 
                 color = "white") + # 角度軸目盛線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y, group = factor(fnc_i)), 
              linewidth = 1) + # 円周
    geom_segment(mapping = aes(x = 0, y = C_vals, xend = abs(A_vals), yend = C_vals)) + # 半径線(x軸線上)
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = y0, xend = x, yend = y, linetype = line_type)) + # 半径線(動径)
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y, color = factor(fnc_i))) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label, color = factor(fnc_i)), 
              parse = TRUE) + # 角ラベル
    geom_segment(data = fnc_line_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = factor(fnc_i)), 
                 linewidth = 1) + # 関数直線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = -Inf, y = sin_t, xend = cos_t, yend = sin_t, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸の補助線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = cos_t, y = sin_t), 
               size = 4) + # 関数点
    scale_linetype_manual(breaks = c("main", "sub", "rev"), 
                          values = c("solid", "twodash", "dashed"), 
                          guide = "none") + # 関数ごとに線分け
    guides(color = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_x_size, axis_x_size), 
                ylim = c(axis_y_min, axis_y_max)) + 
    labs(title = "circle", 
         subtitle = parse(text = fnc_label), 
         x = expression(x == x[0] + r ~ cos~theta), 
         y = expression(y == y[0] + r ~ sin~theta))
  
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
      fnc_i = 1:fnc_num, 
      A     = A_vals, 
      a     = a_vals, 
      alpha = alpha_vals, 
      C     = C_vals
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      sin_t = A * sin(a * theta + alpha) + C
    )
  
  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "sin~theta == ", round(sin(theta), digits = 2), 
    ")"
  )
  
  # 曲線上の点を作図
  curve_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = theta, y = sin_t, color = factor(fnc_i)), 
              linewidth = 1) + # 関数曲線
    geom_vline(mapping = aes(xintercept = theta), 
               linetype = "dotted") + # ラジアン軸の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = Inf, y = sin_t, xend = theta, yend = sin_t, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸の補助線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = theta, y = sin_t), 
               size = 4) + # 曲線上の点
    scale_color_hue(labels = param_label_vec, name = "parameter") + # 凡例表示用
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec), 
                       minor_breaks = FALSE) + # ラジアン軸目盛
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(theta-theta_size, theta), 
                ylim = c(axis_y_min, axis_y_max)) + 
    labs(title = "sine curve", 
         subtitle = parse(text = fnc_label), 
         x = expression(theta), 
         y = expression(A[i] ~ sin(a[i] * theta + alpha[i]) + C[i]))
  
  ## グラフの書出処理
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_graph, circle_graph, 
    guides = "collect"
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1500, height = 800, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, "/", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/sine/waveform_curves_n.gif", delay = 1/30) -> tmp_path # gifファイル書出


