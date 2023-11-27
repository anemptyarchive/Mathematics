
# tan関数の位相の可視化 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 曲線の作図 -------------------------------------------------------------------

# 位相パラメータを指定
alpha <- 0.5 * pi

# 変数(ラジアン)の範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 5000)

# 閾値を指定
threshold <- 4

# 曲線の座標を作成
curve_df <- tibble::tibble(
  theta = theta_vec, 
  tan_t = tan(theta), 
  f_t   = tan(theta + alpha)
) |> 
  dplyr::mutate(
    tan_t = dplyr::if_else(
      (tan_t >= -threshold & tan_t <= threshold), true = tan_t, false = NA_real_
    ), 
    f_t = dplyr::if_else(
      (f_t >= -threshold & f_t <= threshold), true = f_t, false = NA_real_
    )
  ) # 閾値外の値を欠損値に置換


# 範囲πにおける目盛数を指定
tick_num <- 1

# 目盛ラベルの範囲を設定:(π単位で切り捨て・切り上げ)
theta_lower <- floor(min(theta_vec - alpha) / pi) * pi
theta_upper <- ceiling(max(theta_vec - alpha) / pi) * pi

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = theta_lower, to = theta_upper, by = pi/tick_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# 漸近線の描画範囲を設定:(π単位で切り捨て・切り上げ)
theta_lower <- (floor(min(theta_vec) / pi) - 0.5) * pi
theta_upper <- (ceiling(max(theta_vec) / pi) + 0.5) * pi

# 漸近線用の値を作成
asymptote_break_vec <- seq(from = theta_lower, to = theta_upper, by = pi)
asymptote_label_vec <- paste0(round(asymptote_break_vec/pi, digits = 2), " * pi")


# ラベル用の文字列を作成
param_label <- paste0(
  "alpha == ", round(alpha/pi, digits = 2), " * pi"
)

# tan関数曲線を作図
ggplot() + 
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                             xend = c(Inf, 0), yend = c(0, Inf)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_vline(xintercept = asymptote_break_vec, 
             linetype = "twodash") + # 漸近線
  geom_line(data = curve_df, 
            mapping = aes(x = theta, y = tan_t, linetype = "tan"), 
            linewidth = 1) + # 元の曲線
  geom_line(data = curve_df, 
            mapping = aes(x = theta, y = f_t, linetype = "f"), 
            linewidth = 1) + # 変形した曲線
  scale_linetype_manual(breaks = c("f", "tan"), 
                        values = c("solid", "dotted"), 
                        labels = c(expression(A ~ tan(theta)), expression(tan~theta)), 
                        name = "function") + # 凡例表示用
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec), 
                     sec.axis = sec_axis(trans = ~., 
                                         breaks = asymptote_break_vec, 
                                         labels = parse(text = asymptote_label_vec), 
                                         name = "asymptote")) + # ラジアン軸目盛
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 1) + # 図の体裁
  coord_fixed(ratio = 1, 
              xlim = c(min(theta_vec), max(theta_vec))) + # 描画領域
  labs(title = "tangent function: phase", 
       subtitle = parse(text = param_label), 
       x = expression(theta), 
       y = expression(f(theta)))


# パラメータと曲線の関係 ------------------------------------------------------------------

# フレーム数を指定
frame_num <- 100

# 位相パラメータの範囲を指定
alpha_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# ラジアンの範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 5000)

# 閾値を指定
threshold <- 4

# 曲線の座標を作成
anim_curve_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  theta   = theta_vec
) |> # フレームごとにラジアンを複製
  dplyr::mutate(
    alpha = alpha_vals[frame_i], 
    tan_t = tan(theta), 
    f_t   = tan(theta + alpha)
  ) |> 
  dplyr::mutate(
    tan_t = dplyr::if_else(
      (tan_t >= -threshold & tan_t <= threshold), true = tan_t, false = NA_real_
    ), 
    f_t = dplyr::if_else(
      (f_t >= -threshold & f_t <= threshold), true = f_t, false = NA_real_
    )
  ) # 閾値外の値を欠損値に置換


# 漸近線用の値を作成
anim_asymptote_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  alpha   = alpha_vals, 
  t_lower = (floor(min(theta_vec - alpha) / pi) - 0.5) * pi, 
  t_upper = (ceiling(max(theta_vec - alpha) / pi) + 0.5) * pi
) |> 
  dplyr::reframe(
    t = seq(from = t_lower, to = t_upper, by = pi), .by = frame_i
  ) |> # プロット位置を作成
  dplyr::filter(
    t >= min(theta_vec), t <= max(theta_vec)
  ) # 描画範囲外を除去

# 位相の変化の範囲の座標を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  alpha   = alpha_vals, 
  x_from  = 0, 
  x_to    = -alpha, 
  x_med   = -0.5 * alpha, 
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


# tan関数曲線を作図
anim <- ggplot() + 
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                             xend = c(Inf, 0), yend = c(0, Inf)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_vline(data = anim_asymptote_df, 
             mapping = aes(xintercept = t), 
             linetype = "twodash") + # 漸近線
  geom_line(data = anim_curve_df, 
            mapping = aes(x = theta, y = tan_t, linetype = "tan"), 
            linewidth = 1) + # 元の曲線
  geom_line(data = anim_curve_df, 
            mapping = aes(x = theta, y = f_t, linetype = "f"), 
            linewidth = 1) + # 変形した曲線
  geom_segment(data = anim_label_df, 
               mapping = aes(x = x_from, y = y, xend = x_to, yend = y), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "both"), 
               color = "red", linewidth = 0.5) + # 位相の変化の範囲
  geom_text(data = anim_label_df, 
            mapping = aes(x = x_med, y = y), 
            label = "alpha", parse = TRUE, color = "red", vjust = -0.5, size = 6) + # 変形した範囲ラベル
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = param_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # パラメータラベル:(subtitleの代用)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_linetype_manual(breaks = c("f", "tan"), 
                        values = c("solid", "dotted"), 
                        labels = c(expression(tan(theta + alpha)), expression(tan~theta)), 
                        name = "function") + # 凡例表示用
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + 
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(min(theta_vec), max(theta_vec))) + 
  labs(title = "tangent function: phase", 
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
theta_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]
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
    fnc   = c("tan", "f"), 
    theta = c(theta, theta), 
    alpha = c(0, alpha), 
    tan_t = tan(theta + alpha), 
    cos_t = cos(theta + alpha), 
    sin_t = sin(theta + alpha)
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
  dta <- 0.3
  da  <- 0.25
  dr <- 0.005
  angle_mark_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "tan", 
      t   = seq(from = 0, to = theta, length.out = 300), 
      x   = (dt + dr*t) * cos(t), 
      y   = (dt + dr*t) * sin(t)
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = seq(from = 0, to = theta+alpha, length.out = 300), 
      x   = (dta + dr*t) * cos(t), 
      y   = (dta + dr*t) * sin(t)
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = seq(from = theta, to = theta+alpha, length.out = 300), 
      x   = da * cos(t), 
      y   = da * sin(t)
    )
  )
  
  # 角ラベルの座標を作成
  dt  <- 0.1
  dta <- 0.5
  da  <- 0.35
  angle_label_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "tan", 
      t   = 0.5 * theta, 
      x   = dt * cos(t), 
      y   = dt * sin(t), 
      angle_label = "theta"
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = 0.5 * (theta + alpha), 
      x   = dta * cos(t), 
      y   = dta * sin(t), 
      angle_label = "theta + alpha"
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = theta + 0.5*alpha, 
      x   = da * cos(t), 
      y   = da * sin(t), 
      angle_label = "alpha"
    )
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("tan", "f"), 
    x_from = c(1, 1), 
    y_from = c(0, 0), 
    x_to   = x_from, 
    y_to   = c(tan(theta), tan(theta+alpha))
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("tan", "f"), 
    x   = c(1, 1), 
    y   = 0.5 * c(tan(theta), tan(theta+alpha)), 
    a   = 90, 
    h   = 0.5, 
    v   = c(-0.5, 1), 
    fnc_label = c("tan~theta", "tan(theta + alpha)")
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
    geom_point(data = fnc_point_df, 
               mapping = aes(x = cos_t, y = sin_t), 
               size = 4, shape = "circle open") + # 円周上の点
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y, color = fnc)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label, color = fnc), 
              parse = TRUE) + # 角ラベル
    geom_vline(mapping = aes(xintercept = 1), 
               linetype = "twodash") + # 関数直線用の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = 0, y = 0, xend = 1, yend = tan_t), 
                 linetype = "twodash") + # 関数直線用の補助線
    geom_segment(data = fnc_line_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = fnc), 
                 linewidth = 1) + # 関数直線
    geom_text(data = fnc_label_df, 
              mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                            angle = a, hjust = h, vjust = v), 
              parse = TRUE) + # 関数ラベル
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = -Inf, y = tan_t, xend = 1, yend = tan_t, color = fnc), 
                 linetype = "dotted") + # y軸の補助線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = 1, y = tan_t), 
               size = 4) + # 関数点
    scale_color_manual(breaks = c("f", "tan", "alpha"), 
                       values = c("red", "blue", "purple")) + # 関数ごとに色分け
    scale_linetype_manual(breaks = c("r", "tan", "f"), 
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
      fnc   = c("tan", "f"), 
      alpha = c(0, alpha)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      tan_t = tan(theta + alpha)
    ) |> 
    dplyr::mutate(
      tan_t = dplyr::if_else(
        (tan_t >= -axis_size & tan_t <= axis_size), true = tan_t, false = NA_real_
      )
    ) # 閾値外の値を欠損値に置換
  
  # 位相の変化の範囲の座標を作成
  phase_df <- tibble::tibble(
    x     = theta + alpha, 
    y     = tan(theta + alpha), 
    x_to  = theta, 
    x_med = theta + 0.5*alpha, 
    angle_lable = "alpha"
  )
  
  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "alpha == ", round(alpha/pi, digits = 2), " * pi, ", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "tan~theta == ", round(tan(theta), digits = 2), ", ", 
    "tan(theta + alpha) == ", round(tan(theta+alpha), digits = 2), 
    ")"
  )
  
  # 曲線上の点を作図
  curve_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = theta, y = tan_t, color = fnc), 
              linewidth = 1) + # 関数曲線
    geom_vline(xintercept = theta,
               linetype = "dotted") + # ラジアン軸の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = Inf, y = tan_t, xend = theta, yend = tan_t, color = fnc), 
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
               mapping = aes(x = theta, y = tan_t), 
               size = 4) + # 曲線上の点
    scale_color_manual(breaks = c("f", "tan"), 
                       values = c("red", "blue"), 
                       labels = parse(text = c("tan(theta + alpha)", "tan~theta")), 
                       name = "function") + # 関数ごとに色分け
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec), 
                       minor_breaks = FALSE) + # ラジアン軸目盛
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(theta_max-theta_size, theta_max), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "tangent curve: phase", 
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
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1200, height = 600, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, "/", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/tangent/phase_curves_variable.gif", delay = 1/30) -> tmp_path # gifファイル書出


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
axis_size <- 2.5

# パラメータごとに作図
for(i in 1:frame_num) {
  
  # パラメータを取得
  alpha <- alpha_vals[i]
  
  # 円周・曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    fnc   = c("tan", "f"), 
    theta = c(theta, theta), 
    alpha = c(0, alpha), 
    tan_t = tan(theta + alpha), 
    cos_t = cos(theta + alpha), 
    sin_t = sin(theta + alpha)
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
  dta <- 0.3
  da  <- 0.25
  dr <- 0.005
  angle_mark_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "tan", 
      t   = seq(from = 0, to = theta, length.out = 300), 
      x   = (dt + dr*t) * cos(t), 
      y   = (dt + dr*t) * sin(t)
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = seq(from = 0, to = theta+alpha, length.out = 300), 
      x   = (dta + dr*t) * cos(t), 
      y   = (dta + dr*t) * sin(t)
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = seq(from = theta, to = theta+alpha, length.out = 300), 
      x   = da * cos(t), 
      y   = da * sin(t)
    )
  )
  
  # 角ラベルの座標を作成
  dt  <- 0.1
  dta <- 0.5
  da  <- 0.35
  angle_label_df <- dplyr::bind_rows(
    tibble::tibble(
      fnc = "tan", 
      t   = 0.5 * theta, 
      x   = dt * cos(t), 
      y   = dt * sin(t), 
      angle_label = "theta"
    ), 
    tibble::tibble(
      fnc = "f", 
      t   = 0.5 * (theta + alpha), 
      x   = dta * cos(t), 
      y   = dta * sin(t), 
      angle_label = "theta + alpha"
    ), 
    tibble::tibble(
      fnc = "alpha", 
      t   = theta + 0.5*alpha, 
      x   = da * cos(t), 
      y   = da * sin(t), 
      angle_label = "alpha"
    )
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("tan", "f"), 
    x_from = c(1, 1), 
    y_from = c(0, 0), 
    x_to   = x_from, 
    y_to   = c(tan(theta), tan(theta+alpha))
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("tan", "f"), 
    x   = c(1, 1), 
    y   = 0.5 * c(tan(theta), tan(theta+alpha)), 
    a   = 90, 
    h   = 0.5, 
    v   = c(-0.5, 1), 
    fnc_label = c("tan~theta", "tan(theta + alpha)")
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
    geom_point(data = fnc_point_df, 
               mapping = aes(x = cos_t, y = sin_t), 
               size = 4, shape = "circle open") + # 円周上の点
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y, color = fnc)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label, color = fnc), 
              parse = TRUE) + # 角ラベル
    geom_vline(mapping = aes(xintercept = 1), 
               linetype = "twodash") + # 関数直線用の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = 0, y = 0, xend = 1, yend = tan_t), 
                 linetype = "twodash") + # 関数直線用の補助線
    geom_segment(data = fnc_line_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = fnc), 
                 linewidth = 1) + # 関数直線
    geom_text(data = fnc_label_df, 
              mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                            angle = a, hjust = h, vjust = v), 
              parse = TRUE) + # 関数ラベル
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = -Inf, y = tan_t, xend = 1, yend = tan_t, color = fnc), 
                 linetype = "dotted") + # y軸の補助線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = 1, y = tan_t), 
               size = 4) + # 関数点
    scale_color_manual(breaks = c("f", "tan", "alpha"), 
                       values = c("red", "blue", "purple")) + # 関数ごとに色分け
    scale_linetype_manual(breaks = c("r", "tan", "f"), 
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
      fnc   = c("tan", "f"), 
      alpha = c(0, alpha)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      tan_t = tan(theta + alpha)
    ) |> 
    dplyr::mutate(
      tan_t = dplyr::if_else(
        (tan_t >= -axis_size & tan_t <= axis_size), true = tan_t, false = NA_real_
      )
    ) # 閾値外の値を欠損値に置換
  
  # 位相の変化の範囲の座標を作成
  phase_df <- tibble::tibble(
    x     = theta + alpha, 
    y     = tan(theta + alpha), 
    x_to  = theta, 
    x_med = theta + 0.5*alpha, 
    angle_lable = "alpha"
  )
  
  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "alpha == ", round(alpha/pi, digits = 2), " * pi, ", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "tan~theta == ", round(tan(theta), digits = 2), ", ", 
    "tan(theta + alpha) == ", round(tan(theta+alpha), digits = 2), 
    ")"
  )
  
  # 曲線上の点を作図
  curve_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = theta, y = tan_t, color = fnc), 
              linewidth = 1) + # 関数曲線
    geom_vline(xintercept = theta,
               linetype = "dotted") + # ラジアン軸の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = Inf, y = tan_t, xend = theta, yend = tan_t, color = fnc), 
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
               mapping = aes(x = theta, y = tan_t), 
               size = 4) + # 曲線上の点
    scale_color_manual(breaks = c("f", "tan"), 
                       values = c("red", "blue"), 
                       labels = parse(text = c("tan(theta + alpha)", "tan~theta")), 
                       name = "function") + # 関数ごとに色分け
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec), 
                       minor_breaks = FALSE) + # ラジアン軸目盛
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(min(theta_vec), max(theta_vec)), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "tangent curve: phase", 
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
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1200, height = 600, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, "/", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/tangent/phase_curves_param.gif", delay = 1/10) -> tmp_path # gifファイル書出


