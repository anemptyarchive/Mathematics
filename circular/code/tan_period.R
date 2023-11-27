
# tan関数の周期の可視化 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 曲線の作図 -------------------------------------------------------------------

# 周期パラメータを指定
a <- 2

# 変数(ラジアン)の範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 5000)

# 閾値を指定
threshold <- 4

# 曲線の座標を作成
curve_df <- tibble::tibble(
  theta = theta_vec, 
  tan_t = tan(theta), 
  f_t   = tan(a * theta)
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

# 目盛の範囲を設定:(π単位で切り捨て・切り上げ)
tick_lower <- floor(min(theta_vec) / pi) * pi
tick_upper <- ceiling(max(theta_vec) / pi) * pi

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = theta_lower, to = theta_upper, by = pi/tick_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# 漸近線の描画範囲を設定:(π/a単位で切り捨て・切り上げ)
theta_lower <- (floor(min(theta_vec) / pi*abs(a)) - 0.5) * pi/abs(a)
theta_upper <- (ceiling(max(theta_vec) / pi*abs(a)) + 0.5) * pi/abs(a)

# 漸近線用の値を作成
asymptote_break_vec <- seq(from = theta_lower, to = theta_upper, by = pi/abs(a))
asymptote_label_vec <- paste0(round(asymptote_break_vec/pi, digits = 2), " * pi")


# ラベル用の文字列を作成
param_label <- paste0("a == ", a)

# tan関数曲線を作図
ggplot() + 
  geom_vline(xintercept = asymptote_break_vec, 
             linetype = "twodash") + # 漸近線
  geom_line(data = curve_df, 
            mapping = aes(x = theta, y = tan_t, linetype = "tan"), 
            linewidth = 1) + # 元の曲線
  geom_line(data = curve_df, 
            mapping = aes(x = theta, y = f_t, linetype = "f"), 
            linewidth = 1) + # 変形した曲線
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec), 
                     sec.axis = sec_axis(trans = ~., 
                                         breaks = asymptote_break_vec, 
                                         labels = parse(text = asymptote_label_vec), 
                                         name = "asymptote")) + # ラジアン軸目盛
  scale_linetype_manual(breaks = c("f", "tan"), 
                        values = c("solid", "dotted"), 
                        labels = c(expression(tan(a * theta)), expression(tan~theta)), 
                        name = "function") + # 凡例表示用
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁
  coord_fixed(ratio = 1, 
              xlim = c(min(theta_vec), max(theta_vec))) + # 描画領域
  labs(title = "tangent function: period", 
       subtitle = parse(text = param_label), 
       x = expression(theta), 
       y = expression(f(theta)))


# パラメータと曲線の形状の関係 ------------------------------------------------------------------

# フレーム数を指定
frame_num <- 101

# 周期パラメータの範囲を指定
a_vals <- seq(from = -5, to = 5, length.out = frame_num)

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
    a     = a_vals[frame_i], 
    tan_t = tan(theta), 
    f_t   = tan(a * theta)
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
  abs_a   = abs(a_vals) + 1e-10, 
  t_lower = (floor(min(theta_vec) / pi*abs_a) - 0.5) * pi/abs_a, 
  t_upper = (ceiling(max(theta_vec) / pi*abs_a) + 0.5) * pi/abs_a
) |> 
  dplyr::reframe(
    t = seq(from = t_lower, to = t_upper, by = pi/abs_a), .by = c(frame_i, abs_a)
  ) |> # プロット位置を作成
  dplyr::filter(
    t >= min(theta_vec), t <= max(theta_vec)
  ) # 描画範囲外を除去

# 1周期の範囲の座標を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  a       = a_vals, 
  x_from  = 0, 
  x_to    = pi / abs(a), 
  param_label = paste0("a == ", round(a, digits = 2))
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
  geom_vline(data = anim_asymptote_df, 
             mapping = aes(xintercept = t), 
             linetype = "twodash") + # 漸近線
  geom_line(data = anim_curve_df, 
            mapping = aes(x = theta, y = tan_t, linetype = "tan"), 
            linewidth = 1) + # 元の曲線
  geom_line(data = anim_curve_df, 
            mapping = aes(x = theta, y = f_t, linetype = "f"), 
            linewidth = 1) + # 変形した曲線
  geom_segment(mapping = aes(x = 0, y = 0, xend = pi, yend = 0), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "both"), 
               color = "blue", linewidth = 1) + # 元の曲線の1周期の範囲
  geom_segment(data = anim_label_df, 
               mapping = aes(x = x_from, y = 0, xend = x_to, yend = 0), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "both"), 
               color = "red", linewidth = 0.5) + # 変形した曲線の1周期の範囲
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = param_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  scale_linetype_manual(breaks = c("f", "tan"), 
                        values = c("solid", "dotted"), 
                        labels = c(expression(tan(a * theta)), expression(tan~theta)), 
                        name = "function") + # 凡例表示用
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + 
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(min(theta_vec), max(theta_vec))) + 
  labs(title = "tangent function: period", 
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

# 周期パラメータを指定
a <- 2

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
  y = r * sin(t), 
  t_label = paste0("frac(", i, ", ", tick_num, ") ~ pi") # 角度ラベル
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
    a     = c(1, a), 
    tan_t = tan(a * theta), 
    cos_t = cos(a * theta), 
    sin_t = sin(a * theta)
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
  d1 <- 0.2
  da <- 0.3
  dr <- 0.02
  angle_mark_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc = c("tan", "f"), 
      a   = c(1, a), 
      d   = c(d1, da)
    ), 
    t = seq(from = 0, to = theta, length.out = 300)
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      x = (d+dr*t) * cos(a * t),
      y = (d+dr*t) * sin(a * t)
    )
  
  # 角ラベルの座標を作成
  d1 <- 0.1
  da <- 0.4
  angle_label_df <- tibble::tibble(
    fnc = c("tan", "f"), 
    a   = c(1, a), 
    d   = c(d1, da), 
    t   = 0.5 * c(theta, theta), 
    x   = d * cos(a * t),
    y   = d * sin(a * t), 
    angle_label = c("theta", "a * theta")
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("tan", "f"), 
    x_from = c(1, 1), 
    y_from = c(0, 0), 
    x_to   = x_from, 
    y_to   = c(tan(theta), tan(a*theta))
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("tan", "f"), 
    x   = c(1, 1), 
    y   = 0.5 * c(tan(theta), tan(a*theta)), 
    a   = 90, 
    h   = 0.5, 
    v   = c(-0.5, 1), 
    fnc_label = c("tan~theta", "tan(a * theta)")
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
    geom_vline(mapping = aes(xintercept = 1), 
               linetype = "twodash") + # 関数直線用の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = 0, y = 0, xend = 1, yend = tan_t), 
                 linetype = "twodash") + # 関数直線用の補助線
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
    scale_color_manual(breaks = c("f", "tan"), 
                       values = c("red", "blue")) + # 関数ごとに色分け
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
      fnc = c("tan", "f"), 
      a   = c(1, a)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      tan_t = tan(a * theta)
    ) |> 
    dplyr::mutate(
      tan_t = dplyr::if_else(
        (tan_t >= -axis_size & tan_t <= axis_size), true = tan_t, false = NA_real_
      )
    ) # 閾値外の値を欠損値に置換
  
  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "a == ", round(a, digits = 2), ", ", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "tan~theta == ", round(tan(theta), digits = 2), ", ", 
    "tan(a * theta) == ", round(tan(a*theta), digits = 2), 
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
    geom_point(data = fnc_point_df, 
               mapping = aes(x = theta, y = tan_t), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec), 
                       minor_breaks = FALSE) + # ラジアン軸目盛
    scale_color_manual(breaks = c("f", "tan"), 
                       values = c("red", "blue"), 
                       labels = parse(text = c("tan(a * theta)", "tan~theta")), 
                       name = "function") + # 関数ごとに色分け
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(theta-theta_size, theta), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "tangent curve: period", 
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
  magick::image_write_gif(path = "circular/figure/tangent/period_curves_variable.gif", delay = 1/30) -> tmp_path # gifファイル書出


# パラメータと円周と曲線の関係 ----------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 101

# 周期パラメータの範囲を指定
a_vals <- seq(from = -5, to = 5, length.out = frame_num)

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
  y = r * sin(t), 
  t_label = paste0("frac(", i, ", ", tick_num, ") ~ pi") # 角度ラベル
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
i <- 60
# パラメータごとに作図
for(i in 1:frame_num) {
  
  # パラメータを取得
  a <- a_vals[i]
  
  # 円周・曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    fnc   = c("tan", "f"), 
    theta = c(theta, theta), 
    a     = c(1, a), 
    tan_t = tan(a * theta), 
    cos_t = cos(a * theta), 
    sin_t = sin(a * theta)
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
  d1 <- 0.2
  da <- 0.3
  dr <- 0.02
  angle_mark_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc = c("tan", "f"), 
      a   = c(1, a), 
      d   = c(d1, da)
    ), 
    t = seq(from = 0, to = theta, length.out = 300)
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      x = (d+dr*t) * cos(a * t),
      y = (d+dr*t) * sin(a * t)
    )
  
  # 角ラベルの座標を作成
  d1 <- 0.1
  da <- 0.4
  angle_label_df <- tibble::tibble(
    fnc = c("tan", "f"), 
    a   = c(1, a), 
    d   = c(d1, da), 
    t   = 0.5 * c(theta, theta), 
    x   = d * cos(a * t),
    y   = d * sin(a * t), 
    angle_label = c("theta", "a * theta")
  )
  
  # 円上の直線の座標を作成
  fnc_line_df <- tibble::tibble(
    fnc    = c("tan", "f"), 
    x_from = c(1, 1), 
    y_from = c(0, 0), 
    x_to   = x_from, 
    y_to   = c(tan(theta), tan(a*theta))
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <-tibble::tibble(
    fnc = c("tan", "f"), 
    x   = c(1, 1), 
    y   = 0.5 * c(tan(theta), tan(a*theta)), 
    a   = 90, 
    h   = 0.5, 
    v   = c(-0.5, 1), 
    fnc_label = c("tan~theta", "tan(a * theta)")
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
    geom_vline(mapping = aes(xintercept = 1), 
               linetype = "twodash") + # 関数直線用の補助線
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = 0, y = 0, xend = 1, yend = tan_t), 
                 linetype = "twodash") + # 関数直線用の補助線
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
    scale_color_manual(breaks = c("f", "tan"), 
                       values = c("red", "blue")) + # 関数ごとに色分け
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
      fnc = c("tan", "f"), 
      a   = c(1, a)
    ), 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      tan_t = tan(a * theta)
    ) |> 
    dplyr::mutate(
      tan_t = dplyr::if_else(
        (tan_t >= -axis_size & tan_t <= axis_size), true = tan_t, false = NA_real_
      )
    ) # 閾値外の値を欠損値に置換
  
  # 関数ラベルを作成
  fnc_label <- paste0(
    "list(", 
    "a == ", round(a, digits = 2), ", ", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "tan~theta == ", round(tan(theta), digits = 2), ", ", 
    "tan(a * theta) == ", round(tan(a*theta), digits = 2), 
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
    geom_point(data = fnc_point_df, 
               mapping = aes(x = theta, y = tan_t), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec), 
                       minor_breaks = FALSE) + # ラジアン軸目盛
    scale_color_manual(breaks = c("f", "tan"), 
                       values = c("red", "blue"), 
                       labels = parse(text = c("tan(a * theta)", "tan~theta")), 
                       name = "function") + # 関数ごとに色分け
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(min(theta_vec), max(theta_vec)), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "tangent curve: period", 
         subtitle = parse(text = fnc_label), 
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## グラフの書出処理
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_graph, circle_graph, 
    guides = "collect"
  )
  wrap_graph
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
  magick::image_write_gif(path = "circular/figure/tangent/period_curves_param.gif", delay = 1/10) -> tmp_path # gifファイル書出


