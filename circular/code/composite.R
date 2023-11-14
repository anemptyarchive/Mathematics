
# 波の重ね合わせ -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 円周上の点の関係 ----------------------------------------------------------------

# パラメータを指定
A_vals     <- c(3, 2, 1)
a_vals     <- c(1.5, -2, 3)
alpha_vals <- c(0, 0, 0.5) * pi

# 関数の数を設定
fnc_num <- length(a_vals)

# 1周期分の変数(ラジアン)を作成
theta_vec <- seq(from = 0, to = 2*pi/min(abs(a_vals)), length.out = 1000)

# 円周上の点用の変数(ラジアン)を指定
theta_val <- 2/3 * pi


# 関数ごとの円周上の点の座標を作成
point_each_df <- tibble::tibble(
  fnc_i = 1:fnc_num, # 関数番号
  theta = theta_val, 
  # パラメータ
  A     = A_vals, 
  a     = a_vals, 
  alpha = alpha_vals, 
  # 原点が中心の円周上
  x = A * cos(a*theta + alpha), 
  y = A * sin(a*theta + alpha)
) |> 
  dplyr::mutate(
    # 累積点が中心の円周上
    sum_x = cumsum(x), 
    sum_y = cumsum(y), 
    # 円周の中心(1つ前の累積点)
    O_x   = dplyr::lag(sum_x, n = 1, default = 0), 
    O_y   = dplyr::lag(sum_y, n = 1, default = 0)
  )

# 作図用に加工
point_df <- dplyr::bind_rows(
  # 関数ごとの点
  point_each_df |> 
    dplyr::select(fnc_i, theta, x, y, O_x, O_y) |> 
    dplyr::mutate(O_x = 0, O_y = 0, graph_label = "each"), 
  # 関数の累積和の点
  point_each_df |> 
    dplyr::select(fnc_i, theta, x = sum_x, y = sum_y, O_x, O_y) |> 
    dplyr::mutate(graph_label = "sum")
)

# 関数の総和の円周上の点の座標を作成
point_sum_df <- point_each_df |> 
  dplyr::summarise(
    x = sum(x), 
    y = sum(y), 
    .by = theta
  ) |> 
  dplyr::mutate(graph_label = "sum")

# 関数ごとの円周の座標を作成
circle_each_df <- tidyr::expand_grid(
  fnc_i = 1:fnc_num, # 関数番号
  theta = theta_vec
) |> # 関数ごとに変数を複製
  dplyr::mutate(
    # 円周の中心(1つ前の累積点)
    O_x = point_each_df[["O_x"]][fnc_i], 
    O_y = point_each_df[["O_y"]][fnc_i], 
    # パラメータ
    A     = A_vals[fnc_i], 
    a     = a_vals[fnc_i], 
    alpha = alpha_vals[fnc_i], 
    # 原点が中心の円周
    x = A * cos(a*theta + alpha), 
    y = A * sin(a*theta + alpha), 
    # 累積点が中心の円周
    sum_x = O_x + x, 
    sum_y = O_y + y
  )

# 作図用に加工
circle_df <- dplyr::bind_rows(
  # 関数ごとの円周
  circle_each_df |> 
    dplyr::select(fnc_i, theta, x, y, O_x, O_y) |> 
    dplyr::mutate(O_x = 0, O_y = 0, graph_label = "each"), 
  # 関数の累積和の円周
  circle_each_df |> 
    dplyr::select(fnc_i, theta, x = sum_x, y = sum_y, O_x, O_y) |> 
    dplyr::mutate(graph_label = "sum")
)

# グラフサイズを設定
axis_size <- c(
  circle_df[["x"]], circle_df[["y"]]
) |>
  abs() |>
  max() |>
  ceiling()

# ラベル用の文字列を作成
param_label_vec <- paste0(
  "list(", 
  "A[", 1:fnc_num, "] == ", round(A_vals, digits = 2), ", ", 
  "a[", 1:fnc_num, "] == ", round(a_vals, digits = 2), ", ", 
  "alpha[", 1:fnc_num, "] == ", round(alpha_vals/pi, digits = 2), " * pi", 
  ")"
) |> 
  (\(str) {parse(text = str)})() # 数式表示用

# 円周上の点の関係を作図
ggplot() + 
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y, color = factor(fnc_i))) + # 円周
  geom_segment(data = point_df, 
               mapping = aes(x = O_x, y = O_y, xend = x, yend = y, color = factor(fnc_i)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # ベクトル
  geom_segment(data = point_sum_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # ベクトルの和
  scale_color_hue(labels = param_label_vec) + # 凡例表示用
  facet_wrap(graph_label ~ .) + # グラフを分割
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "composite wave", 
       color = "parameter", 
       x = expression(x == O[x] + A[i] ~ cos(a[i]*theta + alpha[i])), 
       y = expression(y == O[y] + A[i] ~ sin(a[i]*theta + alpha[i])))


# 変数と円周上の点の関係 -----------------------------------------------------------

# フレーム数を指定
frame_num <- 100

# パラメータを指定
A_vals     <- c(3, 2, 1)
a_vals     <- c(1.5, -2, 3)
alpha_vals <- c(0, 0, 0.5) * pi

# 関数の数を設定
fnc_num <- length(a_vals)

# 1周期分の変数(ラジアン)を作成
theta_vec <- seq(from = 0, to = 2*pi/min(abs(a_vals)), length.out = 1000)

# 円周上の点用の変数(ラジアン)を作成
theta_vals <- seq(from = min(theta_vec), to = max(theta_vec), length.out = frame_num+1)[1:frame_num]


# 関数ごとの円周上の点の座標を作成
point_each_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  fnc_i   = 1:fnc_num    # 関数番号
) |> # フレームごとにパラメータを複製
  dplyr::mutate(
    theta = theta_vals[frame_i], 
    # パラメータ
    A     = A_vals[fnc_i], 
    a     = a_vals[fnc_i], 
    alpha = alpha_vals[fnc_i], 
    # 原点が中心の円周上
    x = A * cos(a*theta + alpha), 
    y = A * sin(a*theta + alpha)
  ) |> 
  dplyr::mutate(
    # 累積点が中心の円周上
    sum_x = cumsum(x), 
    sum_y = cumsum(y), 
    # 円周の中心(1つ前の累積点)
    O_x   = dplyr::lag(sum_x, n = 1, default = 0), 
    O_y   = dplyr::lag(sum_y, n = 1, default = 0), 
    .by = frame_i
  )

# 作図用に加工
point_df <- dplyr::bind_rows(
  # 関数ごとの点
  point_each_df |> 
    dplyr::select(frame_i, fnc_i, theta, x, y, O_x, O_y) |> 
    dplyr::mutate(O_x = 0, O_y = 0, graph_label = "each"), 
  # 関数の累積和の点
  point_each_df |> 
    dplyr::select(frame_i, fnc_i, theta, x = sum_x, y = sum_y, O_x, O_y) |> 
    dplyr::mutate(graph_label = "sum")
) |> 
  dplyr::mutate(
    graph_label = factor(graph_label, levels = c("each", "sum")) # 配置順を指定
  )

# 関数の総和の円周上の点の座標を作成
point_sum_df <- point_each_df |> 
  dplyr::summarise(
    x = sum(x), 
    y = sum(y), 
    .by = c(frame_i, theta)
  ) |> 
  dplyr::mutate(graph_label = "sum")

# 関数ごとの円周の座標を作成
circle_each_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  fnc_i   = 1:fnc_num, # 関数番号
  theta   = theta_vec
) |> # フレーム・関数ごとに変数を複製
  dplyr::left_join(
    # 円周の中心(1つ前の累積点)
    point_each_df |> 
      dplyr::select(frame_i, fnc_i, O_x, O_y), 
    by = c("frame_i", "fnc_i")
  ) |> 
  dplyr::mutate(
    # パラメータ
    A     = A_vals[fnc_i], 
    a     = a_vals[fnc_i], 
    alpha = alpha_vals[fnc_i], 
    # 原点が中心の円周
    x = A * cos(a*theta + alpha), 
    y = A * sin(a*theta + alpha), 
    # 累積点が中心の円周
    sum_x = O_x + x, 
    sum_y = O_y + y
  )

# 作図用に加工
circle_df <- dplyr::bind_rows(
  # 関数ごとの円周
  circle_each_df |> 
    dplyr::select(frame_i, fnc_i, theta, x, y, O_x, O_y) |> 
    dplyr::mutate(O_x = 0, O_y = 0, graph_label = "each"), # 作図用に値を追加
  # 関数の累積和の円周
  circle_each_df |> 
    dplyr::select(frame_i, fnc_i, theta, x = sum_x, y = sum_y, O_x, O_y) |> 
    dplyr::mutate(graph_label = "sum") # 作図用に値を追加
) |> 
  dplyr::mutate(
    graph_label = factor(graph_label, levels = c("each", "sum")) # 配置順を指定
  )

# グラフサイズを設定
axis_size <- c(
  circle_df[["x"]], circle_df[["y"]]
) |>
  abs() |>
  max() |>
  ceiling()

# ラベル用の文字列を作成
param_label_vec <- paste0(
  "list(", 
  "A[", 1:fnc_num, "] == ", round(A_vals, digits = 2), ", ", 
  "a[", 1:fnc_num, "] == ", round(a_vals, digits = 2), ", ", 
  "alpha[", 1:fnc_num, "] == ", round(alpha_vals/pi, digits = 2), " * pi", 
  ")"
) |> 
  (\(str) {parse(text = str)})() # 数式表示用

# 円周上の点の関係のアニメーションを作図
anim <- ggplot() + 
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y, color = factor(fnc_i))) + # 円周
  geom_segment(data = point_df, 
               mapping = aes(x = O_x, y = O_y, xend = x, yend = y, color = factor(fnc_i)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # ベクトル
  geom_segment(data = point_sum_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # ベクトル和
  geom_text(data = point_df, 
            mapping = aes(x = O_x, y = -Inf, color = factor(fnc_i)), 
            label = "|", size = 4, show.legend = FALSE) + # x軸方向のベクトルの始点用の小細工
  geom_segment(data = point_df, 
               mapping = aes(x = O_x, y = -Inf, xend = x, yend = -Inf, color = factor(fnc_i)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x軸方向のベクトル
  geom_segment(data = point_sum_df, 
               mapping = aes(x = 0, y = -Inf, xend = x, yend = -Inf), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x軸方向のベクトル和
  geom_text(data = point_df, 
            mapping = aes(x = -Inf, y = O_y, color = factor(fnc_i)), 
            label = "|", angle = 90, size = 4, show.legend = FALSE) + # y軸方向のベクトルの始点用の小細工
  geom_segment(data = point_df, 
               mapping = aes(x = -Inf, y = O_y, xend = -Inf, yend = y, color = factor(fnc_i)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y軸方向のベクトル
  geom_segment(data = point_sum_df, 
               mapping = aes(x = -Inf, y = 0, xend = -Inf, yend = y), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y軸方向のベクトル和
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_color_hue(labels = param_label_vec) + # 凡例表示用
  facet_wrap(graph_label ~ .) + # グラフを分割
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "composite wave", 
       color = "parameter", 
       x = expression(x == O[x] + A[i] ~ cos(a[i]*theta + alpha[i])), 
       y = expression(y == O[y] + A[i] ~ sin(a[i]*theta + alpha[i])))

# gif画像を作成
gganimate::animate(plot = anim, nframe = frame_num, fps = 10, width = 800, height = 400)


# 変数と合成波の関係 ---------------------------------------------------------------

# 一時ファイルの書き出し先を指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 100

# 曲線用の変数(ラジアン)の範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)

# 曲線上の点用の変数(ラジアン)を指定
# theta_val <- 1/3 * pi

# 曲線上の点用の変数(ラジアン)を作成
theta_vals <- seq(from = min(theta_vec), to = max(theta_vec), length.out = frame_num+1)[1:frame_num]

# 関数の数を指定
fnc_num <- 5

# パラメータを作成
A_vals     <- 1 / (2 * 1:fnc_num - 1)
a_vals     <- 2 * 1:fnc_num - 1
alpha_vals <- rep(0, times = fnc_num) * pi

# パラメータを指定
A_vals     <- c(3, 2, 1)
a_vals     <- c(1, 2, 3)
alpha_vals <- c(0, 0, 0.5) * pi

# 関数の数を設定
fnc_num <- length(a_vals)


# グラフサイズを設定
axis_size <- 2

# 半周期における目盛数を指定
line_num <- 1

# 目盛ラベルの基準値を指定
theta_lower <- -2*pi

# ラジアン軸目盛ラベル用の値を作成
rad_break_vec <- seq(from = theta_lower, to = ceiling(max(theta_vec)/pi)*pi, by = pi/line_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")

# ラベル用の文字列を作成
param_label_vec <- paste0(
  "list(", 
  "A[", 1:fnc_num, "] == ", round(A_vals, digits = 2), ", ", 
  "a[", 1:fnc_num, "] == ", round(a_vals, digits = 2), ", ", 
  "alpha[", 1:fnc_num, "] == ", round(alpha_vals/pi, digits = 2), " * pi", 
  ")"
) |> 
  (\(str) {parse(text = str)})() # 数式表示用

i <- 30
# 変数ごとにグラフを書き出し
for(i in 1:frame_num) {
  
  # 曲線上の点用の変数を取得
  theta_val <- theta_vals[i]
  
  # 関数ごとの曲線上の点の座標を作成
  point_each_df <- tibble::tibble(
    fnc_i = 1:fnc_num, 
    theta = theta_val, 
    # パラメータ
    A     = A_vals, 
    a     = a_vals, 
    alpha = alpha_vals, 
    # 原点が中心の円周上
    x = A * cos(a*theta + alpha), 
    y = A * sin(a*theta + alpha)
  ) |> 
    dplyr::mutate(
      # 累積点が中心の円周上
      sum_x = cumsum(x), 
      sum_y = cumsum(y), 
      # 円周の中心(1つ前の累積点)
      O_x   = dplyr::lag(sum_x, n = 1, default = 0), 
      O_y   = dplyr::lag(sum_y, n = 1, default = 0)
    )
  
  # 関数の総和の曲線上の点の座標を作成
  point_sum_df <- point_each_df |> 
    dplyr::summarise(
      sum_x = sum(x), 
      sum_y = sum(y), 
      .by = theta
    )
  
  # 関数ごとの曲線の座標を作成
  curve_each_df <- tidyr::expand_grid(
    fnc_i = 1:fnc_num, 
    theta = theta_vec
  ) |> # 関数ごとに変数を複製
    dplyr::mutate(
      # 円周の中心(1つ前の累積点)
      O_x = point_each_df[["O_x"]][fnc_i], 
      O_y = point_each_df[["O_y"]][fnc_i], 
      # パラメータ
      A     = A_vals[fnc_i], 
      a     = a_vals[fnc_i], 
      alpha = alpha_vals[fnc_i], 
      # 原点が中心の円周
      x = A * cos(a*theta + alpha), 
      y = A * sin(a*theta + alpha), 
      # 累積点が中心の円周
      sum_x = O_x + x, 
      sum_y = O_y + y
    )
  
  # 関数の総和の曲線の座標を作成
  curve_sum_df <- curve_each_df |> 
    dplyr::summarise(
      sum_x = sum(x), 
      sum_y = sum(y), 
      .by = theta
    )
  
  
  # # グラフサイズを設定
  # axis_size <- c(
  #   curve_each_df[["sum_x"]], curve_each_df[["sum_y"]]
  # ) |> 
  #   abs() |> 
  #   max() |> 
  #   ceiling()
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "theta == ", round(theta_val/pi, digits = 2), " * pi"
  )
  y_cumsum_label <- paste0(
    "y == (list(", paste0(round(point_each_df[["sum_y"]], digits = 2), collapse = ", "), "))"
  )
  
  # 関数の累積和の円周上の点を作図
  circle_sum_graph <- ggplot() + 
    geom_segment(data = point_each_df, 
                 mapping = aes(x = Inf, y = sum_y, xend = sum_x, yend = sum_y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_path(data = curve_each_df, 
              mapping = aes(x = sum_x, y = sum_y, color = factor(fnc_i))) + # 関数の累積和の円周
    geom_segment(data = point_each_df, 
                 mapping = aes(x = O_x, y = O_y, xend = sum_x, yend = sum_y, color = factor(fnc_i))) + # 関数の累積和の半径線
    geom_point(data = point_each_df, 
               mapping = aes(x = sum_x, y = sum_y, color = factor(fnc_i)), 
               size = 2.5) + # 関数の累積和の点
    geom_point(data = point_sum_df, 
               mapping = aes(x = sum_x, y = sum_y), 
               size = 2.5) + # 関数の総和の点
    geom_text(mapping = aes(x = -Inf, y = Inf), 
              label = var_label, parse = TRUE, hjust = 0, vjust = 1) + # 変数ラベル
    scale_color_hue(labels = param_label_vec) + # 凡例表示用
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "composite wave", 
         subtitle = parse(text = y_cumsum_label), 
         color = "parameter", 
         x = expression(x == x[i-1] + A[i] ~ cos(a[i]*theta + alpha[i])), 
         y = expression(y == y[i-1] + A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # 関数ごとの円周上の点を作図
  circle_each_graph <- ggplot() + 
    geom_segment(data = point_each_df, 
                 mapping = aes(x = Inf, y = y, xend = x, yend = y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_path(data = curve_each_df, 
              mapping = aes(x = x, y = y, color = factor(fnc_i))) + # 関数ごとの円周
    geom_segment(data = point_each_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, color = factor(fnc_i))) + # 関数ごとの半径線
    geom_point(data = point_each_df, 
               mapping = aes(x = x, y = y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    guides(color = FALSE) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(x = expression(x == A[i] ~ cos(a[i]*theta + alpha[i])), 
         y = expression(y == A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  y_sum_label <- paste0(
    "y == ", round(point_sum_df[["sum_y"]], digits = 2)
  )
  
  # 関数の総和の曲線上の点を作図
  curve_sum_graph <- ggplot() + 
    geom_vline(mapping = aes(xintercept = theta_val), 
               linetype = "dotted") + # ラジアン軸目盛の補助線
    geom_hline(data = point_each_df, 
               mapping = aes(yintercept = sum_y, color = factor(fnc_i)), 
               linetype = "dotted") + # y軸目盛の補助線
    geom_line(data = curve_each_df, 
              mapping = aes(x = theta, y = sum_y, color = factor(fnc_i)), 
              linetype = "dashed") + # 関数ごとの曲線
    geom_line(data = curve_sum_df, 
              mapping = aes(x = theta, y = sum_y)) + # 関数の総和の曲線
    geom_point(data = point_each_df, 
               mapping = aes(x = theta, y = sum_y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    geom_point(data = point_sum_df, 
               mapping = aes(x = theta, y = sum_y), 
               size = 2.5) + # 関数の総和の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
    guides(color = FALSE) + 
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(subtitle = parse(text = y_sum_label), 
         x = expression(theta), 
         y = expression(sum({}, i==1, n) ~ A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  y_each_label <- paste0(
    "y == (list(", paste0(round(point_each_df[["y"]], digits = 2), collapse = ", "), "))"
  )
  
  # 関数ごとの曲線上の点を作図
  curve_each_graph <- ggplot() + 
    geom_vline(mapping = aes(xintercept = theta_val), 
               linetype = "dotted") + # ラジアン軸目盛の補助線
    geom_segment(data = point_each_df, 
                 mapping = aes(x = -Inf, y = y, xend = theta_val, yend = y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_line(data = curve_each_df, 
              mapping = aes(x = theta, y = y, color = factor(fnc_i))) + # 関数ごとの曲線
    geom_point(data = point_each_df, 
               mapping = aes(x = theta, y = y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
    guides(color = FALSE) + 
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(subtitle = parse(text = y_each_label), 
         x = expression(theta), 
         y = expression(A[i] ~ sin(a[i]*theta + alpha[i])))
  
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_sum_graph,  curve_sum_graph, 
    circle_each_graph, curve_each_graph, 
    nrow = 2, ncol = 2, guides = "collect"
  ) & 
    theme(legend.position = "right")
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1500, height = 900, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/composite/curves_rectangle_t.gif", delay = 1/10) -> tmp_path # gifファイル書出


# 関数の数と合成波の関係 ---------------------------------------------------------------

# 一時ファイルの書き出し先を指定
dir_path <- "circular/figure/tmp_folder"


# 関数の最大数(フレーム数)を指定
fnc_num_max <- 10

# 曲線上の点用の変数(ラジアン)を指定
theta_val <- 1/4 * pi

# 曲線用の変数(ラジアン)の範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)

# パラメータを作成
A_vals     <- 1 / (2 * 1:fnc_num_max - 1)
a_vals     <- 2 * 1:fnc_num_max - 1
alpha_vals <- rep(0, times = fnc_num_max) * pi


# グラフサイズを設定
axis_size <- 2

# 半周期における目盛数を指定
line_num <- 1

# 目盛ラベルの基準値を指定
theta_lower <- -2*pi

# ラジアン軸目盛ラベル用の値を作成
rad_break_vec <- seq(from = theta_lower, to = ceiling(max(theta_vec)/pi)*pi, by = pi/line_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")

# ラベル用の文字列を作成
param_label_vec <- paste0(
  "list(", 
  "A[", 1:fnc_num_max, "] == ", round(A_vals, digits = 2), ", ", 
  "a[", 1:fnc_num_max, "] == ", round(a_vals, digits = 2), ", ", 
  "alpha[", 1:fnc_num_max, "] == ", round(alpha_vals/pi, digits = 2), " * pi", 
  ")"
) |> 
  (\(str) {parse(text = str)})() # 数式表示用


# フレーム数を設定
frame_num <- fnc_num_max

i <- 3
# 関数の数ごとにグラフを書き出し
for(i in 1:frame_num) {
  
  # 関数の数を設定
  fnc_num <- i
  
  # 関数ごとの曲線上の点の座標を作成
  point_each_df <- tibble::tibble(
    fnc_i = 1:fnc_num, 
    theta = theta_val, 
    # パラメータ
    A     = A_vals[1:fnc_num], 
    a     = a_vals[1:fnc_num], 
    alpha = alpha_vals[1:fnc_num], 
    # 原点が中心の円周上
    x = A * cos(a*theta + alpha), 
    y = A * sin(a*theta + alpha)
  ) |> 
    dplyr::mutate(
      # 累積点が中心の円周上
      sum_x = cumsum(x), 
      sum_y = cumsum(y), 
      # 円周の中心(1つ前の累積点)
      O_x   = dplyr::lag(sum_x, n = 1, default = 0), 
      O_y   = dplyr::lag(sum_y, n = 1, default = 0)
    )
  
  # 関数の総和の曲線上の点の座標を作成
  point_sum_df <- point_each_df |> 
    dplyr::summarise(
      sum_x = sum(x), 
      sum_y = sum(y), 
      .by = theta
    )
  
  # 関数ごとの曲線の座標を作成
  curve_each_df <- tidyr::expand_grid(
    fnc_i = 1:fnc_num, 
    theta = theta_vec
  ) |> # 関数ごとに変数を複製
    dplyr::mutate(
      # 円周の中心(1つ前の累積点)
      O_x = point_each_df[["O_x"]][fnc_i], 
      O_y = point_each_df[["O_y"]][fnc_i], 
      # パラメータ
      A     = A_vals[fnc_i], 
      a     = a_vals[fnc_i], 
      alpha = alpha_vals[fnc_i], 
      # 原点が中心の円周
      x = A * cos(a*theta + alpha), 
      y = A * sin(a*theta + alpha), 
      # 累積点が中心の円周
      sum_x = O_x + x, 
      sum_y = O_y + y
    )
  
  # 関数の総和の曲線の座標を作成
  curve_sum_df <- curve_each_df |> 
    dplyr::summarise(
      sum_x = sum(x), 
      sum_y = sum(y), 
      .by = theta
    )
  
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "theta == ", round(theta_val/pi, digits = 2), " * pi"
  )
  y_cumsum_label <- paste0(
    "y == (list(", paste0(round(point_each_df[["sum_y"]], digits = 2), collapse = ", "), "))"
  )
  
  # 関数の累積和の円周上の点を作図
  circle_sum_graph <- ggplot() + 
    geom_point(mapping = aes(x = -Inf, y = -Inf, color = factor(1:fnc_num_max)), alpha = 0) + # 凡例表示用の小細工
    geom_segment(data = point_each_df, 
                 mapping = aes(x = Inf, y = sum_y, xend = sum_x, yend = sum_y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_path(data = curve_each_df, 
              mapping = aes(x = sum_x, y = sum_y, color = factor(fnc_i))) + # 関数の累積和の円周
    geom_segment(data = point_each_df, 
                 mapping = aes(x = O_x, y = O_y, xend = sum_x, yend = sum_y, color = factor(fnc_i))) + # 関数の累積和の半径線
    geom_point(data = point_each_df, 
               mapping = aes(x = sum_x, y = sum_y, color = factor(fnc_i)), 
               size = 2.5) + # 関数の累積和の点
    geom_point(data = point_sum_df, 
               mapping = aes(x = sum_x, y = sum_y), 
               size = 2.5) + # 関数の総和の点
    geom_text(mapping = aes(x = -Inf, y = Inf), 
              label = var_label, parse = TRUE, hjust = 0, vjust = 1) + # 変数ラベル
    scale_color_manual(breaks = 1:fnc_num_max, 
                       values = scales::hue_pal()(n = fnc_num_max), 
                       labels = param_label_vec) + # 凡例表示用
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "composite wave", 
         subtitle = parse(text = y_cumsum_label), 
         color = "parameter", 
         x = expression(x == x[i-1] + A[i] ~ cos(a[i]*theta + alpha[i])), 
         y = expression(y == y[i-1] + A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # 関数ごとの円周上の点を作図
  circle_each_graph <- ggplot() + 
    geom_segment(data = point_each_df, 
                 mapping = aes(x = Inf, y = y, xend = x, yend = y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_path(data = curve_each_df, 
              mapping = aes(x = x, y = y, color = factor(fnc_i))) + # 関数ごとの円周
    geom_segment(data = point_each_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, color = factor(fnc_i))) + # 関数ごとの半径線
    geom_point(data = point_each_df, 
               mapping = aes(x = x, y = y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    scale_color_manual(breaks = 1:fnc_num_max, 
                       values = scales::hue_pal()(n = fnc_num_max), 
                       labels = param_label_vec) + # 凡例表示用
    guides(color = FALSE) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(x = expression(x == A[i] ~ cos(a[i]*theta + alpha[i])), 
         y = expression(y == A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  y_sum_label <- paste0(
    "y == ", round(point_sum_df[["sum_y"]], digits = 2)
  )
  
  # 関数の総和の曲線上の点を作図
  curve_sum_graph <- ggplot() + 
    geom_vline(mapping = aes(xintercept = theta_val), 
               linetype = "dotted") + # ラジアン軸目盛の補助線
    geom_hline(data = point_each_df, 
               mapping = aes(yintercept = sum_y, color = factor(fnc_i)), 
               linetype = "dotted") + # y軸目盛の補助線
    geom_line(data = curve_each_df, 
              mapping = aes(x = theta, y = sum_y, color = factor(fnc_i)), 
              linetype = "dashed") + # 関数ごとの曲線
    geom_line(data = curve_sum_df, 
              mapping = aes(x = theta, y = sum_y)) + # 関数の総和の曲線
    geom_point(data = point_each_df, 
               mapping = aes(x = theta, y = sum_y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    geom_point(data = point_sum_df, 
               mapping = aes(x = theta, y = sum_y), 
               size = 2.5) + # 関数の総和の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
    scale_color_manual(breaks = 1:fnc_num_max, 
                       values = scales::hue_pal()(n = fnc_num_max), 
                       labels = param_label_vec) + # 凡例表示用
    guides(color = FALSE) + 
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(subtitle = parse(text = y_sum_label), 
         x = expression(theta), 
         y = expression(sum({}, i==1, n) ~ A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  y_each_label <- paste0(
    "y == (list(", paste0(round(point_each_df[["y"]], digits = 2), collapse = ", "), "))"
  )
  
  # 関数ごとの曲線上の点を作図
  curve_each_graph <- ggplot() + 
    geom_vline(mapping = aes(xintercept = theta_val), 
               linetype = "dotted") + # ラジアン軸目盛の補助線
    geom_segment(data = point_each_df, 
                 mapping = aes(x = -Inf, y = y, xend = theta_val, yend = y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_line(data = curve_each_df, 
              mapping = aes(x = theta, y = y, color = factor(fnc_i))) + # 関数ごとの曲線
    geom_point(data = point_each_df, 
               mapping = aes(x = theta, y = y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
    scale_color_manual(breaks = 1:fnc_num_max, 
                       values = scales::hue_pal()(n = fnc_num_max), 
                       labels = param_label_vec) + # 凡例表示用
    guides(color = FALSE) + 
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(subtitle = parse(text = y_each_label), 
         x = expression(theta), 
         y = expression(A[i] ~ sin(a[i]*theta + alpha[i])))
  
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_sum_graph,  curve_sum_graph, 
    circle_each_graph, curve_each_graph, 
    nrow = 2, ncol = 2, guides = "collect"
  ) & 
    theme(legend.position = "right")
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1500, height = 900, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/composite/curves_rectangle_n.gif", delay = 1) -> tmp_path # gifファイル書出


