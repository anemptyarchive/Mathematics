
# 波の重ね合わせ -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 曲線の作図 -------------------------------------------------------------------

# パラメータを指定
A_vals     <- c(3, 2, 1)
a_vals     <- c(1.5, -2, 3)
alpha_vals <- c(0, 0, 0.5) * pi

# 関数の数を設定
fnc_num <- length(a_vals)

# 変数(ラジアン)の範囲を指定
theta_vec <- seq(from = -4*pi, to = 4*pi, length.out = 1000)


# 関数ごとの曲線の座標を作成
curve_each_df <- tidyr::expand_grid(
  fnc_i = 1:fnc_num, # 関数番号
  theta = theta_vec
) |> # 関数ごとにラジアンを複製
  tibble::tibble(
  A     = A_vals[fnc_i], 
  a     = a_vals[fnc_i], 
  alpha = alpha_vals[fnc_i], 
  f_t   = A * sin(a * theta + alpha)
)

# 関数の総和の曲線の座標を作成
curve_sum_df <- curve_each_df |> 
  dplyr::summarise(
    f_t = sum(f_t), .by = theta
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
param_label_vec <- paste0(
  "list(", 
  "A[", 1:fnc_num, "] == ", round(A_vals, digits = 2), ", ", 
  "a[", 1:fnc_num, "] == ", round(a_vals, digits = 2), ", ", 
  "alpha[", 1:fnc_num, "] == ", round(alpha_vals/pi, digits = 2), " * pi", 
  ")"
)

# sin関数の合成波を作図
ggplot() + 
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)),
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_line(data = curve_each_df, 
            mapping = aes(x = theta, y = f_t, color = factor(fnc_i))) + # 関数ごとの曲線
  geom_line(data = curve_sum_df, 
            mapping = aes(x = theta, y = f_t), 
            linewidth = 1) + # 関数の総和の曲線
  scale_color_hue(labels = parse(text = param_label_vec), name = "parameter") + # 凡例表示用
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  theme(legend.text.align = 0) + # 図の体裁
  coord_fixed(ratio = 1) + # 描画領域
  labs(title = "sine function: composite wave", 
       subtitle = expression(f(theta) == sum({}, i==1, n) * A[i]~sin(a[i]*theta + alpha[i])), 
       x = expression(theta), 
       y = expression(f(theta)))


# 関数(パラメータ)と曲線の形状の関係 ------------------------------------------------------------------

# フレーム数を指定
frame_num <- 101

# パラメータの範囲を指定
A_vals_mat <- cbind(
  seq(from = -1, to = 2, length.out = frame_num), 
  seq(from = 2, to = 2, length.out = frame_num), 
  seq(from = -4, to = -4, length.out = frame_num)
)
a_vals_mat <- cbind(
  seq(from = -2, to = 4, length.out = frame_num), 
  seq(from = -2, to = -2, length.out = frame_num), 
  seq(from = 2, to = 1, length.out = frame_num)
)
alpha_vals <- c(0, 0, 0.5) * pi

# 関数の数を設定
fnc_num <- length(alpha_vals)

# 変数(ラジアン)の範囲を指定
theta_vec <- seq(from = -4*pi, to = 4*pi, length.out = 1000)


# 関数ごとの曲線の座標を作成
anim_curve_each_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  fnc_i   = 1:fnc_num, # 関数番号
  theta   = theta_vec
) |> # 関数ごとにラジアンを複製
  dplyr::mutate(
    A     = as.numeric(A_vals_mat[frame_i, fnc_i]), 
    a     = as.numeric(a_vals_mat[frame_i, fnc_i]), 
    alpha = alpha_vals[fnc_i], 
    f_t   = A * sin(a * theta + alpha), 
    .by = c(frame_i, fnc_i, theta)
  )

# 関数の総和の曲線の座標を作成
anim_curve_sum_df <- anim_curve_each_df |> 
  dplyr::summarise(
    f_t = sum(f_t), .by = c(frame_i, theta)
  )


# 範囲πにおける目盛数を指定
tick_num <- 1

# 目盛ラベルの範囲を設定:(π単位で切り捨て・切り上げ)
theta_lower <- floor(min(theta_vec) / pi) * pi
theta_upper <- ceiling(max(theta_vec) / pi) * pi

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = theta_lower, to = theta_upper, by = pi/tick_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# sin関数の合成波のアニメーションを作図
anim <- ggplot() + 
  geom_line(data = anim_curve_each_df, 
            mapping = aes(x = theta, y = f_t, color = factor(fnc_i))) + # 関数ごとの曲線
  geom_line(data = anim_curve_sum_df, 
            mapping = aes(x = theta, y = f_t), 
            linewidth = 1) + # 関数の総和の曲線
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_color_hue(labels = 1:fnc_num, name = "function") + # 凡例表示用
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1) + 
  labs(title = "sine function: composite wave", 
       subtitle = expression(f(theta) == sum({}, i==1, n) * A[i]~sin(a[i]*theta + alpha[i])), 
       x = expression(theta), 
       y = expression(f(theta)))

# gif画像を作成
gganimate::animate(plot = anim, nframe = frame_num, fps = 10, width = 800, height = 600)


# 変数と円周上の点の関係 -----------------------------------------------------------

# フレーム数を指定
frame_num <- 200

# パラメータを指定
A_vals     <- c(3, 2, 1)
a_vals     <- c(1.5, -2, 3)
alpha_vals <- c(0, 0, 0.5) * pi

# 関数の数を設定
fnc_num <- length(a_vals)

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]


# 円周上の点用の値を作成
anim_point_each_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  fnc_i   = 1:fnc_num    # 関数番号
) |> # フレームごとにパラメータを複製
  dplyr::mutate(
    theta = theta_vals[frame_i], 
    # パラメータ
    A     = A_vals[fnc_i], 
    a     = a_vals[fnc_i], 
    alpha = alpha_vals[fnc_i], 
    # 原点が中心の円周上の点
    x = A * cos(a*theta + alpha), 
    y = A * sin(a*theta + alpha)
  ) |> 
  dplyr::mutate(
    # 累積点が中心の円周上の点
    cumsum_x = cumsum(x), 
    cumsum_y = cumsum(y), 
    # 円周の中心(1つ前の累積点)
    O_x = dplyr::lag(cumsum_x, n = 1, default = 0), 
    O_y = dplyr::lag(cumsum_y, n = 1, default = 0), 
    .by = frame_i
  )

# 配置順を指定
facet_level_vec <- c("each", "sum")

# 関数ごとの円周上の点の座標を作成
anim_point_df <- dplyr::bind_rows(
  # 関数ごとの点
  anim_point_each_df |> 
    dplyr::select(frame_i, fnc_i, theta, x, y, O_x, O_y) |> 
    dplyr::mutate(
      O_x = 0, 
      O_y = 0, 
      facet_level = "each"
    ), 
  # 関数の累積和ごとの点
  anim_point_each_df |> 
    dplyr::select(frame_i, fnc_i, theta, x = cumsum_x, y = cumsum_y, O_x, O_y) |> 
    dplyr::mutate(
      facet_level = "sum"
    )
) |> 
  dplyr::mutate(
    facet_level = factor(facet_level, levels = facet_level_vec) # 配置順を設定
  )

# 関数の総和の円周上の点の座標を作成
anim_point_sum_df <- anim_point_each_df |> 
  dplyr::summarise(
    sum_x = sum(x), 
    sum_y = sum(y), 
    .by = c(frame_i, theta)
  ) |> 
  dplyr::mutate(
    facet_level = factor("sum", levels = facet_level_vec)
  )

# 円周用の値を作成
anim_circle_each_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  fnc_i   = 1:fnc_num    # 関数番号
) |> # フレームごとにパラメータを複製
  dplyr::reframe(
    theta = seq(from = 0, to = 2*pi/abs(a_vals[fnc_i]), length.out = 361), .by = c(frame_i, fnc_i) # 1周期分のラジアン
  ) |> # 関数ごとにラジアンを作成
  dplyr::left_join(
    # 円周の中心(1つ前の累積点)
    anim_point_each_df |> 
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
    cumsum_x = O_x + x, 
    cumsum_y = O_y + y
  )

# 関数ごとの円周の座標を作成
anim_circle_df <- dplyr::bind_rows(
  # 関数ごとの円周
  anim_circle_each_df |> 
    dplyr::select(frame_i, fnc_i, theta, x, y, O_x, O_y) |> 
    dplyr::mutate(
      O_x = 0, 
      O_y = 0, 
      facet_level = "each"
    ), 
  # 関数の累積和の円周
  anim_circle_each_df |> 
    dplyr::select(frame_i, fnc_i, theta, x = cumsum_x, y = cumsum_y, O_x, O_y) |> 
    dplyr::mutate(
      facet_level = "sum"
    )
) |> 
  dplyr::mutate(
    facet_level = factor(facet_level, levels = facet_level_vec) # 配置順を設定
  )


# グラフサイズを設定
axis_size <- c(
  anim_circle_df[["x"]], anim_circle_df[["y"]]
) |>
  abs() |>
  max() |>
  ceiling()

# ラベル用の文字列を作成
anim_var_label_df <- tibble::tibble(
  frame_i   = 1:frame_num, 
  var_label = paste0("theta == ", round(theta_vals/pi, digits = 2), " * pi"), 
  facet_level = factor(facet_level_vec[1], levels = facet_level_vec)
)
param_label_vec <- paste0(
  "list(", 
  "A[", 1:fnc_num, "] == ", round(A_vals, digits = 2), ", ", 
  "a[", 1:fnc_num, "] == ", round(a_vals, digits = 2), ", ", 
  "alpha[", 1:fnc_num, "] == ", round(alpha_vals/pi, digits = 2), " * pi", 
  ")"
)
fnc_label_vec <- c(
  "each" = "f[i](theta) == A[i] ~ sin(a[i]*theta + alpha[i])", 
  "sum"  = "g[j](theta) == sum(f[i](theta), i==1, j)"
)

# 円周上の点の関係のアニメーションを作図
anim <- ggplot() + 
  geom_path(data = anim_circle_df, 
            mapping = aes(x = x, y = y, color = factor(fnc_i))) + # 円周
  geom_segment(data = anim_point_df, 
               mapping = aes(x = O_x, y = O_y, xend = x, yend = y, color = factor(fnc_i)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # ベクトル
  geom_segment(data = anim_point_sum_df, 
               mapping = aes(x = 0, y = 0, xend = sum_x, yend = sum_y), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # ベクトルの和
  geom_text(data = anim_point_df, 
            mapping = aes(x = O_x, y = -Inf, color = factor(fnc_i)), 
            label = "|", size = 4, show.legend = FALSE) + # x軸方向のベクトルの始点用の小細工
  geom_segment(data = anim_point_df, 
               mapping = aes(x = O_x, y = -Inf, xend = x, yend = -Inf, color = factor(fnc_i)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x軸方向のベクトル
  geom_segment(data = anim_point_sum_df, 
               mapping = aes(x = 0, y = -Inf, xend = sum_x, yend = -Inf), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x軸方向のベクトルの和
  geom_text(data = anim_point_df, 
            mapping = aes(x = -Inf, y = O_y, color = factor(fnc_i)), 
            label = "|", angle = 90, size = 4, show.legend = FALSE) + # y軸方向のベクトルの始点用の小細工
  geom_segment(data = anim_point_df, 
               mapping = aes(x = -Inf, y = O_y, xend = -Inf, yend = y, color = factor(fnc_i)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y軸方向のベクトル
  geom_segment(data = anim_point_sum_df, 
               mapping = aes(x = -Inf, y = 0, xend = -Inf, yend = sum_y), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y軸方向のベクトルの和
  geom_text(data = anim_var_label_df,
            mapping = aes(x = -Inf, y = Inf, label = var_label),
            parse = TRUE, hjust = 0, vjust = -5) + # 変数ラベル:(subtitleの代用)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_color_hue(labels = parse(text = param_label_vec), name = "parameter") + # 凡例表示用
  facet_wrap(facet_level ~ ., 
             labeller = labeller(facet_level = as_labeller(fnc_label_vec, label_parsed))) + # グラフを分割
  theme(legend.text.align = 0,
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha("white", alpha = 0.8))) +
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "vector sum", 
       subtitle = "", # 変数ラベル用の空行
       x = expression(x[i] == x[i-1] + A[i] ~ cos(a[i]*theta + alpha[i])), 
       y = expression(y[i] == y[i-1] + A[i] ~ sin(a[i]*theta + alpha[i])))

# gif画像を作成
gganimate::animate(plot = anim, nframe = frame_num, fps = 10, width = 800, height = 500)


# 変数と円周と曲線の関係 ---------------------------------------------------------------

# 一時ファイルの書き出し先を指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 200

# 曲線用のラジアンの範囲を指定
theta_vec <- seq(from = -2*pi, to = 2*pi, length.out = 1000)

# 点用のラジアンを作成
theta_vals <- seq(from = min(theta_vec), to = max(theta_vec), length.out = frame_num+1)[1:frame_num]

# 関数の数を指定
fnc_num <- 5

# パラメータを作成
A_vals     <- 1 / (2 * 1:fnc_num - 1)
a_vals     <- 2 * 1:fnc_num - 1
alpha_vals <- rep(0, times = fnc_num) * pi

# パラメータを指定
A_vals     <- c(3, 2, 1)
a_vals     <- c(1.5, -2, 3)
alpha_vals <- c(0, 0, 0.5) * pi

# 関数の数を設定
fnc_num <- length(a_vals)


# ラジアン軸目盛用の値を作成
tick_num <- 1
rad_break_vec <- seq(
  from = floor(min(theta_vec) / pi) * pi, 
  to   = ceiling(max(theta_vec) / pi) * pi, 
  by   = pi/tick_num
)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# グラフサイズを指定
axis_size <- 6

# ラベル用の文字列を作成
param_label_vec <- paste0(
  "list(", 
  "A[", 1:fnc_num, "] == ", round(A_vals, digits = 2), ", ", 
  "a[", 1:fnc_num, "] == ", round(a_vals, digits = 2), ", ", 
  "alpha[", 1:fnc_num, "] == ", round(alpha_vals/pi, digits = 2), " * pi", 
  ")"
)

# 変数ごとにグラフを書き出し
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta_val <- theta_vals[i]
  
  # 関数ごとの円周上の点の座標を作成
  point_each_df <- tibble::tibble(
    fnc_i = 1:fnc_num, 
    theta = theta_val, 
    # パラメータ
    A     = A_vals, 
    a     = a_vals, 
    alpha = alpha_vals, 
    # 原点が中心の円周上の点
    x = A * cos(a*theta + alpha), 
    y = A * sin(a*theta + alpha)
  ) |> 
    dplyr::mutate(
      # 累積点が中心の円周上の点
      cumsum_x = cumsum(x), 
      cumsum_y = cumsum(y), 
      # 円周の中心(1つ前の累積点)
      O_x = dplyr::lag(cumsum_x, n = 1, default = 0), 
      O_y = dplyr::lag(cumsum_y, n = 1, default = 0)
    )
  
  # 関数の総和の曲線上の点の座標を作成
  point_sum_df <- point_each_df |> 
    dplyr::summarise(
      sum_x = sum(x), 
      sum_y = sum(y), 
      .by = theta
    )
  
  # 関数ごとの円周の座標を作成
  circle_each_df <- tibble::tibble(
    fnc_i = 1:fnc_num
  ) |> 
    dplyr::reframe(
      theta = seq(from = 0, to = 2*pi/abs(a_vals[fnc_i]), length.out = 361), .by = fnc_i # 1周期分のラジアン
    ) |> # 関数ごとにラジアンを作成
    dplyr::mutate(
      # パラメータ
      A     = A_vals[fnc_i], 
      a     = a_vals[fnc_i], 
      alpha = alpha_vals[fnc_i], 
      # 原点が中心の円周
      x = A * cos(a*theta + alpha), 
      y = A * sin(a*theta + alpha), 
      # 円周の中心(1つ前の累積点)
      O_x = point_each_df[["O_x"]][fnc_i], 
      O_y = point_each_df[["O_y"]][fnc_i], 
      # 累積点が中心の円周
      cumsum_x = O_x + x, 
      cumsum_y = O_y + y
    )
  
  # 関数ごとの曲線の座標を作成
  curve_each_df <- tidyr::expand_grid(
    fnc_i = 1:fnc_num, 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      # パラメータ
      A     = A_vals[fnc_i], 
      a     = a_vals[fnc_i], 
      alpha = alpha_vals[fnc_i], 
      # 原点が中心の円周
      y = A * sin(a*theta + alpha), 
      # 円周の中心(1つ前の累積点)
      O_y = point_each_df[["O_y"]][fnc_i], 
      # 累積点が中心の円周
      cumsum_y = O_y + y
    )
  
  # 関数の総和の曲線の座標を作成
  curve_sum_df <- curve_each_df |> 
    dplyr::summarise(
      sum_y = sum(y), .by = theta
    )
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "theta == ", round(theta_val/pi, digits = 2), " * pi"
  )
  
  # 関数ごとの円周上の点を作図
  circle_each_graph <- ggplot() + 
    geom_segment(data = point_each_df, 
                 mapping = aes(x = Inf, y = y, xend = x, yend = y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_path(data = circle_each_df, 
              mapping = aes(x = x, y = y, color = factor(fnc_i))) + # 関数ごとの円周
    geom_segment(data = point_each_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, color = factor(fnc_i))) + # 関数ごとの半径線
    geom_point(data = point_each_df, 
               mapping = aes(x = x, y = y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    scale_color_hue(labels = parse(text = param_label_vec), name = "parameter") + # 凡例表示用
    theme(legend.text.align = 0,
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) +
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "sine function: composite wave", 
         subtitle = parse(text = var_label), 
         x = expression(x[i] == A[i] ~ cos(a[i]*theta + alpha[i])), 
         y = expression(y[i] == A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  fnc_each_label <- paste0(
    "f[i](theta) == (list(", paste0(round(point_each_df[["y"]], digits = 2), collapse = ", "), "))"
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
    labs(subtitle = parse(text = fnc_each_label), 
         x = expression(theta), 
         y = expression(f[i](theta) == A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  fnc_cumsum_label <- paste0(
    "g[j](theta) == (list(", paste0(round(point_each_df[["cumsum_y"]], digits = 2), collapse = ", "), "))"
  )
  
  # 関数の累積和の円周上の点を作図
  circle_sum_graph <- ggplot() + 
    geom_segment(data = point_each_df, 
                 mapping = aes(x = Inf, y = cumsum_y, xend = cumsum_x, yend = cumsum_y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_path(data = circle_each_df, 
              mapping = aes(x = cumsum_x, y = cumsum_y, color = factor(fnc_i))) + # 関数の累積和の円周
    geom_segment(data = point_each_df, 
                 mapping = aes(x = O_x, y = O_y, xend = cumsum_x, yend = cumsum_y, color = factor(fnc_i))) + # 関数の累積和の半径線
    geom_point(data = point_each_df, 
               mapping = aes(x = cumsum_x, y = cumsum_y, color = factor(fnc_i)), 
               size = 2.5) + # 関数の累積和の点
    geom_point(data = point_sum_df, 
               mapping = aes(x = sum_x, y = sum_y), 
               size = 1.25) + # 関数の総和の点
    guides(color = FALSE) + 
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(subtitle = parse(text = parse(text = fnc_cumsum_label)), 
         x = expression(x[i] == x[i-1] + A[i] ~ cos(a[i]*theta + alpha[i])), 
         y = expression(y[i] == y[i-1] + A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  fnc_sum_label <- paste0(
    "f(theta) == ", round(point_sum_df[["sum_y"]], digits = 2)
  )
  
  # 関数の総和の曲線上の点を作図
  curve_sum_graph <- ggplot() + 
    geom_vline(mapping = aes(xintercept = theta_val), 
               linetype = "dotted") + # ラジアン軸目盛の補助線
    geom_hline(data = point_each_df, 
               mapping = aes(yintercept = cumsum_y, color = factor(fnc_i)), 
               linetype = "dotted") + # y軸目盛の補助線
    geom_line(data = curve_each_df, 
              mapping = aes(x = theta, y = cumsum_y, color = factor(fnc_i)), 
              linetype = "dashed") + # 関数ごとの曲線
    geom_line(data = curve_sum_df, 
              mapping = aes(x = theta, y = sum_y)) + # 関数の総和の曲線
    geom_point(data = point_each_df, 
               mapping = aes(x = theta, y = cumsum_y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    geom_point(data = point_sum_df, 
               mapping = aes(x = theta, y = sum_y), 
               size = 1.25) + # 関数の総和の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
    guides(color = FALSE) + 
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(subtitle = parse(text = parse(text = fnc_sum_label)), 
         x = expression(theta), 
         y = expression(f(theta) == sum({}, i==1, n) ~ A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_each_graph, curve_each_graph, 
    circle_sum_graph,  curve_sum_graph, 
    nrow = 2, ncol = 2#, guides = "collect"
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1000, height = 800, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/sine/composite_curves_variable.gif", delay = 1/10) -> tmp_path # gifファイルを書出


# 関数と円周と曲線の関係 ---------------------------------------------------------------

# 一時ファイルの書き出し先を指定
dir_path <- "circular/figure/tmp_folder"


# 関数の最大数(フレーム数)を指定
fnc_num_max <- 10

# 点用のラジアンを指定
theta_val <- 1/4 * pi

# 曲線用のラジアンの範囲を指定
theta_vec <- seq(from = 0*pi, to = 2*pi, length.out = 1000)

# パラメータを作成
A_vals     <- 1 / (2 * 1:fnc_num_max - 1)
a_vals     <- 2 * 1:fnc_num_max - 1
alpha_vals <- rep(0, times = fnc_num_max) * pi


# ラジアン軸目盛用の値を作成
tick_num <- 2
rad_break_vec <- seq(
  from = floor(min(theta_vec) / pi) * pi, 
  to   = ceiling(max(theta_vec) / pi) * pi, 
  by   = pi/tick_num
)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# フレーム数を設定
frame_num <- fnc_num_max

# グラフサイズを指定
axis_size <- 2

# ラベル用の文字列を作成
param_label_vec <- paste0(
  "list(", 
  "A[", 1:fnc_num_max, "] == ", round(A_vals, digits = 2), ", ", 
  "a[", 1:fnc_num_max, "] == ", round(a_vals, digits = 2), ", ", 
  "alpha[", 1:fnc_num_max, "] == ", round(alpha_vals/pi, digits = 2), " * pi", 
  ")"
)

# 関数の数ごとにグラフを書き出し
for(i in 1:frame_num) {
  
  # 関数の数を設定
  fnc_num <- i
  
  # 関数ごとの円周上の点の座標を作成
  point_each_df <- tibble::tibble(
    fnc_i = 1:fnc_num, 
    theta = theta_val, 
    # パラメータ
    A     = A_vals[1:fnc_num], 
    a     = a_vals[1:fnc_num], 
    alpha = alpha_vals[1:fnc_num], 
    # 原点が中心の円周上の点
    x = A * cos(a*theta + alpha), 
    y = A * sin(a*theta + alpha)
  ) |> 
    dplyr::mutate(
      # 累積点が中心の円周上の点
      cumsum_x = cumsum(x), 
      cumsum_y = cumsum(y), 
      # 円周の中心(1つ前の累積点)
      O_x = dplyr::lag(cumsum_x, n = 1, default = 0), 
      O_y = dplyr::lag(cumsum_y, n = 1, default = 0)
    )
  
  # 関数の総和の曲線上の点の座標を作成
  point_sum_df <- point_each_df |> 
    dplyr::summarise(
      sum_x = sum(x), 
      sum_y = sum(y), 
      .by = theta
    )
  
  # 関数ごとの円周の座標を作成
  circle_each_df <- tibble::tibble(
    fnc_i = 1:fnc_num
  ) |> 
    dplyr::reframe(
      theta = seq(from = 0, to = 2*pi/abs(a_vals[fnc_i]), length.out = 361), .by = fnc_i # 1周期分のラジアン
    ) |> # 関数ごとにラジアンを作成
    dplyr::mutate(
      # パラメータ
      A     = A_vals[fnc_i], 
      a     = a_vals[fnc_i], 
      alpha = alpha_vals[fnc_i], 
      # 原点が中心の円周
      x = A * cos(a*theta + alpha), 
      y = A * sin(a*theta + alpha), 
      # 円周の中心(1つ前の累積点)
      O_x = point_each_df[["O_x"]][fnc_i], 
      O_y = point_each_df[["O_y"]][fnc_i], 
      # 累積点が中心の円周
      cumsum_x = O_x + x, 
      cumsum_y = O_y + y
    )
  
  # 関数ごとの曲線の座標を作成
  curve_each_df <- tidyr::expand_grid(
    fnc_i = 1:fnc_num, 
    theta = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      # パラメータ
      A     = A_vals[fnc_i], 
      a     = a_vals[fnc_i], 
      alpha = alpha_vals[fnc_i], 
      # 原点が中心の円周
      y = A * sin(a*theta + alpha), 
      # 円周の中心(1つ前の累積点)
      O_y = point_each_df[["O_y"]][fnc_i], 
      # 累積点が中心の円周
      cumsum_y = O_y + y
    )
  
  # 関数の総和の曲線の座標を作成
  curve_sum_df <- curve_each_df |> 
    dplyr::summarise(
      sum_y = sum(y), .by = theta
    )
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "theta == ", round(theta_val/pi, digits = 2), " * pi"
  )
  
  # 関数ごとの円周上の点を作図
  circle_each_graph <- ggplot() + 
    geom_point(mapping = aes(x = -Inf, y = -Inf, color = factor(1:fnc_num_max)), alpha = 0) + # 凡例表示用の小細工
    geom_segment(data = point_each_df, 
                 mapping = aes(x = Inf, y = y, xend = x, yend = y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_path(data = circle_each_df, 
              mapping = aes(x = x, y = y, color = factor(fnc_i))) + # 関数ごとの円周
    geom_segment(data = point_each_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, color = factor(fnc_i))) + # 関数ごとの半径線
    geom_point(data = point_each_df, 
               mapping = aes(x = x, y = y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    scale_color_hue(labels = parse(text = param_label_vec), name = "parameter") + # 凡例表示用
    theme(legend.text.align = 0,
          legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) +
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "sine function: composite wave", 
         subtitle = parse(text = var_label), 
         x = expression(x[i] == A[i] ~ cos(a[i]*theta + alpha[i])), 
         y = expression(y[i] == A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  fnc_each_label <- paste0(
    "f[i](theta) == (list(", paste0(round(point_each_df[["y"]], digits = 2), collapse = ", "), "))"
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
    labs(subtitle = parse(text = fnc_each_label), 
         x = expression(theta), 
         y = expression(f[i](theta) == A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  fnc_cumsum_label <- paste0(
    "g[j](theta) == (list(", paste0(round(point_each_df[["cumsum_y"]], digits = 2), collapse = ", "), "))"
  )
  
  # 関数の累積和の円周上の点を作図
  circle_sum_graph <- ggplot() + 
    geom_segment(data = point_each_df, 
                 mapping = aes(x = Inf, y = cumsum_y, xend = cumsum_x, yend = cumsum_y, color = factor(fnc_i)), 
                 linetype = "dotted") + # y軸目盛の補助線
    geom_path(data = circle_each_df, 
              mapping = aes(x = cumsum_x, y = cumsum_y, color = factor(fnc_i))) + # 関数の累積和の円周
    geom_segment(data = point_each_df, 
                 mapping = aes(x = O_x, y = O_y, xend = cumsum_x, yend = cumsum_y, color = factor(fnc_i))) + # 関数の累積和の半径線
    geom_point(data = point_each_df, 
               mapping = aes(x = cumsum_x, y = cumsum_y, color = factor(fnc_i)), 
               size = 2.5) + # 関数の累積和の点
    geom_point(data = point_sum_df, 
               mapping = aes(x = sum_x, y = sum_y), 
               size = 1.25) + # 関数の総和の点
    guides(color = FALSE) + 
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(subtitle = parse(text = parse(text = fnc_cumsum_label)), 
         x = expression(x[i] == x[i-1] + A[i] ~ cos(a[i]*theta + alpha[i])), 
         y = expression(y[i] == y[i-1] + A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # ラベル用の文字列を作成
  fnc_sum_label <- paste0(
    "f(theta) == ", round(point_sum_df[["sum_y"]], digits = 2)
  )
  
  # 関数の総和の曲線上の点を作図
  curve_sum_graph <- ggplot() + 
    geom_vline(mapping = aes(xintercept = theta_val), 
               linetype = "dotted") + # ラジアン軸目盛の補助線
    geom_hline(data = point_each_df, 
               mapping = aes(yintercept = cumsum_y, color = factor(fnc_i)), 
               linetype = "dotted") + # y軸目盛の補助線
    geom_line(data = curve_each_df, 
              mapping = aes(x = theta, y = cumsum_y, color = factor(fnc_i)), 
              linetype = "dashed") + # 関数ごとの曲線
    geom_line(data = curve_sum_df, 
              mapping = aes(x = theta, y = sum_y)) + # 関数の総和の曲線
    geom_point(data = point_each_df, 
               mapping = aes(x = theta, y = cumsum_y, color = factor(fnc_i)), 
               size = 2.5) + # 関数ごとの点
    geom_point(data = point_sum_df, 
               mapping = aes(x = theta, y = sum_y), 
               size = 1.25) + # 関数の総和の点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
    guides(color = FALSE) + 
    theme(legend.text.align = 0) + 
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(subtitle = parse(text = parse(text = fnc_sum_label)), 
         x = expression(theta), 
         y = expression(f(theta) == sum({}, i==1, n) ~ A[i] ~ sin(a[i]*theta + alpha[i])))
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_each_graph, curve_each_graph, 
    circle_sum_graph,  curve_sum_graph, 
    nrow = 2, ncol = 2, guides = "collect"
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1500, height = 1200, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/sine/composite_curves_fnc.gif", delay = 1) -> tmp_path # gifファイル書出


