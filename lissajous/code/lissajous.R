
# リサージュ曲線の可視化 --------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# チェック用
library(ggplot2)


# 曲線の作図 -----------------------------------------------------------------

### ・作図 -----

# パラメータを指定
A     <- 1
a     <- 2
alpha <- 0
B     <- 1
b     <- 3
beta  <- 4/6 * pi

# リサージュ曲線の座標を作成
curve_df <- tibble::tibble(
  theta = seq(from = 0, to = 2*pi, length.out = 1001), # ラジアン
  x     = A * cos(a * theta + alpha), 
  y     = B * sin(b * theta + beta)
)

# ラベル用の文字列を作成
param_label <- paste0(
  "list(", 
  "A == ", A, ", ", 
  "a == ", a, ", ", 
  "alpha == ", round(alpha/pi, digits = 2), "*pi, ", 
  "B == ", B, ", ", 
  "b == ", b, ", ", 
  "beta == ", round(beta/pi, digits = 2), "*pi", 
  ")"
)

# リサージュ曲線を作図
ggplot() + 
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                             xend = c(Inf, 0), yend = c(0, Inf)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_path(data = curve_df, 
            mapping = aes(x = x, y = y), 
            linewidth = 1) + # 曲線
  coord_fixed(ratio = 1) + 
  labs(title = "Lissajous curve", 
       subtitle = parse(text = param_label), 
       x = expression(x == A ~ cos(a * theta + alpha)), 
       y = expression(y == B ~ sin(b * theta + beta)))


### ・変数と座標の関係 -----

# フレーム数を指定
frame_num <- 300

# 曲線上の点の座標を作成
anim_point_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  theta   = seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num], # ラジアン
  x       = A * cos(a * theta + alpha), 
  y       = B * sin(b * theta + beta), 
  theta_label = paste0(
    "list(", 
    "theta == ", round(theta/pi, digits = 2), "*pi, ", 
    "theta*degree == ", round(theta/pi*180, digits = 2), "*degree", 
    ")"
  ) # 角度ラベル
)

# 曲線上の点のアニメーションを作図
anim <- ggplot() + 
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                             xend = c(Inf, 0), yend = c(0, Inf)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_path(data = curve_df, 
            mapping = aes(x = x, y = y, color = theta/pi), 
            linewidth = 1) + # 曲線
  geom_vline(data = anim_point_df, 
             mapping = aes(xintercept = x), 
             color = "blue", linewidth = 1, linetype = "dotted") + # x軸の補助線
  geom_hline(data = anim_point_df, 
             mapping = aes(yintercept = y), 
             color = "red", linewidth = 1, linetype = "dotted") + # y軸の補助線
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = 0), 
             color = "blue", size = 4) + # x軸線上の点
  geom_point(data = anim_point_df, 
             mapping = aes(x = 0, y = y), 
             color = "red", size = 4) + # y軸線上の点
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y), 
             size = 4) + # 曲線上の点
  geom_label(data = anim_point_df, 
             mapping = aes(x = -Inf, y = Inf, label = theta_label), 
             parse = TRUE, hjust = 0, vjust = 1, alpha = 0.5, label.r = unit(0, units = "pt")) + # ラジアンラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  coord_fixed(ratio = 1) + 
  labs(title = "Lissajous curve", 
       subtitle = parse(text = param_label), 
       color = expression(frac(theta, pi)), 
       x = expression(x == A ~ cos(a * theta + alpha)), 
       y = expression(y == B ~ sin(b * theta + beta)))

# gif画像を作成
gganimate::animate(
  plot = anim, nframes = frame_num, fps = 10, 
  width = 600, height = 600
)


# パラメータと形状の関係：2個のパラメータの比較 ---------------------------------

# 固定するパラメータを指定
A     <- 1
B     <- 1
#a     <- 3
#b     <- 2
alpha <- 0
beta  <- 4/6 * pi

# 比較するパラメータを指定
#A_vals     <- seq(from = -2, to = 2, length.out = 9)
#B_vals     <- seq(from = -2, to = 2, length.out = 9)
a_vals     <- seq(from = -2, to = 2, length.out = 9)
b_vals     <- seq(from = -2, to = 2, length.out = 9)
#alpha_vals <- seq(from = 0, to = 0*pi, length.out = 7)
#beta_vals  <- seq(from = 0, to = 2*pi, length.out = 5)

# リサージュ曲線の座標を作成
curve_df <- tidyr::expand_grid(
  # A = A_vals, 
  # B = B_vals, 
  a = a_vals, 
  b = b_vals, 
  # alpha = alpha_vals, 
  # beta  = beta_vals, 
  theta = seq(from = 0, to = 2*pi, length.out = 1001)
) |> # パラメータの組み合わせごとにラジアンを複製
  dplyr::mutate(
    x = A * cos(a * theta + alpha), 
    y = B * sin(b * theta + beta)
  )

# ラベル用の文字列を作成
param_label <- paste0(
  "list(", 
  "A == ", A, ", ", 
  "B == ", B, ", ", 
  # "a == ", a, ", ", 
  # "b == ", b, ", ", 
  "alpha == ", round(alpha/pi, digits = 2), "*pi, ", 
  "beta == ", round(beta/pi, digits = 2), "*pi", 
  ")"
)

# リサージュ曲線を作図
ggplot() + 
  geom_path(data = curve_df, 
            mapping = aes(x = x, y = y, color = theta/pi), 
            linewidth = 1) + # 曲線
  # facet_grid(B ~ A, labeller = label_bquote(rows = B == .(round(B, 2)), 
  #                                           cols = A == .(round(A, 2)))) + # パラメータごとに分割
  facet_grid(b ~ a, labeller = label_bquote(rows = b == .(round(b, 2)), 
                                            cols = a == .(round(a, 2)))) + # パラメータごとに分割
  # facet_grid(beta ~ alpha, labeller = label_bquote(rows = beta == .(round(beta/pi, 2)) * pi, 
  #                                                  cols = alpha == .(round(alpha/pi, 2)) * pi)) + # パラメータごとに分割
  coord_fixed(ratio = 1) + 
  labs(title = "Lissajous curve", 
       subtitle = parse(text = param_label), 
       color = expression(frac(theta, pi)), 
       x = expression(x == A ~ cos(a * theta + alpha)), 
       y = expression(y == B ~ sin(b * theta + beta)))


# パラメータと形状・変数と座標の関係 ---------------------------------

# フレーム数を指定
frame_num <- 301

# 固定するパラメータを指定
A     <- 1
B     <- 1
#a     <- 2
b     <- 3
alpha <- 4/6 * pi
#beta  <- 0/6 * pi

# 比較するパラメータを指定
#A_vals     <- seq(from = -2, to = 2, length.out = 9)
#B_vals     <- seq(from = -2, to = 2, length.out = 9)
a_vals     <- seq(from = -1, to = 4, length.out = 11)
#b_vals     <- seq(from = -2, to = 2, length.out = 9)
#alpha_vals <- seq(from = 0, to = 2*pi, length.out = 13)
beta_vals  <- seq(from = 0, to = 2*pi, length.out = 7)

# 曲線用のラジアンを作成
theta_vec <- seq(from = 0, to = 2*pi, length.out = frame_num)

# リサージュ曲線の座標を作成
anim_curve_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  a = a_vals, # (パラメータを指定)
  beta = beta_vals # (パラメータを指定)
) |> # フレームごとにパラメータの組み合わせを複製
  dplyr::reframe(
    point_i = 1:frame_i, .by = dplyr::everything()
  ) |> # 各フレームまでの座標を複製
  dplyr::mutate(
    theta = theta_vec[point_i], 
    x     = A * cos(a * theta + alpha), 
    y     = B * sin(b * theta + beta)
  )

# リサージュ曲線上の点の座標を作成
anim_point_df <- anim_curve_df |> 
  dplyr::filter(point_i == max(point_i), .by = frame_i) # 各フレームの座標を抽出


# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  a = min(a_vals), # (パラメータを指定)
  beta = min(beta_vals), # (パラメータを指定)
  theta   = theta_vec, 
  var_label = paste0(
    "list(", 
    "A == ", A, ", ", 
    "B == ", B, ", ", 
    #"a == ", a, ", ", 
    "b == ", b, ", ", 
    "alpha == ", round(alpha/pi, digits = 2), "*pi, ", 
    #"beta == ", round(beta/pi, digits = 2), "*pi, ", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "theta*degree == ", round(theta/pi*180, digits = 1), "*degree", 
    ")"
  ) # ラジアンラベル
)

# リサージュ曲線のアニメーションを作図
anim <- ggplot() + 
  geom_path(data = anim_curve_df, 
            mapping = aes(x = x, y = y, color = theta/pi), 
            linewidth = 0.5) + # 曲線
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y), 
             size = 4) + # 曲線上の点
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -2) + # ラジアンラベル
  gganimate::transition_manual(frame = frame_i) + # フレーム切替
  facet_grid(beta ~ a, labeller = label_bquote(rows = b == .(round(beta/pi, 2)) * pi, 
                                               cols = a == .(round(a, 2)))) + # パラメータごとに分割
  coord_fixed(ratio = 1, clip = "off") + 
  labs(title = "Lissajous curve", 
       subtitle = "", # (ラジアンラベルの表示用)
       color = expression(frac(theta, pi)), 
       x = expression(x == A ~ cos(a * theta + alpha)), 
       y = expression(y == B ~ sin(b * theta + beta)))

# 一時停止フレーム数を指定
ep <- 30

# gif画像を作成
gganimate::animate(
  plot = anim, nframes = frame_num+ep, end_pause = ep, fps = 30, 
  width = 1200, height = 900
)


## (変数ラベルを表示しなければtransition_reveal()を使ってシンプルに実装できる)


# パラメータと形状の関係：n個のパラメータの影響 ------------------------------------------------------------

# フレーム数を指定
frame_num <- 301

# パラメータを指定
A_vals     <- seq(from = 2, to = 2, length.out = frame_num)
a_vals     <- seq(from = 0, to = 4, length.out = frame_num)
alpha_vals <- seq(from = 0*pi, to = 0*pi, length.out = frame_num)
B_vals     <- seq(from = 0.5, to = 1.5, length.out = frame_num)
b_vals     <- seq(from = 1, to = 1, length.out = frame_num)
beta_vals  <- seq(from = 0*pi, to = 2*pi, length.out = frame_num)

# リサージュ曲線の座標を作成
anim_curve_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  theta   = seq(from = 0, to = 2*pi, length.out = 1001)
) |> # フレームごとにラジアンを複製
  dplyr::mutate(
    A     = A_vals[frame_i], 
    a     = a_vals[frame_i], 
    alpha = alpha_vals[frame_i], 
    B     = B_vals[frame_i], 
    b     = b_vals[frame_i], 
    beta  = beta_vals[frame_i], 
    x     = A * cos(a * theta + alpha), 
    y     = B * sin(b * theta + beta)
  )


# 点数を指定
point_num <- 9
point_num <- 13

# 点用のラジアンを作成
theta_vals <- seq(from = 0, to = 2*pi, length.out = point_num)

# 曲線上の点の座標を作成
anim_point_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  theta   = theta_vals, 
) |> # フレームごとにラジアンを複製
  dplyr::mutate(
    A     = A_vals[frame_i], 
    a     = a_vals[frame_i], 
    alpha = alpha_vals[frame_i], 
    B     = B_vals[frame_i], 
    b     = b_vals[frame_i], 
    beta  = beta_vals[frame_i], 
    x     = A * cos(a * theta + alpha), 
    y     = B * sin(b * theta + beta)
  )


# x軸目盛ラベル用の文字列を作成:(A = 1で固定)
axis_x_df <- tibble::tibble(
  theta = theta_vals, 
  cos_t = round(cos(theta), digits = 5), # (数値誤差対策)
  rad_label = paste0(round(theta/pi, digits = 2), "*pi"),        # 弧度法の角度ラベル
  deg_label = paste0(round(theta/pi*180, digits = 1), "*degree") # 度数法の角度ラベル
) |> 
  dplyr::summarise(
    rad_label = paste(rad_label, collapse = ", "), 
    deg_label = paste(deg_label, collapse = ", "), 
    .by = cos_t
  ) |> # 同じ角度のラベルを結合
  dplyr::mutate(
    rad_label = paste0("list(", rad_label, ")"), 
    deg_label = paste0("list(", deg_label, ")")
  ) # expression記法に整形

# y軸目盛ラベル用の文字列を作成:(B = 1で固定)
axis_y_df <- tibble::tibble(
  theta = theta_vals, 
  sin_t = round(sin(theta), digits = 5), # (数値誤差対策)
  rad_label = paste0(round(theta/pi, digits = 2), "*pi"),        # 弧度法の角度ラベル
  deg_label = paste0(round(theta/pi*180, digits = 1), "*degree") # 度数法の角度ラベル
) |> 
  dplyr::summarise(
    rad_label = paste(rad_label, collapse = ", "), 
    deg_label = paste(deg_label, collapse = ", "), 
    .by = sin_t
  ) |> # 同じ角度のラベルを結合
  dplyr::mutate(
    rad_label = paste0("list(", rad_label, ")"), 
    deg_label = paste0("list(", deg_label, ")")
  ) # expression記法に整形

# パラメータラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  A     = A_vals, 
  a     = a_vals, 
  alpha = alpha_vals, 
  B     = B_vals, 
  b     = b_vals, 
  beta  = beta_vals, 
  param_label = paste0(
    "list(", 
    "A == ", round(A, digits = 2), ", ", 
    "a == ", round(a, digits = 2), ", ", 
    "alpha == ", round(alpha/pi, digits = 2), " * pi, ", 
    "B == ", round(B, digits = 2), ", ", 
    "b == ", round(b, digits = 2), ", ", 
    "beta == ", round(beta/pi, digits = 2), " * pi", 
    ")"
  ) # パラメータラベル
)


# リサージュ曲線のアニメーションを作図
anim <- ggplot() + 
  geom_vline(data = axis_x_df, 
             mapping = aes(xintercept = cos_t), 
             linetype = "dotted") + # x軸の角度目盛線
  geom_hline(data = axis_y_df, 
             mapping = aes(yintercept = sin_t), 
             linetype = "dotted") + # y軸の角度目盛線
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                             xend = c(Inf, 0), yend = c(0, Inf)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_path(data = anim_curve_df, 
            mapping = aes(x = x, y = y, color = theta/pi), 
            linewidth = 1) + # 曲線
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y, color = theta/pi), 
             size = 4) + # 曲線上の点
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = param_label), 
            parse = TRUE, hjust = 0, vjust = -5.5) + # パラメータラベル:(角度目盛ラベルを表示しない場合はvjust = -0.5)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_x_continuous(sec.axis = sec_axis(trans = ~., 
                                         breaks = axis_x_df[["cos_t"]], 
                                         labels = parse(text = axis_x_df[["rad_label"]]), 
                                         name = expression(a * theta + alpha))) + # x軸の角度目盛ラベル
  scale_y_continuous(sec.axis = sec_axis(trans = ~., 
                                         breaks = axis_y_df[["sin_t"]], 
                                         labels = parse(text = axis_y_df[["rad_label"]]), 
                                         name = expression(b * theta + beta))) + # y軸の角度目盛ラベル
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0)) + 
  coord_fixed(ratio = 1, clip = "off") + 
  labs(title = "Lissajous curve", 
       subtitle = "", # (パラメータラベルの表示用)
       color = expression(frac(theta, pi)), 
       x = expression(x == A ~ cos(a * theta + alpha)), 
       y = expression(y == B ~ sin(b * theta + beta)))

# gif画像を作成
gganimate::animate(
  plot = anim, nframes = frame_num, fps = 30, 
  width = 600, height = 600
)


# 変数とリサージュ曲線とsin・cos曲線の関係 ----------------------------------------------

# 一時保存フォルダを指定
dir_path <- "lissajous/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# 点用のラジアンを作成
theta_vals <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]


# パラメータを指定
A     <- 2.5
a     <- 1.5
alpha <- 1/4 * pi
B     <- 1.5
b     <- 4
beta  <- 0/6 * pi

# 曲線の座標を作成
curve_df <- tibble::tibble(
  theta = seq(from = 0, to = 2*pi, length.out = 1001), 
  x     = A * cos(a * theta + alpha), 
  y     = B * sin(b * theta + beta)
)


# グリッド線の値を設定
axis_x_size <- 2.5 # x軸目盛の最大値
step_val    <- 0.5 # グリッド線の間隔

# 軸変換図のグリッド線の座標を作成
adapt_grid_df <- tidyr::expand_grid(
  r = seq(from = step_val, to = 2*A, by = step_val), 
  t = seq(from = pi, to = 1.5*pi, length.out = 100)
) |> # グリッド線の数に応じてラジアンを複製
  dplyr::mutate(
    x = axis_x_size + r * cos(t), 
    y = axis_x_size + r * sin(t)
  )


# 目盛ラベル用の文字列を作成
rad_break_vec <- 0:12 / 6 * pi
rad_label_vec <- paste0("frac(", 0:12, ", 6)~pi")

# ラジアンごとにグラフを書き出し
for(i in 1:frame_num) {
  
  # i番目のラジアンを取得
  theta <- theta_vals[i]
  
  # 曲線上の点の座標を作成
  point_df <- tibble::tibble(
    theta = theta, 
    x     = A * cos(a * theta + alpha), 
    y     = B * sin(b * theta + beta)
  )
  
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "list(", 
    "theta == ", round(theta/pi, digits = 2), "*pi, ", 
    "theta*degree == ", round(theta/pi*180, digits = 2), "*degree", 
    ")"
  )

  # リサージュ曲線を作図
  lissajous_graph  <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = curve_df, 
              mapping = aes(x = x, y = y,), 
              linewidth = 1) + # 曲線
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
    coord_fixed(ratio = 1, xlim = c(-A, A), ylim = c(-B, B)) + 
    labs(title = "Lissajous curve", 
         subtitle = parse(text = var_label), 
         x = expression(x == A ~ cos(a * theta + alpha)), 
         y = expression(y == B ~ sin(b * theta + beta)))
  

  # ラベル用の文字列を作成
  sin_label <- paste0(
    B, " * sin(", b, " * theta + ", round(beta/pi, digits = 2), "*pi)", 
    " == ", round(point_df[["y"]], digits = 2)
  )
  
  # sin曲線を作図
  sin_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_line(data = curve_df, 
              mapping = aes(x = theta, y = y), 
              linewidth = 1) + # 曲線
    geom_vline(mapping = aes(xintercept = theta), 
               linewidth = 1, linetype = "dotted") + # ラジアン軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y), 
               color = "red", linewidth = 1, linetype = "dotted") + # y軸の補助線
    geom_point(data = point_df, 
               mapping = aes(x = theta, y = y), 
               color = "red", size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # ラジアン目盛ラベル
    coord_fixed(ratio = 1, ylim = c(-B, B)) + 
    labs(title = "sine curve", 
         subtitle = parse(text = sin_label), 
         x = expression(theta), 
         y = expression(B ~ sin(b * theta + beta)))
  

  # 軸変換曲線の座標を作成
  adapt_line_df <- tibble::tibble(
    t = seq(from = pi, to = 1.5*pi, length.out = 100), 
    x = A + abs(A * cos(a * theta + alpha) - A) * cos(t), 
    y = A + abs(A * cos(a * theta + alpha) - A) * sin(t)
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
               mapping = aes(x = x, y = A),
               color = "blue", size = 4) + # x軸線上の点:(変換前)
    geom_point(data = point_df,
               mapping = aes(x = A, y = x),
               color = "blue", size = 4) + # x軸線上の点:(変換後)
    coord_fixed(ratio = 1, xlim = c(-A, A), ylim = c(-A, A)) + 
    labs(x = "x", y = "x")
  
  
  # ラベル用の文字列を作成
  cos_label <- paste0(
    A, " * cos(", a, " * theta + ", round(alpha/pi, digits = 2), "*pi)", 
    " == ", round(point_df[["x"]], digits = 2)
  )

  # cos曲線を作図
  cos_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_line(data = curve_df, 
              mapping = aes(x = theta, y = x), 
              linewidth = 1) + # 曲線
    geom_vline(mapping = aes(xintercept = theta), 
               linewidth = 1, linetype = "dotted") + # ラジアン軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = x), 
               color = "blue", linewidth = 1, linetype = "dotted") + # x軸の補助線
    geom_point(data = point_df, 
               mapping = aes(x = theta, y = x), 
               color = "blue", size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # ラジアン目盛ラベル
    coord_fixed(ratio = 1, ylim = c(-A, A)) + 
    labs(title = "cosine curve", 
         subtitle = parse(text = cos_label), 
         x = expression(theta), 
         y = expression(A * cos(a * theta + alpha)))
  
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    lissajous_graph, sin_graph, 
    adapt_graph, cos_graph, 
    nrow = 2, ncol = 2, 
    guides = "collect"
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 1200, height = 1000, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読み込み
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "lissajous/figure/lissajous/lissajous_theta.gif", delay = 1/30) -> tmp_path # gifファイル書き出し


# パラメータとリサージュ曲線とsin・cos曲線の関係 ----------------------------------------------

# 一時保存フォルダを指定
dir_path <- "lissajous/figure/tmp_folder"


# フレーム数を指定
frame_num <- 301

# 点数を指定
point_num <- 5

# パラメータを指定
A_vals     <- seq(from = 2, to = 2, length.out = frame_num)
a_vals     <- seq(from = 0, to = 4, length.out = frame_num)
alpha_vals <- seq(from = 0*pi, to = 0*0i, length.out = frame_num+1)[1:frame_num]
B_vals     <- seq(from = 0.5, to = 1.5, length.out = frame_num)
b_vals     <- seq(from = 1, to = 1, length.out = frame_num)
beta_vals  <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]


# グラフサイズ用の値を設定
A_max <- max(A_vals)
B_max <- max(B_vals)

# グリッド線の値を設定
axis_x_size <- 2 # x軸目盛の最大値
step_val    <- 1 # グリッド線の間隔

# 軸変換図のグリッド線の座標を作成
adapt_grid_df <- tidyr::expand_grid(
  r = seq(from = step_val, to = 2*A_max, by = step_val), 
  t = seq(from = pi, to = 1.5*pi, length.out = 100)
) |> # グリッド線の数に応じてラジアンを複製
  dplyr::mutate(
    x = axis_x_size + r * cos(t), 
    y = axis_x_size + r * sin(t)
  )


# 目盛ラベル用の文字列を作成
rad_break_vec <- 0:12 / 6 * pi
rad_label_vec <- paste0("frac(", 0:12, ", 6)~pi")

# パラメータごとにグラフを書き出し
for(i in 1:frame_num) {
  
  # i番目のパラメータを取得
  A     <- A_vals[i]
  a     <- a_vals[i]
  alpha <- alpha_vals[i]
  B     <- B_vals[i]
  b     <- b_vals[i]
  beta  <- beta_vals[i]
  
  # 曲線の座標を作成
  curve_df <- tibble::tibble(
    theta = seq(from = 0, to = 2*pi, length.out = 1001), 
    x     = A * cos(a * theta + alpha), 
    y     = B * sin(b * theta + beta)
  )
  
  # 曲線上の点の座標を作成
  point_df <- tibble::tibble(
    theta = seq(from = 0, to = 2*pi, length.out = point_num), 
    x     = A * cos(a * theta + alpha), 
    y     = B * sin(b * theta + beta)
  )
  

  # リサージュ曲線を作図
  lissajous_graph  <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_vline(data = point_df, 
               mapping = aes(xintercept = x, color = theta/pi), 
               linetype = "dotted") + # x軸の補助線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y, color = theta/pi), 
               linetype = "dotted") + # y軸の補助線
    geom_path(data = curve_df, 
              mapping = aes(x = x, y = y, color = theta/pi), 
              linewidth = 1) + # 曲線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y, color = theta/pi), 
               size = 4) + # 曲線上の点
    coord_fixed(ratio = 1, xlim = c(-A_max, A_max), ylim = c(-B_max, B_max)) + 
    labs(title = "Lissajous curve", 
         color = expression(frac(theta, pi)), 
         x = expression(x == A ~ cos(a * theta + alpha)), 
         y = expression(y == B ~ sin(b * theta + beta)))
  
  
  # ラベル用の文字列を作成
  sin_label <- paste0(
    "list(", 
    "B == ", round(B, digits = 2), ", ", 
    "b == ", round(b, digits = 2), ", ", 
    "beta == ", round(beta/pi, digits = 2), "*pi", 
    ")"
  )
  
  # sin曲線を作図
  sin_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_hline(data = point_df, 
               mapping = aes(yintercept = y, color = theta/pi), 
               linetype = "dotted") + # y軸の補助線
    geom_line(data = curve_df, 
              mapping = aes(x = theta, y = y, color = theta/pi), 
              linewidth = 1) + # 曲線
    geom_point(data = point_df, 
               mapping = aes(x = theta, y = y, color = theta/pi), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # ラジアン目盛ラベル
    coord_fixed(ratio = 1, ylim = c(-B_max, B_max)) + 
    labs(title = "sine curve", 
         subtitle = parse(text = sin_label), 
         color = expression(frac(theta, pi)), 
         x = expression(theta), 
         y = expression(B ~ sin(b * theta + beta)))
  
  
  # ラベル用の文字列を作成
  cos_label <- paste0(
    "list(", 
    "A == ", round(A, digits = 2), ", ", 
    "a == ", round(a, digits = 2), ", ", 
    "alpha == ", round(alpha/pi, digits = 2), "*pi, ", 
    ")"
  )
  
  # cos曲線を作図
  cos_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_vline(data = point_df, 
               mapping = aes(xintercept = x, color = theta/pi), 
               linetype = "dotted") + # x軸の補助線
    geom_path(data = curve_df, 
              mapping = aes(x = x, y = theta, color = theta/pi), 
              linewidth = 1) + # 曲線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = theta, color = theta/pi), 
               size = 4) + # 曲線上の点
    scale_y_reverse(breaks = rad_break_vec, 
                    labels = parse(text = rad_label_vec)) + # ラジアン目盛ラベル
    coord_fixed(ratio = 1, xlim = c(-A_max, A_max)) + 
    labs(title = "cosine curve", 
         subtitle = parse(text = cos_label), 
         color = expression(frac(theta, pi)), 
         x = expression(A ~ cos(a * theta + alpha)), 
         y = expression(theta))
  
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    lissajous_graph, sin_graph, 
    cos_graph, 
    nrow = 2, ncol = 2, 
    guides = "collect"
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 1000, height = 1000, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読み込み
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "lissajous/figure/lissajous/lissajous_param.gif", delay = 1/30) -> tmp_path # gifファイル書き出し


