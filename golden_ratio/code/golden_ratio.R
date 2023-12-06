
# 黄金比 ---------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# パッケージ名の省略用
library(ggplot2)


# 黄金長方形と黄金螺旋の関係 ------------------------------------------------------------

### ・辺の設定 -----

# 黄金数を計算
phi <- 0.5 * (1 + sqrt(5))

# 黄金数の逆数を計算
recip_phi <- 1 / phi

# 辺の長さを指定
a0 <- 1

# 辺の長さを計算
a1 <- a0 * recip_phi
a2 <- a1 * recip_phi
a3 <- a2 * recip_phi
a4 <- a3 * recip_phi
a5 <- a4 * recip_phi
a6 <- a5 * recip_phi


### ・黄金長方形と近似螺旋 -----

# 辺の座標を作成
edge_line_df <- dplyr::bind_rows(
  # 長さがa0の正方形の座標
  tibble::tibble(
    i      = 0, 
    x_from = c(0,  0,  a0, a0), 
    y_from = c(0,  a0, a0, 0), 
    x_to   = c(0,  a0, a0, 0), 
    y_to   = c(a0, a0, 0,  0)
  ), 
  # 長さがa1の正方形の座標
  tibble::tibble(
    i      = 1, 
    x_from = c(0,  0,  a1, a1) + a0, 
    y_from = c(0,  a1, a1, 0)  + a2, 
    x_to   = c(0,  a1, a1, 0)  + a0, 
    y_to   = c(a1, a1, 0,  0)  + a2
  ), 
  # 長さがa2の正方形の座標
  tibble::tibble(
    i      = 2,
    x_from = c(0,  0,  a2, a2) + a0+a3, 
    y_from = c(0,  a2, a2, 0), 
    x_to   = c(0,  a2, a2, 0)  + a0+a3, 
    y_to   = c(a2, a2, 0,  0)
  ), 
  # 長さがa3の正方形の座標
  tibble::tibble(
    i      = 3, 
    x_from = c(0,  0,  a3, a3) + a0, 
    y_from = c(0,  a3, a3, 0), 
    x_to   = c(0,  a3, a3, 0)  + a0, 
    y_to   = c(a3, a3, 0,  0)
  ), 
  # 長さがa4の正方形の座標
  tibble::tibble(
    i      = 4, 
    x_from = c(0,  0,  a4, a4) + a0, 
    y_from = c(0,  a4, a4, 0)  + a3, 
    x_to   = c(0,  a4, a4, 0)  + a0, 
    y_to   = c(a4, a4, 0,  0)  + a3
  ), 
  # 長さがa5の正方形の座標
  tibble::tibble(
    i      = 5, 
    x_from = c(0,  0,  a5, a5) + a0+a4, 
    y_from = c(0,  a5, a5, 0)  + a3+a6, 
    x_to   = c(0,  a5, a5, 0)  + a0+a4, 
    y_to   = c(a5, a5, 0,  0)  + a3+a6
  ), 
  # 長さがa5の辺の座標
  tibble::tibble(
    i      = 5, 
    x_from = a0+a4, 
    y_from = a3, 
    x_to   = a0+a4+a5, 
    y_to   = a3
  ), 
  # 長さがa6の辺の座標
  tibble::tibble(
    i      = 6, 
    x_from = c(0,  a5) + a0+a4, 
    y_from = c(0,  a6) + a3, 
    x_to   = c(0,  a5) + a0+a4, 
    y_to   = c(a6, 0)  + a3
  )
)

# 辺ラベルの座標を格納
edge_label_df <- tibble::tibble(
  x = c(
    0,            0.5*a0, 
    a0+0.5*a1,    a0+a1, 
    a0+a1,        a0+a3+0.5*a2, 
    a0+0.5*a3,    a0,     
    a0,           a0+0.5*a4, 
    a0+a4+0.5*a5, a0+a4+a5, 
    a0+a4+a5
  ), 
  y = c(
    0.5*a0,    a0, 
    a0,        a2+0.5*a1, 
    0.5*a2,    0, 
    0,         0.5*a3, 
    a3+0.5*a4, a2, 
    a2,        a3+a6+0.5*a5, 
    a3+0.5*a6
  ), 
  len_label = c(
    "a[0]", "a[0]", 
    "a[1]", "a[1]", 
    "a[2]", "a[2]", 
    "a[3]", "a[3]", 
    "a[4]", "a[4]", 
    "a[5]", "a[5]", 
    "a[6]"
  ), 
  ratio_label = c(
    "phi1^0", "phi1^0 == 1", 
    "phi1^{-1}", "phi1^{-1}", 
    "phi1^{-2}", "phi1^{-2}", 
    "phi1^{-3}", "phi1^{-3}", 
    "phi1^{-4}", "phi1^{-4}", 
    "phi1^{-5}", "phi1^{-5}", 
    "phi1^{-6}"
  ), 
  h = c(
    1.2,  0.2, 
    0.2,  -0.2, 
    -0.2, 0.2, 
    0.2,  1.2, 
    1.2,  0.2, 
    0.2,  -0.2, 
    -0.2
  ), 
  v = c(
    0.2,  -0.2, 
    -0.2, 0.2, 
    0.2,  1.2, 
    1.2,  0.2, 
    0.2,  -0.2, 
    -0.2, 0.2, 
    0.2
  )
)

# 螺旋の座標を作成
a7 <- a6 * recip_phi
arc_df <- dplyr::bind_rows(
  # 半径がa0の円弧の座標
  tibble::tibble(
    i = 0, 
    t = seq(from = 0.5*pi, to = pi, length.out = 91), 
    x = a0 + a0 * cos(t), 
    y = 0  + a0 * sin(t)
  ), 
  # 半径がa1の円弧の座標
  tibble::tibble(
    i = 1, 
    t = seq(from = 0, to = 0.5*pi, length.out = 91), 
    x = a0 + a1 * cos(t), 
    y = a2 + a1 * sin(t)
  ), 
  # 半径がa2の円弧の座標
  tibble::tibble(
    i = 2, 
    t = seq(from = 1.5*pi, to = 2*pi, length.out = 91), 
    x = a0+a3 + a2 * cos(t), 
    y = a2    + a2 * sin(t)
  ), 
  # 半径がa3の円弧の座標
  tibble::tibble(
    i = 3, 
    t = seq(from = 1*pi, to = 1.5*pi, length.out = 91), 
    x = a0+a3 + a3 * cos(t), 
    y = a3    + a3 * sin(t)
  ), 
  # 半径がa4の円弧の座標
  tibble::tibble(
    i = 4, 
    t = seq(from = 0.5*pi, to = 1*pi, length.out = 91), 
    x = a0+a4 + a4 * cos(t), 
    y = a3    + a4 * sin(t)
  ), 
  # 半径がa5の円弧の座標
  tibble::tibble(
    i = 5, 
    t = seq(from = 0, to = 0.5*pi, length.out = 91), 
    x = a0+a4 + a5 * cos(t), 
    y = a3+a6 + a5 * sin(t)
  ), 
  # 半径がa6の円弧の座標
  tibble::tibble(
    i = 6, 
    t = seq(from = 1.5*pi, to = 2*pi, length.out = 91), 
    x = a0+a4+a7 + a6 * cos(t), 
    y = a3+a6    + a6 * sin(t)
  )
)

# 対角線の座標を作成
diagonal_df <- tibble::tibble(
  alpha = c(
    recip_phi^3, 
    -recip_phi, 
    -phi^3, 
    phi
  ), # 傾き
  beta = c(
    0, 
    a0, 
    a0 * (phi^3 + 1), 
    -a0 * phi
  ) # 切片
)


# 辺の長さを格納
a_vals <- c(a0, a1, a2, a3, a4, a5, a6)

# 分割回数を設定
n <- length(a_vals) - 1

# 辺インデックスを作成
i_vals <- 0:n

# ラベル用の文字列を作成
def_label <- paste0(
  "list(", 
  "phi1 == frac(1 + sqrt(5), 2), ", 
  "a[i+1] : a[i] : a[i-1] == phi1 : 1 : frac(1, phi1), ", 
  "a[i] == frac(a[0], phi1^i)", 
  ")"
)
len_label_vec <- paste0(
  "a[", i_vals, "] == ", round(a_vals, digits = 2)
)

# 黄金長方形上の螺旋を作図
ggplot() + 
  geom_segment(data = edge_line_df, 
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = factor(i)), 
               linewidth = 1) + # 辺
  geom_text(data = edge_label_df,
            mapping = aes(x = x, y = y, label = ratio_label, hjust = h, vjust = v),
            parse = TRUE, size = 6) + # 長さラベル
  scale_color_hue(labels = parse(text = len_label_vec), name = "length") + # 凡例の表示用
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, 
              xlim = c(0, a0+a1), ylim = c(-0.5*a1, a0+0.5*a1)) + # グリッド線の調整用
  labs(title = "golden rectangle", 
       subtitle = parse(text = def_label), 
       x = "x", y = "y")


### ・黄金螺旋-----

# 中心の座標を計算
x0 <- 0.1 * (5 + 3 * sqrt(5)) * a0
y0 <- 0.1 * (5 - sqrt(5)) * a0

# 中心点から見た原点の角度を計算
t0 <- atan2(-y0, -x0)

# 中心点と原点のノルムを計算
r0 <- sqrt(x0^2 + y0^2)
r0 <- -x0 / cos(t0)
r0 <- -y0 / sin(t0)

# パラメータを計算
beta  <- log(phi) * 2/pi
alpha <- r0 / exp(beta * t0)


# 螺旋用のラジアン(弧度法の角度)を作成
t_vec <- seq(from = -10*pi, to = 0, length.out = 1000)

# 螺旋の座標を作成
spiral_df <- tibble::tibble(
  t = t_vec, # ラジアン
  r = alpha * exp(beta * t), # ノルム
  x = x0 + r * cos(t), 
  y = y0 + r * sin(t)
)


# ラベル用の文字列を作成
def_label <- paste0(
  "list(", 
  "r == alpha ~ exp(beta * theta), ", 
  "alpha == ", round(alpha, digits = 2), ", ", 
  "beta == ", round(beta, digits = 2), 
  ")"
)

# 黄金長方形と黄金螺旋を作図
ggplot() + 
  geom_hline(yintercept = y0, linetype = "dotted") + # x軸目盛の補助線
  geom_vline(xintercept = x0, linetype = "dotted") + # y軸目盛の補助線
  geom_segment(data = edge_line_df, 
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = factor(i)), 
               linewidth = 1) + # 辺
  geom_text(data = edge_label_df,
            mapping = aes(x = x, y = y, label = len_label, hjust = h, vjust = v),
            parse = TRUE, size = 6) + # 長さラベル
  geom_abline(data = diagonal_df, 
              mapping = aes(slope = alpha, intercept = beta)) + # 対角線
  geom_path(data = arc_df, 
            mapping = aes(x = x, y = y, group = factor(i)), 
            linewidth = 1) + # 近似螺旋
  geom_path(data = spiral_df,
            mapping = aes(x = x, y = y),
            color = "red", linewidth = 1) + # 黄金螺旋
  scale_color_hue(labels = parse(text = len_label_vec), name = "length") + # 凡例の表示用
  scale_x_continuous(sec.axis = sec_axis(trans = ~., breaks = x0, labels = round(x0, digits = 2))) + # 中心のx座標
  scale_y_continuous(sec.axis = sec_axis(trans = ~., breaks = y0, labels = round(y0, digits = 2))) + # 中心のy座標
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, 
              xlim = c(0, a0+a1), ylim = c(-0.5*a1, a0+0.5*a1)) + # グリッド線の調整用
  labs(title = "golden spiral", 
       subtitle = parse(text = def_label), 
       x = expression(x == x[0] + r ~ cos~theta), 
       y = expression(y == y[0] + r ~ sin~theta))


