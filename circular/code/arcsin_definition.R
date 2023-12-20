
# 逆sin関数の定義の可視化 ---------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 曲線の作図 -------------------------------------------------------------------

# 元の関数の曲線の座標を作成
curve_df <- tibble::tibble(
  theta = seq(from = -pi, to = pi, length.out = 1001), # ラジアン
  sin_t = sin(theta), 
  inv_flag = (theta >= -0.5*pi & theta <= 0.5*pi) # 逆関数の値域
)
tmp_curve_df <- curve_df |> 
  dplyr::filter(inv_flag) # 値域内の値を抽出

# 逆関数の曲線の座標を作成
curve_inv_df <- tibble::tibble(
  x        = seq(from = -1, to = 1, length.out = 201), # 定義域内の値
  arcsin_x = asin(x)
)


# ラベル用の文字列を作成
def_label <- paste0(
  "list(", 
  "arcsin~x == sin^{-1}*x, ", 
  "paste(-1 < x, {} < 1), ", 
  "-frac(pi, 2) < arcsin~x < frac(pi, 2)", 
  ")"
)

# 関数曲線を作図
ggplot() + 
  geom_line(data = curve_inv_df, 
            mapping = aes(x = x, y = arcsin_x, color = "arcsin"), 
            linewidth = 1) + # 逆関数
  geom_line(data = curve_df,
            mapping = aes(x = theta, y = sin_t, color = "sin"), 
            linewidth = 1, linetype = "dotted") + # 元の関数
  geom_line(data = tmp_curve_df,
            mapping = aes(x = theta, y = sin_t, color = "sin"), 
            linewidth = 1) + # 元の関数(値域内)
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dotdash") + # 恒等関数
  scale_color_manual(breaks = c("arcsin", "sin"), 
                     values = c("black", "orange"), 
                     labels = c(expression(arcsin~x), expression(sin~x)), 
                     name = "function") + # 凡例表示用
  scale_linetype_manual(breaks = c(TRUE, FALSE), 
                        values = c("solid", "dotted"), 
                        guide ="none") + 
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "inverse sine function", 
       subtitle = parse(text = def_label), 
       x = expression(x), 
       y = expression(f(x)))


# 範囲πにおける目盛数を指定
tick_num <- 2

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = -pi, to = pi, by = pi/tick_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")

# 逆関数と元の関数の関係を作図
ggplot() + 
  geom_line(data = curve_inv_df, 
            mapping = aes(x = arcsin_x, y = x, color = "arcsin"), 
            linewidth = 1) + # 逆関数
  geom_line(data = curve_df,
            mapping = aes(x = theta, y = sin_t, color = "sin"), 
            linewidth = 1, linetype = "dashed") + # 元の関数
  scale_color_manual(breaks = c("arcsin", "sin"), 
                     values = c("black", "orange"), 
                     labels = c(expression(arcsin~x), expression(sin~theta)), 
                     name = "function") + # 凡例表示用
  scale_x_continuous(breaks = rad_break_vec, 
                     labels = parse(text = rad_label_vec)) + # ラジアン軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "inverse sine function", 
       subtitle = parse(text = def_label), 
       x = expression(theta == arcsin~x), 
       y = expression(x == sin~theta))


# 単位円と曲線の関係 ---------------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 150

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = 0*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]


# 関数曲線の座標を作成
curve_inv_df <- tibble::tibble(
  z        = seq(from = -1, to = 1, length.out = 201), # 定義域内の値
  arcsin_z = asin(z)
)

# 単位円の座標を作成
circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # 1周期分のラジアン
  r = 1, # 半径
  x = r * cos(t), 
  y = r * sin(t)
)


# 範囲πにおける目盛数を指定
tick_num <- 6

# 角度軸線の座標を作成
rad_tick_df <- tibble::tibble(
  i = seq(from = 0, to = 2*tick_num-0.5, by = 0.5), # 目盛番号
  t = i/tick_num * pi, # ラジアン
  r = 1, # 半径
  x = r * cos(t), 
  y = r * sin(t), 
  major_flag = i%%1 == 0, # 主・補助目盛フラグ
  w = dplyr::if_else(major_flag, true = "major", false = "minor")
)

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = -pi, to = pi, by = pi/tick_num)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# グラフサイズを指定
axis_x_size <- 1.5
axis_y_size <- 2
axis_z_size <- 1.5

# 関数の描画順を指定
fnc_level_vec <- c("arcsin", "sin", "cos")

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  # 値域内のラジアンに変換
  tau <- asin(sin(theta))
  
  # 関数点の座標を作成
  fnc_point_df <- tibble::tibble(
    theta    = theta, 
    sin_t    = sin(theta), 
    arcsin_z = asin(sin_t)
  )
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    theta = theta, 
    x     = cos(theta), 
    y     = sin(theta)
  )
  
  # 半径線の終点の座標を作成
  radius_df <- tibble::tibble(
    x = c(1, cos(theta), cos(tau)), 
    y = c(0, sin(theta), sin(tau)), 
    line_type = c("main", "main", "sub") # 反転の描き分け用
  )
  
  # 角マークの座標を作成
  d_in  <- 0.2
  d_spi <- 0.005
  d_out <- 0.3
  angle_mark_df <- dplyr::bind_rows(
    # 元の関数の入力
    tibble::tibble(
      angle = "theta", 
      t = seq(from = 0, to = theta, length.out = 600), 
      x = (d_in + d_spi*t) * cos(t), 
      y = (d_in + d_spi*t) * sin(t)
    ), 
    # 逆関数の出力
    tibble::tibble(
      angle = "tau", 
      t = seq(from = 0, to = tau, length.out = 600), 
      x = d_out * cos(t), 
      y = d_out * sin(t)
    )
  )
  
  # 角ラベルの座標を作成
  d_in  <- 0.1
  d_out <- 0.4
  angle_label_df <- tibble::tibble(
    d = c(d_in, d_out), # ラベル位置の調整用
    t = 0.5 * c(theta, tau), 
    x = d * cos(t), 
    y = d * sin(t), 
    angle_label = c("theta", "tau")
  )
  
  # 関数円弧の座標を作成
  radian_df <- tibble::tibble(
    t = seq(from = 0, to = tau, length.out = 600), 
    x = cos(t), 
    y = sin(t)
  )
  
  # 関数線分の座標を格納
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "arcsin", 
      "sin", "sin", 
      "cos", "cos"
    ) |> 
      factor(levels = fnc_level_vec), # 描き分け用
    x_from = c(
      1, 
      0, cos(theta), 
      0, 0
    ), 
    y_from = c(
      0, 
      0, 0, 
      0, sin(theta)
    ), 
    x_to = c(
      1, 
      0,          cos(theta), 
      cos(theta), cos(theta)
    ), 
    y_to = c(
      tau, 
      sin(theta), sin(theta), 
      0,          sin(theta)
    )
  )
  
  # 関数ラベルの座標を格納
  fnc_label_df <- tibble::tibble(
    fnc = c("arcsin", "sin") |> 
      factor(levels = fnc_level_vec), # 描き分け用
    x = c(
      1, 
      0
    ), 
    y = c(
      0.5 * tau,
      0.5 * sin(theta)
    ), 
    fnc_label = c("arcsin~z", "sin~theta"), 
    a = 90, 
    h = 0.5, 
    v = 1.5
  )
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "list(", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "tau == ", round(tau/pi, digits = 2), " * pi", 
    ")"
  )
  fnc_label_vec <- paste(
    c("arcsin~z", "sin~theta", "cos~theta"), 
    c(tau, sin(theta), cos(theta)) |> 
      round(digits = 2), 
    sep = " == "
  )
  
  # 単位円における関数線分を作図
  circle_graph <- ggplot() + 
    geom_segment(data = rad_tick_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = w), 
                 color = "white", show.legend = FALSE) + # 角度目盛線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)),
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, 
                               linewidth = line_type, linetype = line_type), 
                 show.legend = FALSE) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y, group = angle)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y, label = angle_label), 
              parse = TRUE, size = 5) + # 角ラベル
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 円周上の点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = 1, y = arcsin_z), 
               size = 4) + # 関数点
    geom_segment(mapping = aes(x = cos(theta), y = sin(theta), 
                               xend = cos(tau), yend = sin(tau)), 
                 linetype = "dotted") + # 反転用の補助線
    geom_segment(mapping = aes(x = c(cos(tau), 1), y = c(sin(tau), tau), 
                               xend = c(1, Inf), yend = c(tau, tau)), 
                 color = "#F8766D", linetype = "dotted") + # y軸の補助線
    geom_path(data = radian_df, 
              mapping = aes(x = x, y = y), 
              color = "#F8766D", linewidth = 1) + # 関数円弧
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = fnc), 
                 linewidth = 1) + # 関数線分
    geom_text(data = fnc_label_df,
              mapping = aes(x = x, y = y, label = fnc_label, color = fnc,
                            angle = a, hjust = h, vjust = v),
              parse = TRUE, show.legend = FALSE) + # 関数ラベル
    scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
    scale_linetype_manual(breaks = c("main", "sub"), 
                          values = c("solid", "dashed")) +  # 反転用
    scale_linewidth_manual(breaks = c("main", "sub", "major", "minor"), 
                           values = c(1, 0.5, 0.5, 0.25)) + # 反転用, 主・補助目盛線用
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_z_size, axis_z_size), 
                ylim = c(-axis_y_size, axis_y_size)) + 
    labs(title = "circular functions", 
         subtitle = parse(text = var_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## 関数曲線
  
  # 関数線分の座標を格納
  fnc_seg_df <- tibble::tibble(
    fnc = c("arcsin", "sin") |> 
      factor(levels = fnc_level_vec), # 描き分け用
    z_from = c(
      sin(theta), 
      0
    ), 
    y_from = c(
      0, 
      0
    ), 
    z_to = c(
      sin(theta), 
      sin(theta)
    ), 
    y_to = c(
      tau, 
      0
    )
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(z, tau)) == ", 
    "(list(", round(sin(theta), digits = 2), ", ", round(tau, digits = 2), "))"
  )
  
  # 関数曲線を作図
  curve_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)),
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # z・τ軸線
    geom_line(data = curve_inv_df, 
              mapping = aes(x = z, y = arcsin_z), 
              linewidth = 1) + # 関数曲線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = sin_t, y = arcsin_z), 
               size = 4) + # 関数点
    geom_segment(mapping = aes(x = c(sin(theta), sin(theta)), y = c(tau, tau), 
                               xend = c(-Inf, sin(theta)), yend = c(tau, -Inf)), 
                 color = c("#F8766D", "black"), linetype = "dotted") + # z・τ軸の補助線
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = z_from, y = y_from, xend = z_to, yend = y_to, color = fnc), 
                 linewidth = 1) + # 関数線分
    scale_color_manual(breaks = fnc_level_vec, 
                       values = scales::hue_pal()(n = length(fnc_level_vec))) + # 色の共通化用
    scale_y_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # τ軸目盛
    guides(color = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_z_size, axis_z_size), 
                ylim = c(-axis_y_size, axis_y_size)) + 
    labs(title = "inverse sine function", 
         subtitle = parse(text = coord_label), 
         x = expression(z == sin~theta), 
         y = expression(tau == arcsin~z))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_graph, curve_graph
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 900, height = 600, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, "/", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "circular/figure/inverse/arcsin_curves_variable.gif", delay = 1/15) -> tmp_path # gifファイルを書出


