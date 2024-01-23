
# sin関数の定義の可視化 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 単位円の描画用 -----------------------------------------------------------------

# 円周の座標を作成
circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # 1周期分のラジアン
  r = 1, # 半径
  x = r * cos(t), 
  y = r * sin(t)
)

# 半円(範囲π)における目盛数(分母の値)を指定
tick_num <- 6

# 角度目盛の座標を作成:(補助目盛有り)
d <- 1.1
rad_tick_df <- tibble::tibble(
  # 座標用
  i     = seq(from = 0, to = 2*tick_num-0.5, by = 0.5), # 目盛番号(分子の値)
  t_deg = i/tick_num * 180, # 度数法の角度
  t_rad = i/tick_num * pi,  # 弧度法の角度(ラジアン)
  r     = 1, # 半径
  x     = r * cos(t_rad), 
  y     = r * sin(t_rad), 
  major_flag = i%%1 == 0, # 主・補助フラグ
  grid       = dplyr::if_else(major_flag, true = "major", false = "minor"), # 目盛カテゴリ
  # ラベル用
  deg_label  = dplyr::if_else(
    major_flag, true = paste0(round(t_deg, digits = 1), "*degree"), false = ""
  ), # 角度ラベル
  rad_label  = dplyr::if_else(
    major_flag, true = paste0("frac(", i, ", ", tick_num, ") ~ pi"), false = ""
  ), # ラジアンラベル
  label_x = d * x, 
  label_y = d * y, 
  a = t_deg + 90, 
  h = 1 - (x * 0.5 + 0.5), 
  v = 1 - (y * 0.5 + 0.5), 
  tick_mark = dplyr::if_else(major_flag, true = "|", false = "") # 目盛指示線用
)


# 単位円と関数の関係 ------------------------------------------------------------

# フレーム数を指定
frame_num <- 600

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -4*pi, to = 4*pi, length.out = frame_num+1)[1:frame_num]


# 円周上の点の座標を作成
anim_point_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  t = theta_vals, 
  x = cos(t), 
  y = sin(t)
)

# 半径線の終点の座標を作成
anim_radius_df <- dplyr::bind_rows(
  # 始線
  tibble::tibble(
    frame_i = 1:frame_num, 
    x = 1, 
    y = 0
  ), 
  # 動径
  anim_point_df |> 
    dplyr::select(frame_i, x, y)
)


# 角マークの座標を作成
d  <- 0.15
ds <- 0.005
anim_angle_mark_df <- tibble::tibble(
  frame_i = 1:frame_num
) |> 
  dplyr::reframe(
    u = seq(from = 0, to = theta_vals[frame_i], length.out = 600), .by = frame_i
  ) |> # フレームごとにラジアンを作成
  dplyr::mutate(
    x = (d + ds*u) * cos(u), 
    y = (d + ds*u) * sin(u)
  )

# 角ラベルの座標を作成
d <- 0.25
anim_angle_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  u = 0.5 * theta_vals, 
  x = d * cos(u), 
  y = d * sin(u)
)


# 関数の描画順を指定
fnc_level_vec <- c("sin", "cos")

# 関数線分の座標を作成
line_num <- 4
anim_fnc_seg_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = line_num), 
  fnc = c(
    "sin", "sin", 
    "cos", "cos"
  ) |> 
    rep(each = frame_num) |> 
    factor(levels = fnc_level_vec), # 関数カテゴリ
  x_from = c(
    rep(0, times = frame_num), cos(theta_vals), 
    rep(0, times = frame_num), rep(0, times = frame_num)
  ), 
  y_from = c(
    rep(0, times = frame_num), rep(0, times = frame_num), 
    rep(0, times = frame_num), sin(theta_vals)
  ), 
  x_to = c(
    rep(0, times = frame_num), cos(theta_vals), 
    cos(theta_vals), cos(theta_vals)
  ), 
  y_to = c(
    sin(theta_vals), sin(theta_vals), 
    rep(0, times = frame_num), sin(theta_vals)
  ), 
  label_flag = c(
    TRUE, FALSE, 
    TRUE, FALSE
  ) |> 
    rep(each = frame_num) # 関数ラベル用
)

# 関数ラベルの座標を作成
anim_fnc_label_df <- anim_fnc_seg_df |> 
  dplyr::filter(label_flag) |> # ラベル付け線分を抽出
  dplyr::summarise(
    x = median(c(x_from, x_to)), 
    y = median(c(y_from, y_to)), .by = c(frame_i, fnc)
  ) |> # 中点に配置
  dplyr::left_join(
    # ラベル設定を指定
    tibble::tibble(
      fnc = c("sin", "cos"), 
      fnc_label = c("sin~theta", "cos~theta"), 
      a = c(90, 0), 
      h = c(0.5, 0.5), 
      v = c(-0.5, 1)
    ), 
    by = "fnc"
  )


# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  t = theta_vals, 
  var_label = paste0(
    "list(", 
    "theta == ", round(t/pi, digits = 2), " * pi, ", 
    "sin~theta == ", round(sin(t), digits = 2), 
    ")"
  )
)
fnc_label_vec <- c(sin = "sine", cos = "cosine")

# グラフサイズを設定
axis_size <- 1.5

# 単位円における関数線分のアニメーションを作図
anim <- ggplot() + 
  geom_segment(data = rad_tick_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = grid), 
               color = "white", show.legend = FALSE) + # θ軸目盛線
  geom_text(data = rad_tick_df, 
            mapping = aes(x = x, y = y, angle = a, label = tick_mark), 
            size = 2) + # θ軸目盛指示線
  geom_text(data = rad_tick_df, 
            mapping = aes(x = label_x, y = label_y, label = rad_label, hjust = h, vjust = v), 
            parse = TRUE) + # θ軸目盛ラベル
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y), 
            linewidth = 1) + # 円周
  geom_segment(data = anim_radius_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y), 
               linewidth = 1) + # 半径線
  geom_path(data = anim_angle_mark_df, 
            mapping = aes(x = x, y = y)) + # 角マーク
  geom_text(data = anim_angle_label_df, 
            mapping = aes(x = x, y = y), 
            label = "theta", parse = TRUE, 
            size = 5) + # 角ラベル
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y), 
             size = 4) + # 円周上の点
  geom_segment(data = anim_fnc_seg_df, 
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = fnc), 
               linewidth = 1) + # 関数線分
  geom_text(data = anim_fnc_label_df, 
            mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                          hjust = h, vjust = v, angle = a), 
            parse = TRUE, show.legend = FALSE) + # 関数ラベル
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル:(subtitleの代用)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_color_hue(labels = fnc_label_vec, name = "function") + # 凡例表示用
  scale_linewidth_manual(breaks = c("major", "minor"), 
                         values = c(0.5, 0.25)) + # 主・補助目盛線用
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "circular functions", 
       subtitle = "", # (ラベル表示用の空行)
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = frame_num, fps = 15, width = 6, height = 6, units = "in", res = 250, 
  renderer = gganimate::av_renderer(file = "circular/figure/sine/definition_circle.mp4")
)


# 単位円と曲線の関係：座標 --------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# 曲線用のラジアンの範囲を指定
theta_vec <- seq(from = 0, to = 2*pi, length.out = 1001)

# 点用のラジアンを作成
theta_vals <- seq(from = min(theta_vec), to = max(theta_vec), length.out = frame_num+1)[1:frame_num]


# 関数曲線の座標を作成
fnc_curve_df <- tibble::tibble(
  t     = theta_vec, 
  sin_t = sin(t)
)


# ラジアン軸目盛用の値を作成
tick_num <- 6
rad_break_vec <- seq(
  from = floor(min(theta_vec) / pi) * pi, 
  to   = ceiling(max(theta_vec) / pi) * pi, 
  by   = pi/tick_num
)
tick_vec <- rad_break_vec/pi * tick_num
rad_label_vec <- paste0(c("", "-")[(tick_vec < 0)+1], "frac(", abs(tick_vec), ", ", tick_num, ") ~ pi")


# 関数の描画順を指定
fnc_level_vec <- c("sin", "cos")

# グラフサイズを設定
axis_size <- 2

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = theta, 
    x = cos(t), 
    y = sin(t)
  )
  
  # 半径線の終点の座標を作成
  radius_df <- tibble::tibble(
    x = c(1, cos(theta)), 
    y = c(0, sin(theta))
  )
  
  # 角マークの座標を作成
  d  <- 0.2
  ds <- 0.005
  angle_mark_df <- tibble::tibble(
    u = seq(from = 0, to = theta, length.out = 600), 
    x = (d + ds*u) * cos(u), 
    y = (d + ds*u) * sin(u)
  )
  
  # 角ラベルの座標を作成
  d <- 0.35
  angle_label_df <- tibble::tibble(
    u = 0.5 * theta, 
    x = d * cos(u), 
    y = d * sin(u)
  )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "sin", "sin", 
      "cos", "cos"
    ) |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    x_from = c(
      0, cos(theta), 
      0, 0
    ), 
    y_from = c(
      0, 0, 
      0, sin(theta)
    ), 
    x_to = c(
      0,          cos(theta), 
      cos(theta), cos(theta)
    ), 
    y_to = c(
      sin(theta), sin(theta), 
      0,          sin(theta)
    )
  )
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "theta == ", round(theta/pi, digits = 2), " * pi"
  )
  fnc_label_vec <- paste(
    c("sin~theta", "cos~theta"), 
    c(sin(theta), cos(theta)) |> 
      round(digits = 2), 
    sep = " == "
  )
  
  # 単位円における関数線分を作図
  circle_graph <- ggplot() + 
    geom_segment(data = rad_tick_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = grid), 
                 color = "white", show.legend = FALSE) + # θ軸目盛線
    geom_text(data = rad_tick_df, 
              mapping = aes(x = x, y = y, angle = a, label = tick_mark), 
              size = 2) + # θ軸目盛指示線
    geom_text(data = rad_tick_df, 
              mapping = aes(x = label_x, y = label_y, label = rad_label, hjust = h, vjust = v), 
              parse = TRUE) + # θ軸目盛ラベル
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)),
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y), 
                 linewidth = 1) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), 
              label = "theta", parse = TRUE, 
              size = 5) + # 角ラベル
    geom_segment(mapping = aes(x = cos(theta), y = sin(theta), 
                               xend = Inf, yend = sin(theta)), 
                 linewidth = 0.8, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 円周上の点
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = fnc), 
                 linewidth = 1) + # 関数線分
    scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25)) + # 主・補助目盛線用
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = var_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## 関数曲線
  
  # 曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    t     = theta, 
    sin_t = sin(t)
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", round(theta, digits = 2), ", ", round(sin(theta), digits = 2), "))"
  )
  
  # 関数曲線を作図
  curve_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)),
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・y軸線
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = t, y = sin_t), 
              linewidth = 1) + # 関数曲線
    geom_segment(mapping = aes(x = theta, y = sin(theta), 
                               xend = c(-Inf, theta), yend = c(sin(theta), -Inf)), 
                 linewidth = 0.8, linetype = "dotted") + # θ・y軸の目盛線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = t, y = sin_t), 
               size = 4) + # 曲線上の点
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = t, y = 0, xend = theta, yend = sin_t), 
                 color = "#F8766D", linewidth = 1) + # 関数線分
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "sine function", 
         subtitle = parse(text = coord_label), 
         x = expression(theta), 
         y = expression(sin~theta))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_graph, curve_graph
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 12, height = 6, units = "in", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "circular/figure/sine/definition_curves_coord.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


# 単位円と曲線の関係：推移 --------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 600

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]
theta_min  <- min(theta_vals)

# 曲線用のラジアンのサイズを指定
theta_size <- 2 * pi


# グラフサイズを設定
axis_size <- 2

# 関数の描画順を指定
fnc_level_vec <- c("sin", "cos")

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = theta, 
    x = cos(t), 
    y = sin(t)
  )
  
  # 半径線の終点の座標を作成
  radius_df <- tibble::tibble(
    x = c(1, cos(theta)), 
    y = c(0, sin(theta))
  )
  
  # 角マークの座標を作成
  d  <- 0.2
  ds <- 0.005
  angle_mark_df <- tibble::tibble(
    u = seq(from = 0, to = theta, length.out = 600), 
    x = (d + ds*u) * cos(u), 
    y = (d + ds*u) * sin(u)
  )
  
  # 角ラベルの座標を作成
  d <- 0.35
  angle_label_df <- tibble::tibble(
    u = 0.5 * theta, 
    x = d * cos(u), 
    y = d * sin(u)
  )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "sin", "sin", 
      "cos", "cos"
    ) |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    x_from = c(
      0, cos(theta), 
      0, 0
    ), 
    y_from = c(
      0, 0, 
      0, sin(theta)
    ), 
    x_to = c(
      0,          cos(theta), 
      cos(theta), cos(theta)
    ), 
    y_to = c(
      sin(theta), sin(theta), 
      0,          sin(theta)
    )
  )
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "theta == ", round(theta/pi, digits = 2), " * pi"
  )
  fnc_label_vec <- paste(
    c("sin~theta", "cos~theta"), 
    c(sin(theta), cos(theta)) |> 
      round(digits = 2), 
    sep = " == "
  )
  
  # 単位円における関数線分を作図
  circle_graph <- ggplot() + 
    geom_segment(data = rad_tick_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = grid), 
                 color = "white", show.legend = FALSE) + # θ軸目盛線
    geom_text(data = rad_tick_df, 
              mapping = aes(x = x, y = y, angle = a, label = tick_mark), 
              size = 2) + # θ軸目盛指示線
    geom_text(data = rad_tick_df, 
              mapping = aes(x = label_x, y = label_y, label = rad_label, hjust = h, vjust = v), 
              parse = TRUE) + # θ軸目盛ラベル
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y), 
                 linewidth = 1) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), 
              label = "theta", parse = TRUE, 
              size = 5) + # 角ラベル
    geom_segment(mapping = aes(x = cos(theta), y = sin(theta), 
                               xend = -Inf, yend = sin(theta)), 
                 linewidth = 0.8, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 円周上の点
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = fnc), 
                 linewidth = 1) + # 関数線分
    scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25)) + # 主・補助目盛線用
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = var_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## 関数曲線
  
  # 曲線用のラジアンを作成
  theta_vec <- seq(from = max(theta_min, theta-theta_size), to = theta, length.out = 1000)
  
  # ラジアン軸目盛用の値を作成
  tick_num <- 6
  rad_break_vec <- seq(
    from = floor((theta-theta_size) / pi) * pi, 
    to   = ceiling(theta / pi) * pi, 
    by   = pi/tick_num
  )
  tick_vec <- rad_break_vec/pi * tick_num
  rad_label_vec <- paste0(c("", "-")[(tick_vec < 0)+1], "frac(", abs(tick_vec), ", ", tick_num, ") ~ pi")
  
  # 関数曲線の座標を作成
  fnc_curve_df <- tibble::tibble(
    t     = theta_vec, 
    sin_t = sin(t)
  )
  
  # 曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    t     = theta, 
    sin_t = sin(t)
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", round(theta, digits = 2), ", ", round(sin(theta), digits = 2), "))"
  )
  
  # 関数曲線を作図
  curve_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = t, y = sin_t), 
              linewidth = 1) + # 関数曲線
    geom_segment(mapping = aes(x = theta, y = sin(theta), 
                               xend = c(Inf, theta), yend = c(sin(theta), -Inf)), 
                 linewidth = 0.8, linetype = "dotted") + # θ・y軸の目盛線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = t, y = sin_t), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    coord_fixed(ratio = 1, 
                xlim = c(theta-theta_size, theta), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "sine function", 
         subtitle = parse(text = coord_label), 
         x = expression(theta), 
         y = expression(sin~theta))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_graph, circle_graph
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 12, height = 6, units = "in", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "circular/figure/sine/definition_curves_form.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


