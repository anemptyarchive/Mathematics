
# 円関数の定義の可視化 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 単位円の描画用 -----------------------------------------------------------------

# 単位円の座標を作成
circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # 1周期分のラジアン
  r = 1, # 半径
  x = r * cos(t), 
  y = r * sin(t)
)

# 範囲πにおける目盛数を指定
tick_num <- 6

# 角度目盛の座標を作成
rad_tick_df <- tibble::tibble(
  i = seq(from = 0, to = 2*tick_num-0.5, by = 0.5), # 目盛番号
  t = i/tick_num * pi, # ラジアン
  r = 1, # 半径
  x = r * cos(t), 
  y = r * sin(t), 
  major_flag = i%%1 == 0, # 主・補助フラグ
  grid       = dplyr::if_else(major_flag, true = "major", false = "minor") # 目盛カテゴリ
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
fnc_level_vec <- c("sin", "cos", "versin", "vercos", "cvs", "cvc")

# 関数線分の座標を作成
line_num <- 6
anim_fnc_seg_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = line_num), 
  fnc = c(
    "sin", "cos", 
    "versin", "vercos", 
    "cvs", "cvc"
  ) |> 
    rep(each = frame_num) |> 
    factor(levels = fnc_level_vec), # 関数カテゴリ
  x_from = c(
    cos(theta_vals), rep(0, times = frame_num), 
    cos(theta_vals), cos(theta_vals), 
    rep(0, times = frame_num), rep(0, times = frame_num)
  ), 
  y_from = c(
    rep(0, times = frame_num), sin(theta_vals), 
    rep(0, times = frame_num), rep(0, times = frame_num), 
    sin(theta_vals), sin(theta_vals)
  ), 
  x_to = c(
    cos(theta_vals), cos(theta_vals), 
    rep(1, times = frame_num), rep(-1, times = frame_num), 
    rep(0, times = frame_num), rep(0, times = frame_num)
  ), 
  y_to = c(
    sin(theta_vals), sin(theta_vals), 
    rep(0, times = frame_num), rep(0, times = frame_num), 
    rep(1, times = frame_num), rep(-1, times = frame_num)
  )
)

# ラベル設定を指定
setting_df <- tibble::tibble(
  fnc = c(
    "sin", "cos", 
    "versin", "vercos", 
    "cvs", "cvc"
  ), 
  fnc_label = c(
    "sin~theta", "cos~theta", 
    "versin~theta", "vercos~theta", 
    "coversin~theta", "covercos~theta"
  ), 
  a = c(
    90, 0, 
    0, 0, 
    90, 90
  ), 
  h = c(
    0.5, 0.5, 
    0.5, 0.5, 
    0.5, 0.5
  ), 
  v = c(
    1, -0.5, 
    1, 1, 
    -0.5, -0.5
  )
)

# 関数ラベルの座標を作成
anim_fnc_label_df <- anim_fnc_seg_df |> 
  dplyr::summarise(
    x = median(c(x_from, x_to)), 
    y = median(c(y_from, y_to)), .by = c(frame_i, fnc)
  ) |> # 中点に配置
  dplyr::left_join(setting_df, by = "fnc") # ラベル設定を追加


# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  t = theta_vals, 
  var_label = paste0(
    "list(", 
    "theta == ", round(t/pi, digits = 2), " * pi, ", 
    "(list(x, y)) == (list(", round(cos(t), digits = 2), ", ", round(sin(t), digits = 2), "))", 
    ")"
  )
)
fnc_label_vec <- c(
  sin = "sine", cos = "cosine", 
  versin = "versed sine", vercos = "versed cosine", 
  cvs = "coversed sine", cvc = "coversed cosine"
)


# グラフサイズを設定
axis_size <- 1.5

# 単位円における関数線分のアニメーションを作図
anim <- ggplot() + 
  geom_segment(data = rad_tick_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = grid), 
               color = "white", show.legend = FALSE) + # θ軸目盛線
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
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                             color = fnc), 
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
  plot = anim, nframes = frame_num, fps = 15, width = 9, height = 8, units = "in", res = 250, 
  renderer = gganimate::av_renderer(file = "circular/figure/circular/versed_circle.mp4")
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


# 関数の描画順を指定
fnc_level_vec <- c("sin", "cos", "versin", "vercos", "cvs", "cvc")

# 関数曲線の座標を作成
sin_curve_df <- tidyr::expand_grid(
  fnc = c("sin", "cvs", "cvc") |> 
    factor(levels = fnc_level_vec), 
  t = theta_vec
) |> # 関数ごとにラジアンを複製
  dplyr::mutate(
    f_t = dplyr::case_match(
      .x = fnc, 
      "sin" ~ sin(t), 
      "cvs" ~ 1 - sin(t), 
      "cvc" ~ 1 + sin(t)
    )
  )
cos_curve_df <- tidyr::expand_grid(
  fnc = c("cos", "versin", "vercos") |> 
    factor(levels = fnc_level_vec), 
  t = theta_vec
) |> # 関数ごとにラジアンを複製
  dplyr::mutate(
    f_t = dplyr::case_match(
      .x = fnc, 
      "cos"    ~ cos(t), 
      "versin" ~ 1 - cos(t), 
      "vercos" ~ 1 + cos(t)
    )
  )


# ラジアン軸目盛用の値を作成
tick_num <- 2
rad_break_vec <- seq(
  from = floor(min(theta_vec) / pi) * pi, 
  to   = ceiling(max(theta_vec) / pi) * pi, 
  by   = pi/tick_num
)
rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")


# グラフサイズを設定
axis_size <- 2.5

# 変換軸の目盛間隔を設定
tick_major_val <- 1
tick_minor_val <- 0.5 * tick_major_val

# 変換軸のサイズを設定:(目盛間隔で切り上げ)
grid_size <- ceiling(axis_size / tick_minor_val) * tick_minor_val

# 変換軸のグリッド線の座標を作成
grid_df <- tidyr::expand_grid(
  x = seq(
    from = ceiling((-grid_size * 2*sqrt(2) + grid_size) / tick_minor_val) * tick_minor_val, # (対角線のサイズ分を追加して切り下げ)
    to   = grid_size - tick_minor_val, # (半径0の線を除く)
    by   = tick_minor_val
  ), 
  u = seq(from = pi, to = 1.5*pi, length.out = 91) # ラジアン
) |> # 目盛線ごとにラジアンを複製
  dplyr::mutate(
    x0    = grid_size, 
    y0    = grid_size, 
    arc_r = grid_size - x, 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u), 
    grid  = dplyr::if_else(
      x%%tick_major_val == 0, true = "major", false = "minor"
    ) # 主・補助目盛の書き分け用
  )


# x軸・y軸方向の変化の可視化位置を指定
x0 <- 1
y0 <- -1

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  # 関数点の座標を作成
  sin_point_df <- tibble::tibble(
    fnc = c("sin", "cvs", "cvc") |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    t   = theta, 
    f_t = c(sin(theta), 1-sin(theta), 1+sin(theta)), 
  )
  cos_point_df <- tibble::tibble(
    fnc = c("cos", "versin", "vercos") |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    t   = theta, 
    f_t = c(cos(theta), 1-cos(theta), 1+cos(theta))
  )
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = theta, 
    x = c(1, -1, 1) * cos(theta), 
    y = c(1, 1, -1) * sin(theta), 
    point_type = c("main", "sub", "sub") # 符号の反転用
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
  
  # 半径線の座標を作成
  radius_df <- dplyr::bind_rows(
    # 始線・動径
    tibble::tibble(
      x_from = c(0, 0), 
      y_from = c(0, 0), 
      x_to   = c(1, cos(theta)), 
      y_to   = c(0, sin(theta)), 
      w = "normal" # 補助線用
    ), 
    # x軸・y軸方向の変化
    tibble::tibble(
      x_from = c(
        -cos(theta), cos(theta), 
        x0, x0
      ), 
      y_from = c(
        y0, y0, 
        -sin(theta), sin(theta)
      ), 
      x_to = c(
        1-cos(theta), 1+cos(theta), 
        x0, x0
      ), 
      y_to = c(
        y0, y0, 
        1-sin(theta), 1+sin(theta)
      ), 
      w = "thin" # 補助線用
    )
  )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "sin", "sin", 
      "cos", "cos", 
      "versin", "vercos", 
      "cvs", "cvc"
    )|> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    x_from = c(
      cos(theta), cos(theta), 
      0, 0, 
      cos(theta), cos(theta), 
      0, 0
    ), 
    y_from = c(
      0, 0, 
      sin(theta), sin(theta), 
      0, 0, 
      sin(theta), sin(theta)
    ), 
    x_to = c(
      cos(theta), cos(theta), 
      cos(theta), -cos(theta), 
      1, -1, 
      0, 0
    ), 
    y_to = c(
      sin(theta), -sin(theta), 
      sin(theta), sin(theta), 
      0, 0, 
      1, -1
    ), 
    line_type = c(
      "main", "sub", 
      "main", "sub", 
      "main", "main", 
      "main", "main"
    ) # 符号の反転用
  )
  
  # x軸・y軸の目盛線の座標を作成
  scale_df <- tibble::tibble(
    x_from = c(cos(theta), -cos(theta), cos(theta), cos(theta)), 
    y_from = c(sin(theta), sin(theta), sin(theta), -sin(theta)), 
    x_to   = c(cos(theta), -cos(theta), x0, x0), 
    y_to   = c(y0, y0, sin(theta), -sin(theta))
  )
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "list(", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "(list(x, y)) == ", 
    "(list(", round(cos(theta), digits = 2), ", ", round(sin(theta), digits = 2), "))", 
    
    ")"
  )
  fnc_label_vec <- paste(
    c("sin~theta", "cos~theta", "versin~theta", "vercos~theta", "coversin~theta", "covercos~theta"), 
    c(sin(theta), cos(theta), 1-cos(theta), 1+cos(theta), 1-sin(theta), 1+sin(theta)) |> 
      round(digits = 2), 
    sep = " == "
  )
  
  # 単位円における関数線分を作図
  circle_graph <- ggplot() + 
    geom_segment(data = rad_tick_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = grid), 
                 color = "white", show.legend = FALSE) + # θ軸目盛線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)),
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = w), 
                 show.legend = FALSE) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), 
              label = "theta", parse = TRUE, 
              size = 5) + # 角ラベル
    geom_segment(data = scale_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to), 
                 linewidth = 0.8, linetype = "dotted") + # x軸・y軸の目盛線
    geom_segment(data = cos_point_df, 
                 mapping = aes(x = f_t, y = y0, xend = f_t, yend = -Inf, color = fnc), 
                 linewidth = 0.8, linetype = "dotted", show.legend = FALSE) + # x軸の目盛線
    geom_segment(data = sin_point_df, 
                 mapping = aes(x = x0, y = f_t, xend = Inf, yend = f_t, color = fnc), 
                 linewidth = 0.8, linetype = "dotted", show.legend = FALSE) + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y, shape = point_type), 
               size = 4, show.legend = FALSE) + # 円周上の点
    geom_point(mapping = aes(x = c(-cos(theta), x0), y = c(y0, -sin(theta)), 
                             color = c("cos", "sin"), shape = "sub"), 
               size = 4, show.legend = FALSE) + # 符号の反転用
    geom_point(data = cos_point_df, 
               mapping = aes(x = f_t, y = y0, color = fnc), 
               size = 4) + # 関数点
    geom_point(data = sin_point_df, 
               mapping = aes(x = x0, y = f_t, color = fnc), 
               size = 4) + # 関数点
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, linetype = line_type), 
                 linewidth = 1) + # 関数線分
    scale_color_manual(breaks = fnc_level_vec, 
                       values = scales::hue_pal()(n = length(fnc_level_vec)), 
                       labels = parse(text = fnc_label_vec), 
                       name = "function") + # 凡例表示用, 色の共通化用
    scale_shape_manual(breaks = c("main", "sub"), 
                       values = c("circle", "circle open")) + # 符号の反転用
    scale_linetype_manual(breaks = c("main", "sub"), 
                          values = c("solid", "dashed")) + # 符号の反転用
    scale_linewidth_manual(breaks = c("normal", "thin", "major", "minor"), 
                           values = c(1, 0.5, 0.5, 0.25)) + # 補助線用, 主・補助目盛線用
    guides(linetype = "none") + 
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
  
  ## sin系の関数曲線
  
  # 関数曲線を作図
  curve_sin_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・y軸線
    geom_vline(xintercept = theta, 
               linewidth = 0.8, linetype = "dotted") + # θ軸の目盛線
    geom_segment(data = sin_point_df, 
                 mapping = aes(x = t, y = f_t, xend = -Inf, yend = f_t, color = fnc), 
                 linewidth = 0.8, linetype = "dotted", show.legend = FALSE) + # y軸の目盛線
    geom_line(data = sin_curve_df, 
              mapping = aes(x = t, y = f_t, color = fnc), 
              linewidth = 1) + # 関数曲線
    geom_point(data = sin_point_df, 
               mapping = aes(x = t, y = f_t, color = fnc), 
               size = 4) + # 関数点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    scale_color_manual(breaks = fnc_level_vec, 
                       values = scales::hue_pal()(n = length(fnc_level_vec)), 
                       labels = parse(text = fnc_label_vec), 
                       name = "function") + # 色の共通化用
    theme(legend.text.align = 0, 
          legend.position = c(0, 0), 
          legend.justification = c(0, 0), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "sine function", 
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## 軸変換
  
  # 軸の変換曲線の座標を作成
  convert_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc = c("cos", "versin", "vercos"), 
      arc_r = grid_size - cos_point_df[["f_t"]]
    ), 
    u = seq(from = pi, to = 1.5*pi, length.out = 91) # ラジアン
  ) |> 
    dplyr::mutate(
      x0    = grid_size, 
      y0    = grid_size, 
      arc_x = x0 + arc_r * cos(u), 
      arc_y = y0 + arc_r * sin(u)
    )
  
  # 軸変換を作図
  convert_graph <- ggplot() + 
    geom_path(data = grid_df, 
              mapping = aes(x = arc_x, y = arc_y, group = arc_r, linewidth = grid), 
              color = "white") + # グリッド線
    geom_line(data = convert_df, 
              mapping = aes(x = arc_x, y = arc_y, color = fnc), 
              linewidth = 0.8, linetype = "dotted") + # 変換曲線
    geom_segment(data = cos_point_df, 
                 mapping = aes(x = f_t, y = grid_size, xend = f_t, yend = Inf, color = fnc), 
                 linewidth = 0.8, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = cos_point_df, 
                 mapping = aes(x = grid_size, y = f_t, xend = Inf, yend = f_t, color = fnc), 
                 linewidth = 0.8, linetype = "dotted") + # x軸の目盛線
    geom_point(data = cos_point_df, 
               mapping = aes(x = f_t, y = grid_size, color = fnc), 
               size = 4) + # 関数点
    geom_point(data = cos_point_df, 
               mapping = aes(x = grid_size, y = f_t, color = fnc), 
               size = 4) + # 関数点
    scale_color_manual(breaks = fnc_level_vec, 
                       values = scales::hue_pal()(n = length(fnc_level_vec)), 
                       labels = parse(text = fnc_label_vec)) + # 色の共通化用
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25), guide = "none") + # 主・補助目盛線用
    guides(color = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(x = expression(x), 
         y = expression(x))
  
  ## cos系の関数曲線
  
  # 関数曲線を作図
  curve_cos_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・x軸線
    geom_vline(xintercept = theta, 
               linewidth = 0.8, linetype = "dotted") + # θ軸の目盛線
    geom_segment(data = cos_point_df, 
                 mapping = aes(x = t, y = f_t, xend = -Inf, yend = f_t, color = fnc), 
                 linewidth = 0.8, linetype = "dotted", show.legend = FALSE) + # x軸の目盛線
    geom_line(data = cos_curve_df, 
              mapping = aes(x = t, y = f_t, color = fnc), 
              linewidth = 1) + # 関数曲線
    geom_point(data = cos_point_df, 
               mapping = aes(x = t, y = f_t, color = fnc), 
               size = 4) + # 関数点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    scale_color_manual(breaks = fnc_level_vec, 
                       values = scales::hue_pal()(n = length(fnc_level_vec)), 
                       labels = parse(text = fnc_label_vec), 
                       name = "function") + # 色の共通化用
    theme(legend.text.align = 0, 
          legend.position = c(0, 0), 
          legend.justification = c(0, 0), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "cosine function", 
         subtitle = "", # (空行)
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_graph, curve_sin_graph, 
    convert_graph, curve_cos_graph, 
    nrow = 2, ncol = 2
  )

  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 12, height = 12, units = "in", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "circular/figure/circular/versed_curves_coord.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


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
axis_size <- 2.5

# 変換軸の目盛間隔を設定
tick_major_val <- 1
tick_minor_val <- 0.5 * tick_major_val

# 変換軸のサイズを設定:(目盛間隔で切り上げ)
grid_size <- ceiling(axis_size / tick_minor_val) * tick_minor_val

# 変換軸のグリッド線の座標を作成
grid_df <- tidyr::expand_grid(
  x = seq(
    from = -grid_size + tick_minor_val, # (半径0の線を除去)
    to   = grid_size * 2*sqrt(2) - grid_size, # (対角線のサイズ分を追加)
    by   = tick_minor_val
  ), 
  u = seq(from = 0, to = 0.5*pi, length.out = 91) # ラジアン
) |> # 目盛線ごとにラジアンを複製
  dplyr::mutate(
    x0    = -grid_size, 
    y0    = -grid_size, 
    arc_r = grid_size + x, 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u), 
    grid   = dplyr::if_else(
      x%%tick_major_val == 0, true = "major", false = "minor"
    ) # 主・補助目盛の書き分け用
  )


# 関数の描画順を指定
fnc_level_vec <- c("sin", "cos", "versin", "vercos", "cvs", "cvc")

# x軸・y軸方向の変化の可視化位置を指定
x0 <- -1
y0 <- 1

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  # 関数点の座標を作成
  sin_point_df <- tibble::tibble(
    fnc = c("sin", "cvs", "cvc") |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    t   = theta, 
    f_t = c(sin(theta), 1-sin(theta), 1+sin(theta)), 
  )
  cos_point_df <- tibble::tibble(
    fnc = c("cos", "versin", "vercos") |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    t   = theta, 
    f_t = c(cos(theta), 1-cos(theta), 1+cos(theta))
  )
  
  # 曲線用のラジアンを作成
  theta_vec <- seq(from = max(theta_min, theta-theta_size), to = theta, length.out = 1000)
  
  # ラジアン軸目盛用の値を作成
  tick_num <- 2
  rad_break_vec <- seq(
    from = floor((theta-theta_size) / pi) * pi, 
    to   = ceiling(theta / pi) * pi, 
    by   = pi/tick_num
  )
  rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")
  
  # 関数曲線の座標を作成
  sin_curve_df <- tidyr::expand_grid(
    fnc = c("sin", "cvs", "cvc") |> 
      factor(levels = fnc_level_vec), 
    t = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      f_t = dplyr::case_match(
        .x = fnc, 
        "sin" ~ sin(t), 
        "cvs" ~ 1 - sin(t), 
        "cvc" ~ 1 + sin(t)
      )
    )
  cos_curve_df <- tidyr::expand_grid(
    fnc = c("cos", "versin", "vercos") |> 
      factor(levels = fnc_level_vec), 
    t = theta_vec
  ) |> # 関数ごとにラジアンを複製
    dplyr::mutate(
      f_t = dplyr::case_match(
        .x = fnc, 
        "cos"    ~ cos(t), 
        "versin" ~ 1 - cos(t), 
        "vercos" ~ 1 + cos(t)
      )
    )
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = theta, 
    x = c(1, -1, 1) * cos(theta), 
    y = c(1, 1, -1) * sin(theta), 
    point_type = c("main", "sub", "sub") # 符号の反転用
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
  
  # 半径線の座標を作成
  radius_df <- dplyr::bind_rows(
    # 始線・動径
    tibble::tibble(
      x_from = c(0, 0), 
      y_from = c(0, 0), 
      x_to   = c(1, cos(theta)), 
      y_to   = c(0, sin(theta)), 
      w = "normal" # 補助線用
    ), 
    # x軸・y軸方向の変化
    tibble::tibble(
      x_from = c(
        -cos(theta), cos(theta), 
        x0, x0
      ), 
      y_from = c(
        y0, y0, 
        -sin(theta), sin(theta)
      ), 
      x_to = c(
        1-cos(theta), 1+cos(theta), 
        x0, x0
      ), 
      y_to = c(
        y0, y0, 
        1-sin(theta), 1+sin(theta)
      ), 
      w = "thin" # 補助線用
    )
  )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "sin", "sin", 
      "cos", "cos", 
      "versin", "vercos", 
      "cvs", "cvc"
    )|> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    x_from = c(
      cos(theta), cos(theta), 
      0, 0, 
      cos(theta), cos(theta), 
      0, 0
    ), 
    y_from = c(
      0, 0, 
      sin(theta), sin(theta), 
      0, 0, 
      sin(theta), sin(theta)
    ), 
    x_to = c(
      cos(theta), cos(theta), 
      cos(theta), -cos(theta), 
      1, -1, 
      0, 0
    ), 
    y_to = c(
      sin(theta), -sin(theta), 
      sin(theta), sin(theta), 
      0, 0, 
      1, -1
    ), 
    line_type = c(
      "main", "sub", 
      "main", "sub", 
      "main", "main", 
      "main", "main"
    ) # 符号の反転用
  )
  
  # x軸・y軸の目盛線の座標を作成
  scale_df <- tibble::tibble(
    x_from = c(cos(theta), -cos(theta), cos(theta), cos(theta)), 
    y_from = c(sin(theta), sin(theta), sin(theta), -sin(theta)), 
    x_to   = c(cos(theta), -cos(theta), x0, x0), 
    y_to   = c(y0, y0, sin(theta), -sin(theta))
  )
  
  # ラベル用の文字列を作成
  var_label <- paste0(
    "list(", 
    "theta == ", round(theta/pi, digits = 2), " * pi, ", 
    "(list(x, y)) == ", 
    "(list(", round(cos(theta), digits = 2), ", ", round(sin(theta), digits = 2), "))", 
    
    ")"
  )
  fnc_label_vec <- paste(
    c("sin~theta", "cos~theta", "versin~theta", "vercos~theta", "coversin~theta", "covercos~theta"), 
    c(sin(theta), cos(theta), 1-cos(theta), 1+cos(theta), 1-sin(theta), 1+sin(theta)) |> 
      round(digits = 2), 
    sep = " == "
  )
  
  # 単位円における関数線分を作図
  circle_graph <- ggplot() + 
    geom_segment(data = rad_tick_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = grid), 
                 color = "white", show.legend = FALSE) + # θ軸目盛線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)),
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              linewidth = 1) + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = w), 
                 show.legend = FALSE) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), 
              label = "theta", parse = TRUE, 
              size = 5) + # 角ラベル
    geom_segment(data = scale_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to), 
                 linewidth = 0.8, linetype = "dotted") + # x軸・y軸の目盛線
    geom_segment(data = cos_point_df, 
                 mapping = aes(x = f_t, y = y0, xend = f_t, yend = Inf, color = fnc), 
                 linewidth = 0.8, linetype = "dotted", show.legend = FALSE) + # x軸の目盛線
    geom_segment(data = sin_point_df, 
                 mapping = aes(x = x0, y = f_t, xend = -Inf, yend = f_t, color = fnc), 
                 linewidth = 0.8, linetype = "dotted", show.legend = FALSE) + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y, shape = point_type), 
               size = 4, show.legend = FALSE) + # 円周上の点
    geom_point(mapping = aes(x = c(-cos(theta), x0), y = c(y0, -sin(theta)), 
                             color = c("cos", "sin"), shape = "sub"), 
               size = 4, show.legend = FALSE) + # 符号の反転用
    geom_point(data = cos_point_df, 
               mapping = aes(x = f_t, y = y0, color = fnc), 
               size = 4) + # 関数点
    geom_point(data = sin_point_df, 
               mapping = aes(x = x0, y = f_t, color = fnc), 
               size = 4) + # 関数点
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, linetype = line_type), 
                 linewidth = 1) + # 関数線分
    scale_color_manual(breaks = fnc_level_vec, 
                       values = scales::hue_pal()(n = length(fnc_level_vec)), 
                       labels = parse(text = fnc_label_vec), 
                       name = "function") + # 凡例表示用, 色の共通化用
    scale_shape_manual(breaks = c("main", "sub"), 
                       values = c("circle", "circle open")) + # 符号の反転用
    scale_linetype_manual(breaks = c("main", "sub"), 
                          values = c("solid", "dashed")) + # 符号の反転用
    scale_linewidth_manual(breaks = c("normal", "thin", "major", "minor"), 
                           values = c(1, 0.5, 0.5, 0.25)) + # 補助線用, 主・補助目盛線用
    guides(linetype = "none") + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 0), 
          legend.justification = c(0, 0), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = var_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## sin系の関数曲線
  
  # 関数曲線を作図
  curve_sin_graph <- ggplot() + 
    geom_vline(xintercept = theta, 
               linewidth = 0.8, linetype = "dotted") + # θ軸の目盛線
    geom_segment(data = sin_point_df, 
                 mapping = aes(x = t, y = f_t, xend = Inf, yend = f_t, color = fnc), 
                 linewidth = 0.8, linetype = "dotted", show.legend = FALSE) + # y軸の目盛線
    geom_line(data = sin_curve_df, 
              mapping = aes(x = t, y = f_t, color = fnc), 
              linewidth = 1) + # 関数曲線
    geom_point(data = sin_point_df, 
               mapping = aes(x = t, y = f_t, color = fnc), 
               size = 4) + # 関数点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    scale_color_manual(breaks = fnc_level_vec, 
                       values = scales::hue_pal()(n = length(fnc_level_vec)), 
                       labels = parse(text = fnc_label_vec), 
                       name = "function") + # 色の共通化用
    theme(legend.text.align = 0, 
          legend.position = c(0, 0), 
          legend.justification = c(0, 0), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(theta-theta_size, theta), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "sine function", 
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## 軸変換
  
  # 軸の変換曲線の座標を作成
  convert_df <- tidyr::expand_grid(
    tibble::tibble(
      fnc = c("cos", "versin", "vercos"), 
      arc_r = grid_size + cos_point_df[["f_t"]]
    ), 
    u = seq(from = 0, to = 0.5*pi, length.out = 91) # ラジアン
  ) |> 
    dplyr::mutate(
      x0    = -grid_size, 
      y0    = -grid_size, 
      arc_x = x0 + arc_r * cos(u), 
      arc_y = y0 + arc_r * sin(u)
    )
  
  # 軸変換を作図
  convert_graph <- ggplot() + 
    geom_path(data = grid_df, 
              mapping = aes(x = arc_x, y = arc_y, group = arc_r, linewidth = grid), 
              color = "white") + # グリッド線
    geom_line(data = convert_df, 
              mapping = aes(x = arc_x, y = arc_y, color = fnc), 
              linewidth = 0.8, linetype = "dotted") + # 変換曲線
    geom_segment(data = cos_point_df, 
                 mapping = aes(x = f_t, y = -grid_size, xend = f_t, yend = -Inf, color = fnc), 
                 linewidth = 0.8, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = cos_point_df, 
                 mapping = aes(x = -grid_size, y = f_t, xend = -Inf, yend = f_t, color = fnc), 
                 linewidth = 0.8, linetype = "dotted") + # x軸の目盛線
    geom_point(data = cos_point_df, 
               mapping = aes(x = f_t, y = -grid_size, color = fnc), 
               size = 4) + # 関数点
    geom_point(data = cos_point_df, 
               mapping = aes(x = -grid_size, y = f_t, color = fnc), 
               size = 4) + # 関数点
    scale_color_manual(breaks = fnc_level_vec, 
                       values = scales::hue_pal()(n = length(fnc_level_vec)), 
                       labels = parse(text = fnc_label_vec)) + # 色の共通化用
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25), guide = "none") + # 主・補助目盛線用
    guides(color = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(x = expression(x), 
         y = expression(x))
  
  ## cos系の関数曲線
  
  # 関数曲線を作図
  curve_cos_graph <- ggplot() + 
    geom_vline(xintercept = theta, 
               linewidth = 0.8, linetype = "dotted") + # θ軸の目盛線
    geom_segment(data = cos_point_df, 
                 mapping = aes(x = t, y = f_t, xend = Inf, yend = f_t, color = fnc), 
                 linewidth = 0.8, linetype = "dotted", show.legend = FALSE) + # x軸の目盛線
    geom_line(data = cos_curve_df, 
              mapping = aes(x = t, y = f_t, color = fnc), 
              linewidth = 1) + # 関数曲線
    geom_point(data = cos_point_df, 
               mapping = aes(x = t, y = f_t, color = fnc), 
               size = 4) + # 関数点
    scale_x_continuous(breaks = rad_break_vec,
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    scale_color_manual(breaks = fnc_level_vec, 
                       values = scales::hue_pal()(n = length(fnc_level_vec)), 
                       labels = parse(text = fnc_label_vec), 
                       name = "function") + # 色の共通化用
    theme(legend.text.align = 0, 
          legend.position = c(0, 0), 
          legend.justification = c(0, 0), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(theta-theta_size, theta), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "cosine function", 
         subtitle = "", # (空行)
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_cos_graph, convert_graph, 
    curve_sin_graph, circle_graph, 
    nrow = 2, ncol = 2
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 12, height = 12, units = "in", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "circular/figure/circular/versed_curves_form.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


