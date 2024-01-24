
# 逆sin関数の定義の可視化 ---------------------------------------------------------------

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

# 終域内のラジアンに変換
tau_vals <- asin(sin(theta_vals))


# 円周上の点の座標を作成
anim_point_df <- dplyr::bind_rows(
  # 元の関数の入力
  tibble::tibble(
    angle = "theta", # 角度カテゴリ
    frame_i = 1:frame_num, 
    x = cos(theta_vals), 
    y = sin(theta_vals), 
    point_type = "main" # 入出力用
  ), 
  # 逆関数の出力
  tibble::tibble(
    angle = "tau", # 角度カテゴリ
    frame_i = 1:frame_num, 
    x = cos(tau_vals), 
    y = sin(tau_vals), 
    point_type = "sub" # 入出力用
  )
)

# 半径線の終点の座標を作成
anim_radius_df <- dplyr::bind_rows(
  # 始線
  tibble::tibble(
    frame_i = 1:frame_num, 
    x = 1, 
    y = 0, 
    line_type = "main" # 入出力用
  ), 
  # 動径
  anim_point_df |> 
    dplyr::select(frame_i, x, y, line_type = point_type)
)

# 動径間の補助線の座標を作成
anim_auxil_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  x_from = cos(theta_vals), 
  y_from = sin(theta_vals), 
  x_to   = cos(tau_vals), 
  y_to   = sin(tau_vals)
)


# 角マークの座標を作成
d_in  <- 0.2
d_spi <- 0.005
d_out <- 0.3
anim_angle_mark_df <- dplyr::bind_rows(
  # 元の関数の入力
  tibble::tibble(
    angle = "theta", # 角度カテゴリ
    frame_i = 1:frame_num
  ) |> 
    dplyr::reframe(
      u = seq(from = 0, to = theta_vals[frame_i], length.out = 600), .by = dplyr::everything()
    ) |> # フレームごとにラジアンを作成
    dplyr::mutate(
      x = (d_in + d_spi*u) * cos(u), 
      y = (d_in + d_spi*u) * sin(u)
    ), 
  # 逆関数の出力
  tibble::tibble(
    angle = "tau", # 角度カテゴリ
    frame_i = 1:frame_num
  ) |> 
    dplyr::reframe(
      u = seq(from = 0, to = tau_vals[frame_i], length.out = 600), .by = dplyr::everything()
    ) |> # フレームごとにラジアンを作成
    dplyr::mutate(
      x = d_out * cos(u), 
      y = d_out * sin(u)
    )
)

# 角ラベルの座標を作成
d_in  <- 0.1
d_out <- 0.4
anim_angle_label_df <- dplyr::bind_rows(
  # 元の関数の入力
  tibble::tibble(
    frame_i = 1:frame_num, 
    u = 0.5 * theta_vals, 
    x = d_in * cos(u), 
    y = d_in * sin(u), 
    angle_label = "theta"
  ), 
  # 逆関数の出力
  tibble::tibble(
    frame_i = 1:frame_num, 
    u = 0.5 * tau_vals, 
    x = d_out * cos(u), 
    y = d_out * sin(u), 
    angle_label = "tau"
  )
)


# 関数の描画順を指定
fnc_level_vec <- c("arcsin", "sin", "cos")

# 関数円弧の座標を作成
anim_radian_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  r = 1, # 半径
) |> 
  dplyr::reframe(
    u = seq(from = 0, to = tau_vals[frame_i], length.out = 600), .by = dplyr::everything()
  ) |> # フレームごとにラジアンを作成
  dplyr::mutate(
    x = r * cos(u), 
    y = r * sin(u)
  )

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
    rep(each = frame_num), # 関数ラベル用
)

# 関数ラベルの座標を作成
anim_fnc_label_df <- dplyr::bind_rows(
  # 円関数ラベル
  anim_fnc_seg_df |> 
    dplyr::filter(label_flag) |> # ラベル付け線分を抽出
    dplyr::summarise(
      x = median(c(x_from, x_to)), 
      y = median(c(y_from, y_to)), .by = c(fnc, frame_i)
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
    ), 
  # 逆円関数ラベル
  tibble::tibble(
    fnc = factor("arcsin", levels = fnc_level_vec), 
    frame_i = 1:frame_num, 
    x = cos(0.5 * tau_vals), 
    y = sin(0.5 * tau_vals), 
    fnc_label = "arcsin(sin~theta)", 
    a = 0, 
    h = -0.1, 
    v = 0.5
  )
)


# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  var_label = paste0(
    "list(", 
    "theta == ", round(theta_vals/pi, digits = 2), " * pi, ", 
    "tau == ", round(tau_vals/pi, digits = 2), " * pi", 
    ")"
  )
)
fnc_label_vec <- c(arcsin = "inverse sine", sin = "sine", cos = "cosine")

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
               mapping = aes(x = 0, y = 0, xend = x, yend = y, 
                             linewidth = line_type, linetype = line_type), 
               show.legend = FALSE) + # 半径線
  geom_path(data = anim_angle_mark_df, 
            mapping = aes(x = x, y = y, group = angle)) + # 角マーク
  geom_text(data = anim_angle_label_df, 
            mapping = aes(x = x, y = y, label = angle_label), 
            parse = TRUE, size = 5) + # 角ラベル
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y, shape = point_type), 
             size = 4, show.legend = FALSE) + # 円周上の点
  geom_segment(data = anim_auxil_df, 
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to), 
               linetype = "dotted") + # 動径間の補助線
  geom_path(data = anim_radian_df, 
            mapping = aes(x = x, y = y, color = "arcsin"), 
            linewidth = 1) + # 関数円弧
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
  scale_color_manual(breaks = fnc_level_vec, 
                     values = c("red", scales::hue_pal()(n = length(fnc_level_vec)-1)), 
                     labels = fnc_label_vec, name = "function") + # 凡例表示用
  scale_shape_manual(breaks = c("main", "sub"), 
                     values = c("circle", "circle open")) + # 入出力用
  scale_linetype_manual(breaks = c("main", "sub"), 
                        values = c("solid", "dashed")) + # 入出力用
  scale_linewidth_manual(breaks = c("main", "sub", "major", "minor"), 
                         values = c(1, 0.5, 0.5, 0.25)) + # 入出力用, 主・補助目盛線用
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "circular functions", 
       subtitle = "", # (ラベル表示用の空行)
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = frame_num, fps = 15, width = 7, height = 6, units = "in", res = 250, 
  renderer = gganimate::av_renderer(file = "circular/figure/inverse/arcsin_circle.mp4")
)


# 単位円と曲線の関係 ---------------------------------------------------------------

# 一時書き出し先を指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]


# 関数曲線の座標を作成
curve_df <- tibble::tibble(
  z        = seq(from = -1, to = 1, length.out = 1001), # 定義域内の値
  arcsin_z = asin(z)
)


# 範囲πにおける目盛数を指定
tick_num <- 6

# ラジアン軸目盛用の値を作成
rad_break_vec <- seq(from = -pi, to = pi, by = pi/tick_num)
rad_label_vec <- paste(round(rad_break_vec/pi, digits = 2), "* pi")


# グラフサイズを設定
axis_x_size <- 1.5
axis_y_size <- 2
axis_z_size <- 1.5

# 関数の描画順を指定
fnc_level_vec <- c("arcsin", "sin", "cos")

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  # 終域内のラジアンに変換
  tau <- asin(sin(theta))
  
  # 関数点の座標を作成
  fnc_point_df <- tibble::tibble(
    t        = theta, 
    sin_t    = sin(t), 
    arcsin_z = asin(sin_t)
  )
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    angle = c("theta", "tau"), # 角度カテゴリ
    t = c(theta, tau), 
    x = cos(t), 
    y = sin(t), 
    point_type = c("main", "sub") # 入出力用
  )
  
  # 半径線の終点の座標を作成
  radius_df <- dplyr::bind_rows(
    # 始線
    tibble::tibble(
      x = 1, 
      y = 0, 
      line_type = "main" # 入出力用
    ), 
    # 動径
    point_df |> 
      dplyr::select(x, y, line_type = point_type)
  )
  
  # 角マークの座標を作成
  d_in  <- 0.2
  d_spi <- 0.005
  d_out <- 0.3
  angle_mark_df <- dplyr::bind_rows(
    # 元の関数の入力
    tibble::tibble(
      angle = "theta", # 角度カテゴリ
      u = seq(from = 0, to = theta, length.out = 600), 
      x = (d_in + d_spi*u) * cos(u), 
      y = (d_in + d_spi*u) * sin(u)
    ), 
    # 逆関数の出力
    tibble::tibble(
      angle = "tau", # 角度カテゴリ
      u = seq(from = 0, to = tau, length.out = 600), 
      x = d_out * cos(u), 
      y = d_out * sin(u)
    )
  )
  
  # 角ラベルの座標を作成
  d_in  <- 0.1
  d_out <- 0.4
  angle_label_df <- tibble::tibble(
    u = 0.5 * c(theta, tau), 
    x = c(d_in, d_out) * cos(u), 
    y = c(d_in, d_out) * sin(u), 
    angle_label = c("theta", "tau")
  )
  
  # 関数円弧の座標を作成
  radian_df <- tibble::tibble(
    u = seq(from = 0, to = tau, length.out = 600), 
    r = 1, # 半径
    x = r * cos(u), 
    y = r * sin(u)
  )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "arcsin", 
      "sin", "sin", 
      "cos", "cos"
    ) |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
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
    ), 
    line_type = c(
      "sub", 
      "main", "main", 
      "main", "main"
    ) # 補助線用
  )
  
  # 関数ラベルの座標を作成
  fnc_label_df <- tibble::tibble(
    fnc = c("arcsin", "sin") |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
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
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = grid), 
                 color = "white", show.legend = FALSE) + # θ軸目盛線
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
               mapping = aes(x = x, y = y, shape = point_type), 
               size = 4, show.legend = FALSE) + # 円周上の点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = 1, y = arcsin_z), 
               size = 4) + # 関数点
    geom_segment(mapping = aes(x = c(cos(tau), 1), y = c(sin(tau), tau), 
                               xend = c(1, Inf), yend = c(tau, tau)), 
                 linewidth = 0.8, linetype = "dotted") + # y軸の目盛線
    geom_path(data = radian_df, 
              mapping = aes(x = x, y = y), 
              color = "red", linewidth = 1) + # 関数円弧
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, linetype = line_type), 
                 linewidth = 1) + # 関数線分
    geom_text(data = fnc_label_df,
              mapping = aes(x = x, y = y, label = fnc_label, color = fnc,
                            angle = a, hjust = h, vjust = v),
              parse = TRUE, show.legend = FALSE) + # 関数ラベル
    scale_color_manual(breaks = fnc_level_vec, 
                       values = c("red", scales::hue_pal()(n = length(fnc_level_vec)-1)), 
                       labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用, 色の共通化用
    scale_shape_manual(breaks = c("main", "sub"), 
                       values = c("circle", "circle open")) + # 入出力用
    scale_linetype_manual(breaks = c("main", "sub"), 
                          values = c("solid", "dashed")) + # 補助線用
    scale_linewidth_manual(breaks = c("main", "sub", "major", "minor"), 
                           values = c(1, 0.5, 0.5, 0.25)) + # 補助線用, 主・補助目盛線用
    guides(linetype = "none") + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_x_size, axis_x_size), 
                ylim = c(-axis_y_size, axis_y_size)) + 
    labs(title = "circular functions", 
         subtitle = parse(text = var_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## 関数曲線
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c("arcsin", "sin") |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
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
    geom_line(data = curve_df, 
              mapping = aes(x = z, y = arcsin_z), 
              linewidth = 1) + # 関数曲線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = sin_t, y = arcsin_z), 
               size = 4) + # 関数点
    geom_segment(mapping = aes(x = c(sin(theta), sin(theta)), y = c(tau, tau), 
                               xend = c(-Inf, sin(theta)), yend = c(tau, -Inf)), 
                 linewidth = 0.8, linetype = "dotted") + # z・τ軸の目盛線
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = z_from, y = y_from, xend = z_to, yend = y_to, color = fnc), 
                 linewidth = 1) + # 関数線分
    scale_color_manual(breaks = c("arcsin", "sin"), 
                       values = c("red", "#F8766D")) + # 色の共通化用
    scale_y_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # τ軸目盛
    guides(color = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_z_size, axis_z_size), 
                ylim = c(-axis_y_size, axis_y_size)) + 
    labs(title = "inverse sine function", 
         subtitle = parse(text = coord_label), 
         x = expression(z == sin~tau == sin~theta), 
         y = expression(tau == arcsin~z))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_graph, curve_graph
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 12, height = 8, units = "in", dpi = 120)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "circular/figure/inverse/arcsin_curves.mp4", framerate = 15) -> tmp_path # mp4ファイルを書出


