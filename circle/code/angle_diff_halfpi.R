
# 余角(π/2 - θ)の可視化 ------------------------------------------------------------------

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


# 単位円と角度の関係 ------------------------------------------------------------

# 点用のラジアンを指定
theta <- 2/6 * pi

# 余角を計算
tau <- 0.5*pi - theta


# 円周上の点の座標を作成
point_df <- tibble::tibble(
  t = c(theta, -theta, tau), 
  x = cos(t), 
  y = sin(t), 
  type = c("main", "sub", "target") # 角度カテゴリ
)

# 半径線の終点の座標を作成
radius_df <- dplyr::bind_rows(
  # 始線
  tibble::tibble(
    x = 1, 
    y = 0, 
    type = "main"
  ), 
  # 動径
  point_df |> 
    dplyr::select(x, y, type)
)


# 角マークの座標を作成
ds <- 0.005
angle_mark_df <- tibble::tibble(
  t    = c(theta, -theta, tau), 
  type = c("main", "sub", "target"), # 角度カテゴリ
  d    = c(0.15, 0.25, 0.35) # マークサイズを指定
) |> 
  dplyr::reframe(
    u = seq(from = 0, to = t, length.out = 600), .by = dplyr::everything()
  ) |> # 円弧用のラジアンを作成
  dplyr::mutate(
    x = (d + ds*u) * cos(u), 
    y = (d + ds*u) * sin(u)
  )

# 角ラベルの座標を作成
angle_label_df <- tibble::tibble(
  d = c(0.1, 0.3, 0.4), # ラベル位置を指定
  u = 0.5 * c(theta, -theta, tau), 
  x = d * cos(u), 
  y = d * sin(u), 
  angle_label = c("theta", "-theta", "tau")
)

# 直角マークの座標を作成
d <- 0.1
rightangle_mark_df <- tibble::tibble(
  u = c(-theta, 0.25*pi-theta, tau), 
  x = c(d, sqrt(2)*d, d) * cos(u), 
  y = c(d, sqrt(2)*d, d) * sin(u)
)


# 関数の描画順を指定
fnc_level_vec <- c("sin", "cos")

# 関数線分の座標を作成
fnc_seg_df <- tibble::tibble(
  fnc = c(
    "sin", "cos", "sin", "cos", 
    "sin", "cos"
  ) |> 
    factor(levels = fnc_level_vec), # 関数カテゴリ
  type = c(
    "main", "main", "main", "main", 
    "target", "target"
  ), # 角度カテゴリ
  x_from = c(
    cos(theta), 0, 0, 0, 
    0, cos(tau)
  ), 
  y_from = c(
    0, 0, 0, sin(theta), 
    0, 0
  ), 
  x_to = c(
    cos(theta), cos(theta), 0, cos(theta), 
    cos(tau), cos(tau)
  ), 
  y_to = c(
    sin(theta), 0, sin(theta), sin(theta), 
    0, sin(tau)
  )
)

# 関数ラベルの座標を作成
fnc_label_df <- tibble::tibble(
  fnc = c(
    "sin", "cos", 
    "sin", "cos"
  ), 
  x = c(
    cos(theta), 0.5*cos(theta), 
    0.5*cos(tau), cos(tau)
  ), 
  y = c(
    0.5*sin(theta), 0, 
    0, 0.5*sin(tau)
  ), 
  fnc_label = c(
    "sin~theta", "cos~theta", 
    "cos~tau", "sin~tau"
  ), 
  a = c(
    90, 0, 
    0, 90
  ), 
  h = 0.5, 
  v = c(
    -0.5, -0.5, 
    1.5, 1.5
  )
)

# グラフサイズを設定
axis_size <- 1.5

# ラベル用の文字列を作成
def_label <- "tau == frac(pi, 2) - theta"
angle_label_vec <- paste(
  c("theta", "-theta", "tau"), 
  c(theta/pi, -theta/pi, tau/pi) |> 
    round(digits = 2), 
  sep = " == "
) |> 
  paste("* pi")
fnc_label_vec <- paste(
  c("sin~theta", "cos~theta"), 
  c(sin(theta), cos(theta)) |> 
    round(digits = 2), 
  sep = " == "
)

# 単位円における偏角と関数線分を作図
ggplot() + 
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
               mapping = aes(x = 0, y = 0, xend = x, yend = y, linetype = type), 
               linewidth = 1, show.legend = FALSE) + # 半径線
  geom_path(data = rightangle_mark_df, 
            mapping = aes(x = x, y = y)) + # 直角マーク
  geom_path(data = angle_mark_df, 
            mapping = aes(x = x, y = y, linetype = type), 
            show.legend = FALSE) + # 角マーク
  geom_text(data = angle_label_df, 
            mapping = aes(x = x, y = y, label = angle_label), 
            size = 5, parse = TRUE) + # 角ラベル
  geom_point(data = point_df, 
             mapping = aes(x = x, y = y, shape = type), 
             size = 4, show.legend = FALSE) + # 円周上の点
  geom_segment(data = fnc_seg_df, 
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                             color = fnc, linetype = type), 
               linewidth = 1) + # 関数線分
  geom_text(data = fnc_label_df, 
            mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                          hjust = h, vjust = v, angle = a), 
            parse = TRUE, show.legend = FALSE) + # 関数ラベル
  scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
  scale_shape_manual(breaks = c("main", "sub", "target"), 
                     values = c("circle", "circle open", "circle open")) + # 補助点用
  scale_linetype_manual(breaks = c("main", "sub", "target"), 
                        values = c("solid", "dashed", "twodash"), 
                        labels = parse(text = angle_label_vec), 
                        name = "angle") + # 補助線用, 凡例表示用
  scale_linewidth_manual(breaks = c("major", "minor"), 
                         values = c(0.5, 0.25)) + # 主・補助目盛線用
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + 
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "complementary angle", 
       subtitle = parse(text = def_label), 
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))


# 単位円と角度の関係 ------------------------------------------------------------

# フレーム数を指定
frame_num <- 300

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# 余角を計算
tau_vals <- 0.5*pi - theta_vals


# 円周上の点の座標を作成
anim_point_df <- tidyr::expand_grid(
  type    = c("main", "sub", "target"), # 角度カテゴリ
  frame_i = 1:frame_num # フレーム番号
) |> # フレームごとに複製
  dplyr::bind_cols(
    t = c(theta_vals, -theta_vals, tau_vals)
  ) |> 
  dplyr::mutate(
    x = cos(t), 
    y = sin(t)
  )

# 半径線の終点の座標を作成
anim_radius_df <- dplyr::bind_rows(
  # 始線
  tibble::tibble(
    frame_i = 1:frame_num, 
    x = 1, 
    y = 0, 
    type = "main"
  ), 
  # 動径
  anim_point_df |> 
    dplyr::select(frame_i, x, y, type)
)


# 角マークの座標を作成
ds <- 0.005
anim_angle_mark_df <- tidyr::expand_grid(
  tibble::tibble(
    type = c("main", "sub", "target"), 
    d    = c(0.15, 0.25, 0.35) # マークサイズを指定
  ), 
  frame_i = 1:frame_num
) |> # フレームごとに複製
  dplyr::bind_cols(
    t = c(theta_vals, -theta_vals, tau_vals)
  ) |> 
  dplyr::reframe(
    u = seq(from = 0, to = t, length.out = 600), .by = dplyr::everything()
  ) |> # フレームごとにラジアンを作成
  dplyr::mutate(
    x = (d + ds*u) * cos(u), 
    y = (d + ds*u) * sin(u)
  )

# 角ラベルの座標を作成
anim_angle_label_df <- tidyr::expand_grid(
  tibble::tibble(
    angle_label = c("theta", "-theta", "tau"), 
    d           = c(0.1, 0.25, 0.4) # ラベル位置を指定
  ), 
  frame_i = 1:frame_num
) |> # フレームごとに複製
  dplyr::bind_cols(
    u = 0.5 * c(theta_vals, -theta_vals, tau_vals)
  ) |> 
  dplyr::mutate(
    x = d * cos(u), 
    y = d * sin(u)
  )

# 直角マークの座標を作成
d <- 0.1
anim_rightangle_mark_df <- tidyr::expand_grid(
  d       = c(d, sqrt(2)*d, d), 
  frame_i = 1:frame_num
) |> # フレームごとに複製
  dplyr::bind_cols(
    u = c(-theta_vals, 0.25*pi-theta_vals, tau_vals)
  ) |> 
  dplyr::mutate(
    x = d * cos(u), 
    y = d * sin(u)
  ) |> 
    dplyr::arrange(frame_i) # 確認用


# 関数の描画順を指定
fnc_level_vec <- c("sin", "cos")

# 関数線分の座標を作成
line_num <- 6
anim_fnc_seg_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = line_num), 
  fnc = c(
    "sin", "cos", "sin", "cos", 
    "sin", "cos"
  ) |> 
    rep(each = frame_num) |> 
    factor(levels = fnc_level_vec), # 関数カテゴリ
  type = c(
    "main", "main", "main", "main", 
    "target", "target"
  ) |> 
    rep(each = frame_num), # 角度カテゴリ
  x_from = c(
    cos(theta_vals), rep(0, times = frame_num), rep(0, times = frame_num), rep(0, times = frame_num), 
    rep(0, times = frame_num), cos(tau_vals)
  ), 
  y_from = c(
    rep(0, times = frame_num), rep(0, times = frame_num), rep(0, times = frame_num), sin(theta_vals), 
    rep(0, times = frame_num), rep(0, times = frame_num)
  ), 
  x_to = c(
    cos(theta_vals), cos(theta_vals), rep(0, times = frame_num), cos(theta_vals), 
    cos(tau_vals), cos(tau_vals)
  ), 
  y_to = c(
    sin(theta_vals), rep(0, times = frame_num), sin(theta_vals), sin(theta_vals), 
    rep(0, times = frame_num), sin(tau_vals)
  ), 
  label_flag = c(
    TRUE, TRUE, FALSE, FALSE, 
    TRUE, TRUE
  ) |> 
    rep(each = frame_num) # 関数ラベル用
)

# ラベル設定を指定
setting_df <- tibble::tibble(
  fnc = c(
    "sin", "cos", 
    "sin", "cos"
  ), 
  type = c(
    "main", "main", 
    "target", "target"
  ), 
  fnc_label = c(
    "sin~theta", "cos~theta", 
    "cos~tau", "sin~tau"
  ), 
  a = c(
    90, 0, 
    0, 90
  ), 
  h = 0.5, 
  v = c(
    -0.5, -0.5, 
    1.5, 1.5
  )
)

# 関数ラベルの座標を作成
anim_fnc_label_df <- anim_fnc_seg_df |> 
  dplyr::filter(label_flag) |> # ラベル付け線分を抽出
  dplyr::summarise(
    x = median(c(x_from, x_to)), 
    y = median(c(y_from, y_to)), .by = c(frame_i, fnc, type)
  ) |> # 中点に配置
  dplyr::left_join(setting_df, by = c("fnc", "type")) # ラベル設定を追加
  

# グラフサイズを設定
axis_size <- 1.5

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
angle_label_vec <- c("theta", "-theta", "tau == frac(pi, 2) - theta")
fnc_label_vec   <- c(sin = "sine", cos = "cosine")

# 単位円における偏角と関数線分を作図
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
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                             xend = c(Inf, 0), yend = c(0, Inf)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y), 
            linewidth = 1) + # 円周
  geom_segment(data = anim_radius_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, linetype = type), 
               linewidth = 1, show.legend = FALSE) + # 半径線
  geom_path(data = anim_rightangle_mark_df, 
            mapping = aes(x = x, y = y)) + # 直角マーク
  geom_path(data = anim_angle_mark_df, 
            mapping = aes(x = x, y = y, linetype = type), 
            show.legend = FALSE) + # 角マーク
  geom_text(data = anim_angle_label_df, 
            mapping = aes(x = x, y = y, label = angle_label), 
            size = 5, parse = TRUE) + # 角ラベル
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y, shape = type), 
             size = 4, show.legend = FALSE) + # 円周上の点
  geom_segment(data = anim_fnc_seg_df, 
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                             color = fnc, linetype = type), 
               linewidth = 1) + # 関数線分
  geom_text(data = anim_fnc_label_df, 
            mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                          hjust = h, vjust = v, angle = a), 
            parse = TRUE, show.legend = FALSE) + # 関数ラベル
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル:(subtitleの代用)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
  scale_shape_manual(breaks = c("main", "sub", "target"), 
                     values = c("circle", "circle open", "circle open")) + # 補助点用
  scale_linetype_manual(breaks = c("main", "sub", "target"), 
                        values = c("solid", "dashed", "twodash"), 
                        labels = parse(text = angle_label_vec), 
                        name = "angle") + # 補助線用, 凡例表示用
  scale_linewidth_manual(breaks = c("major", "minor"), 
                         values = c(0.5, 0.25)) + # 主・補助目盛線用
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) + 
  theme(legend.text.align = 0) + 
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "complementary angle", 
       subtitle = "", # (ラベル表示用の空行)
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = frame_num, fps = 15, width = 8, height = 8, units = "in", res = 250, 
  renderer = gganimate::av_renderer(file = "circle/figure/angle/diff_halfpi_circle.mp4")
)


