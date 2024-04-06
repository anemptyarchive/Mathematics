
# π/2 - θ(余角)の可視化 ------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

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
  ) |> # ラジアンを結合
  dplyr::mutate(
    x = cos(t), 
    y = sin(t)
  )
anim_point_symmetry_df <- anim_point_df |> 
  dplyr::filter(type %in% c("main", "target")) # 対称な点を抽出

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
  ) |> # ラジアンを結合
  dplyr::reframe(
    u = seq(from = 0, to = t, length.out = 600), .by = dplyr::everything()
  ) |> # 中間値のラジアンを作成
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
  ) |> # ラジアンを結合
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
  ) |> # ラジアンを結合
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
  ) |> 
    factor(levels = fnc_level_vec), 
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

# グラフサイズを設定
axis_size <- 1.5

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
  # geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # 対象の軸
  # geom_line(data = anim_point_symmetry_df,
  #           mapping = aes(x = x, y = y),
  #           linetype = "dotted") + # 対称な点間
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y), 
            linewidth = 1) + # 円周
  geom_segment(mapping = aes(x = 0, y = 0, xend = 1, yend = 0), 
               linewidth = 1) + # 始線
  geom_segment(data = anim_point_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y, linetype = type), 
               linewidth = 1, show.legend = TRUE) + # 動径線
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
            parse = TRUE, hjust = 0, vjust = -0.5) + # 角度ラベル:(subtitleの代用)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
  scale_shape_manual(breaks = c("main", "sub", "target"), 
                     values = c("circle", "diamond open", "circle open")) + # 補助点用
  scale_linetype_manual(breaks = c("main", "sub", "target"), 
                        values = c("solid", "dashed", "twodash"), 
                        labels = parse(text = angle_label_vec), 
                        name = "angle") + # 補助線用, 凡例表示用
  scale_linewidth_manual(breaks = c("major", "minor"), 
                         values = c(0.5, 0.25)) + # 主・補助目盛線用
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5)), 
         linewidth = "none", shape = "none") + 
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
  renderer = gganimate::av_renderer(file = "circle/figure/declination/diff_right_angle.mp4")
)


