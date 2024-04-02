
# cot関数の定義の可視化 -----------------------------------------------------------

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


### パターン1 -----

# 半径線の終点の座標を作成
anim_radius_df <- dplyr::bind_rows(
  # 始線
  tibble::tibble(
    frame_i = 1:frame_num, 
    x = 1, 
    y = 0, 
    w = "normal" # 補助線用
  ), 
  # 動径
  anim_point_df |> 
    dplyr::select(frame_i, x, y) |> 
    dplyr::mutate(
      w = "normal" # 補助線用
    ), 
  # 補助線
  tibble::tibble(
    frame_i = 1:frame_num, 
    x = 0, 
    y = 1, 
    w = "thin" # 補助線用
  )
)


# 関数の描画順を指定
fnc_level_vec <- c("cot", "tan", "sin", "cos", "exsec", "excsc")

# 関数線分の座標を作成
line_num <- 8
anim_fnc_seg_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = line_num), 
  fnc = c(
    "cot", "tan", 
    "sin", "sin", 
    "cos", "cos", 
    "exsec", "excsc"
  ) |> 
    rep(each = frame_num) |> 
    factor(levels = fnc_level_vec), # 関数カテゴリ
  x_from = c(
    rep(0, times = frame_num), rep(1, times = frame_num), 
    rep(0, times = frame_num), cos(theta_vals), 
    rep(0, times = frame_num), rep(0, times = frame_num), 
    cos(theta_vals), cos(theta_vals)
  ), 
  y_from = c(
    rep(1, times = frame_num), rep(0, times = frame_num), 
    rep(0, times = frame_num), rep(0, times = frame_num), 
    rep(0, times = frame_num), sin(theta_vals), 
    sin(theta_vals), sin(theta_vals)
  ), 
  x_to = c(
    1/tan(theta_vals), rep(1, times = frame_num), 
    rep(0, times = frame_num), cos(theta_vals), 
    cos(theta_vals), cos(theta_vals), 
    rep(1, times = frame_num), 1/tan(theta_vals)
  ), 
  y_to = c(
    rep(1, times = frame_num), 
    tan(theta_vals), 
    sin(theta_vals), sin(theta_vals), 
    rep(0, times = frame_num), sin(theta_vals), 
    tan(theta_vals), 
    rep(1, times = frame_num)
  ), 
  w = c(
    "normal", "normal", 
    "normal", "normal", 
    "normal", "normal", 
    "bold", "thin"
  ) |> 
    rep(each = frame_num), # 重なり対策用
  label_flag = c(
    TRUE, TRUE, 
    TRUE, FALSE, 
    TRUE, FALSE, 
    TRUE, TRUE
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
      fnc = c("cot", "tan", "sin", "cos", "exsec", "excsc"), 
      fnc_label = c("cot~theta", "tan~theta", "sin~theta", "cos~theta", "exsec~theta", "excsc~theta"), 
      a = c(0,    90,  90,  0,   0,    0), 
      h = c(0.5,  0.5, 0.5, 0.5, 1.1, -0.1), 
      v = c(-0.5, 1,  -0.5, 1,   0.5,  0.5)
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
    "cot~theta == ", round(1/tan(t), digits = 2), 
    ")"
  )
)
fnc_label_vec <- c(
  cot = "cotangent", tan = "tangent", sin = "sine", cos = "cosine", exsec = "exsecant", excsc = "excosecant"
)


# グラフサイズを設定
axis_size <- 2

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
               mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = w), 
               show.legend = FALSE) + # 半径線
  geom_path(data = anim_angle_mark_df, 
            mapping = aes(x = x, y = y)) + # 角マーク
  geom_text(data = anim_angle_label_df, 
            mapping = aes(x = x, y = y), 
            label = "theta", parse = TRUE, 
            size = 5) + # 角ラベル
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y), 
             size = 4) + # 円周上の点
  geom_vline(xintercept = 1, linetype = "dashed") + # 補助線
  geom_hline(yintercept = 1, linetype = "dashed") + # 補助線
  geom_segment(data = anim_fnc_seg_df, 
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                             color = fnc, linewidth = w)) + # 関数線分
  geom_text(data = anim_fnc_label_df, 
            mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                          hjust = h, vjust = v, angle = a), 
            parse = TRUE, show.legend = FALSE) + # 関数ラベル
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル:(subtitleの代用)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_color_hue(labels = fnc_label_vec, name = "function") + # 凡例表示用
  scale_linewidth_manual(breaks = c("bold", "normal", "thin", "major", "minor"), 
                         values = c(1.5, 1, 0.5, 0.5, 0.25)) + # 補助線用, 重なり対策用, 主・補助目盛線用
  guides(linewidth = "none") + 
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "circular functions", 
       subtitle = "", # (ラベル表示用の空行)
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = frame_num, fps = 15, width = 8, height = 8, units = "in", res = 250, 
  renderer = gganimate::av_renderer(file = "circular/figure/cotangent/definition_circle_1.mp4")
)


### パターン2 -----

# 半径線の終点の座標を作成
anim_radius_df <- dplyr::bind_rows(
  # 始線
  tibble::tibble(
    frame_i = 1:frame_num, 
    x = 1, 
    y = 0, 
    w = "normal" # 補助線用
  ), 
  # 動径
  anim_point_df |> 
    dplyr::select(frame_i, x, y) |> 
    dplyr::mutate(
      w = "normal" # 補助線用
    ), 
  # 補助線
  tibble::tibble(
    frame_i = 1:frame_num, 
    x = rep(0, times = frame_num), 
    y = ifelse(sin(theta_vals) >= 0, yes = 1, no = 0), 
    w = "thin" # 補助線用
  )
)


# 関数の描画順を指定
fnc_level_vec <- c("cot", "tan", "sin", "cos", "exsec", "excsc")

# 関数線分の座標を作成
line_num <- 8
anim_fnc_seg_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = line_num), 
  fnc = c(
    "cot", "tan", 
    "sin", "sin", 
    "cos", "cos", 
    "exsec", "excsc"
  ) |> 
    rep(each = frame_num) |> 
    factor(levels = fnc_level_vec), # 関数カテゴリ
  x_from = c(
    cos(theta_vals), 
    cos(theta_vals), 
    rep(0, times = frame_num), rep(0, times = frame_num), 
    rep(0, times = frame_num), rep(0, times = frame_num), 
    rep(1, times = frame_num), 
    rep(0, times = frame_num)
  ), 
  y_from = c(
    sin(theta_vals), 
    sin(theta_vals), 
    rep(0, times = frame_num), rep(0, times = frame_num), 
    sin(theta_vals), ifelse(sin(theta_vals) >= 0, yes = 1, no = -1), 
    rep(0, times = frame_num), 
    rep(1, times = frame_num)
  ), 
  x_to = c(
    rep(0, times = frame_num), 
    1/cos(theta_vals), 
    rep(0, times = frame_num), abs(sin(theta_vals))*cos(theta_vals), 
    cos(theta_vals), abs(sin(theta_vals))*cos(theta_vals), 
    1/cos(theta_vals), 
    rep(0, times = frame_num)
  ), 
  y_to = c(
    1/sin(theta_vals), 
    rep(0, times = frame_num), 
    sin(theta_vals), abs(sin(theta_vals))*sin(theta_vals), 
    sin(theta_vals), abs(sin(theta_vals))*sin(theta_vals), 
    rep(0, times = frame_num), 
    1/sin(theta_vals)
  ), 
  w = c(
    "normal", "normal", 
    "bold", "normal", 
    "normal", "normal", 
    "normal", "thin"
  ) |> 
    rep(each = frame_num), # 重なり対策用
  label_flag = c(
    TRUE, TRUE, 
    TRUE, FALSE, 
    TRUE, FALSE, 
    TRUE, TRUE
  ) |> 
    rep(each = frame_num), # 関数ラベル用
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
      fnc = c("cot", "tan", "sin", "cos", "exsec", "excsc"), 
      fnc_label = c("cot~theta", "tan~theta", "sin~theta", "cos~theta", "exsec~theta", "excsc~theta"), 
      a = c(0,     0,    90,   0,   0,   90), 
      h = c(-0.2, -0.2,  0.5,  0.5, 0.5, 0.5), 
      v = c(0.5,   0.5, -0.5, -0.5, 1,   1)
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
    "cot~theta == ", round(1/tan(t), digits = 2), 
    ")"
  )
)
fnc_label_vec <- c(
  cot = "cotangent", tan = "tangent", sin = "sine", cos = "cosine", exsec = "exsecant", excsc = "excosecant"
)


# グラフサイズを設定
axis_size <- 2

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
               mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = w), 
               show.legend = FALSE) + # 半径線
  geom_path(data = anim_angle_mark_df, 
            mapping = aes(x = x, y = y)) + # 角マーク
  geom_text(data = anim_angle_label_df, 
            mapping = aes(x = x, y = y), 
            label = "theta", parse = TRUE, 
            size = 5) + # 角ラベル
  geom_point(data = anim_point_df, 
             mapping = aes(x = x, y = y), 
             size = 4) + # 円周上の点
  geom_vline(xintercept = 0, linetype = "dashed") + # 補助線
  geom_hline(yintercept = 0, linetype = "dashed") + # 補助線
  geom_segment(data = anim_fnc_seg_df, 
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                             color = fnc, linewidth = w)) + # 関数線分
  geom_text(data = anim_fnc_label_df, 
            mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                          hjust = h, vjust = v, angle = a), 
            parse = TRUE, show.legend = FALSE) + # 関数ラベル
  geom_text(data = anim_label_df, 
            mapping = aes(x = -Inf, y = Inf, label = var_label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # 変数ラベル:(subtitleの代用)
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_color_hue(labels = fnc_label_vec, name = "function") + # 凡例表示用
  scale_linewidth_manual(breaks = c("bold", "normal", "thin", "major", "minor"), 
                         values = c(1.5, 1, 0.5, 0.5, 0.25)) + # 補助線用, 重なり対策用, 主・補助目盛線用
  guides(linewidth = "none") + 
  coord_fixed(ratio = 1, clip = "off", 
              xlim = c(-axis_size, axis_size), 
              ylim = c(-axis_size, axis_size)) + 
  labs(title = "circular functions", 
       subtitle = "", # (ラベル表示用の空行)
       x = expression(x == r ~ cos~theta), 
       y = expression(y == r ~ sin~theta))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = frame_num, fps = 15, width = 8, height = 8, units = "in", res = 250, 
  renderer = gganimate::av_renderer(file = "circular/figure/cotangent/definition_circle_2.mp4")
)


# 単位円とcot曲線の関係：座標 --------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# 曲線用のラジアンの範囲を指定
theta_vec <- seq(from = 0, to = 2*pi, length.out = 1001)

# 点用のラジアンを作成
theta_vals <- seq(from = min(theta_vec), to = max(theta_vec), length.out = frame_num+1)[1:frame_num]


# 閾値を指定
threshold <- 4

# 関数曲線の座標を作成
fnc_curve_df <- tibble::tibble(
  t     = theta_vec, 
  cot_t = 1/tan(t)
) |> 
  dplyr::mutate(
    cot_t = dplyr::if_else(
      (cot_t >= -threshold & cot_t <= threshold), true = cot_t, false = NA_real_
    )
  ) # 閾値外を欠損値に置換


# ラジアン軸目盛用の値を作成
tick_num <- 6
rad_break_vec <- seq(
  from = floor(min(theta_vec) / pi) * pi, 
  to   = ceiling(max(theta_vec) / pi) * pi, 
  by   = pi/tick_num
)
tick_vec <- rad_break_vec/pi * tick_num
rad_label_vec <- paste0(c("", "-")[(tick_vec < 0)+1], "frac(", abs(tick_vec), ", ", tick_num, ") ~ pi")


# グラフサイズを設定
axis_size <- threshold

# 関数の描画順を指定
fnc_level_vec <- c("cot", "tan", "sin", "cos", "exsec", "excsc")

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  # 作図へのInfの影響回避用の小細工
  if(theta == 0) { # (プログラム上では0のときのみInfになる)
    
    # 微小な値を加算
    theta <- atan(tan(2*pi))
  }
  
  # 曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    t     = theta, 
    cot_t = 1/tan(t)
  )
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = theta, 
    x = cos(t), 
    y = sin(t)
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
  radius_df <- tibble::tibble(
    x_from = c(0, 0, 0, 1/tan(theta)), 
    y_from = c(0, 0, 0, 0), 
    x_to   = c(1, cos(theta), 0, 1/tan(theta)), 
    y_to   = c(0, sin(theta), 1, 1), 
    w      = c("normal", "normal", "thin", "thin") # 補助線用
  )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "cot", "cot", "cot", 
      "tan", 
      "sin", "cos", 
      "exsec", "excsc"
    ) |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    x_from = c(
      0, 0, 0, 
      1, 
      cos(theta), 0, 
      cos(theta), cos(theta)
    ), 
    y_from = c(
      0, 1, 0, 
      0, 
      0, sin(theta), 
      sin(theta), sin(theta)
    ), 
    x_to = c(
      1/tan(theta), 1/tan(theta), 0, 
      1, 
      cos(theta), cos(theta), 
      1, 1/tan(theta)
    ), 
    y_to = c(
      0, 1, 1/tan(theta), 
      tan(theta), 
      sin(theta), sin(theta), 
      tan(theta), 1
    ), 
    w = c(
      "normal", "normal", "normal", 
      "normal", 
      "normal", "normal", 
      "bold", "thin"
    ), # 重なり対策用
    line_type = c(
      "main", "main", "sub", 
      "main", 
      "main", "main", 
      "main", "main"
    ) # 軸の変換用
  )
  
  # 軸の変換曲線の座標を作成
  convert_df <- tibble::tibble(
    u     = seq(from = 0, to = 0.5*pi, length.out = 91), # ラジアン
    x0    = 0, 
    y0    = 0, 
    arc_r = 1/tan(theta), 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u)
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "list(", 
    "(list(r, theta)) == (list(1, ", round(theta/pi, digits = 2), " * pi)), ", 
    "(list(x, y)) == (list(", round(cos(theta), digits = 2), ", ", round(sin(theta), digits = 2), ")), ", 
    ")"
  )
  fnc_label_vec <- paste(
    c("cot~theta", "tan~theta", "sin~theta", "cos~theta", "exsec~theta", "excsc~theta"), 
    c(1/tan(theta), tan(theta), sin(theta), cos(theta), 1/cos(theta)-1, 1/sin(theta)-1) |> 
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
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = w), 
                 show.legend = FALSE) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), 
              label = "theta", parse = TRUE, 
              size = 5) + # 角ラベル
    geom_segment(mapping = aes(x = 0, y = 1/tan(theta), xend = Inf, yend = 1/tan(theta)), 
                 linewidth = 0.8, linetype = "dotted") + # y軸の目盛線
    geom_line(data = convert_df, 
              mapping = aes(x = arc_x, y = arc_y), 
              linewidth = 0.8, linetype = "dotted") + # 変換曲線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 円周上の点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = 0, y = cot_t), 
               size = 4) + # 関数点
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, linewidth = w, linetype = line_type)) + # 関数線分
    scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
    scale_linetype_manual(breaks = c("main", "sub"), 
                          values = c("solid", "dashed")) + # 補助線用
    scale_linewidth_manual(breaks = c("bold", "normal", "thin", "major", "minor"), 
                           values = c(1.5, 1, 0.5, 0.5, 0.25)) + # 重なり対策用, 主・補助目盛線用
    guides(linetype = "none", linewidth = "none") + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = coord_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## cot関数曲線
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", round(theta, digits = 2), ", ", round(1/tan(theta), digits = 2), "))"
  )
  
  # 関数曲線を作図
  curve_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)),
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・x軸線
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = t, y = cot_t), 
              linewidth = 1) + # 関数曲線
    geom_segment(mapping = aes(x = theta, y = 1/tan(theta), 
                               xend = c(-Inf, theta), yend = c(1/tan(theta), -Inf)), 
                 linewidth = 0.8, linetype = "dotted") + # θ・y軸の目盛線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = t, y = cot_t), 
               size = 4) + # 曲線上の点
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = t, y = 0, xend = theta, yend = cot_t), 
                 color = "#F8766D", linewidth = 1) + # 関数線分
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "cotangent function", 
         subtitle = parse(text = coord_label), 
         x = expression(theta), 
         y = expression(cot~theta))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_graph, curve_graph
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 12, height = 8, units = "in", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "circular/figure/cotangent/definition_curves_coord.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


# 単位円とcot曲線の関係：推移 --------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 600

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]
theta_min  <- min(theta_vals)

# 曲線用のラジアンのサイズを指定
theta_size <- 2 * pi


# 閾値を指定
threshold <- 4

# グラフサイズを設定
axis_size <- threshold

# 関数の描画順を指定
fnc_level_vec <- c("cot", "tan", "sin", "cos", "exsec", "excsc")

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  # 作図へのInfの影響回避用の小細工
  if(theta == 0) { # (プログラム上では0のときのみInfになる)
    
    # 微小な値を加算
    theta <- atan(tan(2*pi))
  }
  
  # 曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    t     = theta, 
    cot_t = 1/tan(t)
  )
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = theta, 
    x = cos(t), 
    y = sin(t)
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
  radius_df <- tibble::tibble(
    x_from = c(0, 0, 0, 1/tan(theta)), 
    y_from = c(0, 0, 0, 0), 
    x_to   = c(1, cos(theta), 0, 1/tan(theta)), 
    y_to   = c(0, sin(theta), 1, 1), 
    w      = c("normal", "normal", "thin", "thin") # 補助線用
  )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "cot", "cot", "cot", 
      "tan", 
      "sin", "cos", 
      "exsec", "excsc"
    ) |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    x_from = c(
      0, 0, 0, 
      1, 
      cos(theta), 0, 
      cos(theta), cos(theta)
    ), 
    y_from = c(
      0, 1, 0, 
      0, 
      0, sin(theta), 
      sin(theta), sin(theta)
    ), 
    x_to = c(
      1/tan(theta), 1/tan(theta), 0, 
      1, 
      cos(theta), cos(theta), 
      1, 1/tan(theta)
    ), 
    y_to = c(
      0, 1, 1/tan(theta), 
      tan(theta), 
      sin(theta), sin(theta), 
      tan(theta), 1
    ), 
    w = c(
      "normal", "normal", "normal", 
      "normal", 
      "normal", "normal", 
      "bold", "thin"
    ), # 重なり対策用
    line_type = c(
      "main", "main", "sub", 
      "main", 
      "main", "main", 
      "main", "main"
    ) # 軸の変換用
  )
  
  # 軸の変換曲線の座標を作成
  convert_df <- tibble::tibble(
    u     = seq(from = 0, to = 0.5*pi, length.out = 91), # ラジアン
    x0    = 0, 
    y0    = 0, 
    arc_r = 1/tan(theta), 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u)
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "list(", 
    "(list(r, theta)) == (list(1, ", round(theta/pi, digits = 2), " * pi)), ", 
    "(list(x, y)) == (list(", round(cos(theta), digits = 2), ", ", round(sin(theta), digits = 2), ")), ", 
    ")"
  )
  fnc_label_vec <- paste(
    c("cot~theta", "tan~theta", "sin~theta", "cos~theta", "exsec~theta", "excsc~theta"), 
    c(1/tan(theta), tan(theta), sin(theta), cos(theta), 1/cos(theta)-1, 1/sin(theta)-1) |> 
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
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = w), 
                 show.legend = FALSE) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), 
              label = "theta", parse = TRUE, 
              size = 5) + # 角ラベル
    geom_segment(mapping = aes(x = 0, y = 1/tan(theta), xend = -Inf, yend = 1/tan(theta)), 
                 linewidth = 0.8, linetype = "dotted") + # y軸の目盛線
    geom_line(data = convert_df, 
              mapping = aes(x = arc_x, y = arc_y), 
              linewidth = 0.8, linetype = "dotted") + # 変換曲線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 円周上の点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = 0, y = cot_t), 
               size = 4) + # 関数点
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, linewidth = w, linetype = line_type)) + # 関数線分
    scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
    scale_linetype_manual(breaks = c("main", "sub"), 
                          values = c("solid", "dashed")) + # 補助線用
    scale_linewidth_manual(breaks = c("bold", "normal", "thin", "major", "minor"), 
                           values = c(1.5, 1, 0.5, 0.5, 0.25)) + # 重なり対策用, 主・補助目盛線用
    guides(linetype = "none", linewidth = "none") + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = coord_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## cot関数曲線
  
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
    cot_t = 1/tan(t)
  ) |> 
    dplyr::mutate(
      cot_t = dplyr::if_else(
        (cot_t >= -threshold & cot_t <= threshold), true = cot_t, false = NA_real_
      )
    ) # 閾値外を欠損値に置換
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", round(theta, digits = 2), ", ", round(1/tan(theta), digits = 2), "))"
  )
  
  # 関数曲線を作図
  curve_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = t, y = cot_t), 
              linewidth = 1) + # 関数曲線
    geom_segment(mapping = aes(x = theta, y = 1/tan(theta), 
                               xend = c(Inf, theta), yend = c(1/tan(theta), -Inf)), 
                 linewidth = 0.8, linetype = "dotted") + # θ・y軸の目盛線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = t, y = cot_t), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    coord_fixed(ratio = 1, 
                xlim = c(theta-theta_size, theta), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "cotangent function", 
         subtitle = parse(text = coord_label), 
         x = expression(theta), 
         y = expression(cot~theta))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_graph, circle_graph
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 12, height = 8, units = "in", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "circular/figure/cotangent/definition_curves_form.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


# 単位円とtan・cot曲線の関係：座標 --------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 300

# 曲線用のラジアンの範囲を指定
theta_vec <- seq(from = 0, to = 2*pi, length.out = 1001)

# 点用のラジアンを作成
theta_vals <- seq(from = min(theta_vec), to = max(theta_vec), length.out = frame_num+1)[1:frame_num]


# 閾値を指定
threshold <- 3

# 関数曲線の座標を作成
fnc_curve_df <- tibble::tibble(
  t     = theta_vec, 
  cot_t = 1/tan(t), 
  tan_t = tan(t)
) |> 
  dplyr::mutate(
    cot_t = dplyr::if_else(
      (cot_t >= -threshold & cot_t <= threshold), true = cot_t, false = NA_real_
    ), 
    tan_t = dplyr::if_else(
      (tan_t >= -threshold & tan_t <= threshold), true = tan_t, false = NA_real_
    )
  ) # 閾値外を欠損値に置換


# ラジアン軸目盛用の値を作成
tick_num <- 6
rad_break_vec <- seq(
  from = floor(min(theta_vec) / pi) * pi, 
  to   = ceiling(max(theta_vec) / pi) * pi, 
  by   = pi/tick_num
)
tick_vec <- rad_break_vec/pi * tick_num
rad_label_vec <- paste0(c("", "-")[(tick_vec < 0)+1], "frac(", abs(tick_vec), ", ", tick_num, ") ~ pi")


# グラフサイズを設定
axis_size <- threshold

# 変換軸の目盛間隔を設定
tick_major_val <- 2
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


# 関数の描画順を指定
fnc_level_vec <- c("cot", "tan", "sin", "cos", "exsec", "excsc")

# 色の共通化用のベクトルを作成
color_vec <- scales::hue_pal()(n = length(fnc_level_vec))

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  # 作図へのInfの影響回避用の小細工
  if(theta == 0) { # (プログラム上では0のときのみInfになる)
    
    # 微小な値を加算
    theta <- atan(tan(2*pi))
  }
  
  # 曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    t     = theta, 
    cot_t = 1/tan(t), 
    tan_t = tan(t)
  )
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = theta, 
    x = cos(t), 
    y = sin(t)
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
  
  # 半径線の終点の座標を作成
  radius_df <- tibble::tibble(
    x = c(1, cos(theta), 0), 
    y = c(0, sin(theta), 1), 
    w = c("normal", "normal", "thin") # 補助線用
  )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "cot", "tan", 
      "sin", "sin", 
      "cos", "cos", 
      "exsec", "excsc"
    ) |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    x_from = c(
      0, 1, 
      0, cos(theta), 
      0, 0, 
      cos(theta), cos(theta)
    ), 
    y_from = c(
      1, 0, 
      0, 0, 
      0, sin(theta), 
      sin(theta), sin(theta)
    ), 
    x_to = c(
      1/tan(theta), 1, 
      0, cos(theta), 
      cos(theta), cos(theta), 
      1, 1/tan(theta)
    ), 
    y_to = c(
      1, tan(theta), 
      sin(theta), sin(theta), 
      0, sin(theta), 
      tan(theta), 1
    ), 
    w = c(
      "normal", "normal", 
      "normal", "normal", 
      "normal", "normal", 
      "bold", "thin"
    ) # 重なり対策用
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "list(", 
    "(list(r, theta)) == (list(1, ", round(theta/pi, digits = 2), " * pi)), ", 
    "(list(x, y)) == (list(", round(cos(theta), digits = 2), ", ", round(sin(theta), digits = 2), ")), ", 
    ")"
  )
  fnc_label_vec <- paste(
    c("cot~theta", "tan~theta", "sin~theta", "cos~theta", "exsec~theta", "excsc~theta"), 
    c(1/tan(theta), tan(theta), sin(theta), cos(theta), 1/cos(theta)-1, 1/sin(theta)-1) |> 
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
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = w), 
                 show.legend = FALSE) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), 
              label = "theta", parse = TRUE, 
              size = 5) + # 角ラベル
    geom_segment(mapping = aes(x = c(1/tan(theta), 1), y = c(1, tan(theta)), 
                               xend = c(1/tan(theta), Inf), yend = c(-Inf, tan(theta))), 
                 linewidth = 0.8, linetype = "dotted") + # x・y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 円周上の点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = 1, y = tan_t), 
               size = 4) + # 関数点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = cot_t, y = 1), 
               size = 4) + # 関数点
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, linewidth = w)) + # 関数線分
    scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
    scale_linewidth_manual(breaks = c("bold", "normal", "thin", "major", "minor"), 
                           values = c(1.5, 1, 0.5, 0.5, 0.25)) + # 重なり対策用, 主・補助目盛線用
    guides(linetype = "none", linewidth = "none") + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = coord_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## tan関数曲線
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", round(theta, digits = 2), ", ", round(tan(theta), digits = 2), "))"
  )
  
  # 関数曲線を作図
  curve_tan_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)),
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・y軸線
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = t, y = tan_t), 
              linewidth = 1) + # 関数曲線
    geom_vline(xintercept = theta, 
               linewidth = 0.8, linetype = "dotted") + # θ軸の目盛線
    geom_segment(mapping = aes(x = theta, y = tan(theta), xend = -Inf, yend = tan(theta)), 
                 linewidth = 0.8, linetype = "dotted") + # y軸の目盛線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = t, y = tan_t), 
               size = 4) + # 曲線上の点
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = t, y = 0, xend = theta, yend = tan_t), 
                 color = color_vec[2], linewidth = 1) + # 関数線分
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "tangent function", 
         subtitle = parse(text = coord_label), 
         x = expression(theta), 
         y = expression(tan~theta))
  
  ## 軸変換
  
  # 軸の変換曲線の座標を作成
  convert_df <- tibble::tibble(
    u     = seq(from = pi, to = 1.5*pi, length.out = 91), # ラジアン
    x0    = grid_size, 
    y0    = grid_size, 
    arc_r = grid_size - 1/tan(theta), 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u)
  )
  
  # 軸変換を作図
  convert_graph <- ggplot() + 
    geom_path(data = grid_df, 
              mapping = aes(x = arc_x, y = arc_y, group = arc_r, linewidth = grid), 
              color = "white") + # グリッド線
    geom_line(data = convert_df, 
              mapping = aes(x = arc_x, y = arc_y), 
              linewidth = 0.8, linetype = "dotted") + # 変換曲線
    geom_segment(mapping = aes(x = c(1/tan(theta), grid_size), y = c(grid_size, 1/tan(theta)), 
                               xend = c(1/tan(theta), Inf), yend = c(Inf, 1/tan(theta))), 
                 linewidth = 0.8, linetype = "dotted") + # x・x軸の目盛線
    geom_point(mapping = aes(x = c(1/tan(theta), grid_size), y = c(grid_size, 1/tan(theta))), 
               size = 4) + # 関数点
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25)) + # 主・補助目盛線用
    guides(linewidth = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(x = expression(x), 
         y = expression(x))
  
  ## cot関数曲線
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", round(theta, digits = 2), ", ", round(1/tan(theta), digits = 2), "))"
  )
  
  # 関数曲線を作図
  curve_cot_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)),
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # θ・x軸線
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = t, y = cot_t), 
              linewidth = 1) + # 関数曲線
    geom_vline(xintercept = theta, 
               linewidth = 0.8, linetype = "dotted") + # θ軸の目盛線
    geom_segment(mapping = aes(x = theta, y = 1/tan(theta), xend = -Inf, yend = 1/tan(theta)), 
                 linewidth = 0.8, linetype = "dotted") + # x軸の目盛線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = t, y = cot_t), 
               size = 4) + # 曲線上の点
    geom_segment(data = fnc_point_df, 
                 mapping = aes(x = t, y = 0, xend = theta, yend = cot_t), 
                 color = color_vec[1], linewidth = 1) + # 関数線分
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    coord_fixed(ratio = 1, 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "cotangent function", 
         subtitle = parse(text = coord_label), 
         x = expression(theta), 
         y = expression(cot~theta))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_graph, curve_tan_graph, 
    convert_graph, curve_cot_graph, 
    nrow = 2, ncol = 2
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 13, height = 13, units = "in", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "circular/figure/cotangent/definition_curves_tan_coord.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


# 単位円とtan・cot曲線の関係：推移 --------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "circular/figure/tmp_folder"


# フレーム数を指定
frame_num <- 600

# 点用のラジアンの範囲を指定
theta_vals <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num]
theta_min  <- min(theta_vals)

# 曲線用のラジアンのサイズを指定
theta_size <- 2 * pi


# 閾値を指定
threshold <- 3

# グラフサイズを設定
axis_size <- threshold

# 変換軸の目盛間隔を設定
tick_major_val <- 2
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
fnc_level_vec <- c("cot", "tan", "sin", "cos", "exsec", "excsc")

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  theta <- theta_vals[i]
  
  # 作図へのInfの影響回避用の小細工
  if(theta == 0) { # (プログラム上では0のときのみInfになる)
    
    # 微小な値を加算
    theta <- atan(tan(2*pi))
  }
  
  # 曲線上の点の座標を作成
  fnc_point_df <- tibble::tibble(
    t     = theta, 
    cot_t = 1/tan(t), 
    tan_t = tan(t)
  )
  
  # 曲線用のラジアンを作成
  theta_vec <- seq(from = max(theta_min, theta-theta_size), to = theta, length.out = 5000)
  
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
    cot_t = 1/tan(t), 
    tan_t = tan(t)
  ) |> 
    dplyr::mutate(
      cot_t = dplyr::if_else(
        (cot_t >= -threshold & cot_t <= threshold), true = cot_t, false = NA_real_
      ), 
      tan_t = dplyr::if_else(
        (tan_t >= -threshold & tan_t <= threshold), true = tan_t, false = NA_real_
      )
    ) # 閾値外を欠損値に置換
  
  ## 単位円と関数の関係
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = theta, 
    x = cos(t), 
    y = sin(t)
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
  
  # 半径線の終点の座標を作成
  radius_df <- tibble::tibble(
    x = c(1, cos(theta), 0), 
    y = c(0, sin(theta), 1), 
    w = c("normal", "normal", "thin") # 補助線用
  )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    fnc = c(
      "cot", "tan", 
      "sin", "sin", 
      "cos", "cos", 
      "exsec", "excsc"
    ) |> 
      factor(levels = fnc_level_vec), # 関数カテゴリ
    x_from = c(
      0, 1, 
      0, cos(theta), 
      0, 0, 
      cos(theta), cos(theta)
    ), 
    y_from = c(
      1, 0, 
      0, 0, 
      0, sin(theta), 
      sin(theta), sin(theta)
    ), 
    x_to = c(
      1/tan(theta), 1, 
      0, cos(theta), 
      cos(theta), cos(theta), 
      1, 1/tan(theta)
    ), 
    y_to = c(
      1, tan(theta), 
      sin(theta), sin(theta), 
      0, sin(theta), 
      tan(theta), 1
    ), 
    w = c(
      "normal", "normal", 
      "normal", "normal", 
      "normal", "normal", 
      "bold", "thin"
    ) # 重なり対策用
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "list(", 
    "(list(r, theta)) == (list(1, ", round(theta/pi, digits = 2), " * pi)), ", 
    "(list(x, y)) == (list(", round(cos(theta), digits = 2), ", ", round(sin(theta), digits = 2), ")), ", 
    ")"
  )
  fnc_label_vec <- paste(
    c("cot~theta", "tan~theta", "sin~theta", "cos~theta", "exsec~theta", "excsc~theta"), 
    c(1/tan(theta), tan(theta), sin(theta), cos(theta), 1/cos(theta)-1, 1/sin(theta)-1) |> 
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
                 mapping = aes(x = 0, y = 0, xend = x, yend = y, linewidth = w), 
                 show.legend = FALSE) + # 半径線
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y)) + # 角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), 
              label = "theta", parse = TRUE, 
              size = 5) + # 角ラベル
    geom_segment(mapping = aes(x = c(1/tan(theta), 1), y = c(1, tan(theta)), 
                               xend = c(1/tan(theta), -Inf), yend = c(Inf, tan(theta))), 
                 linewidth = 0.8, linetype = "dotted") + # x・y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 円周上の点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = 1, y = tan_t), 
               size = 4) + # 関数点
    geom_point(data = fnc_point_df, 
               mapping = aes(x = cot_t, y = 1), 
               size = 4) + # 関数点
    geom_segment(data = fnc_seg_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, linewidth = w)) + # 関数線分
    scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
    scale_linewidth_manual(breaks = c("bold", "normal", "thin", "major", "minor"), 
                           values = c(1.5, 1, 0.5, 0.5, 0.25)) + # 重なり対策用, 主・補助目盛線用
    guides(linetype = "none", linewidth = "none") + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha("white", alpha = 0.8))) + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "unit circle", 
         subtitle = parse(text = coord_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  ## tan関数曲線
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", round(theta, digits = 2), ", ", round(tan(theta), digits = 2), "))"
  )
  
  # 関数曲線を作図
  curve_tan_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = t, y = tan_t), 
              linewidth = 1) + # 関数曲線
    geom_vline(xintercept = theta, 
               linewidth = 0.8, linetype = "dotted") + # θ軸の目盛線
    geom_segment(mapping = aes(x = theta, y = tan(theta), xend = Inf, yend = tan(theta)), 
                 linewidth = 0.8, linetype = "dotted") + # y軸の目盛線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = t, y = tan_t), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    coord_fixed(ratio = 1, 
                xlim = c(theta-theta_size, theta), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "tangent function", 
         subtitle = parse(text = coord_label), 
         x = expression(theta), 
         y = expression(tan~theta))
  
  ## 軸変換
  
  # 軸の変換曲線の座標を作成
  convert_df <- tibble::tibble(
    u     = seq(from = 0, to = 0.5*pi, length.out = 91), # ラジアン
    x0    = -grid_size, 
    y0    = -grid_size, 
    arc_r = grid_size + 1/tan(theta), 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u)
  )
  
  # 軸変換を作図
  convert_graph <- ggplot() + 
    geom_path(data = grid_df, 
              mapping = aes(x = arc_x, y = arc_y, group = arc_r, linewidth = grid), 
              color = "white") + # グリッド線
    geom_line(data = convert_df, 
              mapping = aes(x = arc_x, y = arc_y), 
              linewidth = 0.8, linetype = "dotted") + # 変換曲線
    geom_segment(mapping = aes(x = c(1/tan(theta), -grid_size), y = c(-grid_size, 1/tan(theta)), 
                               xend = c(1/tan(theta), -Inf), yend = c(-Inf, 1/tan(theta))), 
                 linewidth = 0.8, linetype = "dotted") + # x・x軸の目盛線
    geom_point(mapping = aes(x = c(1/tan(theta), -grid_size), y = c(-grid_size, 1/tan(theta))), 
               size = 4) + # 関数点
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25)) + # 主・補助目盛線用
    guides(linewidth = "none") + 
    coord_fixed(ratio = 1, 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(x = expression(x), 
         y = expression(x))
  
  ## cot関数曲線
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", round(theta, digits = 2), ", ", round(1/tan(theta), digits = 2), "))"
  )
  
  # 関数曲線を作図
  curve_cot_graph <- ggplot() + 
    geom_line(data = fnc_curve_df, 
              mapping = aes(x = t, y = cot_t), 
              linewidth = 1) + # 関数曲線
    geom_vline(xintercept = theta, 
               linewidth = 0.8, linetype = "dotted") + # θ軸の目盛線
    geom_segment(mapping = aes(x = theta, y = 1/tan(theta), xend = Inf, yend = 1/tan(theta)), 
                 linewidth = 0.8, linetype = "dotted") + # x軸の目盛線
    geom_point(data = fnc_point_df, 
               mapping = aes(x = t, y = cot_t), 
               size = 4) + # 曲線上の点
    scale_x_continuous(breaks = rad_break_vec, 
                       labels = parse(text = rad_label_vec)) + # θ軸目盛
    coord_fixed(ratio = 1, 
                xlim = c(theta-theta_size, theta), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "cotangent function", 
         subtitle = parse(text = coord_label), 
         x = expression(theta), 
         y = expression(cot~theta))
  
  ## グラフの書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    curve_cot_graph, convert_graph, 
    curve_tan_graph, circle_graph, 
    nrow = 2, ncol = 2
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 13, height = 13, units = "in", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "circular/figure/cotangent/definition_curves_tan_form.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


