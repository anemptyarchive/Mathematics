
# 円関数の可視化 -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)
library(magick)

# チェック用
library(ggplot2)


# 単位円の作図 -------------------------------------------------------------------

# 半径を指定
r <- 1

# 円周の座標を計算
circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 601), # ラジアン
  x = r * cos(t), 
  y = r * sin(t)
)

# 半円の目盛の数(分母の値)を指定
denom <- 6

# 角度目盛の描画用の値を作成
d <- 1.1
radian_lable_df <- tibble::tibble(
  nomer = seq(from = 0, to = 2*denom-1, by = 1), # 目盛の通し番号(分子の値)を作成
  t_deg = nomer / denom * 180, # 度数法
  t_rad = nomer / denom * pi,  # 弧度法
  x = r * cos(t_rad), 
  y = r * sin(t_rad), 
  label_x = d * x, 
  label_y = d * y, 
  rad_label = paste0("frac(", nomer, ", ", denom, ")~pi"), # ラジアンラベル
  h = 1 - (x * 0.5 + 0.5), 
  v = 1 - (y * 0.5 + 0.5)
)


# グラフサイズを指定
axis_size <- 1.5

# 円周を作図
ggplot() + 
  geom_path(data = circle_df, 
            mapping = aes(x = x, y = y), 
            size = 1) + # 円周
  geom_segment(data = radian_lable_df, 
               mapping = aes(x = 0, y = 0, xend = x, yend = y), 
               linetype = "dotted") + # 角度目盛グリッド
  geom_text(data = radian_lable_df, 
            mapping = aes(x = x, y = y, angle = t_deg+90), 
            label = "|", size = 2) + # 角度目盛指示線
  geom_text(data = radian_lable_df, 
            mapping = aes(x = label_x, y = label_y, label = rad_label, 
                          hjust = h, vjust = v), parse = TRUE) + # 角度目盛ラベル
  coord_fixed(ratio = 1, 
              xlim = c(-axis_size, axis_size), ylim = c(-axis_size, axis_size)) + # 描画領域
  labs(title = "unit circle", 
       subtitle = parse(text = paste0("r==", r)), 
       x = expression(x == r~cos~theta), 
       y = expression(y == r~sin~theta))


# 単位円上のなす角と関数曲線の関係：1周 ----------------------------------------------------------

# 一時出力先を指定
dir_path <- "trigonometric/figure/tmp_folder"

# フレーム数を指定
frame_num <- 60

# なす角(変数)の値を作成
theta_i <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# 関数曲線用の変数を作成
theta_vec <- seq(from = 0, to = 2*pi, length.out = 1000)

# 目盛ラベル用の文字列を作成
denom <- 6
numer_vec <- seq(from = 0, to = 2*pi / pi * denom, by = 1)
label_vec <- paste0(c("", "-")[(numer_vec < 0)+1], "frac(", abs(numer_vec), ", ", denom, ")~pi")

# 関数ラベルのレベルを指定
fnc_level_vec <- c("r", "sin", "cos", "tan", "cot", "sec", "csc")

# グラフサイズを指定
axis_size <- 2

# 関数曲線の座標を計算
function_curve_df <- tibble::tibble(
  fnc = c("sin", "cos", "tan", "cot", "sec", "csc") |> 
    rep(each = length(theta_vec)) |> 
    factor(levels = fnc_level_vec), # 色用
  t = theta_vec |> 
    rep(times = 6), # (関数の数)
  f_t = c(
    sin(theta_vec), 
    cos(theta_vec), 
    tan(theta_vec), 
    1/tan(theta_vec), 
    1/cos(theta_vec), 
    1/sin(theta_vec)
  )
) |> 
  dplyr::mutate(
    f_t = dplyr::if_else((f_t >= -axis_size & f_t <= axis_size), true = f_t, false = NA_real_) # 描画領域外の値を欠損値に置換
  )

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # i番目の変数を取得
  theta <- theta_i[i]
  
  ## 関数直線の作図
  
  # 単位円上の点の座標を計算
  point_df <- tibble::tibble(
    t = theta, 
    sin_t = sin(theta), 
    cos_t = cos(theta)
  )
  
  # なす角マークの座標を計算
  d <- 0.15
  angle_mark_df <- tibble::tibble(
    t = seq(from = 0, to = theta, length.out = 100), 
    x = d * cos(t), 
    y = d * sin(t)
  )
  
  # なす角ラベルの座標を計算
  d <- 0.21
  angle_label_df <- tibble::tibble(
    t = 0.5 * theta, 
    x = d * cos(t), 
    y = d * sin(t)
  )
  
  # 反転フラグを設定
  sin_flag <- sin(theta) >= 0
  cos_flag <- cos(theta) >= 0
  
  # 関数直線の線分の座標を計算
  function_line_df <- tibble::tibble(
    fnc = c(
      "r", "r", "r", "r", 
      "sin", "sin", 
      "cos", "cos", 
      "tan", "tan", 
      "cot", "cot", 
      "sec", "sec", 
      "csc", "csc"
    ) |> 
      factor(levels = fnc_level_vec), # 色用
    x_from = c(
      0, 0, 1/tan(theta), ifelse(test = cos_flag, yes = NA, no = -1/tan(theta)), 
      0, cos(theta), 
      0, 0, 
      1, ifelse(test = sin_flag, yes = NA, no = 1), 
      0, ifelse(test = cos_flag, yes = NA, no = 0), 
      0, ifelse(test = sin_flag, yes = NA, no = 0), 
      0, ifelse(test = cos_flag, yes = NA, no = 0)
    ), 
    y_from = c(
      0, 0, 0, ifelse(test = cos_flag, yes = NA, no = 0), 
      0, 0, 
      0, sin(theta), 
      0, ifelse(test = sin_flag, yes = NA, no = 0), 
      0, ifelse(test = cos_flag, yes = NA, no = 0), 
      0, ifelse(test = sin_flag, yes = NA, no = 0), 
      0, ifelse(test = cos_flag, yes = NA, no = 0)
    ), 
    x_to = c(
      1, cos(theta), 1/tan(theta), ifelse(test = cos_flag, yes = NA, no = -1/tan(theta)), 
      0, cos(theta), 
      cos(theta), cos(theta), 
      1, ifelse(test = sin_flag, yes = NA, no = 1), 
      1/tan(theta), ifelse(test = cos_flag, yes = NA, no = -1/tan(theta)), 
      1, ifelse(test = sin_flag, yes = NA, no = 1), 
      ifelse(test = cos_flag, yes = 1/tan(theta), no = -1/tan(theta)), ifelse(test = cos_flag, yes = NA, no = 1/tan(theta))
    ), 
    y_to = c(
      0, sin(theta), 1, ifelse(test = cos_flag, yes = NA, no = 1), 
      sin(theta), sin(theta), 
      0, sin(theta), 
      tan(theta), ifelse(test = sin_flag, yes = NA, no = -tan(theta)), 
      0, ifelse(test = cos_flag, yes = NA, no = 0), 
      ifelse(test = sin_flag, yes = tan(theta), no = -tan(theta)), ifelse(test = sin_flag, yes = NA, no = tan(theta)), 
      1, ifelse(test = cos_flag, yes = NA, no = 1)
    ), 
    width = c(
      "normal", "normal", "thin", "thin", 
      "normal", "normal", 
      "bold", "normal", 
      "normal", "normal", 
      "thin", "thin", 
      "bold", "bold", 
      "thin", "thin"
    ), # 太さ用
    type = c(
      "main", "main", "main", "main", 
      "main", "main", 
      "main", "main", 
      "main", "sub", 
      "main", "sub", 
      "main", "sub", 
      "main", "sub"
    ), # 線種用
    label_flag = c(
      FALSE, FALSE, FALSE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE
    ) # 関数ラベル用
  ) |> 
    dplyr::mutate(
      x_from = dplyr::case_when(x_from == Inf ~ 1e+10, x_from == -Inf ~ -1e+10, TRUE ~ x_from), 
      x_to   = dplyr::case_when(x_to == Inf ~ 1e+10, x_to == -Inf ~ -1e+10, TRUE ~ x_to)
    ) # 発散した場合は大きな(小さな)値に置換
  
  # 関数ラベルの座標を計算
  function_label_df <- function_line_df |> 
    dplyr::filter(label_flag) |> # ラベル付けする線分を抽出
    dplyr::group_by(fnc) |> # 中点の計算用
    dplyr::summarise(
      x = median(c(x_from, x_to)), 
      y = median(c(y_from, y_to)), .groups = "drop"
    ) |> # 線分の中点に配置
    tibble::add_column(
      angle = c(90, 0, 90, 0, 0, 0), 
      h = c(0.5, 0.5, 0.5, 0.5, 1.2, -0.2), 
      v = c(-0.5, 1, 1, -0.5, 0.5, 0.5), 
      fnc_label = c("sin~theta", "cos~theta", "tan~theta", "cot~theta", "sec~theta", "csc~theta") # 関数ラベル
    )
  
  # sec・csc直線の角度を設定
  if(cos(theta) >= 0) {
    theta_sec <- acos(cos(theta))
  } else {
    theta_sec <- pi + acos(cos(theta))
  }
  if(sin(theta) >= 0) {
    theta_csc <- asin(sin(theta))
  } else {
    theta_csc <- pi + asin(sin(theta))
  }
  
  # 軸変換曲線の座標を計算
  num <- 100
  adapt_curve_df <- tibble::tibble(
    fnc = c(
      rep("cos", times = num), 
      rep("cot", times = num), 
      rep("sec", times = num), 
      rep("csc", times = num)
    ), # 色用
    rad = c(
      seq(from = 0, to = 0.5*pi, length.out = num), 
      seq(from = 0, to = 0.5*pi, length.out = num), 
      seq(from = theta_sec, to = ifelse(test = cos(theta) >= 0, yes = 0.5*pi, no = 1.5*pi), length.out = num), 
      seq(from = theta_csc, to = ifelse(test = sin(theta) >= 0, yes = 0.5*pi, no = 1.5*pi), length.out = num)
    ), 
    d = c(
      rep(cos(theta), times = num), 
      rep(1/tan(theta), times = num), 
      rep(abs(1/cos(theta)), times = num), 
      rep(abs(1/sin(theta)), times = num)
    ), 
    x = d * cos(rad), 
    y = d * sin(rad)
  ) |> 
    dplyr::mutate(
      x = dplyr::if_else(condition = x == Inf, true = 1e+10, false = x), 
      y = dplyr::if_else(condition = y == Inf, true = 1e+10, false = y)
    ) # 発散した場合は大きな値に置換
  
  # 関数曲線との対応線の座標を計算
  l <- 0.3
  segment_circle_df <- tibble::tibble(
    fnc = c("sin", "cos", "tan", "sec", "csc", "cot") |> 
      factor(levels = fnc_level_vec), # 色用
    x = c(0, 0, 0, 0, 0, 0), 
    y = c(sin(theta), cos(theta), tan(theta), 1/cos(theta), 1/sin(theta), 1/tan(theta)), 
    x_to = axis_size+l
  ) |> 
    dplyr::mutate(
      y = dplyr::if_else(condition = y == Inf, true = 1e+10, false = y), # 発散した場合は大きな値に置換
      y_to = y
    )
  
  # 変数ラベル用の文字列を作成
  variable_label <- paste0(
    "list(", 
    "theta==", round(theta/pi, digits = 2), "*pi", 
    ", sin~theta==", round(sin(theta), digits = 2), 
    ", cos~theta==", round(cos(theta), digits = 2), 
    ")"
  )
  
  # 単位円上に関数直線を作図
  circle_graph <- ggplot() + 
    geom_segment(data = radian_lable_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y), 
                 color = "white") + # 角度目盛グリッド
    geom_text(data = radian_lable_df, 
              mapping = aes(x = x, y = y, angle = t_deg+90), label = "|", 
              size = 2) + # 角度目盛指示線
    geom_text(data = radian_lable_df, 
              mapping = aes(x = label_x, y = label_y, label = rad_label, 
                            hjust = h, vjust = v), parse = TRUE) + # 角度目盛ラベル
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y), 
              size = 0.5) + # なす角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), label = "theta", parse = TRUE, 
              size = 5) + # なす角ラベル
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              size = 1) + # 円周
    geom_point(data = point_df, 
               mapping = aes(x = cos_t, y = sin_t), 
               size = 4) + # 円周上の点
    geom_hline(yintercept = 1, linetype = "dashed") + # 補助線
    geom_vline(xintercept = 1, linetype = "dashed") + # 補助線
    geom_segment(data = function_line_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, size = width, linetype = type), na.rm = TRUE) + # 関数直線
    geom_text(data = function_label_df, 
              mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                            hjust = h, vjust = v, angle = angle), 
              parse = TRUE, na.rm = TRUE, show.legend = FALSE) + # 関数ラベル
    geom_path(data = adapt_curve_df, 
              mapping = aes(x = x, y = y, color = fnc), 
              size = 0.6, linetype = "dotted", show.legend = FALSE) + # 変換曲線
    geom_point(data = segment_circle_df, 
               mapping = aes(x = x, y = y, color = fnc), 
               size = 4, alpha = 0.5, show.legend = FALSE) + # 変換点
    geom_segment(data = segment_circle_df, 
                 mapping = aes(x = x, y = y, xend = x_to, yend = y, color = fnc), 
                 size = 0.6, linetype = "dotted", show.legend = FALSE) + # 関数曲線との対応線
    scale_color_manual(breaks = fnc_level_vec, 
                       values = c("black", scales::hue_pal()(length(fnc_level_vec)-1))) + # (半径直線を黒色にしたい)
    scale_size_manual(breaks = c("normal", "bold", "thin"), 
                      values = c(1, 1.6, 0.8), guide = "none") + # (線が重なる対策)
    scale_linetype_manual(breaks = c("main", "sub"), 
                          values = c("solid", "twodash"), guide = "none") + # (補助線を描き分けたい)
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), ylim = c(-axis_size, axis_size)) + # 描画領域
    labs(title = "circular functions", 
         subtitle = parse(text = variable_label), 
         color = "function", 
         x = "x", y = "y")
  
  ## 関数曲線の作図
  
  # 関数直線との対応線の座標を計算
  l <- 0.7
  segment_curve_df <- tibble::tibble(
    fnc = c("sin", "cos", "tan", "cot", "sec", "csc") |> 
      factor(levels = fnc_level_vec), 
    x = theta, 
    y = c(sin(theta), cos(theta), tan(theta), 1/tan(theta), 1/cos(theta), 1/sin(theta)), 
    x_to = -l
  ) |> 
    dplyr::mutate(
      y = dplyr::if_else(condition = y == Inf, true = 1e+10, false = y), # 発散した場合は大きな値に置換
      y_to = y
    )
    
  # 変数ラベル用の文字列を作成
  variable_label <- paste0(
    "list(", 
    "theta==", round(theta, digits = 2), 
    ", sin~theta==", round(sin(theta), digits = 2), 
    ", cos~theta==", round(cos(theta), digits = 2), 
    ", tan~theta==", round(tan(theta), digits = 2), 
    ", cot~theta==", round(1/tan(theta), digits = 2), 
    ", sec~theta==", round(1/cos(theta), digits = 2), 
    ", csc~theta==", round(1/sin(theta), digits = 2), 
    ")"
  )
  
  # 関数曲線を作図
  curve_graph <- ggplot() + 
    geom_vline(xintercept = theta, size = 0.6, linetype = "dotted") + # 変数垂線
    geom_line(data = function_curve_df, 
              mapping = aes(x = t, y = f_t, color = fnc), na.rm = TRUE, 
              size = 1) + # 関数曲線
    geom_point(data = segment_curve_df, 
               mapping = aes(x = x, y = y, color = fnc), 
               size = 4, alpha = 0.5) + # 曲線上の点
    geom_segment(data = segment_curve_df, 
                 mapping = aes(x = x, y = y, xend = x_to, yend = y, color = fnc), 
                 size = 0.6, linetype = "dotted", show.legend = FALSE) + # 関数直線との対応線
    scale_x_continuous(breaks = numer_vec/denom*pi, 
                       labels = parse(text = label_vec)) + # 角度目盛ラベル
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(0, 2*pi), ylim = c(-axis_size, axis_size)) + # 描画領域
    theme(legend.position = "none") + # 図の体裁
    labs(title = "", 
         subtitle = parse(text = variable_label), 
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## グラフの出力
  
  # 並べて描画
  circular_graph <- patchwork::wrap_plots(circle_graph, curve_graph, guides = "collect")
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = circular_graph, width = 1600, height = 800, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読み込み
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "trigonometric/figure/curves/circular_1cycle.gif", delay = 0.1) -> tmp_path # gifファイル書き出し


# 単位円上のなす角と関数曲線の関係：n周 ----------------------------------------------------------

# 一時出力先を指定
dir_path <- "trigonometric/figure/tmp_folder"

# フレーム数を指定
frame_num <- 120

# なす角(変数)の値を作成
theta_i <- seq(from = -2*pi, to = 2*pi, length.out = frame_num+1)[1:frame_num] # 範囲を指定

# 関数ラベルのレベルを指定
fnc_level_vec <- c("r", "sin", "cos", "tan", "cot", "sec", "csc")

# グラフサイズを指定
axis_size <- 2

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # i番目の変数を取得
  theta <- theta_i[i]
  
  ## 関数直線の作図
  
  # 単位円上の点の座標を計算
  point_df <- tibble::tibble(
    t = theta, 
    sin_t = sin(theta), 
    cos_t = cos(theta)
  )
  
  # なす角マークの座標を計算
  d <- 0.15
  angle_mark_df <- tibble::tibble(
    t = seq(from = 0, to = theta, length.out = 100), 
    x = d * cos(t), 
    y = d * sin(t)
  )
  
  # なす角ラベルの座標を計算
  d <- 0.21
  angle_label_df <- tibble::tibble(
    t = 0.5 * theta, 
    x = d * cos(t), 
    y = d * sin(t)
  )
  
  # 反転フラグを設定
  sin_flag <- sin(theta) >= 0
  cos_flag <- cos(theta) >= 0
  
  # 関数直線の線分の座標を計算
  function_line_df <- tibble::tibble(
    fnc = c(
      "r", "r", "r", "r", 
      "sin", "sin", 
      "cos", "cos", 
      "tan", "tan", 
      "cot", "cot", 
      "sec", "sec", 
      "csc", "csc"
    ) |> 
      factor(levels = fnc_level_vec), # 色用
    x_from = c(
      0, 0, 1/tan(theta), ifelse(test = cos_flag, yes = NA, no = -1/tan(theta)), 
      0, cos(theta), 
      0, 0, 
      1, ifelse(test = sin_flag, yes = NA, no = 1), 
      0, ifelse(test = cos_flag, yes = NA, no = 0), 
      0, ifelse(test = sin_flag, yes = NA, no = 0), 
      0, ifelse(test = cos_flag, yes = NA, no = 0)
    ), 
    y_from = c(
      0, 0, 0, ifelse(test = cos_flag, yes = NA, no = 0), 
      0, 0, 
      0, sin(theta), 
      0, ifelse(test = sin_flag, yes = NA, no = 0), 
      0, ifelse(test = cos_flag, yes = NA, no = 0), 
      0, ifelse(test = sin_flag, yes = NA, no = 0), 
      0, ifelse(test = cos_flag, yes = NA, no = 0)
    ), 
    x_to = c(
      1, cos(theta), 1/tan(theta), ifelse(test = cos_flag, yes = NA, no = -1/tan(theta)), 
      0, cos(theta), 
      cos(theta), cos(theta), 
      1, ifelse(test = sin_flag, yes = NA, no = 1), 
      1/tan(theta), ifelse(test = cos_flag, yes = NA, no = -1/tan(theta)), 
      1, ifelse(test = sin_flag, yes = NA, no = 1), 
      ifelse(test = cos_flag, yes = 1/tan(theta), no = -1/tan(theta)), ifelse(test = cos_flag, yes = NA, no = 1/tan(theta))
    ), 
    y_to = c(
      0, sin(theta), 1, ifelse(test = cos_flag, yes = NA, no = 1), 
      sin(theta), sin(theta), 
      0, sin(theta), 
      tan(theta), ifelse(test = sin_flag, yes = NA, no = -tan(theta)), 
      0, ifelse(test = cos_flag, yes = NA, no = 0), 
      ifelse(test = sin_flag, yes = tan(theta), no = -tan(theta)), ifelse(test = sin_flag, yes = NA, no = tan(theta)), 
      1, ifelse(test = cos_flag, yes = NA, no = 1)
    ), 
    width = c(
      "normal", "normal", "thin", "thin", 
      "normal", "normal", 
      "bold", "normal", 
      "normal", "normal", 
      "thin", "thin", 
      "bold", "bold", 
      "thin", "thin"
    ), # 太さ用
    type = c(
      "main", "main", "main", "main", 
      "main", "main", 
      "main", "main", 
      "main", "sub", 
      "main", "sub", 
      "main", "sub", 
      "main", "sub"
    ), # 線種用
    label_flag = c(
      FALSE, FALSE, FALSE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE, 
      TRUE, FALSE
    ) # 関数ラベル用
  ) |> 
    dplyr::mutate(
      x_from = dplyr::case_when(x_from == Inf ~ 1e+10, x_from == -Inf ~ -1e+10, TRUE ~ x_from), 
      x_to   = dplyr::case_when(x_to == Inf ~ 1e+10, x_to == -Inf ~ -1e+10, TRUE ~ x_to)
    ) # 発散した場合は大きな(小さな)値に置換
  
  # 関数ラベルの座標を計算
  function_label_df <- function_line_df |> 
    dplyr::filter(label_flag) |> # ラベル付けする線分を抽出
    dplyr::group_by(fnc) |> # 中点の計算用
    dplyr::summarise(
      x = median(c(x_from, x_to)), 
      y = median(c(y_from, y_to)), .groups = "drop"
    ) |> # 線分の中点に配置
    tibble::add_column(
      angle = c(90, 0, 90, 0, 0, 0), 
      h = c(0.5, 0.5, 0.5, 0.5, 1.2, -0.2), 
      v = c(-0.5, 1, 1, -0.5, 0.5, 0.5), 
      fnc_label = c("sin~theta", "cos~theta", "tan~theta", "cot~theta", "sec~theta", "csc~theta") # 関数ラベル
    )
  
  # sec・csc直線の角度を設定
  if(cos(theta) >= 0) {
    theta_sec <- acos(cos(theta))
  } else {
    theta_sec <- pi + acos(cos(theta))
  }
  if(sin(theta) >= 0) {
    theta_csc <- asin(sin(theta))
  } else {
    theta_csc <- pi + asin(sin(theta))
  }
  
  # 軸変換曲線の座標を計算
  num <- 100
  adapt_curve_df <- tibble::tibble(
    fnc = c(
      rep("cos", times = num), 
      rep("cot", times = num), 
      rep("sec", times = num), 
      rep("csc", times = num)
    ), # 色用
    rad = c(
      seq(from = 0, to = 0.5*pi, length.out = num), 
      seq(from = 0, to = 0.5*pi, length.out = num), 
      seq(from = theta_sec, to = ifelse(test = cos(theta) >= 0, yes = 0.5*pi, no = 1.5*pi), length.out = num), 
      seq(from = theta_csc, to = ifelse(test = sin(theta) >= 0, yes = 0.5*pi, no = 1.5*pi), length.out = num)
    ), 
    d = c(
      rep(cos(theta), times = num), 
      rep(1/tan(theta), times = num), 
      rep(abs(1/cos(theta)), times = num), 
      rep(abs(1/sin(theta)), times = num)
    ), 
    x = d * cos(rad), 
    y = d * sin(rad)
  ) |> 
    dplyr::mutate(
      x = dplyr::if_else(condition = x == Inf, true = 1e+10, false = x), 
      y = dplyr::if_else(condition = y == Inf, true = 1e+10, false = y)
    ) # 発散した場合は大きな値に置換
  
  # 関数曲線との対応線の座標を計算
  l <- 0.6
  segment_circle_df <- tibble::tibble(
    fnc = c("sin", "cos", "tan", "sec", "csc", "cot") |> 
      factor(levels = fnc_level_vec), # 色用
    x = c(0, 0, 1, 0, 0, 0), 
    y = c(sin(theta), cos(theta), tan(theta), 1/cos(theta), 1/sin(theta), 1/tan(theta)), 
    x_to = -axis_size-l
  ) |> 
    dplyr::mutate(
      y = dplyr::if_else(condition = y == Inf, true = 1e+10, false = y), # 発散した場合は大きな値に置換
      y_to = y
    )
  
  # 変数ラベル用の文字列を作成
  variable_label <- paste0(
    "list(", 
    "theta==", round(theta/pi, digits = 2), "*pi", 
    ", sin~theta==", round(sin(theta), digits = 2), 
    ", cos~theta==", round(cos(theta), digits = 2), 
    ")"
  )
  
  # 単位円上に関数直線を作図
  circle_graph <- ggplot() + 
    geom_segment(data = radian_lable_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y), 
                 color = "white") + # 角度目盛グリッド
    geom_text(data = radian_lable_df, 
              mapping = aes(x = x, y = y, angle = t_deg+90), label = "|", 
              size = 2) + # 角度目盛指示線
    geom_text(data = radian_lable_df, 
              mapping = aes(x = label_x, y = label_y, label = rad_label, 
                            hjust = h, vjust = v), parse = TRUE) + # 角度目盛ラベル
    geom_path(data = angle_mark_df, 
              mapping = aes(x = x, y = y), 
              size = 0.5) + # なす角マーク
    geom_text(data = angle_label_df, 
              mapping = aes(x = x, y = y), label = "theta", parse = TRUE, 
              size = 5) + # なす角ラベル
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              size = 1) + # 円周
    geom_point(data = point_df, 
               mapping = aes(x = cos_t, y = sin_t), 
               size = 4) + # 円周上の点
    geom_hline(yintercept = 1, linetype = "dashed") + # 補助線
    geom_vline(xintercept = 1, linetype = "dashed") + # 補助線
    geom_segment(data = function_line_df, 
                 mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                               color = fnc, size = width, linetype = type), na.rm = TRUE) + # 関数直線
    geom_text(data = function_label_df, 
              mapping = aes(x = x, y = y, label = fnc_label, color = fnc, 
                            hjust = h, vjust = v, angle = angle), 
              parse = TRUE, na.rm = TRUE, show.legend = FALSE) + # 関数ラベル
    geom_path(data = adapt_curve_df, 
              mapping = aes(x = x, y = y, color = fnc), 
              size = 0.6, linetype = "dotted", show.legend = FALSE) + # 変換曲線
    geom_point(data = segment_circle_df, 
               mapping = aes(x = x, y = y, color = fnc), 
               size = 4, alpha = 0.5, show.legend = FALSE) + # 変換点
    geom_segment(data = segment_circle_df, 
                 mapping = aes(x = x, y = y, xend = x_to, yend = y, color = fnc), 
                 size = 0.6, linetype = "dotted", show.legend = FALSE) + # 関数曲線との対応線
    scale_color_manual(breaks = fnc_level_vec, 
                       values = c("black", scales::hue_pal()(length(fnc_level_vec)-1))) + # (半径直線を黒色にしたい)
    scale_size_manual(breaks = c("normal", "bold", "thin"), 
                      values = c(1, 1.6, 0.8), guide = "none") + # (線が重なる対策)
    scale_linetype_manual(breaks = c("main", "sub"), 
                          values = c("solid", "twodash"), guide = "none") + # (補助線を描き分けたい)
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), ylim = c(-axis_size, axis_size)) + # 描画領域
    labs(title = "", 
         subtitle = parse(text = variable_label), 
         color = "function", 
         x = "x", y = "y")
  
  ## 関数曲線の作図
  
  # 関数曲線用の変数を作成
  theta_size <- 2 * pi
  theta_min  <- theta - theta_size
  theta_vec  <- seq(from = max(min(theta_i), theta_min), to = theta, length.out = 1000)
  
  # 目盛ラベル用の文字列を作成
  denom <- 6
  numer_vec <- seq(
    from = floor(theta_min / pi * denom), 
    to   = ceiling(theta / pi * denom), 
    by = 1
  )
  label_vec <- paste0(c("", "-")[(numer_vec < 0)+1], "frac(", abs(numer_vec), ", ", denom, ")~pi")
  
  # 関数曲線の座標を計算
  function_curve_df <- tibble::tibble(
    fnc = c("sin", "cos", "tan", "cot", "sec", "csc") |> 
      rep(each = length(theta_vec)) |> 
      factor(levels = fnc_level_vec), # 色用
    t = theta_vec |> 
      rep(times = 6), # (関数の数)
    f_t = c(
      sin(theta_vec), 
      cos(theta_vec), 
      tan(theta_vec), 
      1/tan(theta_vec), 
      1/cos(theta_vec), 
      1/sin(theta_vec)
    )
  ) |> 
    dplyr::mutate(
      f_t = dplyr::if_else((f_t >= -axis_size & f_t <= axis_size), true = f_t, false = NA_real_) # 描画領域外の値を欠損値に置換
    )
  
  # 関数直線との対応線の座標を計算
  l <- 0.5
  segment_curve_df <- tibble::tibble(
    fnc = c("sin", "cos", "tan", "cot", "sec", "csc") |> 
      factor(levels = fnc_level_vec), 
    x = theta, 
    y = c(sin(theta), cos(theta), tan(theta), 1/tan(theta), 1/cos(theta), 1/sin(theta)), 
    x_to = theta+l, 
    y_to = y
  ) |> 
    dplyr::mutate(
      y = dplyr::if_else(condition = y == Inf, true = 1e+10, false = y), # 発散した場合は大きな値に置換
      y_to = y
    )
  
  # 変数ラベル用の文字列を作成
  variable_label <- paste0(
    "list(", 
    "theta==", round(theta, digits = 2), 
    ", sin~theta==", round(sin(theta), digits = 2), 
    ", cos~theta==", round(cos(theta), digits = 2), 
    ", tan~theta==", round(tan(theta), digits = 2), 
    ", cot~theta==", round(1/tan(theta), digits = 2), 
    ", sec~theta==", round(1/cos(theta), digits = 2), 
    ", csc~theta==", round(1/sin(theta), digits = 2), 
    ")"
  )
  
  # 関数曲線を作図
  curve_graph <- ggplot() + 
    geom_vline(xintercept = theta, size = 0.6, linetype = "dotted") + # 変数垂線
    geom_line(data = function_curve_df, 
              mapping = aes(x = t, y = f_t, color = fnc), na.rm = TRUE, 
              size = 1) + # 関数曲線
    geom_point(data = segment_curve_df, 
               mapping = aes(x = x, y = y, color = fnc), 
               size = 4, alpha = 0.5) + # 曲線上の点
    geom_segment(data = segment_curve_df, 
                 mapping = aes(x = x, y = y, xend = x_to, yend = y, color = fnc), 
                 size = 0.6, linetype = "dotted", show.legend = FALSE) + # 関数直線との対応線
    scale_x_continuous(breaks = numer_vec/denom*pi, 
                       labels = parse(text = label_vec)) + # 角度目盛ラベル
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(theta_min, theta), ylim = c(-axis_size, axis_size)) + # 描画領域
    theme(legend.position = "none") + # 図の体裁
    labs(title = "circular functions", 
         subtitle = parse(text = variable_label), 
         x = expression(theta), 
         y = expression(f(theta)))
  
  ## グラフの出力
  
  # 並べて描画
  circular_graph <- patchwork::wrap_plots(curve_graph, circle_graph, guides = "collect")
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = circular_graph, width = 1600, height = 800, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読み込み
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "trigonometric/figure/curves/circular_ncycle.gif", delay = 0.1) -> tmp_path # gifファイル書き出し


