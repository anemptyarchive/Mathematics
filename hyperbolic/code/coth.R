
# hyperbolic cotangent function ------------------------------------------------

## 定義の可視化


# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)
library(magick)

# パッケージを読込
library(ggplot2)


# 単位双曲線の設定 -------------------------------------------------------------

# 曲線用の変数を作成
theta_vec <- seq(from = -2, to = 2, length.out = 1001) # 範囲を指定

# 単位双曲線の座標を作成
hype_curve_df <- tibble::tibble(
  t    = rep(theta_vec, times = 2), # 確認用
  x    = c(cosh(theta_vec), -cosh(theta_vec)), 
  y    = c(sinh(theta_vec), sinh(theta_vec)), 
  sign = rep(c("plus", "minus"), each = length(theta_vec)) # 符号
)

# グラフサイズを設定
axis_size <- hype_curve_df[["y"]] |> 
  abs() |> 
  max() |> 
  ceiling()
axis_size


# 単位双曲線とtanh・cothの関係 -------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "hyperbolic/figure/tmp_folder"

# フレーム数を指定
frame_num <- 401

# 点用の変数を作成
theta_vals <- seq(from = min(theta_vec), to = max(theta_vec), length.out = frame_num)

# 関数曲線の座標を作成
fnc_curve_df <- tibble::tibble(
  t      = theta_vec, 
  tanh_t = tanh(t), 
  coth_t = 1/tanh(t)
) |> 
  dplyr::mutate(
    coth_t = dplyr::if_else(
     (coth_t >= -axis_size & coth_t <= axis_size), true = coth_t, false = as.numeric(NA)
    )
  ) # 描画範囲外を欠損値に置換

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
fnc_level_vec <- c("coth", "tanh", "sinh", "cosh")

# 配色の共通化用の色ベクトルを作成
color_vec <- scales::hue_pal()(n = length(fnc_level_vec))

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 点用の変数を取得
  theta <- theta_vals[i]
  
  # 関数点の座標を作成
  fnc_point_df <- tibble::tibble(
    t      = theta, 
    tanh_t = tanh(t), 
    coth_t = 1/tanh(t)
  )
  
  ## 単位双曲線と関数線分
  
  # 双曲線に対する点の座標を作成
  hype_point_df <- tibble::tibble(
    t = theta, # 確認用
    x = c(cosh(theta), 1, 1/tanh(theta)), 
    y = c(sinh(theta), tanh(theta), 1)
  )
  
  # 変数領域の座標を作成
  var_area_df <- dplyr::bind_rows(
    tibble::tibble(
      x     = seq(from = 0, to = 1, length.out = 11), 
      curve = 0 # x軸線
    ), 
    tibble::tibble(
      x     = seq(from = 1, to = cosh(theta), length.out = 100), 
      sign  = dplyr::if_else(theta >= 0, true = 1, false = -1), # 曲線の向き
      curve = sign * sqrt(x^2 - 1) # 双曲線
    )
  ) |> 
    dplyr::mutate(
      straight = sinh(theta)/cosh(theta) * x # 原点と双曲線上の点の線分
    )
  
  # 関数線分の座標を作成
  fnc_seg_df <- tibble::tibble(
    x_from = c(
      0, 
      1, 
      cosh(theta), 
      0
    ), 
    y_from = c(
      1, 
      0, 
      0, 
      0
    ), 
    x_to = c(
      1/tanh(theta), 
      1, 
      cosh(theta), 
      cosh(theta)
    ), 
    y_to = c(
      1, 
      tanh(theta), 
      sinh(theta), 
      0
    ), 
    fnc = c(
      "coth", 
      "tanh", 
      "sinh", 
      "cosh"
    ) |> 
      factor(levels = fnc_level_vec) # 配色用
  )
  aux_seg_df <- tibble::tibble(
    x_from = c(
      min(0, 1/tanh(theta)), 
      0, 0
    ), 
    y_from = c(
      ifelse(theta >= 0, yes = 0, no = 1), 
      0, 0
    ), 
    x_to = c(
      ifelse((sinh(theta) > 0 & sinh(theta) < 1), yes = 1/tanh(theta), no = cosh(theta)), 
      0, 1
    ), 
    y_to = c(
      ifelse((sinh(theta) > 0 & sinh(theta) < 1), yes = 1, no = sinh(theta)), 
      1, 0
    ), 
    fnc = c(
      "(other)", 
      "1", "1"
    ) # 確認用
  )
  
  # 変数ラベルを作成
  coord_label <- paste0(
    "list(", 
    "(list(a, b, theta)) == (list(1, 1, ", sprintf(theta, fmt = "%.2f"), ")), ", 
    "(list(x, y)) == (list(", sprintf(cosh(theta), fmt = "%.2f"), ", ", sprintf(sinh(theta), fmt = "%.2f"), "))", 
    ")"
  )
  fnc_label_vec <- paste(
    c("coth~theta", "tanh~theta", "sinh~theta", "cosh~theta"), 
    c(1/tanh(theta), tanh(theta), sinh(theta), cosh(theta)) |> 
      round(digits = 2), 
    sep = " == "
  )
  
  # 単位双曲線における関数線分を作図
  hyperbola_graph <- ggplot() + 
    geom_segment(
      mapping = aes(
        x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)
      ),
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # x・y軸線
    geom_abline(
      slope = c(1, -1), intercept = c(0, 0), 
      linewidth = 0.5, linetype = "dashed"
    ) + # 漸近線
    geom_path(
      data    = hype_curve_df, 
      mapping = aes(x = x, y = y, group = sign), 
      size = 1
    ) + # 双曲線
    geom_ribbon(
      data    = var_area_df, 
      mapping = aes(x = x, ymin = curve, ymax = straight), 
      fill = "#00A968", alpha = 0.5
    ) + # 変数領域
    geom_text(
      mapping = aes(x = 0.5, y = 0.25*tanh(theta)), 
      label = "frac(theta, 2)", parse = TRUE, 
      size = 3
    ) + # 変数ラベル
    geom_segment(
      mapping = aes(
        x    = c(1/tanh(theta), 1), 
        y    = c(1, tanh(theta)), 
        xend = c(1/tanh(theta), Inf), 
        yend = c(-Inf, tanh(theta))
      ), 
      linewidth = 0.8, linetype = "dotted"
    ) + # x・y軸の目盛補助線
    geom_point(
      data    = hype_point_df, 
      mapping = aes(x = x, y = y), 
      size = 4
    ) + # 双曲線に対する点
    geom_segment(
      data    = aux_seg_df, 
      mapping = aes(
        x = x_from, y = y_from, xend = x_to, yend = y_to
      )
    ) + # 補助線分
    geom_segment(
      data    = fnc_seg_df, 
      mapping = aes(
        x = x_from, y = y_from, xend = x_to, yend = y_to, 
        color = fnc
      ), 
      linewidth = 1
    ) + # 関数線分
    scale_color_hue(labels = parse(text = fnc_label_vec), name = "function") + # 凡例表示用
    theme(
      legend.text.align    = 0, 
      legend.position      = c(0, 1), 
      legend.justification = c(0, 1), 
      legend.background    = element_rect(fill = alpha("white", alpha = 0.8))
    ) + 
    coord_fixed(
      ratio = 1, 
      xlim = c(-axis_size, axis_size), 
      ylim = c(-axis_size, axis_size)
    ) + 
    labs(
      title = "unit hyperbola", 
      subtitle = parse(text = coord_label), 
      x = expression(x == a ~ cosh~theta), 
      y = expression(y == b ~ sinh~theta)
    )
  
  ## tanh関数曲線
  
  # 関数ラベルを作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", sprintf(theta, fmt = "%.2f"), ", ", sprintf(tanh(theta), fmt = "%.2f"), "))"
  )
  
  # tanh関数曲線の作図
  curve_tanh_graph <- ggplot() + 
    geom_segment(
      mapping = aes(
        x = c(-Inf, 0), y = c(0, -Inf), 
        xend = c(Inf, 0), yend = c(0, Inf)
      ),
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # θ・y軸線
    geom_line(
      data    = fnc_curve_df, 
      mapping = aes(x = t, y = tanh_t), 
      linewidth = 1
    ) + # 関数曲線
    geom_segment(
      mapping = aes(
        x = c(theta, theta), y = c(-Inf, tanh(theta)), 
        xend = c(theta, -Inf), yend = c(Inf, tanh(theta))
      ), 
      linewidth = 0.8, linetype = "dotted"
    ) + # θ・y軸の目盛補助線
    geom_point(
      data    = fnc_point_df, 
      mapping = aes(x = t, y = tanh_t), 
      size = 4
    ) + # 関数点
    geom_segment(
      data    = fnc_point_df, 
      mapping = aes(x = t, y = 0, xend = t, yend = tanh_t), 
      color = color_vec[which(fnc_level_vec=="tanh")], linewidth = 1
    ) + # 関数線分
    coord_fixed(
      ratio = 1, 
      ylim = c(-axis_size, axis_size)
    ) + 
    labs(
      title = "hyperbolic tangent function", 
      subtitle = parse(text = coord_label), 
      x = expression(theta), 
      y = expression(f(theta) == tanh~theta)
    )
  
  ## 軸変換
  
  # 軸の変換曲線の座標を作成
  convert_df <- tibble::tibble(
    u     = seq(from = pi, to = 1.5*pi, length.out = 91), # ラジアン
    x0    = grid_size, 
    y0    = grid_size, 
    arc_r = grid_size - 1/tanh(theta), 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u)
  )
  
  # 軸変換を作図
  convert_graph <- ggplot() + 
    geom_path(
      data    = grid_df, 
      mapping = aes(x = arc_x, y = arc_y, group = arc_r, linewidth = grid), 
      color = "white", show.legend = FALSE
    ) + # グリッド線
    geom_line(
      data    = convert_df, 
      mapping = aes(x = arc_x, y = arc_y), 
      linewidth = 0.8, linetype = "dotted"
    ) + # 変換曲線
    geom_segment(
      mapping = aes(
        x = c(1/tanh(theta), grid_size), y = c(grid_size, 1/tanh(theta)), 
        xend = c(1/tanh(theta), Inf), yend = c(Inf, 1/tanh(theta))
      ), 
      linewidth = 0.8, linetype = "dotted"
    ) + # x・x軸の目盛線
    geom_point(
      mapping = aes(x = c(1/tanh(theta), grid_size), y = c(grid_size, 1/tanh(theta))), 
      size = 4
    ) + # 関数点
    scale_linewidth_manual(
      breaks = c("major", "minor"), 
      values = c(0.5, 0.25)
    ) + # 主・補助目盛線用
    coord_fixed(
      ratio = 1, 
      xlim = c(-axis_size, axis_size), 
      ylim = c(-axis_size, axis_size)
    ) + 
    labs(
      x = expression(x), 
      y = expression(x)
    )
  
  ## coth関数曲線
  
  # 関数ラベルを作成
  coord_label <- paste0(
    "(list(theta, f(theta))) == ", 
    "(list(", sprintf(theta, fmt = "%.2f"), ", ", sprintf(1/tanh(theta), fmt = "%.2f"), "))"
  )
  
  # coth関数曲線の作図
  curve_coth_graph <- ggplot() + 
    geom_segment(
      mapping = aes(
        x = c(-Inf, 0), y = c(0, -Inf), 
        xend = c(Inf, 0), yend = c(0, Inf)
      ),
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # θ・x軸線
    geom_line(
      data    = fnc_curve_df, 
      mapping = aes(x = t, y = coth_t), 
      linewidth = 1
    ) + # 関数曲線
    geom_segment(
      mapping = aes(
        x = c(theta, theta), y = c(-Inf, 1/tanh(theta)), 
        xend = c(theta, -Inf), yend = c(Inf, 1/tanh(theta))
      ), 
      linewidth = 0.8, linetype = "dotted"
    ) + # θ・x軸の目盛補助線
    geom_point(
      data    = fnc_point_df, 
      mapping = aes(x = t, y = coth_t), 
      size = 4
    ) + # 関数点
    geom_segment(
      data    = fnc_point_df, 
      mapping = aes(x = t, y = 0, xend = t, yend = coth_t), 
      color = color_vec[which(fnc_level_vec=="coth")], linewidth = 1
    ) + # 関数線分
    coord_fixed(
      ratio = 1, 
      ylim = c(-axis_size, axis_size)
    ) + 
    labs(
      title = "hyperbolic cotangent function", 
      subtitle = parse(text = coord_label), 
      x = expression(theta), 
      y = expression(f(theta) == coth~theta)
    )
  
  ## 図の書き出し
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    hyperbola_graph, curve_tanh_graph, 
    convert_graph, curve_coth_graph, 
    nrow = 2, ncol = 2
  )
  
  # ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = wrap_graph, 
    width = 900, height = 1200, units = "px", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "hyperbolic/figure/coth/definition_coth_and_tanh_curve.mp4", framerate = 50) -> tmp_path # mp4ファイルを書出


