
# 円関数の可視化 -----------------------------------------------------------------

# 利用パッケージ
library(shiny)
library(ggplot2)


# server
function(input, output) {

  output$plot <- renderPlot({

    # グラフサイズを設定:(UIサイドの入力設定に依存)
    tick_size <- 2
    x_size    <- tick_size * pi
    y_size    <- 2
    
    # ラジアン軸目盛用の値を作成
    tick_num <- 2
    tick_vec      <- seq(from = -tick_size, to = tick_size, by = 1/tick_num)
    rad_break_vec <- tick_vec * pi
    rad_label_vec <- paste0(round(rad_break_vec/pi, digits = 2), " * pi")
    
    # 基本形の曲線の座標を作成
    base_df <- tibble::tibble(
      t   = seq(from = -x_size, to = x_size, length.out = 1000), 
      f_t = sin(t)
    )
    
    # パラメータを取得
    A     <- input$amplitude 
    a     <- input$period
    alpha <- input$phase * pi
    
    # 変形した曲線の座標を作成
    wave_df <- tibble::tibble(
      t   = seq(from = input$curve_variable[1]*pi, to = input$curve_variable[2]*pi, length.out = 1000), 
      f_t = A * sin(a * t + alpha)
    )
    
    # 変形した曲線上の点の座標を作成
    point_df <- tibble::tibble(
      t   = input$point_variable * pi, 
      f_t = A * sin(a * t + alpha)
    )
    
    # ラベル用の文字列を作成
    param_label <- paste0(
      "list(", 
      "A == ", round(A, digits = 2), ", ", 
      "a == ", round(a, digits = 2), ", ", 
      "alpha == ", round(alpha/pi, digits = 2), "*pi, ", 
      "(list(theta, f(theta))) == ", 
      "(list(", round(point_df[["t"]], digits = 2), ", ", round(point_df[["f_t"]], digits = 2), "))", 
      ")"
    )
    
    # 関数曲線を作図
    ggplot() + 
      geom_segment(
        mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)), 
        arrow = arrow(length = unit(10, units = "pt"), ends = "last")
      ) + # x・y軸線
      geom_line(
        data = base_df, 
        mapping = aes(x = t, y = f_t, linetype = "base")
      ) + # 基本形の曲線
      geom_line(
        data = wave_df, 
        mapping = aes(x = t, y = f_t, linetype = "wave")
      ) + # 関数曲線
      geom_point(
        data = point_df, 
        mapping = aes(x = t, y = f_t), 
        size = 4
      ) + # 関数点
      scale_linetype_manual(
        breaks = c("base", "wave"), 
        values = c("dashed", "solid"), 
        labels = c(expression(sin~theta), expression(A~sin(a*theta+alpha))), 
        name = "function"
      ) + # 凡例の表示用
      scale_x_continuous(
        breaks = rad_break_vec, 
        labels = parse(text = rad_label_vec)
      ) + # ラジアン軸目盛
      theme(
        legend.text.align = 0, 
        legend.position = c(0, 1), 
        legend.justification = c(0, 1), 
        legend.background = element_rect(fill = alpha("white", alpha = 0.8))
      ) + 
      coord_equal(
        xlim = c(-x_size, x_size), 
        ylim = c(-y_size, y_size)
      ) + 
      labs(
        title = "sine function", 
        subtitle = parse(text = param_label), 
        x = expression(theta), 
        y = expression(f(theta))
      )
    
  })
}
