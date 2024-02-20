
# 円関数の可視化 -----------------------------------------------------------------

# 利用パッケージ
library(shiny)


# UI
fluidPage(
  
  # タイトル
  titlePanel(title = "circle functions"), 
  
  # 出力
  fluidRow(
    column(
      width = 12, 
      
      # グラフ
      plotOutput(
        outputId = "plot"
      )
    )
  ), 
  
  # 入力
  fluidRow(
    
    # 変数
    column(
      width = 6, 
      "variable", 
      
      # 曲線用の変数の範囲
      sliderInput(
        inputId = "curve_variable", 
        label = "range: θ = xπ", 
        min = -2, 
        max = 2, 
        value = c(0, 1), 
        step = 0.1
      ), 
      
      # 点用の変数
      sliderInput(
        inputId = "point_variable", 
        label = "coord: θ = xπ", 
        min = -2, 
        max = 2, 
        value = 0, 
        step = 0.1
      )
    ), 
    
    # パラメータ
    column(
      width = 6, 
      "parameter", 
      
      # 振幅パラメータ
      sliderInput(
        inputId = "amplitude", 
        label = "amplitude: A = x", 
        min = -2, 
        max = 2, 
        value = 1, 
        step = 0.1, 
        animate = animationOptions(
          interval = 500
        )
      ), 
      
      # 周期パラメータ
      sliderInput(
        inputId = "period", 
        label = "period: a = x", 
        min = -2, 
        max = 2, 
        value = 1, 
        step = 0.1, 
        animate = animationOptions(
          interval = 500
        )
      ), 
      
      # 位相パラメータ
      sliderInput(
        inputId = "phase", 
        label = "phase: α = xπ", 
        min = -2, 
        max = 2, 
        value = 0, 
        step = 0.1, 
        animate = animationOptions(
          interval = 500
        )
      )
    )
  )
)