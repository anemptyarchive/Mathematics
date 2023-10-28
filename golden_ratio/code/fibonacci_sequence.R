
# フィボナッチ数列 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)

# パッケージ名の省略用
library(ggplot2)


# フィボナッチ数列の実装 -------------------------------------------------------------

# フィボナッチ数の計算を実装
fibonacci_val <- function(n) {
  
  # フィボナッチ数を出力
  if(n %in% c(0, 1)) {
    return(n)
  } else {
    # フィボナッチ数を計算
    return(Recall(n = n-2) + Recall(n = n-1))
  }
}

# フィボナッチ数列の計算を実装
fibonacci_vec <- function(n) {
  
  # フィボナッチ数を出力
  if(n == 0) {
    
    # 0番目の項のみの場合
    return(0)
    
  } else if(n == 1) {
    
    # 1番目までの項の場合
    return(c(0, 1))
  }
  
  # n番目までの項の受け皿を作成
  vec <- c(0, 1, rep(NA, times = n-1))
  
  # 項番号ごとに処理
  for(i in 2:n) {
    
    # フィボナッチ数を格納
    vec[i+1] <- sum(vec[c(i-1, i)])
  }
  
  # 数列を出力
  return(vec)
}


# フィボナッチ数列の可視化 ------------------------------------------------------------

# 要素数の最大値を指定
n_max <- 10

# フィボナッチ数列を作成
fib_vec <- sapply(0:n_max, fibonacci_val)
fib_vec <- fibonacci_vec(n = n_max)

# フィボナッチ数列を格納
fib_df <- tibble::tibble(
  n = 0:n_max, # 要素数
  v = fib_vec  # 値
)


# ラベル用の文字列を作成
def_label <- "list(F[0] == 0, F[1] == 1, F[n] == F[n-2] + F[n-1] ~~ (n >= 2))"

# フィボナッチ数を作図
ggplot() + 
  geom_bar(data = fib_df, 
           mapping = aes(x = n, y = v, fill = factor(n)), 
           stat = "identity", show.legend = FALSE) + # 要素数ごとの値
  geom_hline(data = fib_df, 
             mapping = aes(yintercept = v, color = factor(n)), 
             linewidth = 1, linetype = "dashed", show.legend = FALSE) + # 値ごとの軸目盛線
  geom_line(data = fib_df, 
            mapping = aes(x = n, y = v), 
            linewidth = 1) + # 値の推移
  scale_x_continuous(breaks = 0:n_max, minor_breaks = FALSE) + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~., breaks = fib_vec)) + # 値ラベル
  labs(title = "Fibonacci number", 
       subtitle = parse(text = def_label), 
       x = "n", y = "value")


# 各要素の2要素和を作成
pre_term_df <- dplyr::bind_rows(
  # 要素数が2未満のデータ
  tibble::tibble(
    n = 0:1, 
    m = n, 
    v = 0:1
  ), 
  # 要素数が2以上のデータ
  tibble::tibble(
    n = 2:n_max # 要素番号(要素数)
  ) |> 
    dplyr::reframe(
      m = n - c(2, 1), .by = n
    ) |> # 2・1つ前の要素番号を作成
    dplyr::mutate(
      v = fib_vec[m+1]
    )
)

# フィボナッチ数列の構成要素を作図
seq_graph <- ggplot() + 
  geom_bar(data = pre_term_df, 
           mapping = aes(x = n, y = v, fill = factor(m)), 
           stat = "identity", position = "stack", 
           show.legend = FALSE) + # 構成要素
  geom_bar(data = fib_df, 
           mapping = aes(x = n, y = v, color = factor(n), fill = factor(n)), 
           stat = "identity", position = "stack", 
           linewidth = 1, alpha = 0) + # 要素数ごとの値
  geom_hline(data = fib_df, 
             mapping = aes(yintercept = v, color = factor(n)), 
             linewidth = 1, linetype = "dashed", show.legend = FALSE) + # 値ごとの軸目盛線
  scale_x_continuous(breaks = 0:n_max, minor_breaks = FALSE) + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~., breaks = fib_vec)) + # 値ラベル
  scale_color_manual(breaks = 0:n_max, values = scales::hue_pal()(n_max+1)) + # (要素数ごとに色付け用の小細工)
  scale_fill_manual(breaks = 0:n_max, values = scales::hue_pal()(n_max+1)) + # (要素数ごとに色付け用の小細工)
  guides(fill = guide_legend(override.aes = list(alpha = 1, linewidth = 0))) + # (全ての要素数を凡例に表示用の小細工)
  labs(title = "Fibonacci sequence", 
       subtitle = parse(text = def_label), 
       color = "n", fill = "n", 
       x = "n", y = "value")

# 最大要素のn-2要素和を作成
stack_df <- tibble::tibble(
  n = c(0, 1, 1:(n_max-2)), # 要素番号(要素数)
  v = fib_vec[n+1]
)

# フィボナッチ数の構成要素を作図
stack_graph <- ggplot() + 
  geom_bar(data = stack_df, 
           mapping = aes(x = n_max, y = v, fill = factor(n)), 
           stat = "identity", position = position_stack(reverse = TRUE), linewidth = 1.5, show.legend = FALSE) + # 構成要素
  geom_hline(data = fib_df, 
             mapping = aes(yintercept = v, color = factor(n)), 
             linewidth = 1, linetype = "dashed", show.legend = FALSE) + # 値ごとの軸目盛線
  scale_x_continuous(breaks = n_max, limits = c(n_max-0.6, n_max+0.6), minor_breaks = FALSE) + 
  scale_fill_manual(breaks = 0:n_max, values = scales::hue_pal()(n_max+1)) + # (要素数ごとに色付け用の小細工)
  labs(x = "n", y = "value")

# 並べて描画
patchwork::wrap_plots(
  seq_graph, stack_graph, 
  nrow = 1, widths = c(n_max+1, 1), guides = "collect"
)


