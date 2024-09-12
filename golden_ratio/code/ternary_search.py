# 黄金比シリーズ
# 三分探索の可視化

# %%

# ライブラリを読込
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

### 関数の設定 ------------------------------------------------------------------

# 関数を作成
def eval_fnc(x):

    # 計算式を指定
    y = 0.5 * x**2 - 4.0 * x + 12
    return y


# 変数の範囲を指定
x_min = -5.0
x_max = 15.0

# 曲線用の変数を作成
x_vec = np.linspace(start=x_min, stop=x_max, num=1001)

# 関数曲線を計算
f_x_vec = eval_fnc(x_vec)


# %%

### 三分探索の推移の作図 ---------------------------------------------------------

### 推移の作成 -----

# 分割比を指定
ratio_l = 1.0 / 3.0
ratio_u = 2.0 / 3.0

# 収束判定の閾値を指定
threshold = 1e-5

# 収束判定の値を初期化
diff = np.inf

# 探索範囲の初期値を設定
x_lower = x_min
x_upper = x_max

# 分割回数を初期化
cnt = 0

# 記録用のオブジェクトを初期化
trace_lt = []

# 三分探索
while True:

    # 分割回数をカウント
    cnt += 2
    
    # 分割点を計算
    x_l = x_lower + ratio_l * (x_upper - x_lower)
    x_u = x_lower + ratio_u * (x_upper - x_lower)

    # 更新値を記録
    trace_lt.append(((x_l, x_u), (x_lower, x_upper)))
    
    # 関数値を計算
    f_u = eval_fnc(x_u)
    f_l = eval_fnc(x_l)
    
    # 関数値の差を計算
    diff = abs(f_u - f_l)
    
    # 収束を判定
    if diff <= threshold:
        break

    if f_u > f_l:
        # 上限を更新
        x_upper = x_u
    else:
        # 下限を更新
        x_lower = x_l

    # 途中経過を表示
    print(f'count: {cnt}, x1 = {x_l:.3f}, x2 = {x_u:.3f}, diff: {diff}')# 分割比を指定

# %%

### アニメーションの作図 -----

# フレーム数を設定
frame_num = len(trace_lt)

# グラフサイズを設定
f_min = 0.0
f_max = np.ceil(f_x_vec.max())

# 配色を指定
color_l = 'C1'
color_u = 'C2'

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white', 
                       constrained_layout=True)
fig.suptitle('ternary search', fontsize=20)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 分割点を取得
    (x_l, x_u), (x_lower, x_upper) = trace_lt[frame_i]
    f_l = eval_fnc(x_l)
    f_u = eval_fnc(x_u)

    # 装飾用の設定
    cnt = frame_i * 2 + 1
    label_l = '$(x_{' + str(cnt) + '}, f(x_{' + str(cnt) + '}))$'
    label_u = '$(x_{' + str(cnt+1) + '}, f(x_{' + str(cnt+1) + '}))$'
    if f_l < f_u:
        min_label = f'$(x, f(x)) = ({x_l:.5f}, {f_l:.5f})$'
    else:
        min_label = f'$(x, f(x)) = ({x_u:.5f}, {f_u:.5f})$'
    
    # 関数曲線を描画
    ax.plot(x_vec, f_x_vec, color='black') # 関数
    ax.vlines(x=[x_lower, x_upper], ymin=f_min, ymax=f_max, 
              color='black', linestyle='dashed') # 探索範囲
    ax.vlines(x=[x_l, x_u], ymin=f_min, ymax=f_max, 
              color=[color_l, color_u], linestyle='dashed') # 分割値
    ax.hlines(y=[f_l, f_u], xmin=x_min, xmax=[x_l, x_u], 
              color=[color_l, color_u], linestyle='dotted') # 分割点の値
    ax.scatter(x=x_l, y=f_l, 
               s=50, color=color_l, label=label_l) # 分割点(小)
    ax.scatter(x=x_u, y=f_u, 
               s=50, color=color_u, label=label_u) # 分割点(大)
    ax.set_xlabel('$x$')
    ax.set_ylabel('$f(x)$')
    ax.set_title(min_label, loc='left')
    ax.grid()
    ax.legend()
    ax.set_xlim(xmin=x_min, xmax=x_max)
    ax.set_ylim(ymin=f_min, ymax=f_max)

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=500)

# 動画を書出
ani.save(
    filename='../figure/ternary_search/TernarySearch_trace.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%


