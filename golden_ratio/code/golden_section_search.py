# 黄金比シリーズ
# 黄金分割探索の可視化

# %%

# ライブラリを読込
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation


# %%

### 評価関数の設定 --------------------------------------------------------------

# 評価関数を作成
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

### 黄金分割探索の推移の作図 -----------------------------------------------------

### 推移の作成 -----

# 黄金比の逆数を計算
ratio_g = 0.5 * (np.sqrt(5.0) - 1.0)

# 収束判定の閾値を指定
threshold = 1e-5

# 探索範囲の初期値を設定
x_lower = x_min
x_upper = x_max

# 分割点の初期値を計算
x_l = x_upper - ratio_g * (x_upper - x_lower)
x_u = x_lower + ratio_g * (x_upper - x_lower)

# 分割回数をカウント
cnt_l   = 1
cnt_u   = 2
tmp_cnt = max(cnt_l, cnt_u)

# 初期値を記録
trace_lt = [((cnt_l, cnt_u), (x_l, x_u), (x_lower, x_upper))]

# 黄金分割探索
while True:
        
    # 評価関数を計算
    f_u = eval_fnc(x_u)
    f_l = eval_fnc(x_l)
    
    # 評価値の差を計算
    diff = abs(f_u - f_l)

    # 途中経過を表示
    print(f'count: {max(cnt_l, cnt_u)}, x1 = {x_l:.3f}, x2 = {x_u:.3f}, diff: {diff}')

    # 収束を判定
    if diff <= threshold:
        break

    # 分割回数をカウント
    tmp_cnt += 1
    
    if f_u > f_l:
        # 上限を更新
        x_upper = x_u

        # 分割点を更新
        x_u = x_l
        x_l = x_upper - ratio_g * (x_upper - x_lower)
        cnt_u = cnt_l
        cnt_l = tmp_cnt
    else:
        # 下限を更新
        x_lower = x_l

        # 分割点を更新
        x_l = x_u
        x_u = x_lower + ratio_g * (x_upper - x_lower)
        cnt_l = cnt_u
        cnt_u = tmp_cnt

    # 更新値を記録
    trace_lt.append(((cnt_l, cnt_u), (x_l, x_u), (x_lower, x_upper)))

# %%

### アニメーションの作図 -----

# フレーム数を設定
frame_num = len(trace_lt)

# グラフサイズを設定
f_min = 0.0
f_max = np.ceil(f_x_vec.max())

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white', 
                       constrained_layout=True)
fig.suptitle('golden section search', fontsize=20)

# 作図処理を定義
def update(frame_i):
    
    # 前フレームのグラフを初期化
    ax.cla()

    # 分割点を取得
    (cnt_l, cnt_u), (x_l, x_u), (x_lower, x_upper) = trace_lt[frame_i]
    f_l = eval_fnc(x_l)
    f_u = eval_fnc(x_u)

    # 装飾用の設定
    tmp_cnt = max(cnt_l, cnt_u)
    color_l = 'C' + str(cnt_l)
    color_u = 'C' + str(cnt_u)
    label_l = '$(x_{' + str(cnt_l) + '}, f(x_{' + str(cnt_l) + '}))$'
    label_u = '$(x_{' + str(cnt_u) + '}, f(x_{' + str(cnt_u) + '}))$'
    if f_l < f_u:
        min_label = f'$(x, f(x)) = ({x_l:.5f}, {f_l:.5f})$'
    else:
        min_label = f'$(x, f(x)) = ({x_u:.5f}, {f_u:.5f})$'
    
    # 関数曲線を描画
    ax.plot(x_vec, f_x_vec, color='black') # 評価関数
    ax.vlines(x=[x_lower, x_upper], ymin=f_min, ymax=f_max, 
              color='black', linestyle='dashed') # 探索範囲
    ax.vlines(x=[x_l, x_u], ymin=f_min, ymax=f_max, 
              color=[color_l, color_u], linestyle='dashed') # 分割値
    ax.hlines(y=[f_l, f_u], xmin=x_min, xmax=[x_l, x_u], 
              color=[color_l, color_u], linestyle='dotted') # 分割点の値
    if tmp_cnt%2 == 0: # (凡例用に試行ごとに入れ替わる分割点に対応)
        ax.scatter(x=x_l, y=f_l, 
                   s=50, color=color_l, label=label_l) # 分割点(小)
        ax.scatter(x=x_u, y=f_u, 
                   s=50, color=color_u, label=label_u) # 分割点(大)
    else:
        ax.scatter(x=x_u, y=f_u, 
                   s=50, color=color_u, label=label_u) # 分割点(大)
        ax.scatter(x=x_l, y=f_l, 
                   s=50, color=color_l, label=label_l) # 分割点(小)
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
    filename='../figure/golden_section_search/trace.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)


# %%


