
# 円関数の定義の可視化

# %%

# 利用ライブラリ
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 3次元の単位円周の可視化

# フレーム数を指定
frame_num = 600

# 曲線用のラジアンの範囲を指定
theta_vec = np.linspace(start=0*np.pi, stop=2*np.pi, num=1001)

# 点用のラジアンを作成
theta_vals = np.linspace(start=theta_vec.min(), stop=theta_vec.max(), num=frame_num+1)[:frame_num+1]

# 曲線の座標を計算
x_vec = np.cos(theta_vec)
y_vec = np.sin(theta_vec)


# 範囲πにおける目盛数を指定
tick_num = 6

# ラジアン軸目盛用の値を作成
rad_break_vec = np.arange(
    start=np.floor(theta_vec.min() /np.pi*tick_num), 
    stop =np.ceil(theta_vec.max() /np.pi*tick_num) + 1
)
rad_break_vec *= np.pi/tick_num
rad_label_vec = [f'${t/np.pi:.2f} \pi$' for t in rad_break_vec]


# グラフサイズを設定
axis_size  = 2
axis_t_min = rad_break_vec.min()
axis_t_max = rad_break_vec.max()

# 目盛間隔を設定
tick_val = 0.5


# 矢サイズを指定
alr = 0.25

# グラフオブジェクトを初期化
fig, ax = plt.subplots(figsize=(12, 12), dpi=100, facecolor='white', 
                       subplot_kw={'projection': '3d'})
fig.suptitle('unit circle', fontsize=20)

# 作図処理を定義
def update(i):
    
    # 前フレームのグラフを初期化
    plt.cla()
    
    # 点用のラジアンを取得
    theta_val = theta_vals[i]

    # 点の座標を計算
    x_val = np.cos(theta_val)
    y_val = np.sin(theta_val)

    # 角マークの座標を作成
    d  = 0.2
    ds = 0.005
    u_vec = np.linspace(start=0, stop=theta_val, num=600)
    angle_mark_x_vec = (d + ds*u_vec) * np.cos(u_vec)
    angle_mark_y_vec = (d + ds*u_vec) * np.sin(u_vec)

    # 角ラベルの座標を作成
    d = 0.4
    u_val = 0.5 * theta_val
    angle_label_x_val = d * np.cos(u_val)
    angle_label_y_val = d * np.sin(u_val)
    
    # グリッド線の座標を作成
    tick_vals = np.arange(start=-axis_size, stop=axis_size+tick_val, step=tick_val)
    coord_X, coord_Y = np.meshgrid(tick_vals, tick_vals)
    coord_T = np.tile(theta_val, reps=coord_X.shape)
    
    # 3D単位円周を作図
    ax.quiver(axis_t_min, [0, 0, axis_size], [0, -axis_size, 0], 
              axis_t_max-axis_t_min, 0, 0, 
              color='black', linewidth=1, arrow_length_ratio=alr/(axis_t_max-axis_t_min), 
              linestyle=['solid', 'dashed', 'dashed']+['solid']*6) # θ軸線
    ax.quiver(axis_t_min, [-axis_size, 0], [0, -axis_size], 
              0, [2*axis_size, 0], [0, 2*axis_size], 
              color='black', linewidth=1, arrow_length_ratio=alr*0.5/axis_size, 
              linestyle=['dashed']*2+['solid']*4) # x・y軸線
    ax.plot(x_vec, y_vec, zs=axis_t_min, zdir='x', 
            color='C0', linestyle='dashed') # 円周
    ax.plot(theta_vec, x_vec, zs=-axis_size, zdir='z', 
            color='navy', linestyle='dashed') # cos曲線
    ax.plot(theta_vec, y_vec, zs=axis_size, zdir='y', 
            color='crimson', linestyle='dashed') # sin曲線
    ax.quiver([theta_val, axis_t_min], 0 ,0, 
              0, x_val, y_val, 
              color='black', arrow_length_ratio=0) # 動径
    ax.plot(axis_t_min.repeat(600), angle_mark_x_vec, angle_mark_y_vec, 
            color='black', linewidth=1) # 角マーク
    ax.text(axis_t_min, angle_label_x_val, angle_label_y_val, 
            s='$\\theta$', size=15, ha='center', va='center') # 角ラベル
    ax.plot(theta_vec, x_vec, y_vec, linewidth=2) # 関数曲線
    ax.plot_surface(coord_T, coord_X, 
                    coord_Y, color='whitesmoke', alpha=0.1) # xy平面
    ax.plot_wireframe(coord_T, coord_X, coord_Y, 
                      color='gray', linewidth=0.5) # xyグリッド
    ax.scatter([theta_val, axis_t_min, theta_val, theta_val], 
               [x_val, x_val, x_val, axis_size], 
               [y_val, y_val, -axis_size, y_val], 
               s=100) # 関数点
    ax.quiver(theta_val, x_val, y_val, 
              [axis_t_min-theta_val, 0, 0], [0, 0, axis_size-x_val], [0, -axis_size-y_val, 0], 
              arrow_length_ratio=0, color='gray', linestyle='dotted') # 点の目盛線
    ax.quiver([theta_val, axis_t_min, theta_val], 0, [0, 0, -axis_size], 
              0, x_val, 0, 
              arrow_length_ratio=0, color='navy', label='$\cos \\theta$') # cos線分
    ax.quiver([theta_val, axis_t_min, theta_val], [x_val, x_val, axis_size], 0, 
              0, 0, y_val, 
              arrow_length_ratio=0, color='crimson', label='$\sin \\theta$') # sin線分
    ax.set_xticks(ticks=rad_break_vec, labels=rad_label_vec) # θ軸目盛
    ax.set_xlim(xmin=axis_t_min, xmax=axis_t_max)
    ax.set_ylim(ymin=-axis_size, ymax=axis_size)
    ax.set_zlim(zmin=-axis_size, zmax=axis_size)
    ax.set_xlabel('$\\theta$')
    ax.set_ylabel('$x = r\ \cos \\theta$')
    ax.set_zlabel('$y = r\ \sin \\theta$')
    ax.set_title(f'$r = 1, \\theta = {theta_val/np.pi:.2f} \pi, (x, y) = ({x_val:.2f}, {y_val:.2f})$', loc='left')
    ax.legend()
    ax.set_box_aspect((axis_t_max-axis_t_min, 2*axis_size, 2*axis_size))
    #ax.view_init(elev=15, azim=theta_val/np.pi*180-60) # 表示角度

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=50)

# 動画を書出
ani.save(
    filename='../figure/circular/unitcircle_3d.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%


