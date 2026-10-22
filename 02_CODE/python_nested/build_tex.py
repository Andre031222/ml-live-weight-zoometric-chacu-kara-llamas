import pandas as pd, json, re, ast
S=pd.read_csv('out/cv_summary_C.csv'); A=pd.read_csv('out/per_age_errors.csv'); B=pd.read_csv('out/bland_altman_calibration.csv')
T=pd.read_csv('out/strategies.csv'); bm=json.load(open('out/best_models.json')); I=pd.read_csv('out/permutation_importance.csv')
DS=["K'ara","K'ara males","K'ara females","Ch'acu","Ch'acu males","Ch'acu females","Pooled"]
f=lambda x:f"{x:.2f}"
# CV table
cols=["MLR","Allometric","ENet","KNN","SVM","XGBoost","AdaBoost","ANN"]
rows=[]
for d in DS:
    g=S[S.dataset==d].set_index('model'); cells=[]
    for m in cols:
        s=f"{f(g.loc[m,'RMSE'])} ({f(g.loc[m,'RMSE_sd'])})"; cells.append(f"\\textbf{{{s}}}" if g.loc[m,'equivalent'] else s)
    rows.append(d+" & "+" & ".join(cells)+r" \\")
cv=r"""\begin{table}[!htbp]
\centering
\small
\caption{Nested cross-validated RMSE (kg) with all twelve measurements: mean and, in parentheses, standard deviation over the 100 outer test folds (10-fold cross-validation repeated 10 times; hyperparameters tuned by inner 5-fold cross-validation). Bold values mark the best model and those statistically equivalent to it (corrected resampled t-test, $P \geq 0.05$). PLS, CART and RF are shown only in Fig.~\ref{fig:heatmap}.}
\label{tab:cv}
\footnotesize
\setlength{\tabcolsep}{2pt}
\begin{tabular}{@{}lcccccccc@{}}
\toprule
Dataset & """+" & ".join(cols)+r""" \\
\midrule
"""+"\n".join(rows)+r"""
\bottomrule
\end{tabular}
\end{table}"""
# strategies table
srows=[]
for d in DS:
    t={s:T[(T.strategy==s)&(T.dataset==d)].iloc[0] for s in "ABC"}
    def nv(r): return f"{r.n_vars_min}" if r.n_vars_min==r.n_vars_max else f"{r.n_vars_min}--{r.n_vars_max}"
    def top(r):
        fr=ast.literal_eval(r.sel_freq); return ", ".join(k for k,v in fr.items() if v>=15)
    best=min(t[s].best_RMSE for s in "ABC")
    def val(s): v=f"{f(t[s].best_RMSE)} ({t[s].best_model})"; return f"\\textbf{{{v}}}" if t[s].best_RMSE==best else v
    srows.append(f"{d} & {nv(t['A'])} & {top(t['A'])} & {val('A')} & {nv(t['B'])} & {top(t['B'])} & {val('B')} & {val('C')} \\\\")
st=r"""\begin{table}[!htbp]
\centering
\caption{Predictor selection performed inside each outer training fold: number of predictors retained, predictors selected in at least half of the 30 outer folds, and nested cross-validated RMSE (kg) of the best model (in parentheses) under each strategy (10-fold cross-validation repeated 3 times for all strategies). Bold values mark the lowest RMSE per dataset.}
\label{tab:strategies}
\scriptsize
\setlength{\tabcolsep}{2.5pt}
\begin{tabularx}{\textwidth}{@{}l c>{\raggedright\arraybackslash}X c c>{\raggedright\arraybackslash}X c c@{}}
\toprule
 & \multicolumn{3}{c}{A. Filter 0.75 + Boruta} & \multicolumn{3}{c}{B. Guided filter 0.95 + Boruta} & C. All 12 \\
\cmidrule(lr){2-4}\cmidrule(lr){5-7}\cmidrule(lr){8-8}
Dataset & $p$ & Predictors & RMSE & $p$ & Predictors & RMSE & RMSE \\
\midrule
"""+"\n".join(srows)+r"""
\bottomrule
\end{tabularx}
\end{table}"""
# per-age table (best model)
arows=[]
for d in DS:
    g=A[(A.dataset==d)&(A.model==bm[d])]
    cells=[]
    for age in ['1','2','3','4','5','all']:
        r=g[g.age.astype(str)==age].iloc[0]; cells.append(f"{f(r.RMSE)} / {r.R2:.2f}")
    arows.append(f"{d} ({bm[d]}) & "+" & ".join(cells)+r" \\")
at=r"""\begin{table}[!htbp]
\centering
\caption{Error of the best model of each dataset within each age class: RMSE (kg) / $R^2$ computed from out-of-fold predictions averaged over the 10 repeats of the outer cross-validation.}
\label{tab:age}
\scriptsize
\setlength{\tabcolsep}{3pt}
\begin{tabular}{@{}lcccccc@{}}
\toprule
Dataset (model) & 1 year & 2 years & 3 years & 4 years & 5 years & All ages \\
\midrule
"""+"\n".join(arows)+r"""
\bottomrule
\end{tabular}
\end{table}"""
brows=[]
for d in DS:
    r=B[(B.dataset==d)&(B.model==bm[d])].iloc[0]
    sci=[float(x) for x in re.findall(r"[-\d.]+(?=\))", r.slope_ci)] if 'np.float64' in r.slope_ci else list(ast.literal_eval(r.slope_ci))
    ici=[float(x) for x in re.findall(r"[-\d.]+(?=\))", r.int_ci)] if 'np.float64' in r.int_ci else list(ast.literal_eval(r.int_ci))
    brows.append(f"{d} ({bm[d]}) & {r.bias:.2f} & {r.LoA_low:.2f} to {r.LoA_high:.2f} & {r.prop_bias_slope:.3f} ({r.prop_bias_p:.3f}) & {r.calib_slope:.3f} ({sci[0]:.3f}--{sci[1]:.3f}) & {r.calib_intercept:.2f} ({ici[0]:.2f} to {ici[1]:.2f}) \\\\")
bt=r"""\begin{table}[!htbp]
\centering
\caption{Agreement and calibration of the best model of each dataset (out-of-fold predictions): Bland--Altman bias and 95\% limits of agreement (LoA, kg), slope of the difference on the mean (proportional bias; $P$ in parentheses), and slope and intercept (95\% confidence intervals) of the regression of observed on predicted weight.}
\label{tab:ba}
\scriptsize
\setlength{\tabcolsep}{2.5pt}
\begin{tabular}{@{}lccccc@{}}
\toprule
Dataset (model) & Bias & 95\% LoA & Proportional bias & Calibration slope & Calibration intercept \\
\midrule
"""+"\n".join(brows)+r"""
\bottomrule
\end{tabular}
\end{table}"""
open('tables.tex','w').write("%CV\n"+cv+"\n%ST\n"+st+"\n%AGE\n"+at+"\n%BA\n"+bt)
print(open('tables.tex').read())
top=I.sort_values('mean',ascending=False).groupby('dataset').head(3).groupby('dataset').variable.apply(list); print(top)
