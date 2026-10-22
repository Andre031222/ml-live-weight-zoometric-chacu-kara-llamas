import numpy as np, pandas as pd, json, scipy.stats as st
import matplotlib; matplotlib.use("Agg"); import matplotlib.pyplot as plt
OK = ["#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#000000","#999999"]
plt.rcParams.update({"font.family":"DejaVu Sans","font.size":9,"axes.spines.top":False,"axes.spines.right":False,"savefig.dpi":600})
DS = ["K'ara","K'ara males","K'ara females","Ch'acu","Ch'acu males","Ch'acu females","Pooled"]
MOD = ["MLR","Allometric","ENet","PLS","KNN","CART","RF","XGBoost","SVM","ANN","AdaBoost"]
import nested; data = nested.load()
def load(s):
    p = pd.read_pickle(f"out/nested_{s}.pkl"); r = pd.DataFrame(p["rows"]); return r, p["logs"]
C, logsC = load("C")
def nb_test(a, b, ratio=1/9):  # Nadeau & Bengio corrected resampled t
    d = np.asarray(a) - np.asarray(b); J = len(d); v = d.var(ddof=1)
    t = d.mean() / np.sqrt((1/J + ratio) * v) if v > 0 else 0.0
    return t, 1 - st.t.cdf(t, J - 1)
summ = []
for ds in DS:
    sub = C[C.dataset == ds]; piv = sub.pivot_table(index=["rep","fold"], columns="model", values="RMSE")
    means = piv.mean().sort_values(); best = means.index[0]
    fr = st.friedmanchisquare(*[piv[m].values for m in MOD])
    for m in MOD:
        s = sub[sub.model == m]; t, p = nb_test(piv[m], piv[best]) if m != best else (0.0, 1.0)
        summ.append(dict(dataset=ds, model=m, RMSE=s.RMSE.mean(), RMSE_sd=s.RMSE.std(), MAE=s.MAE.mean(), MAPE=s.MAPE.mean(), R2=s.R2.mean(),
                         train_RMSE=s.train_RMSE.mean(), gap=(s.RMSE - s.train_RMSE).mean(), best=(m == best), t=t, p=p, equivalent=(p >= 0.05),
                         friedman_chi2=fr.statistic, friedman_p=fr.pvalue))
S = pd.DataFrame(summ); S.to_csv("out/cv_summary_C.csv", index=False)
# ANN hyperparameters chosen
ann = C[C.model == "ANN"].copy(); ann["size"] = ann.params.map(lambda x: json.loads(x).get("transformedtargetregressor__regressor__hidden_layer_sizes")); ann["decay"] = ann.params.map(lambda x: json.loads(x).get("transformedtargetregressor__regressor__alpha"))
annhp = ann.groupby("dataset")[["size","decay"]].agg(lambda x: x.value_counts().to_dict()); annhp.to_csv("out/ann_hyperparams.csv")
# OOF predictions -> per-animal mean over repeats
def oof(ds, m):
    s = C[(C.dataset == ds) & (C.model == m)]; rows = [(n, y, p, r) for _, rr in s.iterrows() for (n, y, p) in rr.oof for r in [rr.rep]]
    o = pd.DataFrame(rows, columns=["NRO","obs","pred","rep"]); a = o.groupby("NRO").agg(obs=("obs","first"), pred=("pred","mean")).reset_index()
    return a.merge(data[["NRO","VARIE","SEXO","EDAD"]], on="NRO")
def met(y, p):
    e = p - y; return dict(n=len(y), RMSE=np.sqrt(np.mean(e**2)), MAE=np.mean(np.abs(e)), MAPE=100*np.mean(np.abs(e)/y), R2=1-np.sum(e**2)/np.sum((y-y.mean())**2), bias=e.mean())
bestm = S[S.best].set_index("dataset").model.to_dict()
age_rows, ba_rows = [], []
for ds in DS:
    for m in sorted(set([bestm[ds], "ANN", "MLR"])):
        a = oof(ds, m)
        for age, g in a.groupby("EDAD"): age_rows.append(dict(dataset=ds, model=m, age=age, **met(g.obs.values, g.pred.values)))
        age_rows.append(dict(dataset=ds, model=m, age="all", **met(a.obs.values, a.pred.values)))
        d = a.pred - a.obs; mean = (a.pred + a.obs) / 2; lr = st.linregress(a.pred, a.obs); pr = st.linregress(mean, d)
        ba_rows.append(dict(dataset=ds, model=m, bias=d.mean(), sd=d.std(ddof=1), LoA_low=d.mean()-1.96*d.std(ddof=1), LoA_high=d.mean()+1.96*d.std(ddof=1),
                            prop_bias_slope=pr.slope, prop_bias_p=pr.pvalue, calib_slope=lr.slope, calib_intercept=lr.intercept,
                            slope_ci=(lr.slope-1.96*lr.stderr, lr.slope+1.96*lr.stderr), int_ci=(lr.intercept-1.96*lr.intercept_stderr, lr.intercept+1.96*lr.intercept_stderr)))
A = pd.DataFrame(age_rows); A.to_csv("out/per_age_errors.csv", index=False)
B = pd.DataFrame(ba_rows); B.to_csv("out/bland_altman_calibration.csv", index=False)
# strategies (reps 0-2 for fair comparison)
strat = []
for s in "ABC":
    try: R, _ = load(s)
    except Exception: continue
    R = R[R.rep < 3]
    for ds in DS:
        g = R[R.dataset == ds].groupby("model").RMSE.mean().sort_values()
        nv = R[R.dataset == ds].n_vars
        strat.append(dict(strategy=s, dataset=ds, best_model=g.index[0], best_RMSE=g.iloc[0], ANN=g.get("ANN"), MLR=g.get("MLR"), n_vars_min=nv.min(), n_vars_max=nv.max(),
                          sel_freq=R[(R.dataset == ds) & (R.model == "MLR")].vars.str.split(",").explode().value_counts().to_dict()))
T = pd.DataFrame(strat); T.to_csv("out/strategies.csv", index=False)
# outlier log
L = pd.DataFrame(logsC, columns=["part","NRO","variable","value","replacement","z"]).drop_duplicates(["part","NRO","variable"]); L.to_csv("out/outliers_infold.csv", index=False)
pd.set_option("display.width", 250); pd.set_option("display.max_columns", 30)
print(S.round(3).to_string()); print(annhp); print(B.round(3).to_string()); print(T.round(3).drop(columns="sel_freq").to_string()); print(L.groupby(["part","NRO","variable"]).size())
print(A[A.age != "all"].round(2).to_string())
# ---------- figures ----------
import os; os.makedirs("fig", exist_ok=True)
lab = {"K'ara":"K'ara","K'ara males":"K'ara ♂","K'ara females":"K'ara ♀","Ch'acu":"Ch'acu","Ch'acu males":"Ch'acu ♂","Ch'acu females":"Ch'acu ♀","Pooled":"Pooled"}
# Fig 5: strategies
if len(T):
    fig, ax = plt.subplots(figsize=(7, 3.2)); w = 0.26
    for i, s in enumerate("ABC"):
        v = T[T.strategy == s].set_index("dataset").reindex(DS).best_RMSE
        ax.bar(np.arange(7) + (i-1)*w, v, w, color=OK[[5,0,4][i]], label={"A":"A: |r|≥0.75 + Boruta","B":"B: guided |r|≥0.95 + Boruta","C":"C: all 12 measurements"}[s])
    ax.set_xticks(range(7)); ax.set_xticklabels([lab[d] for d in DS]); ax.set_ylabel("Nested CV RMSE of best model (kg)"); ax.legend(frameon=False, fontsize=8)
    fig.tight_layout(); fig.savefig("fig/Figure_5.pdf"); fig.savefig("fig/Figure_5.png"); plt.close(fig)
# Fig 6: heatmap
H = S.pivot(index="dataset", columns="model", values="RMSE").reindex(index=DS, columns=MOD); E = S.pivot(index="dataset", columns="model", values="equivalent").reindex(index=DS, columns=MOD)
fig, ax = plt.subplots(figsize=(7.2, 3.4)); im = ax.imshow(H.values, cmap="cividis_r", aspect="auto")
for i in range(7):
    for j in range(11):
        ax.text(j, i, f"{H.values[i,j]:.2f}", ha="center", va="center", fontsize=7, fontweight="bold" if E.values[i,j] else "normal", color="white" if H.values[i,j] > np.nanpercentile(H.values, 60) else "black")
ax.set_xticks(range(11)); ax.set_xticklabels(MOD, rotation=45, ha="right"); ax.set_yticks(range(7)); ax.set_yticklabels([lab[d] for d in DS])
fig.colorbar(im, ax=ax, label="RMSE (kg)"); fig.tight_layout(); fig.savefig("fig/Figure_6.pdf"); fig.savefig("fig/Figure_6.png"); plt.close(fig)
# Fig 7: observed vs predicted (OOF, best model), Fig 9: Bland-Altman
for kind in ("ovp", "ba"):
    fig, axs = plt.subplots(2, 4, figsize=(7.4, 4.0)); axs = axs.ravel()
    for k, ds in enumerate(DS):
        a = oof(ds, bestm[ds]); ax = axs[k]; col = [OK[int(e)-1] for e in a.EDAD]
        if kind == "ovp":
            ax.scatter(a.pred, a.obs, s=7, c=col, lw=0); lo, hi = a[["obs","pred"]].min().min()-3, a[["obs","pred"]].max().max()+3
            ax.plot([lo,hi],[lo,hi],"--",color="grey",lw=0.8); ax.set_xlim(lo,hi); ax.set_ylim(lo,hi)

        else:
            d = a.pred - a.obs; m = (a.pred + a.obs)/2; b = d.mean(); sd = d.std(ddof=1)
            ax.scatter(m, d, s=7, c=col, lw=0)
            for y, ls in ((b,"-"),(b-1.96*sd,"--"),(b+1.96*sd,"--")): ax.axhline(y, color="grey", ls=ls, lw=0.8)

        ax.set_title(f"{lab[ds]} ({bestm[ds]})", fontsize=8)
    ax = axs[7]; ax.axis("off")
    for e in range(5): ax.scatter([], [], c=OK[e], s=15, label=f"{e+1} year{'s' if e else ''}")
    ax.legend(frameon=False, title="Age", loc="center")
    fig.supxlabel("Predicted live weight (kg)" if kind == "ovp" else "Mean of observed and predicted (kg)", fontsize=9)
    fig.supylabel("Observed live weight (kg)" if kind == "ovp" else "Predicted − observed (kg)", fontsize=9)
    fig.tight_layout(); n = "Figure_7" if kind == "ovp" else "Figure_9"; fig.savefig(f"fig/{n}.pdf"); fig.savefig(f"fig/{n}.png"); plt.close(fig)
# Fig 10: per-age RMSE best model
fig, ax = plt.subplots(figsize=(7, 3.0)); AA = A[(A.age != "all")]
for k, ds in enumerate(DS):
    g = AA[(AA.dataset == ds) & (AA.model == bestm[ds])]; ax.plot(g.age.astype(int), g.RMSE, marker="o", color=OK[k] if k < 7 else "k", label=lab[ds], lw=1)
ax.set_xlabel("Age (years)"); ax.set_ylabel("Within-age RMSE (kg)"); ax.set_xticks(range(1,6)); ax.legend(frameon=False, ncol=4, fontsize=7)
fig.tight_layout(); fig.savefig("fig/Figure_10.pdf"); fig.savefig("fig/Figure_10.png"); plt.close(fig)
json.dump(bestm, open("out/best_models.json","w"))
