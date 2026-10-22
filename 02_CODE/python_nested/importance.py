"""Permutation importance inside the outer folds of repeat 0 (no leakage): the best model of each dataset
is tuned by inner CV on the outer-training part, and each predictor is permuted 20 times in the outer-test fold."""
import json, numpy as np, pandas as pd, nested as n
from joblib import Parallel, delayed
from sklearn.model_selection import StratifiedKFold, KFold, GridSearchCV
best = json.load(open("out/best_models.json")); d = n.load()
def job(ds, k, a, t, sub):
    tr, te, _ = n.clean(sub.iloc[a], sub.iloc[t]); m = best[ds]; est, grid = n.models(12, k)[m]
    cv = list(KFold(5, shuffle=True, random_state=k).split(tr))
    fit = GridSearchCV(est, grid, cv=cv, scoring="neg_root_mean_squared_error").fit(tr[n.Z], tr.PEVI) if grid else est.fit(tr[n.Z], tr.PEVI)
    rmse = lambda X: np.sqrt(np.mean((np.ravel(fit.predict(X)) - te.PEVI.values) ** 2)); base = rmse(te[n.Z]); rng = np.random.default_rng(k); out = []
    for v in n.Z:
        inc = []
        for _ in range(20):
            X = te[n.Z].copy(); X[v] = rng.permutation(X[v].values); inc.append(rmse(X) - base)
        out.append(dict(dataset=ds, model=m, fold=k, variable=v, inc=np.mean(inc)))
    return out
jobs = []
for ds, (b, s) in n.DATASETS.items():
    sub = d[d.VARIE == b] if b else d; sub = (sub[sub.SEXO == s] if s else sub).reset_index(drop=True)
    for k, (a, t) in enumerate(StratifiedKFold(10, shuffle=True, random_state=2026).split(sub, sub.CELL)): jobs.append((ds, k, a, t, sub))
R = pd.DataFrame([x for o in Parallel(n_jobs=6)(delayed(job)(*j) for j in jobs) for x in o])
I = R.groupby(["dataset","model","variable"]).inc.agg(["mean","std"]).reset_index().sort_values(["dataset","mean"], ascending=[True, False])
I.to_csv("out/permutation_importance.csv", index=False); print(I.groupby("dataset").head(3).round(2).to_string())
import matplotlib; matplotlib.use("Agg"); import matplotlib.pyplot as plt
plt.rcParams.update({"font.size":8,"axes.spines.top":False,"axes.spines.right":False})
DS = ["K'ara","K'ara males","K'ara females","Ch'acu","Ch'acu males","Ch'acu females","Pooled"]
fig, axs = plt.subplots(2, 4, figsize=(7.4, 4.2)); axs = axs.ravel()
for k, ds in enumerate(DS):
    g = I[I.dataset == ds].head(6).iloc[::-1]; axs[k].barh(g.variable, g["mean"], xerr=g["std"], color="#0072B2", ecolor="grey")
    axs[k].set_title(f"{ds} ({g.model.iloc[0]})", fontsize=8); axs[k].set_xlabel("ΔRMSE (kg)")
axs[7].axis("off"); fig.tight_layout(); fig.savefig("fig/Figure_8.pdf"); fig.savefig("fig/Figure_8.png", dpi=600)
