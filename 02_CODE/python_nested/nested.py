"""Re-analysis with nested, leakage-free cross-validation (Python/scikit-learn reimplementation
of the original R/caret pipeline). Outer: repeated stratified 10-fold (by breed x sex x age cell).
Inner: 5-fold CV for hyperparameters (+ predictor selection for strategies A/B).
Outlier screening (robust MAD z on cell-centred residuals, |z|>8 -> training cell median) fitted on
outer-training data only and applied to the outer-test fold with training statistics."""
import sys, json, warnings, numpy as np, pandas as pd
from joblib import Parallel, delayed
from sklearn.model_selection import StratifiedKFold, KFold, GridSearchCV
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import MinMaxScaler, FunctionTransformer
from sklearn.compose import TransformedTargetRegressor
from sklearn.linear_model import LinearRegression, ElasticNet
from sklearn.cross_decomposition import PLSRegression
from sklearn.neighbors import KNeighborsRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import RandomForestRegressor, AdaBoostRegressor
from sklearn.svm import SVR
from sklearn.neural_network import MLPRegressor
from xgboost import XGBRegressor
from sklearn.preprocessing import StandardScaler
warnings.filterwarnings("ignore")
import os
DATA = os.environ.get("LLAMAS_XLSX", os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "01_DATA", "BASE DATOS LLAMAS_CHACU_KARA_1_a_5_year_VRI_2026.xlsx"))
Z = ["ALCRU","ALGRU","PETO","PEAB","LARDO","LACU","PROF","AMPLI","ALAB","LAGRU","ANGRU","PECAN"]

def load():
    d = pd.read_excel(DATA, sheet_name="viq_todo", header=None, skiprows=18, nrows=241, usecols="A:Q")
    d.columns = [str(c).strip() for c in d.iloc[0]]; d = d[1:].reset_index(drop=True)
    d = d.rename(columns={"Nro": "NRO"})
    for c in Z + ["PEVI"]: d[c] = d[c].astype(float)
    d["VARIE"] = d.VARIE.map({"KARA": "K'ara", "CHACU": "Ch'acu"}); d["SEXO"] = d.SEXO.map({"MACHO": "Male", "HEMBRA": "Female"})
    d["EDAD"] = d.EDAD.astype(int); d["CELL"] = d.VARIE + "_" + d.SEXO + "_" + d.EDAD.astype(str)
    return d

DATASETS = {"K'ara": ("K'ara", None), "K'ara males": ("K'ara", "Male"), "K'ara females": ("K'ara", "Female"),
            "Ch'acu": ("Ch'acu", None), "Ch'acu males": ("Ch'acu", "Male"), "Ch'acu females": ("Ch'acu", "Female"),
            "Pooled": (None, None)}

def clean(train, test, thr=8.0, ref=None):
    """Original rule (calidad.R) fitted on training rows only: residual from the breed x sex x age cell mean,
    robust z = (res - median(res)) / MAD(res); |z| > thr -> training cell median. Applied to predictors of
    train and test with training statistics (the response of test animals is never altered)."""
    tr, te = train.copy(), test.copy(); log = []
    ref = tr if ref is None else ref   # rows used to fit the rule (never contains test animals)
    for v in Z:
        mean = ref.groupby("CELL")[v].mean(); res = ref[v] - ref.CELL.map(mean)
        c, mad = np.median(res), 1.4826 * np.median(np.abs(res - np.median(res)))
        for df, part in ((tr, "train"), (te, "test")):
            z = (df[v] - df.CELL.map(mean) - c) / mad
            for i in df.index[z.abs() > thr]:
                cell = ref[(ref.CELL == df.at[i, "CELL"]) & (ref.NRO != df.at[i, "NRO"])][v]
                rep = float(cell.median()); log.append((part, int(df.at[i, "NRO"]), v, float(df.at[i, v]), rep, float(z[i])))
                df.at[i, v] = rep
    return tr, te, log

def find_corr(X, cutoff, y=None):
    """caret::findCorrelation (exact=FALSE-like) for A; response-guided removal for B."""
    keep = list(X.columns)
    C = X.corr(method="spearman").abs().copy()
    while True:
        M = np.array(C.loc[keep, keep].to_numpy(), dtype=float, copy=True); np.fill_diagonal(M, 0); sub = pd.DataFrame(M, index=keep, columns=keep)
        if M.max() < cutoff or len(keep) <= 2: break
        i, j = np.unravel_index(np.argmax(M), M.shape); a, b = sub.index[i], sub.columns[j]
        if y is None: drop = a if sub[a].mean() > sub[b].mean() else b
        else:
            ry = X[[a, b]].corrwith(y, method="spearman").abs(); drop = a if ry[a] < ry[b] else b
        keep.remove(drop)
    return keep

def boruta(X, y, seed, n_iter=50):
    """Minimal Boruta (shadow features, RF permutation-free impurity importance, binomial test)."""
    from scipy.stats import binomtest
    rng = np.random.default_rng(seed); cols = list(X.columns); hits = np.zeros(len(cols))
    for it in range(n_iter):
        sh = X.apply(lambda c: rng.permutation(c.values)); sh.columns = ["sh_" + c for c in cols]
        rf = RandomForestRegressor(200, random_state=int(rng.integers(1e9)), n_jobs=1).fit(pd.concat([X, sh], axis=1), y)
        imp = rf.feature_importances_; hits += imp[:len(cols)] > imp[len(cols):].max()
    p_acc = np.array([binomtest(int(h), n_iter, 0.5, alternative="greater").pvalue for h in hits])
    p_rej = np.array([binomtest(int(h), n_iter, 0.5, alternative="less").pvalue for h in hits])
    sel = [c for c, pa, pr in zip(cols, p_acc, p_rej) if pr >= 0.05 / len(cols)]  # confirmed + tentative (rough fix: keep tentative with hits>median)
    sel = [c for c in sel if (p_acc[cols.index(c)] < 0.05 / len(cols)) or hits[cols.index(c)] > np.median(hits)]
    if len(sel) < 2: sel = [cols[i] for i in np.argsort(-hits)[:2]]
    return sel

def tt(est):  # standardise target (nnet linout / SVR epsilon on comparable scale)
    return TransformedTargetRegressor(regressor=est, transformer=StandardScaler())

def models(p, seed):
    mm = MinMaxScaler()
    return {
        "MLR": (make_pipeline(MinMaxScaler(), LinearRegression()), {}),
        "Allometric": (TransformedTargetRegressor(regressor=make_pipeline(FunctionTransformer(np.log), LinearRegression()), func=np.log, inverse_func=np.exp), {}),
        "ENet": (make_pipeline(MinMaxScaler(), tt(ElasticNet(max_iter=20000))), {"transformedtargetregressor__regressor__l1_ratio": [0.1, 0.5, 0.9, 1.0], "transformedtargetregressor__regressor__alpha": list(np.logspace(-4, 0, 8))}),
        "PLS": (make_pipeline(MinMaxScaler(), PLSRegression(scale=False)), {"plsregression__n_components": list(range(1, p + 1))}),
        "KNN": (make_pipeline(MinMaxScaler(), KNeighborsRegressor()), {"kneighborsregressor__n_neighbors": [3, 5, 7, 9, 11, 13, 15]}),
        "CART": (DecisionTreeRegressor(random_state=seed), {"ccp_alpha": [0, 0.5, 1, 2, 5, 10, 20]}),
        "RF": (RandomForestRegressor(300, random_state=seed, n_jobs=1), {"max_features": sorted(set([1, max(1, p // 4), max(1, p // 3), max(1, p // 2), p]))}),
        "XGBoost": (XGBRegressor(random_state=seed, n_jobs=1, verbosity=0), {"n_estimators": [100, 300], "max_depth": [2, 3, 4], "learning_rate": [0.05, 0.1, 0.3]}),
        "SVM": (make_pipeline(MinMaxScaler(), tt(SVR(kernel="rbf", gamma="scale"))), {"transformedtargetregressor__regressor__C": [0.25, 0.5, 1, 2, 4, 8, 16, 32]}),
        "ANN": (make_pipeline(MinMaxScaler(), tt(MLPRegressor(solver="lbfgs", max_iter=500, random_state=seed))),
                {"transformedtargetregressor__regressor__hidden_layer_sizes": [(1,), (2,), (3,), (4,), (6,), (8,)],
                 "transformedtargetregressor__regressor__alpha": [0.001, 0.01, 0.1, 0.3, 1.0]}),
        "AdaBoost": (AdaBoostRegressor(random_state=seed), {"n_estimators": [50, 100], "estimator": [DecisionTreeRegressor(max_depth=k) for k in (2, 3, 4)]}),
    }

def metrics(y, p):
    e = p - y; return dict(RMSE=float(np.sqrt(np.mean(e**2))), MAE=float(np.mean(np.abs(e))), MAPE=float(100*np.mean(np.abs(e)/y)),
                           R2=float(1 - np.sum(e**2)/np.sum((y-np.mean(y))**2)))

FULL = None
def one_fold(dname, strat, rep, k, tr_idx, te_idx, data, model_names):
    seed = 1000 * rep + k
    full = load(); ref = full[~full.NRO.isin(data.iloc[te_idx].NRO)]  # all 240 llamas except this outer-test fold
    tr, te, log = clean(data.iloc[tr_idx], data.iloc[te_idx], ref=ref)
    Xtr, ytr, Xte, yte = tr[Z], tr.PEVI.values, te[Z], te.PEVI.values
    if strat == "A": vars_ = boruta(Xtr[find_corr(Xtr, 0.75)], ytr, seed)
    elif strat == "B": vars_ = boruta(Xtr[find_corr(Xtr, 0.95, pd.Series(ytr, index=Xtr.index))], ytr, seed)
    else: vars_ = Z
    out = []; inner = StratifiedKFold(5, shuffle=True, random_state=seed)
    groups = tr.CELL.values
    inner_splits = list(KFold(5, shuffle=True, random_state=seed).split(Xtr)) if len(set(groups)) * 5 > len(groups) else list(inner.split(Xtr, groups))
    for m, (est, grid) in models(len(vars_), seed).items():
        if m not in model_names: continue
        if grid:
            gs = GridSearchCV(est, grid, cv=inner_splits, scoring="neg_root_mean_squared_error", n_jobs=1, error_score=np.nan).fit(Xtr[vars_], ytr)
            fit, best = gs.best_estimator_, {kk: str(v) for kk, v in gs.best_params_.items()}
        else: fit, best = est.fit(Xtr[vars_], ytr), {}
        ptr, pte = np.ravel(fit.predict(Xtr[vars_])), np.ravel(fit.predict(Xte[vars_]))
        out.append(dict(dataset=dname, strategy=strat, rep=rep, fold=k, model=m, n_vars=len(vars_), vars=",".join(vars_), params=json.dumps(best),
                        train_RMSE=metrics(ytr, ptr)["RMSE"], **metrics(yte, pte),
                        oof=list(zip(te.NRO.astype(int).tolist(), yte.tolist(), pte.tolist()))))
    return out, log

if __name__ == "__main__":
    strat, reps = sys.argv[1], int(sys.argv[2]); model_names = sys.argv[3].split(",")
    d = load(); jobs = []
    for dn, (b, s) in DATASETS.items():
        sub = d[d.VARIE == b] if b else d; sub = sub[sub.SEXO == s] if s else sub; sub = sub.reset_index(drop=True)
        for r in range(reps):
            for k, (a, t) in enumerate(StratifiedKFold(10, shuffle=True, random_state=2026 + r).split(sub, sub.CELL)):
                jobs.append((dn, strat, r, k, a, t, sub, model_names))
    res = Parallel(n_jobs=6, verbose=2)(delayed(one_fold)(*j) for j in jobs)
    rows = [x for o, _ in res for x in o]; logs = [l for _, lg in res for l in lg]
    pd.to_pickle(dict(rows=rows, logs=logs), f"out/nested_{strat}.pkl")
    print("done", len(rows))
