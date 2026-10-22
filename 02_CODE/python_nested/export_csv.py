# Exporta los .pkl de la validación anidada a CSV para las figuras en R (02_CODE/figuras_v2.R)
import pandas as pd, pathlib
base = pathlib.Path(__file__).resolve().parents[2] / "03_RESULTS" / "nested_2026-10-20"; out = base / "csv"; out.mkdir(exist_ok=True)
for s in "ABC":
    r = pd.DataFrame(pd.read_pickle(base / f"nested_{s}.pkl")["rows"])
    r.drop(columns=["oof", "params"]).to_csv(out / f"folds_{s}.csv", index=False)
    if s == "C":
        o = pd.DataFrame([(d, m, rep, n, y, p) for d, m, rep, oo in zip(r.dataset, r.model, r.rep, r.oof) for (n, y, p) in oo],
                         columns=["dataset", "model", "rep", "NRO", "obs", "pred"])
        o.groupby(["dataset", "model", "NRO"]).agg(obs=("obs", "first"), pred=("pred", "mean")).reset_index().to_csv(out / "oof_C.csv", index=False)
