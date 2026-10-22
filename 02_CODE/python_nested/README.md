# Re-análisis con validación cruzada anidada (20/10/2026)
Ejecutar desde 02_CODE/python_nested; los datos se leen de ../../01_DATA/ (o de la variable LLAMAS_XLSX).
    python -m venv venv && venv/bin/pip install -r requirements.txt
    venv/bin/python nested.py C 10 MLR,Allometric,ENet,PLS,KNN,CART,RF,XGBoost,SVM,ANN,AdaBoost   # ~40 min, 6 núcleos
    venv/bin/python nested.py A 3 ...   ;   venv/bin/python nested.py B 3 ...                      # ~15 min cada uno
    venv/bin/python summarize.py && venv/bin/python importance.py && python3 build_tex.py
Resultados en out/ y fig/. Es una reimplementación en Python/scikit-learn del pipeline R/caret: en el box no había R, y en la Mac caret no permitía anidar la selección de predictores sin reescribir el código.
