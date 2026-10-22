# Prepara las fotos de campo del CIP La Raya (Excel del Dr. Ibañez) para la Fig. 2:
# difumina las caras del personal y amplía la vista lateral. Salida en 03_RESULTS/figuras.
import pathlib
from PIL import Image, ImageFilter

raiz = pathlib.Path(__file__).resolve().parents[2]
origen, destino = raiz / "01_DATA" / "fotos_campo", raiz / "03_RESULTS" / "figuras"

def difuminar(im, caja):
    zona = im.crop(caja).filter(ImageFilter.GaussianBlur(14))
    im.paste(zona, caja)
    return im

fotos = {  # archivo: (caja de la cara en píxeles originales, factor de ampliación, salida)
    "image7.png": ((48, 132, 112, 206), 2, "llama_campo_lateral.png"),
    "image5.png": ((330, 180, 420, 270), 1, "campo_baston.png"),
    "image6.png": ((695, 280, 795, 360), 1, "campo_cinta.png"),
}
for archivo, (caja, factor, salida) in fotos.items():
    im = difuminar(Image.open(origen / archivo).convert("RGB"), caja)
    if factor > 1:
        im = im.resize((im.width * factor, im.height * factor), Image.LANCZOS)
    im.save(destino / salida)
    print(salida, im.size)
