import sys
from rdkit.Chem import MolFromSmiles     
from rdkit.Chem import Draw
import base64
from io import BytesIO

# Hide RDKit warnings
from rdkit import RDLogger
lg = RDLogger.logger()
lg.setLevel(RDLogger.CRITICAL)

TRANSPARENTPIXEL = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII='

def draw_smiles(smiles):
    try:
        mol = MolFromSmiles(smiles)
        image = Draw.MolToImage(mol, size = (150,75))
        buffered = BytesIO()
        image.save(buffered, format="PNG")
        img_str = base64.b64encode(buffered.getvalue())
        image_str_utf8 = img_str.decode("utf-8")
    except:
        image_str_utf8 = TRANSPARENTPIXEL
    return f'<img src="data:image/png;base64,{image_str_utf8}"></img>'

if __name__ == "__main__":
    import json
    from multiprocessing import Pool, cpu_count
    
    inputsmiles = [line for line in sys.stdin]
    
    with Pool(max([1, cpu_count() - 1])) as p:
        res = p.map(draw_smiles, inputsmiles)
        
    sys.stdout.write(json.dumps(res))
