import sys
from rdkit import Chem 
from rdkit.Chem import AllChem
# Hide RDKit warnings
from rdkit import RDLogger
lg = RDLogger.logger()
lg.setLevel(RDLogger.CRITICAL)
import json

def morgan_fp_bitvector(smiles, radius = 3, nbits = 1024):
    try:
        mol = Chem.MolFromSmiles(smiles, sanitize = True)
        bitvec = AllChem.GetMorganFingerprintAsBitVect(mol, radius = radius,nBits=nbits)
        return [smiles] + list(bitvec.ToBitString())
    except:
        return [smiles] + nbits * [""]

radius = 3
nbits = 1024
res = [["smiles"] + [f'fp{x}' for x in range(nbits)] ] + [morgan_fp_bitvector(line.strip(), radius, nbits) for line in sys.stdin]
sys.stdout.write(json.dumps(res))
