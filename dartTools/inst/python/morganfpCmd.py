import sys
from rdkit import Chem 
from rdkit.Chem import AllChem
# Hide RDKit warnings
from rdkit import RDLogger
lg = RDLogger.logger()
lg.setLevel(RDLogger.CRITICAL)
import json

def morgan_fp_bitvector_strings(smiles, radius = 3, nbits = 1024):
    """
    returns list with smiles and bitvector as strings
    """
    try:
        mol = Chem.MolFromSmiles(smiles, sanitize = True)
        bitvec = AllChem.GetMorganFingerprintAsBitVect(mol, radius = radius,nBits=nbits)
        return [smiles] + list(bitvec.ToBitString())
    except:
        return [smiles] + nbits * [""]

def morgan_fp_bitvector_int(smiles, radius = 3, nbits = 1024):
    """
    returns list without smiles and bitvector as integers (or float('nan') if no fingerprint can be created
    """

    try:
        mol = Chem.MolFromSmiles(smiles, sanitize = True)
        bitvec = AllChem.GetMorganFingerprintAsBitVect(mol, radius = radius,nBits=nbits)
        return bitvec.ToList()
    except:
        return nbits * [float('nan')]

if __name__ == "__main__":
    radius = 3
    nbits = 1024
    res = [["smiles"] + [f'fp{x}' for x in range(nbits)] ] + [morgan_fp_bitvector_strings(line.strip(), radius, nbits) for line in sys.stdin]
    sys.stdout.write(json.dumps(res))
