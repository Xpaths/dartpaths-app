import sys
import os
from rdkit import Chem 
from rdkit.Chem.inchi import MolFromInchi

# Hide RDKit warnings
from rdkit import RDLogger
lg = RDLogger.logger()
lg.setLevel(RDLogger.CRITICAL)
import json

def inchi_to_smiles(inchi):
    try:
        mol = MolFromInchi(inchi.strip()) 
        return Chem.MolToSmiles(mol)
    except:
        return ""

if __name__ == "__main__":
    
    res = [inchi_to_smiles(line) for line in sys.stdin]
    
    sys.stdout.write(json.dumps(res))
