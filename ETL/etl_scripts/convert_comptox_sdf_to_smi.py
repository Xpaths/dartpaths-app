# see also https://iwatobipen.wordpress.com/2018/01/06/simple-way-for-making-smiles-file-rdkit/
# ftp://newftp.epa.gov/COMPTOX/Sustainable_Chemistry_Data/Chemistry_Dashboard/DSSTox_v2000_full.zip

from rdkit import Chem
from rdkit.Chem.rdmolfiles import SmilesWriter
import argparse
from zipfile import ZipFile

def main(inputfile, outputfile):
    zipped = False
    if zipped:
        # this part does not yet work correctly
        with ZipFile(inputfile) as myzip:
            with myzip.open('DSSTox_v2000_full.sdf') as myfile:
                sdf = Chem.SDMolSupplier(myfile)
    else:
        sdf = Chem.SDMolSupplier(inputfile)
    writer = SmilesWriter(outputfile, delimiter = "\t")
    n = 0
    for mol in sdf:
        if mol is None:
            continue
        if n == 0:
            prop_names = [x for x in mol.GetPropNames()]
            # move Preferred name to last column
            # to avoid issues with newline in Preferred name field
            prop_names.remove('Preferred_name')
            prop_names = prop_names + ['Preferred_name']
            writer.SetProps(prop_names)        
        n = n + 1
        if n%1000 == 0:
            print(f"processed{n} smiles")
        writer.write(mol)
    writer.close()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Convert sdf to smi')
    parser.add_argument('--inputfile')
    parser.add_argument('--outputfile')
    args = parser.parse_args()
    main(args.inputfile, args.outputfile)
