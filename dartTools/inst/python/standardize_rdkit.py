import sys
import os
from rdkit import Chem 
from rdkit.Chem.MolStandardize import rdMolStandardize
from rdkit import RDLogger
import json
from concurrent.futures import TimeoutError
from pebble import ProcessPool, ProcessExpired
from multiprocessing import cpu_count

# Hide RDKit warnings
lg = RDLogger.logger()
lg.setLevel(RDLogger.CRITICAL)

def check_env(varName, defaultValue = "true"):
    defaultValue = str(defaultValue).lower()
    value = os.getenv(varName, defaultValue)
    return value.lower() == "true"

largestFragmentChooser = rdMolStandardize.LargestFragmentChooser(preferOrganic = True)
tautomerEnumerator = rdMolStandardize.TautomerEnumerator()

def standardize_smiles(smiles):
    """
    standardize a smiles string using rdkit
    
    :param smiles: string with structure in SMILES notation
    
    """
    
    smiles = smiles.strip()
    
    if smiles == "":
        return ""
    try:
        mol = Chem.MolFromSmiles(smiles) #
        if check_env("DART_fragment"):
            mol = largestFragmentChooser.choose(mol) # largest organic fragment
        if check_env("DART_isotope"):
            _ = [atom.SetIsotope(0) for atom in mol.GetAtoms()]
        if check_env("DART_charge"):
            # remove explicit charges (only keeps the largest fragment)
            mol = rdMolStandardize.ChargeParent(mol, skipStandardize = True)
        if check_env("DART_tautomer"):
            mol = tautomerEnumerator.Canonicalize(mol) # tautomer canonicalization
        #return Chem.InchiToInchiKey(Chem.MolToInchi(mol)), Chem.MolToSmiles(mol)
        return Chem.MolToSmiles(mol)
    except Exception as e:
        print(str(e))
        #return "", ""
        return ""

def process_smiles_file(input_file, output_file):
    """
    Processes all smiles provided in input file and write those to output file
    """
    
    with open(input_file, "r") as infile:
        inputsmiles = infile.readlines()

    res = process_smiles_list(inputsmiles)
        
    with open(output_file, "w") as outfile:
        json.dump(res,outfile)

def process_smiles_list(smiles_list, timeout = 60, verbose = False):
    """
    Processes a list of smiles in parallel. If processing of a single smiles takes more
    than <timeout> seconds, it will be skipped
    
    :param smiles_list: list with SMILES strings
    :param timeout: timeout in seconds
    :param verbose: If True, more information will be printed
    """
    
    n_workers = max([1, cpu_count() - 1])
    with ProcessPool(n_workers) as pool:
        future = pool.map(standardize_smiles, smiles_list, timeout=timeout)
    
        iterator = future.result()
        results = []
    
        while True:
            try:
                result = next(iterator)
                results.append(result)
            except StopIteration:
                break
            except TimeoutError as error:
                if verbose:
                    print(f"Compound skipped as standardization took longer than {timeout} seconds")
                results.append("")
            except ProcessExpired as error:
                if verbose:
                    print("%s. Exit code: %d" % (error, error.exitcode))
                results.append("")
            except Exception as error:
                if verbose:
                    if hasattr(error, "traceback"):
                        print("Compound skipped due to the following error")
                        print(error.traceback)  # Python's traceback of remote process
                    else:
                        print("Compound skipped due to error")
                results.append("")
                
    return results


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser("Smiles standardization using rdkit")
    parser.add_argument("--input_file", help="output file")
    parser.add_argument("--output_file", help="output file")   
    args = parser.parse_args()
    process_smiles_file(args.input_file, args.output_file)
    
