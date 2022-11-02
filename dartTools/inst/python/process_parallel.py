from standardize_rdkit import standardize_smiles
from drawMolecule import draw_smiles
from morganfpCmd import morgan_fp_bitvector_int
from inchi2smiles import inchi_to_smiles

from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, TimeoutError
from pebble import ThreadPool, ProcessPool

from multiprocessing import set_start_method, get_start_method, cpu_count
# set_start_method('spawn')
print(f"using start method {get_start_method()}", flush = True)
#import os
#os.chdir(os.path.dirname(__file__))

def process_smiles(function_name, smiles, n_workers = None, strategy = "multiprocessing", timeout = 10):
    assert strategy in ["multiprocessing", "threading", "sequential"]
    verbose = True 
    
    fun = eval(function_name)
    
    if smiles is None:
        smiles = []
    elif not isinstance(smiles, list):
        smiles = [smiles]
    
    if n_workers is None:
        n_workers = max([1, cpu_count() - 1])
    
    if n_workers == 1 or strategy == "sequential":
        results = [fun(x) for x in smiles]
        
    else :
        # use pebble instead of concurrent.futures interface to get timeout based on start of the task 
        # as opposed to start of the pool.map()
        Executor = {"multiprocessing" : ProcessPool, "threading" : ThreadPool}[strategy]
        with Executor(n_workers) as pool:
            iterator = pool.map(fun, smiles, timeout = timeout).result()
            results = []
        
            while True:
                try:
                    result = next(iterator)
                    results.append(result)
                except StopIteration:
                    break
                except TimeoutError as error:
                    if verbose:
                        print(f"Compound skipped as processing took more than {timeout} seconds")
                    results.append("")
                except Exception as error:
                    if verbose:
                        print("Compound skipped due to the following error")
                        print(error.__repr__())
                    results.append("")
                
    return results

