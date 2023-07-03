import os
from pathlib import Path
import subprocess
from subprocess import TimeoutExpired
import sys

def system_call(cmd : str, timeout=None):
    proc = subprocess.Popen(cmd, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE)

    try:
        stdout, stderr = proc.communicate(timeout=timeout)
    except TimeoutExpired:
        proc.kill()
        return None, '', ''
   
    return proc.returncode, stdout.decode('utf-8').strip(), stderr.decode('utf-8').strip()


############################## MODIFY ##############################
# Install the NuSMV tool (https://nusmv.fbk.eu/) and add the absolute path to this tool
nusmv_path = '<changeHere>'
############################## MODIFY - END ##############################


if not os.path.isfile(nusmv_path):
    print('The path ', nusmv_path, ' is no valid path. Please provide the path to the NuSMV tool')
    exit(0)
    
files = sys.argv

for f in files:
    p = Path(f)

    new_path = p.parent / Path("_" + p.stem + ".smv")

    script_content = "read_model -i " + str(p) + "; flatten_hierarchy; write_flat_model -o " + str(new_path) + "; quit;"

    script_path = './nusmv_script.txt'

    with open(script_path, 'w') as f:
        f.write(script_content)

    code, out, err = system_call(nusmv_path + ' -source ' + script_path)

    if code != 0 or err != '':
        print(code, out, err) 
        continue

print('Done')