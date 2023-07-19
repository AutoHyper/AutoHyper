import time 
import subprocess

with open("symbolic_instances.txt") as file:
    lines = [line.rstrip().split(" ") for line in file if line.strip() != ""]

for l in lines: 
    sys = l[0]
    prop = l[1]
    a = l[2:]

    print("")
    print("----------------------------------------")
    print("System: " + sys)
    print("Property: " + prop)

    args = ["--nusmv"] + a + ["-m", "incl_spot"]

    startTime = time.time()
    result = subprocess.run(["../app/AutoHyper"] + args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    endTime = time.time()
    et = endTime - startTime 

    out = result.stdout.decode("utf-8").strip()

    print("\033[94m" + out + "\033[0m")
    print("")
    print("Time :", "%.2f" % et, "s")
    print("----------------------------------------")
