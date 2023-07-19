import time 
import subprocess

with open("bp_programs.txt") as file:
    progs = [line.rstrip() for line in file if line.strip() != ""]

solvers = ["incl_spot"]

print("| Program |", end="", flush=True)

for solver in solvers: 
    print(" " + solver +" |", end="", flush=True)

print("")
print("-----------------------------------------------------------------------")

for p in progs:
    print("|  " + p + "  |", end="", flush=True)
    for solver in solvers: 
        startTime = time.time()
        try:
            result = subprocess.run(["../app/AutoHyper", "--bp", "./bp/" + p,  "./bp/gni.txt", "-m", solver], stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=60)

            endTime = time.time()
            et = endTime - startTime 

            out = result.stdout.decode("utf-8").strip()

            if out == "SAT" or out == "UNSAT":
                print("  " + "%.2f" % et + "  |", end="", flush=True)
            else:
                print("   ERR  |", end="", flush=True)
                print(out)
        except subprocess.TimeoutExpired:
            print("   TO  |", end="", flush=True)


        

    print("")