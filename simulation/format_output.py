import sys
import re

form = "mapper, machine, page size, percent accesses, num threads, cost"

if len(sys.argv) != 5:
    print("Usage: python format_output.py input_file page_size percent_accesses num_threads")
    exit(1)

input_file = sys.argv[1]
page_size = int(sys.argv[2])
percent_accesses = sys.argv[3]
num_threads = int(sys.argv[4])

with open(input_file, 'r') as f:
    for line in f:
        matches = re.match(r'^Cost was (\d+) for seed (\d+) on (.+)$', line)
        res = matches.groups()
        cost = res[0]
        seed = res[1]
        machine = res[2]
        print(f"random_mapper {seed}, {machine}, {page_size}, {percent_accesses}, {num_threads}, {cost}")
    
