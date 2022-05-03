import sys
import re
import random


if len(sys.argv) != 6:
    print("Usage: python trace_page_map.py <trace_file:str> <output_file:str> <page_size:int> <num_threads:int> <percent_include:double>")
    exit(1)

trace_file = sys.argv[1]
output_file = sys.argv[2]
page_size = int(sys.argv[3])
num_threads = int(sys.argv[4])
percent_include = float(sys.argv[5])
random.seed(0)

page_mapping = {}
pages_mapped = 0
with open(trace_file, 'r') as f:
    with open(output_file, 'w+') as out:
        
        for line in f:
            if random.uniform(0, 1) > percent_include:
                continue
            match = re.match(r'^(\d+) (.): (0x[0-9|a|b|c|d|e|f]+)$', line)
            res = match.groups()
            thread_id = int(res[0])
            access_type = res[1]
            address = int(res[2], 16)
            
            page = address // page_size
            if page not in page_mapping:
                page_mapping[page] = pages_mapped
                pages_mapped += 1

            mapped_page = page_mapping[page]
        
            out.write(f"{thread_id} {access_type}: {mapped_page}\n")

print(f"{pages_mapped} pages of size {page_size}")
