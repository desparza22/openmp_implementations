import sys
import re

if len(sys.argv) != 7:
    print("Usage: python matrix_view.py <trace_file:str> <output_file:str> <start_access:int> <end_access:int> <page_size:int> <num_threads:int>\n if end_access == -1 then matric_view goes to last access")
    exit(1)

trace_file = sys.argv[1]
output_file = sys.argv[2]
start_access = int(sys.argv[3])
end_access = int(sys.argv[4])
page_size = int(sys.argv[5])
num_threads = int(sys.argv[6])

accesses = {}

with open(trace_file, 'r') as f:
    for line in f:
        match = re.match(r'^(\d+) (.): (0x[0-9|a|b|c|d|e|f]+)$', line)
        res = match.groups()
        thread_id = int(res[0])
        address = int(res[2], 16)

        page = address // page_size

        page_accesses = None
        if page in accesses:
            page_accesses = accesses[page]
        else:
            page_accesses = [0 for i in range(num_threads)]

        page_accesses[thread_id] += 1
        accesses[page] = page_accesses
        
    print(f"Memory accesses read from {trace_file}")

communication_matrix = [[0 for i in range(num_threads)] for j in range(num_threads)]
for page in accesses:
    page_accesses = accesses[page]
    for i in range(num_threads-1):
        if page_accesses[i] == 0:
            continue
        for j in range(i+1, num_threads):
            communication_matrix[i][j] += page_accesses[i] * page_accesses[j]
            communication_matrix[j][i] += page_accesses[i] * page_accesses[j]

print("Communication matrix computed")
            
import csv
with open(output_file, 'w+') as out:
    writer = csv.writer(out)

    def header_symbol(pos):
        if pos == 0:
            return ' '
        else:
            return str(pos - 1)

    def row_symbol(row, col):
        if col == 0:
            return str(row)
        else:
            return str(communication_matrix[row][col-1])
    
    header_to_print = [header_symbol(i) for i in range(num_threads+1)]
    writer.writerow(header_to_print)
    
    for i in range(num_threads):
        row_to_print = [row_symbol(i, j) for j in range(num_threads+1)]
        writer.writerow(row_to_print)

    print(f"Results printed to {output_file}")
