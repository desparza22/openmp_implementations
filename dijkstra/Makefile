
.PHONY: traces \
	matrices \
	all \
	clean

all: dijkstra_openmp \
	traces \
	matrices

dijkstra_openmp: dijkstra_openmp.c
	gcc -fopenmp -o dijkstra_openmp dijkstra_openmp.c

traces: trace8 trace16 trace32 trace64

trace8: export OMP_NUM_THREADS=8
trace8: dijkstra_openmp 
	dijkstra_openmp > trace8

trace16: export OMP_NUM_THREADS=16
trace16: dijkstra_openmp
	dijkstra_openmp > trace16

trace32: export OMP_NUM_THREADS=32
trace32: dijkstra_openmp
	dijkstra_openmp > trace32

trace64: export OMP_NUM_THREADS=64
trace64: dijkstra_openmp
	dijkstra_openmp > trace64

matrices: matrix_8 matrix_16 matrix_32 matrix_64

matrix5_8.csv: trace8
	python3 ../matrix_produce.py \
	trace8 matrix5_8.csv 0 -1 5 8

matrix10_8.csv: trace8
	python3 ../matrix_produce.py \
	trace8 matrix10_8.csv 0 -1 10 8

matrix20_8.csv: trace8
	python3 ../matrix_produce.py \
	trace8 matrix20_8.csv 0 -1 20 8

matrix_8: matrix5_8.csv matrix10_8.csv matrix20_8.csv

matrix5_16.csv: trace16
	python3 ../matrix_produce.py \
	trace16 matrix5_16.csv 0 -1 5 16

matrix10_16.csv: trace16
	python3 ../matrix_produce.py \
	trace16 matrix10_16.csv 0 -1 10 16

matrix20_16.csv: trace16
	python3 ../matrix_produce.py \
	trace16 matrix20_16.csv 0 -1 20 16

matrix_16: matrix5_16.csv matrix10_16.csv matrix20_16.csv

matrix5_32.csv: trace32
	python3 ../matrix_produce.py \
	trace32 matrix5_32.csv 0 -1 5 32

matrix10_32.csv: trace32
	python3 ../matrix_produce.py \
	trace32 matrix10_32.csv 0 -1 10 32

matrix20_32.csv: trace32
	python3 ../matrix_produce.py \
	trace32 matrix20_32.csv 0 -1 20 32

matrix_32: matrix5_32.csv matrix10_32.csv matrix20_32.csv

matrix5_64.csv: trace64
	python3 ../matrix_produce.py \
	trace64 matrix5_64.csv 0 -1 5 64

matrix10_64.csv: trace64
	python3 ../matrix_produce.py \
	trace64 matrix10_64.csv 0 -1 10 64

matrix20_64.csv: trace64
	python3 ../matrix_produce.py \
	trace64 matrix20_64.csv 0 -1 20 64

matrix_64: matrix5_64.csv matrix10_64.csv matrix20_64.csv

clean:
	rm -f dijkstra_openmp
