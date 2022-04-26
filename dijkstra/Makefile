
.PHONY: open_matrix \
	traces \
	matrices \
	all \
	clean

all: dijkstra \
	traces \
	matrices

dijkstra: dijkstra_openmp.c
	gcc -fopenmp -o dijkstra_openmp dijkstra_openmp.c

traces: trace8 trace16 trace32 trace64

trace8: dijkstra
	OMP_NUM_THREADS=8
	export OMP_NUM_THREADS
	dijkstra_openmp > trace8


trace16: dijkstra
	OMP_NUM_THREADS=16
	export OMP_NUM_THREADS
	dijkstra_openmp > trace16


trace32: dijkstra
	OMP_NUM_THREADS=32
	export OMP_NUM_THREADS
	dijkstra_openmp > trace32


trace64: dijkstra
	OMP_NUM_THREADS=64
	export OMP_NUM_THREADS
	dijkstra_openmp > trace64

matrices: matrix_8 matrix_16 matrix_32 matrix_64

matrix5_8.csv:
	python3 ../matrix_view.py \
	trace8.txt matrix5_8.csv 0 -1 5 8

matrix10_8.csv:
	python3 ../matrix_view.py \
	trace8.txt matrix10_8.csv 0 -1 10 8

matrix20_8.csv:
	python3 ../matrix_view.py \
	trace8.txt matrix20_8.csv 0 -1 20 8

matrix_8: matrix5_8.csv matrix10_8.csv matrix20_8.csv

matrix5_16.csv:
	python3 ../matrix_view.py \
	trace16.txt matrix5_16.csv 0 -1 5 16

matrix10_16.csv:
	python3 ../matrix_view.py \
	trace16.txt matrix10_16.csv 0 -1 10 16

matrix20_16.csv:
	python3 ../matrix_view.py \
	trace16.txt matrix20_16.csv 0 -1 20 16

matrix_16: matrix5_16.csv matrix10_16.csv matrix20_16.csv

matrix5_32.csv:
	python3 ../matrix_view.py \
	trace32.txt matrix5_32.csv 0 -1 5 32

matrix10_32.csv:
	python3 ../matrix_view.py \
	trace32.txt matrix10_32.csv 0 -1 10 32

matrix20_32.csv:
	python3 ../matrix_view.py \
	trace32.txt matrix20_32.csv 0 -1 20 32

matrix_32: matrix5_32.csv matrix10_32.csv matrix20_32.csv

matrix5_64.csv:
	python3 ../matrix_view.py \
	trace64.txt matrix5_64.csv 0 -1 5 64

matrix10_64.csv:
	python3 ../matrix_view.py \
	trace64.txt matrix10_64.csv 0 -1 10 64

matrix20_64.csv:
	python3 ../matrix_view.py \
	trace64.txt matrix20_64.csv 0 -1 20 64

matrix_64: matrix5_64.csv matrix10_64.csv matrix20_64.csv

clean:
	rm -f dijkstra_openmp
