
.PHONY: open_matrix dijkstra dijkstra_matrix all clean

dijkstra: dijkstra/dijkstra_openmp.c
	export OMP_NUM_THREADS=64
	gcc -fopenmp -o dijkstra_openmp dijkstra/dijkstra_openmp.c

dijkstra_matrix: dijkstra/trace.txt matrix_view.py
	python3 matrix_view.py dijkstra/trace.txt dijkstra/matrix.csv 0 -1 10 72

open_matrix:
	column dijkstra/matrix.csv -t -s ","

clean:
	rm -f dijkstra_openmp
