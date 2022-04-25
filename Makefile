
.PHONY: open_matrix dijkstra dijkstra_matrix all clean

dijkstra: dijkstra/dijkstra_openmp.c
	gcc -fopenmp -o dijkstra_openmp dijkstra/dijkstra_openmp.c

dijkstra_matrix: dijkstra/trace.txt matrix_view.py
	dijkstra_openmp > dijkstra/trace72.txt
	python3 matrix_view.py dijkstra/trace.txt dijkstra/matrix5_72.csv 0 -1 5 72
	python3 matrix_view.py dijkstra/trace.txt dijkstra/matrix10_72.csv 0 -1 10 72
	python3 matrix_view.py dijkstra/trace.txt dijkstra/matrix20_72.csv 0 -1 20 72



clean:
	rm -f dijkstra_openmp
