import matplotlib.pyplot as plt
import numpy as np
import sys

f_name = sys.argv[1]

f = np.genfromtxt(f_name, delimiter=",")

without_headers = np.delete(np.delete(f, 0, 0), 0, 1)

plt.imshow(without_headers, cmap='binary')
plt.show()

