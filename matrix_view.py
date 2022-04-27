import matplotlib.pyplot as plt
import numpy as np
import sys

f_name = sys.argv[1]

f = np.genfromtxt(f_name, delimiter=",")

plt.imshow(f, cmap='gray')
plt.show()

