"""
1.

2
"""
import matplotlib as mp
import matplotlib.pyplot as plt
from sklearn import datasets
from math import sqrt
import numpy as np
# import iris dataset
iris = datasets.load_iris()
# colors
colors = mp.colors.ListedColormap(['r', 'g', 'b'], 'indexed')
# iris petal
petals = iris.data[:, 2:]
# iris classes
y = iris.target
newclasscolor = {0: 'r', 1: 'g', 2: 'b'}
# create figure object
plt.figure(figsize=(10, 6))
# draw plot
# labels
plt.xlabel('Petal length')
plt.ylabel('Petal width')
# show plot


def euclidianDistance(u, v):
    """Euclidian distances function."""
    return sqrt((u[0] - v[0]) ** 2 + (u[1]-v[1]) ** 2)


def knn1(z):
    """1NN algorithm."""
    irisDist = [euclidianDistance(petals[i], z) for i in range(0, len(petals))]
    miniris = min(irisDist)
    min_index = [i for i in range(len(irisDist)) if irisDist[i] == miniris]
    return y[min_index[0]]


for i in np.arange(1.0, 7.0, 0.1):
    for j in np.arange(-1.0, 3.0, 0.1):
        z = [i, j]
        class_object = knn1(z)
        plt.scatter(z[0], z[1], c=newclasscolor[class_object], marker='.')

path = '/home/aearondir/Programming/ml_labs/'
plt.savefig('{}.{}'.format(path+'pic_1_4_2', 'png'), fmt='png')
