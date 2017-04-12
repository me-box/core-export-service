import numpy as np
import matplotlib.pyplot as plt


dir = "c1000_p1000"
f = open(dir + "/queue_count", "r")

lines = [line.split() for line in f.readlines()]

time = []
count = []

for line in lines:
    time.append(float(line[0]) / 1000.)
    count.append(line[1])

t_min = min(time)
t_max = max(time)

plt.axis([t_min, t_max, 0, 1000])
plt.plot(time, count)
plt.show()




