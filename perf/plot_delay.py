import numpy as np
import matplotlib.pyplot as plt


dir = "c1000_p1000"
f = open(dir + "/request_delay", "r")

lines = [line.split() for line in f.readlines()]

for line in lines:
    line[1] = float(line[1]) / 1000.
    line[2] = float(line[2]) / 1000.
    line[3] = float(line[3]) / 1000.

lines.sort(key=lambda x:x[1])

time = []
process = []
finished = []

for line in lines:
    time.append(line[1])
    process.append(line[2] - line[1])
    finished.append(line[3] - line[1])

t_min = lines[0][1]
t_max = lines[-1][1]

delay_max = 0.
for line in lines:
    if line[-1] - line[1] > delay_max:
        delay_max = line[-1] - line[1]


plt.axis([t_min, t_max, 0, delay_max * 1.2])
plt.plot(time, process, time, finished)
plt.show()
