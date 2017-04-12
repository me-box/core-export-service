import numpy as np
import matplotlib.pyplot as plt


dir = "c1000_p1000"
f = open(dir + "/throughput", "r")

lines = [line.split() for line in f.readlines()]

time = []
bytes_count = []

for line in lines:
    time.append(float(line[0]))
    bytes_count.append(int(line[1]))

prev_time = time[0]
prev_count = bytes_count[0]
speed = []

for t,c in zip(time[1:], bytes_count[1:]):
    s = (c - prev_count) / (t - prev_time) * 1000.
    speed.append(s)
    prev_time = t
    prev_count = c

plt.plot(time[1:], speed)
plt.show()
