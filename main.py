import random
import os

list = []

for i in range(10000):
    if not os.path.exists('/home/zepedro/Documents/PFL/Projeto1/data.txt'):
        with open('/home/zepedro/Documents/PFL/Projeto1/data.txt', 'w') as file:
            file.write('[')

    with open('/home/zepedro/Documents/PFL/Projeto1/data.txt', 'a') as file:
        file.write(f'("{i}", "{i+1}", 1),')

    list.append((i, i+1, 1))

with open('/home/zepedro/Documents/PFL/Projeto1/data.txt', 'a') as file:
    file.write(']')
