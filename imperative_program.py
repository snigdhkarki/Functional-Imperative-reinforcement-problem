# Library used

import random 
import numpy as np 

# Code for creating the environment of maze

maze = ['w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 
        'w', 0, 1, 2, 'w', 3, 4, 5, 6, 7, 'w', 
        'w', 8, 9, 10, 'w', 11, 12, 13, 14, 15, 'w', 
        'w', 16, 'w', 17, 18, 19, 20, 21, 22, 23, 'w',
        'w', 24, 'w', 25, 26, 27, 'w', 28, 29, 30, 'w', 
        'w', 31, 32, 33, 34, 35, 'w', 36, 37, 38, 'w', 
        'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w', 'w']
states = 39
action_dim = 4
Q = np.zeros((states, action_dim))

def bounce_or_not(old, new):
    if new == 'w':
        return old
    else:
        return new

def new_state(old_state, action):
    position = maze.index(old_state)
    if action == 0:
        return bounce_or_not(maze[position], maze[position - 1])
    elif action == 1:
        return bounce_or_not(maze[position], maze[position + 1])
    elif action == 2:
        return bounce_or_not(maze[position], maze[position - 11])
    else:
        return bounce_or_not(maze[position], maze[position + 11])

def rewardfunc(state, action):
    if (state == 32 and action == 0) or (state == 24 and action == 3):
        return 100000
    return 0

def getaction(state, epsilon):
    if random.random() > (1 - epsilon):
        return random.randint(0, 3)
    else:
        return np.argmax(Q[state])

# Code for the Q learning
    
max_step = 100
no_of_episode = 500
gammaknot = 0.9
alphaknot = 1
alphataper = 0.003
epsilonknot = 1
epsilontaper = 0.0025

for games in range(no_of_episode + 1):
    curr_state = random.randint(0, 38)
    epsilon = epsilonknot / (1 + (games * epsilontaper))
    alpha = alphaknot / (1 + (games * alphataper))
    reward = 0
    
    for step in range(max_step):
        prev_state = curr_state
        action = getaction(prev_state, epsilon)
        state = new_state(prev_state, action)
        reward = rewardfunc(prev_state, action)
        curr_state = state

        old_qsa = Q[prev_state][action]
        max_q_next = np.argmax(Q[curr_state, :])
        Q[prev_state, action] += alpha * (reward + (gammaknot * Q[curr_state, max_q_next]) - Q[prev_state, action])
        
        if reward == 100000:
            break

def pathfinder(Q, state):
    while state != 31:
        action = getaction(state, 0)
        print(action)
        state = new_state(state, action)
    print("end")

print(Q)
pathfinder(Q, 36)
