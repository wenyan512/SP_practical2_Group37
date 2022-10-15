# Statistical Programming Practical 2
## Group 37
## Group Memeber: Enyan WU (s2303128), Huiyu WU (s2303136), Xuantao LI (s1822046)

## Project Description
In this project, we are going to use stochastic simulation to investigate a somewhat surprising probability puzzle. 

Set up of the puzzle:
- 2n prisoners each have a unique prisoner number from 1 to 2n.
- The prison contains a room in which there are 2n boxes, each with a unique number from 1 to 2n painted on its lid.
- 2n cards, each printed with a unique number from 1 to 2n, are randomly placed one in each box.
- The prisoners have the task of finding the card with their number on it by opening a maximum on n boxes.
- After each prisoner’s go, the room is returned exactly to its original state and the prisoner is not allowed to communicate with prisoners yet to have their go.
- If all prisoners succeed in finding their number, then they all go free.

There are totally three strategy we are going to apply as follows:
1. The prisoner starts at the box with their number on it, opens it and reads the number on the card: k, say. If k is not their prisoner number, they go to box number k, open it and repeat the process until they have either found the card with their number on it, or opened n boxes without finding it.
2. As strategy 1, but starting from a randomly selected box.
3. They open n boxes at random, checking each card for their number.
