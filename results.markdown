## Single Player

### Knight's Tour

#### 5/5

MCTS
    90 100 90 100 90

DFS (static)
    90 85 80 96 90

DFS (inverse mobility)
    85 96 90 90 90

#### 30/5

MCTS
    96 100 96 90 96

DFS (static)
    80 80 90 90 90

DFS (inverse mobility)
    80 90 96 85 85

### Pancakes 88

#### 5/5

MCTS
    45 45 45 50 40

DFS (static)
    0 0 0 0 0

DFS (goal distance)
    0 0 0 0 0

#### 30/5

MCTS
    50 45 45 50 50

DFS (static)
    80 80 80 80 80

DFS (goal distance)
    0 0 0 0 0

### 8 Puzzle

#### 5/5

MCTS
    0 0 0 0 0

DFS (static)
    0 0

DFS (goal distance)
    0 0

#### 30/5

MCTS
    0 0

## Two Player

### Connect Four

Clocks: 30 start, 5 play
Player 1: MCTS
Player 2: Minimax
    MCTS 100, Minimax 0
    MCTS 100, Minimax 0
    MCTS 100, Minimax 0
    MCTS 100, Minimax 0
    MCTS 100, Minimax 0

Clocks: 30 start, 5 play
Player 1: Minimax
Player 2: MCTS
    MCTS 100, Minimax 0
    MCTS 100, Minimax 0
    MCTS 100, Minimax 0
    MCTS 100, Minimax 0
    MCTS 0, Minimax 100

### Breakthrough

Clocks: 30 start, 5 play
Player 1: MCTS
Player 2: Minimax
    MCTS 0, Minimax 100
    MCTS 0, Minimax 100
    MCTS 0, Minimax 100
    MCTS 0, Minimax 100
    MCTS 0, Minimax 100

### Bidding Tic Tac Toe (Simultaneous Moves)

Clocks: 30 start, 5 play
Player 1: MCTS
Player 2: Minimax
    MCTS 0, Minimax 100
    MCTS 0, Minimax 100
    MCTS 0, Minimax 100
    MCTS 0, Minimax 100
    MCTS 0, Minimax 100

## Multiplayer Games

### Chinese Checkers 3

Clocks: 30 start, 5 play
Player 1: MCTS
Player 2: Minimax
Player 3: Random
    MCTS 100, Minimax 50, Random 50
    MCTS 100, Minimax 50, Random 25
    MCTS 100, Minimax 50, Random 0
    MCTS 100, Minimax 50, Random 0
    MCTS 100, Minimax 25, Random 0

### Pacman 3p (Simultaneous Moves)

Clocks: 30 start, 5 play
Player 1: MCTS
Player 2: Minimax
Player 3: Random
    MCTS 44, Minimax 100, Random 0
    MCTS 38, Minimax 100, Random 0
    MCTS 26, Minimax 100, Random 0
    MCTS 71, Minimax 0, Random 0
    MCTS 44, Minimax 100, Random 0
