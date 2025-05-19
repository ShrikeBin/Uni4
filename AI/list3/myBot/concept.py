import sys
import random
import math
import socket

# --- CONSTANTS ---
BOARD_SIZE = 5;
EMPTY_CELL = 0;
SYMBOLS = {0: ' ', 1: '⨉', 2: '◯'}

# --- Board Utils ---
def createBoard():
    return [[EMPTY_CELL for _ in range(BOARD_SIZE)] for _ in range(BOARD_SIZE)]

def printBoard(board):
    print('┏' + '━━━┳' * (BOARD_SIZE - 1) + '━━━┓')

    for row in range(BOARD_SIZE):
        row_str = '┃'

        for col in range(BOARD_SIZE):
            row_str += (f' {SYMBOLS[board[row][col]]} ┃')
        print(row_str)

        if row < BOARD_SIZE - 1:
            print('┣' + '━━━╋' * (BOARD_SIZE - 1) + '━━━┫')
        else:
            print('┗' + '━━━┻' * (BOARD_SIZE - 1) + '━━━┛')

def reverseBoard(inBoard):
    outBoard = inBoard
    for i in outBoard:
        for j in outBoard:
            if outBoard[i][j] == 1:
                outBoard[i][j] = 2
            elif outBoard[i][j] == 2:
                outBoard[i][j] == 1

    return outBoard


# --- Move Utils ---
def getValidMoves(board):
    moves = []
    for i in range(BOARD_SIZE):
        for j in range(BOARD_SIZE):
            if board[i][j] == EMPTY_CELL:
                moves.append((i,j))
    return moves

def makeMove(board, move, playerSymbol):
    if move == None:
        print("Empty Move")
        return board;

    i, j = move

    if(i >= 0 and i < BOARD_SIZE and j >= 0 and j < BOARD_SIZE and board[i][j] == EMPTY_CELL):
        newBoard = board
        newBoard[i][j] = playerSymbol
        return newBoard
    else:
        print(f"Invalid move for ({i},{j})")
        return board
    
# If 3 in a line you loseeee
def checkIfForcedLoss(board, playerSymbol):
    threeInRowFound = 0

    # Rows
    for i in range(BOARD_SIZE):
        for j in range(BOARD_SIZE - 2):
            segment = [board[i][j+k] for k in range(3)]
            # Aaand continue but not today ig..

    # Columns


    # Diagonal

    return threeInRowFound > 0
    

# --- HEURISTICS ---
def heuristics(board, playerSymbol):
    # placeholder, random for now
    return random.randint(-100, 100)


# --- MINIMAX ---
def minimax(playerSymbol, board, depth, alpha, beta, isMaximizingPlayer):
    # I sure hope I ain't gonna be looking through 25!/15! states....
    # I'm not sure if that doesnt unnecessarilly generate states...
    # Ideally order moves from best to worst maybe?... idk
    # I'm gonna go watch https://youtu.be/l-hh51ncgDI
    if depth == 0:
        return heuristics(playerSymbol, board)
    
    if isMaximizingPlayer:
        minEval = -999_999_999

        for newBoard in getValidMoves(board):
            eval = minimax(playerSymbol, newBoard, depth - 1, alpha, beta, False)
            maxEval = max(maxEval, eval)
            alpha = max(alpha, eval)
            if beta <= alpha:
                break

        return maxEval
    
    else:
        minEval = +999_999_999

        for newBoard in getValidMoves(board):
            eval = minimax(playerSymbol, newBoard, depth - 1, alpha, beta, True)
            minEval = min(minEval, eval)
            beta = min(beta, eval)
            if beta <= alpha:
                break
        
        # But shouldn't I be returning the best board (move) for this max depth?
        return minEval
    

# --- SERVER STUFF ---
def serverConnect(ip, port):
    try:
        socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        socket.connect((ip, port))
        print(f"Succesfully connected to {ip}:{port}")
        return socket
    
    except socket.error as e:
        print(f"Error connecting to server: {e}")
        sys.exit(1)

def sendMessage(socket, message):
    try:
        print(f"Sending: {message}")
        socket.sendall(message.encode('utf-8'))
    except socket.error as e:
        print(f"Error while sending: {e}")
        sys.exit(1)

def receiveMessage(socket):
    try:
        data = socket.recv(1024)
        if not data:
            print("Serwer stream is empty, no more data")
            return None
        message = data.decode('utf-8').strip()
        print(f"Received: {message}")
        return message
    
    except socket.error as e:
        print(f"Error receiving message: {e}")
        return None
    
    except UnicodeDecodeError as e:
        print(f"Error decoding from UTF-8: {e} (Received Data: {data})")
        return None