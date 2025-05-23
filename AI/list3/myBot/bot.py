import sys
import random
import math
import socket as pysocket

# --- CONSTANTS ---
BOARD_SIZE = 5;
EMPTY_CELL = 0;
SYMBOLS = {0: ' ', 1: '\033[1;96m⨉\033[0m', 2: '\033[1;93m◯\033[0m'}

# --- Board Utils ---
def createBoard():
    return [[EMPTY_CELL for _ in range(BOARD_SIZE)] for _ in range(BOARD_SIZE)]

def printBoard(board):
    print('\033[90m┏' + '━━━┳' * (BOARD_SIZE - 1) + '━━━┓\033[0m')

    for row in range(BOARD_SIZE):
        row_str = '\033[90m┃\033[0m'

        for col in range(BOARD_SIZE):
            row_str += (f' {SYMBOLS[board[row][col]]} \033[90m┃\033[0m')
        print(row_str)

        if row < BOARD_SIZE - 1:
            print('\033[90m┣' + '━━━╋' * (BOARD_SIZE - 1) + '━━━┫\033[0m')
        else:
            print('\033[90m┗' + '━━━┻' * (BOARD_SIZE - 1) + '━━━┛\033[0m')

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
        # Invalid Move
        return False
    
# If 3 in a line you loseeee
def checkIfForcedLoss(board, playerSymbol):
    def isThreeInLine(segment):
        return segment.count(playerSymbol) == 3 and segment.count(0) == 0

    # Check rows
    for i in range(BOARD_SIZE):
        for j in range(BOARD_SIZE - 2):
            segment = [board[i][j + k] for k in range(3)]
            if isThreeInLine(segment):
                return True

    # Check columns
    for j in range(BOARD_SIZE):
        for i in range(BOARD_SIZE - 2):
            segment = [board[i + k][j] for k in range(3)]
            if isThreeInLine(segment):
                return True

    # Check diagonals (top-left to bottom-right)
    for i in range(BOARD_SIZE - 2):
        for j in range(BOARD_SIZE - 2):
            segment = [board[i + k][j + k] for k in range(3)]
            if isThreeInLine(segment):
                return True

    # Check diagonals (top-right to bottom-left)
    for i in range(BOARD_SIZE - 2):
        for j in range(2, BOARD_SIZE):
            segment = [board[i + k][j - k] for k in range(3)]
            if isThreeInLine(segment):
                return True

    return False

# Basically if 4 in a row
def checkIfWinCondition(board, playerSymbol):
    def isFourInLine(segment):
        return segment.count(playerSymbol) == 4

    # Check rows
    for i in range(BOARD_SIZE):
        for j in range(BOARD_SIZE - 3):
            segment = [board[i][j + k] for k in range(4)]
            if isFourInLine(segment):
                return True

    # Check columns
    for j in range(BOARD_SIZE):
        for i in range(BOARD_SIZE - 3):
            segment = [board[i + k][j] for k in range(4)]
            if isFourInLine(segment):
                return True

    # Check diagonals (top-left to bottom-right)
    for i in range(BOARD_SIZE - 3):
        for j in range(BOARD_SIZE - 3):
            segment = [board[i + k][j + k] for k in range(4)]
            if isFourInLine(segment):
                return True

    # Check diagonals (top-right to bottom-left)
    for i in range(BOARD_SIZE - 3):
        for j in range(3, BOARD_SIZE):
            segment = [board[i + k][j - k] for k in range(4)]
            if isFourInLine(segment):
                return True

    return False

# --- HEURISTICS ---
# Here happens the magic
# Unfortunately it's empty
# (For Now)
# TODO ADD WORTH HEURISTICS!
def heuristics(board, playerSymbol):
    value = random.randint(0,100)
    
    
    # Returns, Heuristic, isWinning, isLosing
    if checkIfWinCondition(board, playerSymbol):
        return value, True, False 
    if checkIfForcedLoss(board, playerSymbol):
        return value, False, True
    
    return value, False, False


# --- MINIMAX ---
# TODO check what to do to make it not PRUNE BRANCHES WITH ONLY POSSIBLE NOT LOSING MOVE
def minimax(playerSymbol, board, depth, alpha, beta, isMaximizingPlayer):
    eval, isWinningMove, isLosingMove = heuristics(board, playerSymbol)
    # Terminal state exit (win/lose)
    if depth == 0 or isWinningMove or isLosingMove:
        if isWinningMove:
            return 999_999
        if isLosingMove:
            return -999_999
        return eval

    if isMaximizingPlayer:
        maxEval = -999_999_999

        for newMove in getValidMoves(board):
            newBoard = makeMove(board, newMove, playerSymbol)
            if newBoard:
                eval = minimax(playerSymbol, newBoard, depth - 1, alpha, beta, False)
                maxEval = max(maxEval, eval)
                alpha = max(alpha, eval)
                if beta <= alpha:
                    break
        return maxEval
    
    else:
        minEval = 999_999_999
        opponent = 1 if playerSymbol == 2 else 2

        for newMove in getValidMoves(board):
            newBoard = makeMove(board, newMove, opponent)
            if newBoard:
                eval = minimax(playerSymbol, newBoard, depth - 1, alpha, beta, True)
                minEval = min(minEval, eval)
                beta = min(beta, eval)
                if beta <= alpha:
                    break
        return minEval
    

# --- MINIMAX Special ---
# TODO to check terminal states (αβ returned instant loss)
# Somtething's here fishy....
def minimaxNoPrune(playerSymbol, board, depth, isMaximizingPlayer):
    eval, isWinningMove, isLosingMove = heuristics(board, playerSymbol)
    # Terminal state exit (win/lose)
    if depth == 0 or isWinningMove or isLosingMove:
        if isWinningMove:
            return 999_999
        if isLosingMove:
            return -999_999
        return eval

    if isMaximizingPlayer:
        maxEval = -999_999_999

        for newMove in getValidMoves(board):
            newBoard = makeMove(board, newMove, playerSymbol)
            if newBoard:
                eval = minimaxNoPrune(playerSymbol, newBoard, depth - 1, False)
                maxEval = max(maxEval, eval)
                
        if maxEval == -999_999_999:
            return eval
        return maxEval

    else:
        minEval = 999_999_999
        opponent = 1 if playerSymbol == 2 else 2

        for newMove in getValidMoves(board):
            newBoard = makeMove(board, newMove, opponent)
            if newBoard:
                eval = minimaxNoPrune(playerSymbol, newBoard, depth - 1, True)
                minEval = min(minEval, eval)
                
        if minEval == 999_999_999:
            return eval
        return minEval


# --- SERVER STUFF ---
def serverConnect(ip, port):
    try:
        socket = pysocket.socket(pysocket.AF_INET, pysocket.SOCK_STREAM)
        socket.connect((ip, port))
        print(f"Succesfully connected to {ip}:{port}")
        return socket
    
    except pysocket.error as e:
        print(f"Error connecting to server: {e}")
        sys.exit(1)

def sendMessage(socket, message):
    try:
        print(f"Sending message [{message}]")
        socket.sendall(message.encode('utf-8'))
    except pysocket.error as e:
        print(f"Error while sending: {e}")
        sys.exit(1)

def receiveMessage(socket):
    try:
        data = socket.recv(1024)
        if not data:
            print("Server stream is empty, no more data")
            return None
        message = data.decode('utf-8').strip()
        print(f"Received message [{message}]")
        return message
    
    except pysocket.error as e:
        print(f"Error receiving message: {e}")
        return None
    
    except UnicodeDecodeError as e:
        print(f"Error decoding from UTF-8: {e} (Received Data: {data})")
        return None



# --- Main Bot Loop ---
# THIS NEEDS FIXING
def mainLoop(ip, port, player_number, player_name, depth):
    CURRENT_BOARD = createBoard()
    s = serverConnect(ip, port)
    my_symbol = int(player_number)
    opponent_symbol = 1 if my_symbol == 2 else 2

    while True:
        msg = receiveMessage(s)
        if msg is None:
            break

        if msg.startswith("700"):
            sendMessage(s, f"{player_number} {player_name}")

        elif msg.startswith("600"):
            if my_symbol == 1:
                # It's our turn
                best_score = -math.inf
                best_move = None
                for move in getValidMoves(CURRENT_BOARD):
                    tempBoard = [row[:] for row in CURRENT_BOARD]
                    makeMove(tempBoard, move, my_symbol)

                    score = minimax(my_symbol, tempBoard, depth, -math.inf, math.inf, True)
                    if score > best_score:
                        best_score = score
                        best_move = move
                if best_score <= -999_999:
                    print(f"got best score: {best_score}")
                    best_score = -math.inf
                    best_move = None
                    for move in getValidMoves(CURRENT_BOARD):
                        tempBoard = [row[:] for row in CURRENT_BOARD]
                        makeMove(tempBoard, move, my_symbol)
                        print(f"Avaliable move: {move}")
                        score = minimaxNoPrune(my_symbol, tempBoard, depth, True) #TODO Here change to false?
                        if score >= best_score:
                            best_score = score
                            best_move = move
                if best_move:
                    move_str = f"{best_move[0]+1}{best_move[1]+1}"
                    sendMessage(s, move_str)
                    makeMove(CURRENT_BOARD, best_move, my_symbol)

                printBoard(CURRENT_BOARD)

        elif msg.isdigit() and len(msg) == 2:
            # Opponent's move
            row = int(msg[0]) - 1
            col = int(msg[1]) - 1
            makeMove(CURRENT_BOARD , (row, col), opponent_symbol)
            printBoard(CURRENT_BOARD)
            # It's our turn
            best_score = -math.inf
            best_move = None
            for move in getValidMoves(CURRENT_BOARD):
                tempBoard = [row[:] for row in CURRENT_BOARD]
                makeMove(tempBoard, move, my_symbol)

                score = minimax(my_symbol, tempBoard, depth, -math.inf, math.inf, True)
                if score > best_score:
                    best_score = score
                    best_move = move
            if best_score <= -999_999:
                print(f"got best score: {best_score}")
                best_score = -math.inf
                best_move = None
                for move in getValidMoves(CURRENT_BOARD):
                    tempBoard = [row[:] for row in CURRENT_BOARD]
                    makeMove(tempBoard, move, my_symbol)
                    print(f"Avaliable move: {move}")
                    score = minimaxNoPrune(my_symbol, tempBoard, depth, True) #TODO HERE CHANGE TO FALSE?
                    print(f"score from noprune: {score}")
                    if score >= best_score:
                        best_score = score
                        best_move = move
            if best_move:
                move_str = f"{best_move[0]+1}{best_move[1]+1}"
                sendMessage(s, move_str)
                makeMove(CURRENT_BOARD, best_move, my_symbol)

            printBoard(CURRENT_BOARD)

        elif msg.startswith("1"):
            print(f"\nYou win! (You are {SYMBOLS[my_symbol]})\n    Final Board:")
            row = int(msg[1]) - 1
            col = int(msg[2]) - 1
            makeMove(CURRENT_BOARD , (row, col), opponent_symbol)
            printBoard(CURRENT_BOARD)
            break
        elif msg.startswith("2"):
            print(f"\nYou lose! (You are {SYMBOLS[my_symbol]})\n    Final Board:")
            row = int(msg[1]) - 1
            col = int(msg[2]) - 1
            makeMove(CURRENT_BOARD , (row, col), opponent_symbol)
            printBoard(CURRENT_BOARD)
            break
        elif msg.startswith("3"):
            print(f"Draw. (You are {SYMBOLS[my_symbol]})\n    Final Board:")
            row = int(msg[1]) - 1
            col = int(msg[2]) - 1
            makeMove(CURRENT_BOARD , (row, col), opponent_symbol)
            printBoard(CURRENT_BOARD)
            break
        elif msg.startswith("400"):
            print("Win due to opponent's error.")
            break
        elif msg.startswith("500"):
            print("Loss due to your error.")
            break

    s.close()


#--- Run The Bot ---
if __name__ == "__main__":
    import sys
    if len(sys.argv) != 6:
        print("Usage: python client.py <ip> <port> <player_number> <player_name> <depth>")
        sys.exit(1)

    ip = sys.argv[1]
    port = int(sys.argv[2])
    player_number = int(sys.argv[3])
    player_name = sys.argv[4]
    depth = int(sys.argv[5])

    mainLoop(ip, port, player_number, player_name, depth)
