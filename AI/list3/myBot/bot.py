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
    random.shuffle(moves) # add some NOISE, because the bot tends to play from the top left corner
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
    def evalBoard(board, playerSymbol):
        opponentSymbol = 1 if playerSymbol == 2 else 2
        count_free = 0
        count_blocked = 0
        diag_two_in_a_row = 0
        continues_after_gap = 0

        directions = [(0,1), (1,0), (1,1), (1,-1)]  # right, down, diag-right, diag-left

        visited_pairs = set()  # avoid double-counting

        for i in range(BOARD_SIZE):
            for j in range(BOARD_SIZE):
                if board[i][j] == playerSymbol:
                    for dx, dy in directions:
                        x1, y1 = i + dx, j + dy

                        if (0 <= x1 < BOARD_SIZE and 0 <= y1 < BOARD_SIZE and 
                            board[x1][y1] == playerSymbol):

                            pair_key = tuple(sorted([(i, j), (x1, y1)]))
                            if pair_key in visited_pairs:
                                continue
                            visited_pairs.add(pair_key)

                            blocked = False

                            # Check forward block
                            x2, y2 = i + 2*dx, j + 2*dy
                            if 0 <= x2 < BOARD_SIZE and 0 <= y2 < BOARD_SIZE:
                                if board[x2][y2] == opponentSymbol:
                                    blocked = True

                            # Check backward block
                            bx, by = i - dx, j - dy
                            if 0 <= bx < BOARD_SIZE and 0 <= by < BOARD_SIZE:
                                if board[bx][by] == opponentSymbol:
                                    blocked = True

                            if blocked:
                                count_blocked += 1
                            else:
                                count_free += 1

                            # Specifically count diagonals
                            if dx != 0 and dy != 0:
                                diag_two_in_a_row += 1

                            # Check continuation after gap (forward)
                            gap_x, gap_y = i + 2*dx, j + 2*dy
                            after_gap_x, after_gap_y = i + 3*dx, j + 3*dy
                            if (0 <= gap_x < BOARD_SIZE and 0 <= gap_y < BOARD_SIZE and board[gap_x][gap_y] == EMPTY_CELL and
                                0 <= after_gap_x < BOARD_SIZE and 0 <= after_gap_y < BOARD_SIZE and board[after_gap_x][after_gap_y] == playerSymbol):
                                continues_after_gap += 1

                            # Check continuation after gap (backward)
                            gap_bx, gap_by = i - dx, j - dy
                            after_gap_bx, after_gap_by = i - 2*dx, j - 2*dy
                            if (0 <= gap_bx < BOARD_SIZE and 0 <= gap_by < BOARD_SIZE and board[gap_bx][gap_by] == EMPTY_CELL and
                                0 <= after_gap_bx < BOARD_SIZE and 0 <= after_gap_by < BOARD_SIZE and board[after_gap_bx][after_gap_by] == playerSymbol):
                                continues_after_gap += 1
        evaluation = 0
        evaluation += 100*count_free
        evaluation += 5*count_blocked
        evaluation += 100*diag_two_in_a_row
        evaluation += 1000*continues_after_gap
        return evaluation
        
    # Returns, Heuristic, isWinning, isLosing
    if checkIfWinCondition(board, playerSymbol):
        value = 999_999
        return value, True, False
    if checkIfForcedLoss(board, playerSymbol):
        value = -999_999
        return value, False, True
    
    opponent = 1 if playerSymbol == 2 else 2
    value = evalBoard(board, playerSymbol) - evalBoard(board, opponent)
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
                # best_score = -math.inf
                # best_move = None
                
                best_move = (random.randint(1, BOARD_SIZE-2), random.randint(1, BOARD_SIZE-2)) 
                # First move is random in the center
                
                # for move in getValidMoves(CURRENT_BOARD):
                #     tempBoard = [row[:] for row in CURRENT_BOARD]
                #     makeMove(tempBoard, move, my_symbol)

                #     score = minimax(my_symbol, tempBoard, depth, -math.inf, math.inf, True)
                #     if score > best_score:
                #         best_score = score
                #         best_move = move
                # if best_score <= -999_999:
                #     best_score = -math.inf
                #     best_move = None
                #     for move in getValidMoves(CURRENT_BOARD):
                #         tempBoard = [row[:] for row in CURRENT_BOARD]
                #         makeMove(tempBoard, move, my_symbol)
                #         score = minimaxNoPrune(my_symbol, tempBoard, depth, True) #TODO Here change to false?
                #         if score >= best_score:
                #             best_score = score
                #             best_move = move
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
                best_score = -math.inf
                best_move = None
                for move in getValidMoves(CURRENT_BOARD):
                    tempBoard = [row[:] for row in CURRENT_BOARD]
                    makeMove(tempBoard, move, my_symbol)
                    score = minimaxNoPrune(my_symbol, tempBoard, depth, True) #TODO HERE CHANGE TO FALSE?
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
    if not (len(sys.argv) == 6 or (len(sys.argv) == 2 and sys.argv[1] == "-1")):
        print("Usage: python bot.py <ip> <port> <player_number> <player_name> <depth>")
        print("Or for debug mode: python bot.py -1")
        sys.exit(1)

    if sys.argv[1]!= "-1":
        ip = sys.argv[1]
        port = int(sys.argv[2])
        player_number = int(sys.argv[3])
        player_name = sys.argv[4]
        depth = int(sys.argv[5])

        mainLoop(ip, port, player_number, player_name, depth)
        
    else:
        print("Debug mode:")
        board = createBoard()
        printBoard(board)
        print("You can now test your heuristics and minimax function here.")
        print("Use `move` to make a move.")
        print("Use `print` to print the board.")
        print("Use `heuristics` to get heuristic value.")
        print("Use `minimax` to run minimax with pruning.")
        print("Use `minimax_no_prune` to run minimax without pruning.")
        print("Use `exit` or `quit` to exit debug mode.")
        while True:
            try:
                commmand = input("Enter command : ").strip().lower()
                if commmand == "move":
                    row = int(input("Enter row (0-4): ")[0])
                    col = int(input("Enter column (0-4): ")[0])
                    playerSymbol = int(input("Enter player symbol (1 or 2): "))
                    if row < 0 or row >= BOARD_SIZE or col < 0 or col >= BOARD_SIZE:
                        print("Invalid move. Try again.")
                        continue
                    board = makeMove(board, (row, col), playerSymbol)
                    printBoard(board)
                elif commmand == "print":
                    printBoard(board)
                elif commmand == "heuristics":
                    playerSymbol = int(input("Enter player symbol (1 or 2): "))
                    value, isWinning, isLosing = heuristics(board, playerSymbol)
                    print(f"Heuristic value: {value}, Winning: {isWinning}, Losing: {isLosing}")
                elif commmand == "minimax":
                    depth = int(input("Enter depth for minimax: "))
                    playerSymbol = int(input("Enter player symbol (1 or 2): "))
                    best_score = minimax(playerSymbol, board, depth, -math.inf, math.inf, True)
                    print(f"Best score from minimax: {best_score}")
                elif commmand == "minimax_no_prune":
                    depth = int(input("Enter depth for minimax no prune: "))
                    playerSymbol = int(input("Enter player symbol (1 or 2): "))
                    best_score = minimaxNoPrune(playerSymbol, board, depth, True)
                    print(f"Best score from minimax no prune: {best_score}")
                elif commmand == "exit" or commmand == "quit":
                    print("Exiting debug mode.")
                    break
                else:
                    print("Unknown command")
                    continue
            except ValueError:
                print("Invalid input. Please enter numbers only.")
            except Exception as e:
                print(f"An error occurred: {e}")
        print("Exiting debug mode.")
        sys.exit(0) 