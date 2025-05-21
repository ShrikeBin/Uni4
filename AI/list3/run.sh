# --- CONFIG ---
IP="127.0.0.1"
PORT="5000"
PLAYER1=1
PLAYER2=2
NICK1="BOT_1"
NICK2="BOT_2"
DEPTH=2

# --- BUILD SERVER ---
echo "[+] Building server..."
cd ./server || exit 1
make clean
make || { echo "[-] Build failed"; exit 1; }
cd ..

# --- START SERVER ---
echo "[+] Starting server on $IP:$PORT..."
./server/game_server $IP $PORT &
SERVER_PID=$!
sleep 1

# --- START FIRST BOT ---
echo "[+] Starting first bot ($NICK1)..."
python3 ./myBot/bot.py $IP $PORT $PLAYER1 $NICK1 $DEPTH &
BOT1_PID=$!
sleep 1

# --- START SECOND BOT ---
echo "[+] Starting second bot ($NICK2)..."
python3 ./myBot/bot.py $IP $PORT $PLAYER2 $NICK2 $DEPTH &
BOT2_PID=$!

# --- WAIT FOR BOTS TO FINISH ---
wait $BOT1_PID
wait $BOT2_PID

# --- CLEANUP ---
echo "[+] Game finished. Shutting down server..."
kill $SERVER_PID 2>/dev/null
wait $SERVER_PID 2>/dev/null

echo "[+] Done."
