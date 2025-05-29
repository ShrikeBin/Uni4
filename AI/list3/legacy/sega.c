/****************************
Implementacja klienta gry z algorytmem minimax
Autor: barteksega1
Data: 2025-05-28
Wersja: 2.0 - z ulepszeniami przeciw samobójstwu
****************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <time.h>
#include <limits.h>

#include "./board.h"

// Struktura przechowująca wynik minimax
typedef struct {
    int score;
    int move;
} MinimaxResult;

// Globalne zmienne
int my_player;
int opponent_player;
int search_depth;

// Kopiuje planszę
void copy_board(int source[5][5], int dest[5][5]) {
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            dest[i][j] = source[i][j];
        }
    }
}

/**
 * SPRAWDZA CZY DANY RUCH SPOWODUJE PRZEGRANIE DLA GRACZA
 * 
 * Funkcja symuluje wykonanie ruchu i sprawdza czy gracz będzie miał
 * 3 symbole w linii BEZ równoczesnego posiadania 4 symboli w linii.
 * 
 * @param board - aktualna plansza
 * @param move - ruch do sprawdzenia (format 11-55)
 * @param player - gracz wykonujący ruch
 * @return true jeśli ruch prowadzi do przegranej
 */
bool would_lose_with_move(int board[5][5], int move, int player) {
    int row = (move / 10) - 1;
    int col = (move % 10) - 1;
    
    // Walidacja ruchu
    if (row < 0 || row > 4 || col < 0 || col > 4 || board[row][col] != 0) {
        return true; // Nieprawidłowy ruch = przegrana
    }
    
    // Stwórz kopię planszy z tym ruchem
    int temp_board[5][5];
    copy_board(board, temp_board);
    temp_board[row][col] = player;
    
    // Sprawdź czy teraz gracz ma 3 w linii
    bool has_three = false;
    for (int i = 0; i < 48; i++) {
        int player_count = 0;
        for (int j = 0; j < 3; j++) {
            int check_row = lose[i][j][0];
            int check_col = lose[i][j][1];
            if (temp_board[check_row][check_col] == player) {
                player_count++;
            }
        }
        if (player_count == 3) {
            has_three = true;
            break;
        }
    }
    
    if (!has_three) return false; // Nie ma 3 w linii, więc OK
    
    // Ma 3 w linii - sprawdź czy równocześnie ma 4 w linii (wtedy wygrywa)
    for (int i = 0; i < 28; i++) {
        int win_count = 0;
        for (int j = 0; j < 4; j++) {
            int win_row = win[i][j][0];
            int win_col = win[i][j][1];
            if (temp_board[win_row][win_col] == player) {
                win_count++;
            }
        }
        if (win_count == 4) {
            return false; // Ma 4 w linii, więc wygrywa mimo 3 w linii
        }
    }
    
    return true; // Ma 3 w linii bez 4 - przegrywa
}

/**
 * SPRAWDZA CZY RUCH DAJE NATYCHMIASTOWE WYGRANIE
 * 
 * @param board - aktualna plansza
 * @param move - ruch do sprawdzenia
 * @param player - gracz wykonujący ruch
 * @return true jeśli ruch daje wygranie (4 w linii)
 */
bool would_win_with_move(int board[5][5], int move, int player) {
    int row = (move / 10) - 1;
    int col = (move % 10) - 1;
    
    if (row < 0 || row > 4 || col < 0 || col > 4 || board[row][col] != 0) {
        return false;
    }
    
    int temp_board[5][5];
    copy_board(board, temp_board);
    temp_board[row][col] = player;
    
    // Sprawdź czy ma 4 w linii
    for (int i = 0; i < 28; i++) {
        int player_count = 0;
        for (int j = 0; j < 4; j++) {
            int check_row = win[i][j][0];
            int check_col = win[i][j][1];
            if (temp_board[check_row][check_col] == player) {
                player_count++;
            }
        }
        if (player_count == 4) return true;
    }
    return false;
}

/**
 * SPRAWDZA CZY RUCH BLOKUJE PRZECIWNIKA PRZED WYGRANIEM
 * 
 * @param board - aktualna plansza
 * @param move - ruch do sprawdzenia
 * @param player - gracz wykonujący ruch
 * @return true jeśli ruch blokuje wygranie przeciwnika
 */
bool blocks_opponent_win(int board[5][5], int move, int player) {
    int opponent = (player == 1) ? 2 : 1;
    int row = (move / 10) - 1;
    int col = (move % 10) - 1;
    
    if (row < 0 || row > 4 || col < 0 || col > 4 || board[row][col] != 0) {
        return false;
    }
    
    // Sprawdź czy w tym miejscu przeciwnik mógłby wygrać
    int temp_board[5][5];
    copy_board(board, temp_board);
    temp_board[row][col] = opponent;
    
    for (int i = 0; i < 28; i++) {
        int opponent_count = 0;
        for (int j = 0; j < 4; j++) {
            int check_row = win[i][j][0];
            int check_col = win[i][j][1];
            if (temp_board[check_row][check_col] == opponent) {
                opponent_count++;
            }
        }
        if (opponent_count == 4) return true;
    }
    return false;
}

/**
 * FUNKCJA HEURYSTYCZNA - SZCZEGÓŁOWY OPIS STRATEGII
 * 
 * Hierarchia priorytetów:
 * 1. BEZPIECZEŃSTWO - unikanie samobójstw (3 w linii bez 4)
 * 2. WYGRANE - szukanie możliwości 4 w linii
 * 3. OBRONA - blokowanie przeciwnika
 * 4. ROZWÓJ - budowanie potencjału
 * 
 * WARTOŚCI ZWRACANE:
 * +50000  - Gracz już wygrał (4 w linii)
 * -50000  - Przeciwnik już wygrał
 * +30000  - Przeciwnik przegrał (3 w linii bez 4)
 * -30000  - Gracz przegrał (3 w linii bez 4)
 * 
 * WZORCE WYGRYWAJĄCE (+10 do +1000):
 * - 3 nasze symbole w linii na 4 : +1000
 * - 2 nasze symbole w linii na 4 : +100
 * - 1 nasz symbol w linii na 4: +10
 * 
 * BLOKOWANIE PRZECIWNIKA (+80 do +800):
 * - Przeciwnik ma 3 w linii na 4: +800 (trzeba go zablokowac ino roz)
 * - Przeciwnik ma 2 w linii na 4: +80
 * 
 * KONTROLA CENTRUM (+10 do +25):
 * - Środek planszy (2,2): +25
 * - Pozycje wokół środka: +15
 * - Narożniki środkowego kwadratu: +10
 * 
 * KARY BEZPIECZEŃSTWA (-30 do -50):
 * - Potencjalne 3 w linii (2 nasze + 1 puste): -50
 * - Zagrożenia przeciwnika: -30
 */
int evaluate_position(int board[5][5], int player) {
    int score = 0;
    int opponent = (player == 1) ? 2 : 1;
    
    // NAJWYŻSZY PRIORYTET: Sprawdź stany terminalne
    if (winCheck(player)) return 50000;      // Już wygrał
    if (winCheck(opponent)) return -50000;   // Już przegrał
    if (loseCheck(player)) return -30000;    // Przegrał przez 3 w linii
    if (loseCheck(opponent)) return 30000;   // Przeciwnik przegrał przez 3 w linii
    
    // 1. ANALIZA WZORCÓW WYGRYWAJĄCYCH (4 w linii)
    for (int i = 0; i < 28; i++) {
        int player_count = 0;
        int opponent_count = 0;
        int empty_count = 0;
        
        for (int j = 0; j < 4; j++) {
            int row = win[i][j][0];
            int col = win[i][j][1];
            if (board[row][col] == player) player_count++;
            else if (board[row][col] == opponent) opponent_count++;
            else empty_count++;
        }
        
        // Nasze potencjalne linie wygrywające (tylko nasze symbole + puste)
        if (opponent_count == 0) {
            if (player_count == 3) score += 1000;      // Bardzo blisko wygranej
            else if (player_count == 2) score += 100;  // Dobra pozycja
            else if (player_count == 1) score += 10;   // Początek linii
        }
        
        // Blokowanie przeciwnika (tylko jego symbole + puste)
        if (player_count == 0) {
            if (opponent_count == 3) score += 800;     // KRYTYCZNE: blokuj wygraną
            else if (opponent_count == 2) score += 80; // Blokuj zagrożenie
        }
    }
    
    // 2. ANALIZA WZORCÓW PRZEGRYWAJĄCYCH (3 w linii) - OSTROŻNIE!
    for (int i = 0; i < 48; i++) {
        int player_count = 0;
        int opponent_count = 0;
        
        for (int j = 0; j < 3; j++) {
            int row = lose[i][j][0];
            int col = lose[i][j][1];
            if (board[row][col] == player) player_count++;
            else if (board[row][col] == opponent) opponent_count++;
        }
        
        // OSTRZEŻENIE: potencjalne 3 w linii (2 nasze + 1 puste)
        if (opponent_count == 0 && player_count == 2) {
            score -= 50; // Uważaj na trzeci symbol!
        }
        
        // Przeciwnik ma zagrożenie 3 w linii
        if (player_count == 0 && opponent_count == 2) {
            score -= 30; // Może być niebezpieczny
        }
    }
    
    // 3. KONTROLA CENTRUM PLANSZY
    // Pozycje środkowe dają więcej możliwości tworzenia linii
    int center_positions[][3] = {
        {2, 2, 25}, // Środek - najważniejszy
        {1, 2, 15}, {2, 1, 15}, {3, 2, 15}, {2, 3, 15}, // Krzyż wokół środka
        {1, 1, 10}, {1, 3, 10}, {3, 1, 10}, {3, 3, 10}  // Narożniki środkowego kwadratu
    };
    
    for (int i = 0; i < 9; i++) {
        int row = center_positions[i][0];
        int col = center_positions[i][1];
        int bonus = center_positions[i][2];
        
        if (board[row][col] == player) {
            score += bonus;
        }
    }
    
    return score;
}

// Sprawdza czy gra się skończyła
bool is_terminal_state(int board[5][5]) {
    // Sprawdź wygrane/przegrane dla obu graczy
    if (winCheck(1) || winCheck(2) || loseCheck(1) || loseCheck(2)) {
        return true;
    }
    
    // Sprawdź czy plansza pełna (remis)
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            if (board[i][j] == 0) return false;
        }
    }
    return true;
}

// Pobiera listę dostępnych ruchów
void get_available_moves(int board[5][5], int moves[], int* count) {
    *count = 0;
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            if (board[i][j] == 0) {
                moves[(*count)++] = (i + 1) * 10 + (j + 1);
            }
        }
    }
}

/**
 * ALGORYTM MINIMAX Z ALFA-BETA CIĘCIAMI
 * 
 * Implementuje klasyczny algorytm minimax z optymalizacją alfa-beta:
 * 
 * ZASADA DZIAŁANIA:
 * - Gracz maksymalizujący (my_player) chce maksymalny wynik
 * - Gracz minimalizujący (opponent) chce minimalny wynik
 * - Alpha: najlepsza wartość znaleziona dla gracza maksymalizującego
 * - Beta: najlepsza wartość znaleziona dla gracza minimalizującego
 * - Cięcie: gdy alpha >= beta, można przerwać przeszukiwanie
 * 
 * @param board - aktualna plansza
 * @param depth - pozostała głębokość przeszukiwania
 * @param alpha - najlepsza wartość dla gracza maksymalizującego
 * @param beta - najlepsza wartość dla gracza minimalizującego
 * @param maximizing_player - czy aktualny gracz maksymalizuje
 * @param current_player - aktualny gracz (1 lub 2)
 * @return struktura z najlepszym wynikiem i ruchem
 */
MinimaxResult minimax(int board[5][5], int depth, int alpha, int beta, 
                     bool maximizing_player, int current_player) {
    MinimaxResult result = {0, -1};
    
    // Warunki końcowe rekurencji
    if (depth == 0 || is_terminal_state(board)) {
        result.score = evaluate_position(board, my_player);
        return result;
    }
    
    // Pobierz dostępne ruchy
    int moves[25];
    int move_count;
    get_available_moves(board, moves, &move_count);
    
    if (maximizing_player) {
        // Gracz maksymalizujący szuka najwyższego wyniku
        result.score = INT_MIN;
        
        for (int i = 0; i < move_count; i++) {
            int move = moves[i];
            
            // Zapisz stan planszy
            int temp_board[5][5];
            copy_board(board, temp_board);
            
            // Wykonaj ruch
            if (setMove(move, current_player)) {
                // Rekurencyjne wywołanie dla przeciwnika
                MinimaxResult child_result = minimax(board, depth - 1, alpha, beta, 
                                                   false, (current_player == 1) ? 2 : 1);
                
                // Przywróć stan planszy
                copy_board(temp_board, board);
                
                // Aktualizuj najlepszy wynik
                if (child_result.score > result.score) {
                    result.score = child_result.score;
                    result.move = move;
                }
                
                // Alfa-beta cięcie
                alpha = (alpha > result.score) ? alpha : result.score;
                if (beta <= alpha) {
                    break; // Cięcie beta
                }
            } else {
                // Nieprawidłowy ruch - przywróć planszę
                copy_board(temp_board, board);
            }
        }
    } else {
        // Gracz minimalizujący szuka najniższego wyniku
        result.score = INT_MAX;
        
        for (int i = 0; i < move_count; i++) {
            int move = moves[i];
            
            // Zapisz stan planszy
            int temp_board[5][5];
            copy_board(board, temp_board);
            
            // Wykonaj ruch
            if (setMove(move, current_player)) {
                // Rekurencyjne wywołanie dla nas
                MinimaxResult child_result = minimax(board, depth - 1, alpha, beta, 
                                                   true, (current_player == 1) ? 2 : 1);
                
                // Przywróć stan planszy
                copy_board(temp_board, board);
                
                // Aktualizuj najlepszy wynik
                if (child_result.score < result.score) {
                    result.score = child_result.score;
                    result.move = move;
                }
                
                // Alfa-beta cięcie
                beta = (beta < result.score) ? beta : result.score;
                if (beta <= alpha) {
                    break; // Cięcie alfa
                }
            } else {
                // Nieprawidłowy ruch - przywróć planszę
                copy_board(temp_board, board);
            }
        }
    }
    
    return result;
}

/**
 * INTELIGENTNY WYBÓR NAJLEPSZEGO RUCHU (WERSJA 2.0)
 * 
 * HIERARCHIA DECYZYJNA:
 * 1. WYGRAJ NATYCHMIAST - jeśli możesz wygrać jednym ruchem
 * 2. BLOKUJ PRZECIWNIKA - jeśli przeciwnik może wygrać (ale nie popełniaj samobójstwa)
 * 3. UNIKAJ SAMOBÓJSTW - filtruj ruchy prowadzące do 3 w linii
 * 4. UŻYJ MINIMAX - na bezpiecznych ruchach znajdź optymalny
 * 5. OSTATECZNOŚĆ - jeśli wszystko samobójcze, wybierz najmniej szkodliwy
 * 
 * @param player - numer gracza (1 lub 2)
 * @param depth - głębokość przeszukiwania minimax
 * @return najlepszy ruch (format 11-55)
 */
int get_best_move(int player, int depth) {
    int moves[25];
    int move_count;
    get_available_moves(board, moves, &move_count);
    
    if (move_count == 0) return -1; // Brak dostępnych ruchów
    
    printf("🎯 Analiza %d dostępnych ruchów...\n", move_count);
    
    // PRIORYTET 1: Sprawdź czy można wygrać jednym ruchem
    for (int i = 0; i < move_count; i++) {
        if (would_win_with_move(board, moves[i], player)) {
            printf("🏆 WYGRANA! Ruch wygrywający: %d\n", moves[i]);
            return moves[i];
        }
    }
    
    // PRIORYTET 2: Sprawdź czy trzeba zablokować przeciwnika
    int opponent = (player == 1) ? 2 : 1;
    for (int i = 0; i < move_count; i++) {
        if (blocks_opponent_win(board, moves[i], player)) {
            // Ale tylko jeśli to nie jest samobójstwo!
            if (!would_lose_with_move(board, moves[i], player)) {
                printf("🛡️ BLOKADA! Blokuję przeciwnika: %d\n", moves[i]);
                return moves[i];
            } else {
                printf("⚠️ Ruch %d blokuje przeciwnika, ale to samobójstwo!\n", moves[i]);
            }
        }
    }
    
    // PRIORYTET 3: Filtruj ruchy samobójcze
    int safe_moves[25];
    int safe_count = 0;
    
    printf("🔍 Sprawdzam bezpieczeństwo ruchów:\n");
    for (int i = 0; i < move_count; i++) {
        if (!would_lose_with_move(board, moves[i], player)) {
            safe_moves[safe_count++] = moves[i];
            printf("   ✅ Ruch %d - bezpieczny\n", moves[i]);
        } else {
            printf("   ❌ Ruch %d - SAMOBÓJSTWO!\n", moves[i]);
        }
    }
    
    // SYTUACJA KRYTYCZNA: Wszystkie ruchy samobójcze
    if (safe_count == 0) {
        printf("😱 KRYZYS! Wszystkie ruchy prowadzą do samobójstwa!\n");
        printf("🎲 Wybieram losowy ruch w desperacji...\n");
        srand(time(NULL));
        return moves[rand() % move_count];
    }
    
    printf("🤖 Mam %d bezpiecznych ruchów. Uruchamiam minimax (głębokość %d)...\n", 
           safe_count, depth);
    
    // PRIORYTET 4: Użyj minimax na bezpiecznych ruchach
    int best_move = -1;
    int best_score = INT_MIN;
    
    for (int i = 0; i < safe_count; i++) {
        int move = safe_moves[i];
        
        // Zapisz stan planszy
        int temp_board[5][5];
        copy_board(board, temp_board);
        
        if (setMove(move, player)) {
            // Wywołaj minimax dla tego ruchu
            MinimaxResult result = minimax(board, depth - 1, INT_MIN, INT_MAX, 
                                         false, opponent);
            
            // Przywróć planszę
            copy_board(temp_board, board);
            
            printf("   🧮 Ruch %d -> score: %d\n", move, result.score);
            
            // Aktualizuj najlepszy ruch
            if (result.score > best_score) {
                best_score = result.score;
                best_move = move;
            }
        } else {
            // Nie powinno się zdarzyć, ale na wszelki wypadek
            copy_board(temp_board, board);
            printf("   ⚠️ Błąd wykonania ruchu %d\n", move);
        }
    }
    
    // Sprawdź czy znaleziono ruch
    if (best_move == -1) {
        printf("😕 Minimax nie znalazł ruchu. Wybieram pierwszy bezpieczny: %d\n", safe_moves[0]);
        return safe_moves[0];
    }
    
    printf("🎯 WYBÓR: Ruch %d (score: %d)\n", best_move, best_score);
    return best_move;
}

int main(int argc, char *argv[]) {
    int server_socket;
    struct sockaddr_in server_addr;
    char server_message[16], player_message[16];
    
    bool end_game;
    int player, msg, move;
    
    // Sprawdź argumenty linii poleceń
    if (argc != 6) {
        printf("❌ BŁĄD: Nieprawidłowa liczba argumentów!\n\n");
        printf("UŻYCIE: %s <IP> <PORT> <PLAYER_NUMBER> <PLAYER_NAME> <DEPTH>\n", argv[0]);
        printf("\nPARAMETRY:\n");
        printf("  IP            - adres IP serwera (np. 127.0.0.1)\n");
        printf("  PORT          - numer portu serwera (np. 8080)\n");
        printf("  PLAYER_NUMBER - numer gracza: 1 (X) lub 2 (O)\n");
        printf("  PLAYER_NAME   - nazwa gracza (maksymalnie 9 znaków)\n");
        printf("  DEPTH         - głębokość przeszukiwania minimax (1-10)\n");
        printf("\nPRZYKŁAD:\n");
        printf("  %s 127.0.0.1 8080 1 MiniMax 5\n", argv[0]);
        return -1;
    }
    
    // Parsuj i waliduj argumenty
    search_depth = atoi(argv[5]);
    if (search_depth < 1 || search_depth > 10) {
        printf("❌ BŁĄD: Głębokość musi być między 1 a 10 (podano: %d)\n", search_depth);
        return -1;
    }
    
    my_player = atoi(argv[3]);
    if (my_player != 1 && my_player != 2) {
        printf("❌ BŁĄD: Numer gracza musi być 1 lub 2 (podano: %d)\n", my_player);
        return -1;
    }
    opponent_player = (my_player == 1) ? 2 : 1;
    
    if (strlen(argv[4]) > 9) {
        printf("❌ BŁĄD: Nazwa gracza zbyt długa (max 9 znaków, podano: %zu)\n", strlen(argv[4]));
        return -1;
    }
    
    // Wyświetl informacje o konfiguracji
    printf("╔════════════════════════════════════╗\n");
    printf("║        KLIENT MINIMAX 2.0          ║\n");
    printf("║     z ochroną przed samobójstwem   ║\n");
    printf("╠════════════════════════════════════╣\n");
    printf("║ Gracz:     %d (%c)                   ║\n", my_player, (my_player == 1) ? 'X' : 'O');
    printf("║ Nazwa:     %-9s               ║\n", argv[4]);
    printf("║ Głębokość: %d                      ║\n", search_depth);
    printf("║ Serwer:    %s:%-9s     ║\n", argv[1], argv[2]);
    printf("╚════════════════════════════════════╝\n\n");
    
    // Utwórz socket
    server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (server_socket < 0) {
        printf("❌ Błąd tworzenia socket\n");
        return -1;
    }
    printf("✅ Socket utworzony pomyślnie\n");
    
    // Konfiguracja adresu serwera
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(atoi(argv[2]));
    server_addr.sin_addr.s_addr = inet_addr(argv[1]);
    
    // Połącz z serwerem
    printf("🔗 Łączenie z serwerem %s:%s...\n", argv[1], argv[2]);
    if (connect(server_socket, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        printf("❌ Nie można połączyć z serwerem\n");
        return -1;
    }
    printf("✅ Połączono z serwerem pomyślnie\n");
    
    // Otrzymaj komunikat 700 (prośba o identyfikację)
    memset(server_message, '\0', sizeof(server_message));
    if (recv(server_socket, server_message, sizeof(server_message), 0) < 0) {
        printf("❌ Błąd otrzymywania wiadomości z serwera\n");
        return -1;
    }
    printf("📨 Komunikat serwera: %s\n", server_message);
    
    // Wyślij identyfikację (numer gracza + nazwa)
    memset(player_message, '\0', sizeof(player_message));
    snprintf(player_message, sizeof(player_message), "%s %s", argv[3], argv[4]);
    if (send(server_socket, player_message, strlen(player_message), 0) < 0) {
        printf("❌ Nie można wysłać identyfikacji\n");
        return -1;
    }
    printf("📤 Wysłano identyfikację: %s\n", player_message);
    
    // Inicjalizuj planszę i zmienne gry
    setBoard();
    end_game = false;
    player = my_player;
    
    printf("\n🎮 ROZPOCZYNANIE GRY...\n");
    printf("═══════════════════════════════════════\n");
    printBoard();
    
    // GŁÓWNA PĘTLA GRY
    while (!end_game) {
        // Odbierz komunikat z serwera
        memset(server_message, '\0', sizeof(server_message));
        if (recv(server_socket, server_message, sizeof(server_message), 0) < 0) {
            printf("❌ Błąd otrzymywania wiadomości z serwera\n");
            return -1;
        }
        
        // Parsuj komunikat (format: XYZ gdzie X to typ, YZ to ruch)
        sscanf(server_message, "%d", &msg);
        move = msg % 100;
        msg = msg / 100;
        
        printf("\n📨 Komunikat serwera: %s (typ: %d, ruch: %d)\n", server_message, msg, move);
        
        // Jeśli otrzymaliśmy ruch przeciwnika
        if (move != 0) {
            printf("👥 Ruch przeciwnika: %d\n", move);
            setMove(move, opponent_player);
            printf("\n📋 Plansza po ruchu przeciwnika:\n");
            printBoard();
        }
        
        // Sprawdź czy to nasza kolej (komunikat 0xx lub 600)
        if ((msg == 0) || (msg == 6)) {
            printf("\n🤖 MOJA KOLEJ! Obliczam najlepszy ruch...\n");
            printf("─────────────────────────────────────\n");
            
            // Użyj inteligentnego algorytmu do wyboru ruchu
            move = get_best_move(my_player, search_depth);
            
            if (move == -1) {
                printf("💥 BŁĄD KRYTYCZNY: Nie można znaleźć ruchu!\n");
                break;
            }
            
            printf("─────────────────────────────────────\n");
            printf("✨ WYKONUJĘ RUCH: %d\n", move);
            
            // Wykonaj ruch na lokalnej planszy
            setMove(move, my_player);
            printf("\n📋 Plansza po moim ruchu:\n");
            printBoard();
            
            // Wyślij ruch do serwera
            memset(player_message, '\0', sizeof(player_message));
            snprintf(player_message, sizeof(player_message), "%d", move);
            if (send(server_socket, player_message, strlen(player_message), 0) < 0) {
                printf("❌ Nie można wysłać ruchu do serwera\n");
                return -1;
            }
            printf("📤 Ruch wysłany do serwera\n");
            
        } else {
            // Koniec gry
            end_game = true;
            printf("\n🏁 KONIEC GRY!\n");
            printf("═══════════════════════════════════════\n");
            
            switch (msg) {
                case 1: 
                    printf("🎉🎉🎉 WYGRAŁEM! 🎉🎉🎉\n");
                    printf("Algorytm minimax triumfuje!\n");
                    break;
                case 2: 
                    printf("😞 Przegrałem...\n");
                    printf("Przeciwnik był lepszy tym razem.\n");
                    break;
                case 3: 
                    printf("🤝 Remis!\n");
                    printf("Równorzędni przeciwnicy.\n");
                    break;
                case 4: 
                    printf("🎉 Wygrałem dzięki błędowi przeciwnika!\n");
                    printf("Przeciwnik popełnił błąd.\n");
                    break;
                case 5: 
                    printf("😞 Przegrałem przez własny błąd...\n");
                    printf("Muszę poprawić algorytm.\n");
                    break;
                default: 
                    printf("❓ Nieznany komunikat końca gry: %d\n", msg);
                    break;
            }
            printf("═══════════════════════════════════════\n");
        }
    }
    
    // Zamknij połączenie
    close(server_socket);
    printf("\n👋 Rozłączono z serwerem. Do widzenia!\n");
    
    return 0;
}