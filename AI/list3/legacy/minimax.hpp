#ifndef MINIMAX_HPP
#define MINIMAX_HPP

#include <climits>

#include "board.hpp"
#include "constants.hpp"
#include "heuristics.hpp"
#include "minimax.hpp"
#include "checks.hpp"

/**
 * Minimax results
 * 
 * @param score - score
 * @param move - move with that score
 */
struct MinimaxResult {
    int score;
    int move;
};

/**
 * MINIMAX w/ Alpha Beta prunning
 * 
 * - Prune: alpha >= beta, we can prune branch
 * 
 * @param board - current board
 * @param depth - depth left
 * @param alpha - best for maximising player
 * @param beta - best for minimizing player
 * @param maximizing_player - if now we maximize or minimize
 * @param current_player - current player 1 or 2
 * @return struct with best possible move
 */
MinimaxResult minimax(int board[5][5], int depth, int alpha, int beta, 
                     bool maximizing_player, int current_player) {
    MinimaxResult result = {0, -1};
    
    // Warunki końcowe rekurencji
    if (depth == 0 || is_terminal_state(board)) {
        result.score = evaluate_position(board, MY_SYMBOL);
        return result;
    }
    
    // Pobierz dostępne ruchy
    int moves[25];
    int move_count;
    getMoves(board, moves, &move_count);
    
    if (maximizing_player) {
        // Gracz maksymalizujący szuka najwyższego wyniku
        result.score = INT_MIN;
        
        for (int i = 0; i < move_count; i++) {
            int move = moves[i];
            
            // Zapisz stan planszy
            int temp_board[5][5];
            copyBoard(board, temp_board);
            
            // Wykonaj ruch
            if (makeMove(move, board ,current_player)) {
                // Rekurencyjne wywołanie dla przeciwnika
                MinimaxResult child_result = minimax(board, depth - 1, alpha, beta, 
                                                   false, (current_player == 1) ? 2 : 1);
                
                // Przywróć stan planszy
                copyBoard(temp_board, board);
                
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
                copyBoard(temp_board, board);
            }
        }
    } else {
        // Gracz minimalizujący szuka najniższego wyniku
        result.score = INT_MAX;
        
        for (int i = 0; i < move_count; i++) {
            int move = moves[i];
            
            // Zapisz stan planszy
            int temp_board[5][5];
            copyBoard(board, temp_board);
            
            // Wykonaj ruch
            if (makeMove(move, board, current_player)) {
                // Rekurencyjne wywołanie dla nas
                MinimaxResult child_result = minimax(board, depth - 1, alpha, beta, 
                                                   true, (current_player == 1) ? 2 : 1);
                
                // Przywróć stan planszy
                copyBoard(temp_board, board);
                
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
                copyBoard(temp_board, board);
            }
        }
    }
    
    return result;
}

#endif