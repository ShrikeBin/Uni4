#ifndef HEURISTICS_HPP
#define HEURISTICS_HPP

#include "constants.hpp"
#include "board.hpp"
#include "checks.hpp"

//══════════════════════════════════════════════════════════════════════════════
//
//         FIX IT TO BE BETTER THAN THE ORIGINAL, VIBE CODED VERSION
//
//              OR.... JUST HAVE SOME FUN NOW THAT IT WORKS!
//
//══════════════════════════════════════════════════════════════════════════════

int evaluateBoard(int board[BOARD_SIZE][BOARD_SIZE], int playerSymbol){
    int score = 0;
    int opponentSymbol = (playerSymbol == 1) ? 2 : 1;
    
    // HIGHEST PRIORITY
    if (winCheck(playerSymbol, board)) return 100000;      // Player won
    if (winCheck(opponentSymbol, board)) return -100000;   // Enemy won
    if (loseCheck(playerSymbol, board)) return -50000;    // Player tupid
    if (loseCheck(opponentSymbol, board)) return 50000;   // Enemy tupid
    
    // 1. Check for possible 4 in line
    // Here we analyze every possible 4 in line
    for (int i = 0; i < 28; ++i){
        int player_count = 0;
        int opponent_count = 0;
        int empty_count = 0;
        
        for (int j = 0; j < 4; ++j){
            int row = winTable[i][j][0];
            int col = winTable[i][j][1];
            if (board[row][col] == playerSymbol) ++player_count;
            else if (board[row][col] == opponentSymbol) ++opponent_count;
            else empty_count++;
        }
        
        if (opponent_count == 0){
            if (player_count == 3) score += 1100;      // Close to winning
            else if (player_count == 2) score += 100;  // 
            else if (player_count == 1) score += 10;   // 
        }
        
        if (player_count == 0){
            if (opponent_count == 3) score += 800;     // BLOCK WINNING
            else if (opponent_count == 2) score += 80; // Block danger
        }
    }
    
    // 2. Look for possible 3 in line
    // Here we also punish "unsafe play", potentially allowing enemy to force us into suicide
    // Here we analize all possible loosing 3 in line
    for (int i = 0; i < 48; ++i){
        int player_count = 0;
        int opponent_count = 0;
        bool player_middle = false;
        bool opponent_middle = false;
        
        for (int j = 0; j < 3; ++j){
            int row = loseTable[i][j][0];
            int col = loseTable[i][j][1];
            if (board[row][col] == playerSymbol) ++player_count;
            else if (board[row][col] == opponentSymbol) ++opponent_count;
            if(j == 1){
                if(board[row][col] == playerSymbol) player_middle = true;
                else if (board[row][col] == opponentSymbol) opponent_middle = true;
            }
        }
        
        // These two do not really make sense but yeah maybe they do I dunno
        // We have Potential 3 in line for us
        if (opponent_count == 0 && player_count == 2 && player_middle){
            score -= 30;
        }
        
        // Enemy has potential 3 in line
        if (player_count == 0 && opponent_count == 2 && opponent_middle){
            score -= 30;
        }

        // We have (X)( )(X)
        if (opponent_count == 0 && player_count == 2 && !player_middle){
            score += 100;
        }

        // Enemy has (X)( )(X)
        if (player_count == 0 && opponent_count == 2 && !opponent_middle){
            score -= 120;
        }
    }
    
    // 3. Be present in the middle
    int center_positions[][3] = {
        {2, 2, 20},                                     // Middle
        {1, 2, 15}, {2, 1, 15}, {3, 2, 15}, {2, 3, 15}, // Middle cross
        {1, 1, 15}, {1, 3, 15}, {3, 1, 15}, {3, 3, 15}  // Middle corners
    };
    
    for (int i = 0; i < 9; ++i){
        int row = center_positions[i][0];
        int col = center_positions[i][1];
        int bonus = center_positions[i][2];
        if (board[row][col] == playerSymbol){
            score += bonus;
        }
    }
    
    return score;
}

#endif