/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package kidsSchoolProjects;

import java.util.Scanner;

/**
 *
 * @author gabriellecosmiano
 */
public class TileGame {
    
    int[][] gameBoard = {
        {1, 3, 8, 12},
        {10, 2, 0, 11},
        {6, 7, 13, 9},
        {4, 14, 15, 5}
    };

    int[][] gameSolution = new int[][]{
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12},
        {13, 14, 15, 0}
    };

    public static void main(String[] args) {
        new TileGame();

    }

    public TileGame() {
        //place all methods here 
        boolean gameOver = false;
        while (!gameOver) {
            printGameOnScreen();

            int chosenTile = getUserSelectedTile();
            int[] tileLocationOnTheBoard = getTileLocation(chosenTile);
            int[] emptyLocationOnTheBoard = getTileLocation(0);
            if (canTileBeMoved(tileLocationOnTheBoard, emptyLocationOnTheBoard)) {
                moveTile(tileLocationOnTheBoard, emptyLocationOnTheBoard);
                if (isGameSolved()) {
                    printCongratsMessage();
                    gameOver = true;
                }
            }
        }
        printGameOnScreen(); // printing solved board just before exiting game

    }

    private void printGameOnScreen() {
        for (int row = 0; row < 4; row++) {
            for (int col = 0; col < 4; col++) {
                int value = gameBoard[row][col];
                if (value < 10) {
                    System.out.print(" ");
                }

            }
        }
    }

    private int getUserSelectedTile() {

        boolean validInput = false;

        String userInput = "";
        int value = 0;
        Scanner keyboard = new Scanner(System.in);
        // your code to get the tile
        System.out.println("Put in a number between 1 and 15");
        // your code to check if the tile number is valid (1-15)
        while (!validInput) {

            try {
                userInput = keyboard.nextLine();
                value = Integer.parseInt(userInput);

                if (value >= 1 && value <= 15) {
                    validInput = true;

                } else {
                    System.out.println("Invalid Tile Number, please try again.");
                }

            } catch (NumberFormatException ex) {
                System.out.println("Invalid Tile Number, please try again.");
            }

        }
        return value;
    }

    private int[] getTileLocation(int chosenTile) {
        int rowNumber = -1; // array intiates at 0
        int colNumber = -1;

        for (int row = 0; row < 4; row++) {
            for (int col = 0; col < 4; col++) {
                int value = gameBoard[row][col];

                if (value == chosenTile) {
                    rowNumber = row;
                    colNumber = col;
                }

            }

        }
        return new int[]{rowNumber, colNumber};
    }

    private boolean canTileBeMoved(int[] tileLocationOnTheBoard, int[] emptyLocationOnTheBoard) {
// [0] = row 
//[1]= column 

//above empty tile 
        if ((emptyLocationOnTheBoard[0] == tileLocationOnTheBoard[0] - 1)
                && (emptyLocationOnTheBoard[1] == tileLocationOnTheBoard[1])) {
            return true;

            //below empty tile 
        } else if ((emptyLocationOnTheBoard[0] == tileLocationOnTheBoard[0] + 1)
                && (emptyLocationOnTheBoard[1] == tileLocationOnTheBoard[1])) {
            return true;
            // left of empty tile 
        } else if ((emptyLocationOnTheBoard[0] == tileLocationOnTheBoard[0])
                && (emptyLocationOnTheBoard[1] == tileLocationOnTheBoard[1] - 1)) {
            return true;
            //right of empty tile 
        } else if ((emptyLocationOnTheBoard[0] == tileLocationOnTheBoard[0])
                && (emptyLocationOnTheBoard[1] == tileLocationOnTheBoard[1] + 1)) {
            return true;
        } else {

//            int tileLocationValue = gameBoard[tileLocationOnTheBoard[0]][tileLocationOnTheBoard[1]];
//            int emptyLocationValue = gameBoard[emptyLocationOnTheBoard[0]][emptyLocationOnTheBoard[1]];
//            System.out.println("tile location value: "
//                    + tileLocationValue + " empty location value :  "
//                    + emptyLocationValue);
            return false;
        }

    }

    private boolean moveTile(int[] tileLocationOnTheBoard, int[] emptyLocationOnTheBoard) {

        int tileLocationValue = gameBoard[tileLocationOnTheBoard[0]][tileLocationOnTheBoard[1]];
        int emptyLocationValue = gameBoard[emptyLocationOnTheBoard[0]][emptyLocationOnTheBoard[1]];

        gameBoard[emptyLocationOnTheBoard[0]][emptyLocationOnTheBoard[1]] = tileLocationValue;

        gameBoard[tileLocationOnTheBoard[0]][tileLocationOnTheBoard[1]] = emptyLocationValue;

        for (int row = 0; row < 4; row++) {
            for (int col = 0; col < 4; col++) {
                int value = gameBoard[row][col];
                if (value < 10) {
                    System.out.print(" ");
                }

                System.out.print(" " + value);
            }
            System.out.println();

        }

        return true;

    }

    private boolean isGameSolved() {

        boolean gameSolved = true;
        for (int row = 0; row < 4; row++) {
            for (int col = 0; col < 4; col++) {
                int value = gameBoard[row][col];

                int solvedValue = gameSolution[row][col];

                if (value != solvedValue) {
                    gameSolved = false;
                    return gameSolved;
                }

            }

        }
        return gameSolved;
    }

    private void printCongratsMessage() {
        System.out.print(" Congrats you solved the game!");
    }

}
