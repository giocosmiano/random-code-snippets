/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package kidsSchoolProjects;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

public class SudokuLab3 {

    Scanner keyboard = new Scanner(System.in);
    int[][] gameBoard
            = {
                {0, 4, 0, 0, 0, 0, 1, 7, 9},
                {0, 0, 2, 0, 0, 8, 0, 5, 4},
                {0, 0, 6, 0, 0, 5, 0, 0, 8},
                {0, 8, 0, 0, 7, 0, 9, 1, 0},
                {0, 5, 0, 0, 9, 0, 0, 3, 0},
                {0, 1, 9, 0, 6, 0, 0, 4, 0},
                {3, 0, 0, 4, 0, 0, 7, 0, 0},
                {5, 7, 0, 1, 0, 0, 2, 0, 0},
                {9, 2, 8, 0, 0, 0, 0, 6, 0}
            };

    public static void main(String[] args) {
        new SudokuLab3();
    }

    public SudokuLab3() {
        boolean gameOver = false;
        int[] currentCellLocation = {0, 0};
        printGameBoard();
        while (!gameOver) {
            int[] locationOfTheNextEmptyCell = getLocationOfTheNextEmptyCell(currentCellLocation);
            List<Integer> listOfSurvivors = getSurvivorsAfterApplyingTheGameRules(locationOfTheNextEmptyCell);
            if (listOfSurvivors.size() == 1) {
                placeNumberInEmptyCell(listOfSurvivors.get(0), locationOfTheNextEmptyCell);
                printGameBoard();
                currentCellLocation = new int[]{0, 0};
                if (isGameOver()) {
                    printCongratulationsMessage();
                    gameOver = true;
                }
            } else {
                currentCellLocation = updateCurrentCellLocation(locationOfTheNextEmptyCell);
            }
        }
    }

    private void printGameBoard() {
        for (int row = 0; row < gameBoard.length; row++) {
            for (int col = 0; col < gameBoard[row].length; col++) {
                if (gameBoard[row][col] < 10) {
                    System.out.print(gameBoard[row][col] + "  ");
                }
            }
            System.out.println(" ");
        }
    }

    private int[] getLocationOfTheNextEmptyCell(int[] currentCellLocation) {

        int currentRow = currentCellLocation[0];
        int currentCol = currentCellLocation[1];

        for (int row = currentRow; row < gameBoard.length; row++) {
            for (int col = currentCol; col < gameBoard[row].length; col++) {
                if (gameBoard[row][col] == 0) {
                    return new int[]{row, col};
                }
            }
        }
        return new int[]{currentRow, currentCol};
    }

    private List<Integer> getSurvivorsAfterApplyingTheGameRules(
            int[] locationOfTheEmptyCell) {
        List<Integer> rule1SurvivorsList = getSurvivorsAfterRule1(
                locationOfTheEmptyCell);
        List<Integer> rule2SurvivorsList = getSurvivorsAfterRule2(
                locationOfTheEmptyCell);
        List<Integer> rule3SurvivorsList = getSurvivorsAfterRule3(
                locationOfTheEmptyCell);
        List<Integer> overallSurvivorsList
                = getOverallSurvivorsList(rule1SurvivorsList, rule2SurvivorsList,
                        rule3SurvivorsList);
        return overallSurvivorsList;
    }

    private List<Integer> getSurvivorsAfterRule1(int[] locationOfTheEmptyCell) {

        int currentRow = locationOfTheEmptyCell[0];
        List<Integer> listOfAllPossibleNumbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9);
        List<Integer> numbersInTheRow = new ArrayList<>();
        for (int col = 0; col < 9; col++) {
            if (gameBoard[currentRow][col] != 0) {
                numbersInTheRow.add(gameBoard[currentRow][col]);
            }
        }
        List<Integer> result = new ArrayList<>(listOfAllPossibleNumbers);
        result.removeAll(numbersInTheRow);
        return result;

    }

    private List<Integer> getSurvivorsAfterRule2(int[] locationOfTheEmptyCell) {

        int currentCol = locationOfTheEmptyCell[1];
        List<Integer> listOfAllPossibleNumbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9);
        List<Integer> numbersInTheCol = new ArrayList<>();
        for (int row = 0; row < 9; row++) {
            if (gameBoard[row][currentCol] != 0) {
                numbersInTheCol.add(gameBoard[row][currentCol]);
            }
        }
        List<Integer> result = new ArrayList<>(listOfAllPossibleNumbers);
        result.removeAll(numbersInTheCol);
        return result;

    }

    private List<Integer> getSurvivorsAfterRule3(int[] locationOfTheEmptyCell) {

        int currentRow = locationOfTheEmptyCell[0];
        int currentCol = locationOfTheEmptyCell[1];

        int initialRow = (currentRow / 3) * 3;
        int initialCol = (currentCol / 3) * 3;

        int finalRow = initialRow + 3;
        int finalCol = initialCol + 3;

        List<Integer> listOfAllPossibleNumbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9);
        List<Integer> numbersInTheBlock = new ArrayList<>();
        for (int row = initialRow; row < finalRow; row++) {
            for (int col = initialCol; col < finalCol; col++) {
                if (gameBoard[row][col] != 0) {
                    numbersInTheBlock.add(gameBoard[row][col]);
                }
            }
        }
        List<Integer> result = new ArrayList<>(listOfAllPossibleNumbers);
        result.removeAll(numbersInTheBlock);
        return result;

    }

    private List<Integer> getOverallSurvivorsList(List<Integer> rule1SurvivorsList,
            List<Integer> rule2SurvivorsList, List<Integer> rule3SurvivorsList) {
        List<Integer> interceptingList1And2 = rule1SurvivorsList.stream()
                .distinct()
                .filter(rule2SurvivorsList::contains)
                .collect(Collectors.toList());
        List<Integer> interceptingList1And2And3 = interceptingList1And2.stream()
                .distinct()
                .filter(rule3SurvivorsList::contains)
                .collect(Collectors.toList());
        return interceptingList1And2And3;

    }

    private void placeNumberInEmptyCell(Integer value, int[] locationOfTheNextEmptyCell) {

        int row = locationOfTheNextEmptyCell[0];
        int col = locationOfTheNextEmptyCell[1];

        gameBoard[row][col] = value;

        System.out.println("cell ( " + row + "," + col + " ) " + value);
        System.out.println();
    }

    private void placeNumberInEmptyCell2(Integer value, int[] locationOfTheNextEmptyCell) {

        int row = locationOfTheNextEmptyCell[0];
        int col = locationOfTheNextEmptyCell[1];

        gameBoard[row][col] = value;

        System.out.println("cell ( " + row + "," + col + " ) " + value);

    }

    private boolean isGameOver() {
        boolean gameSolved = true;
        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                int value = gameBoard[row][col];

                if (value == 0) {
                    gameSolved = false;
                    return gameSolved;
                }

            }

        }
        return gameSolved;

    }

    private void printCongratulationsMessage() {
        System.out.println("Congrats you solved the Sudoku game!");

    }

    private int[] updateCurrentCellLocation(int[] locationOfTheNextEmptyCell) {

        int currentRow = locationOfTheNextEmptyCell[0];
        int currentCol = locationOfTheNextEmptyCell[1] + 1;

        if (currentCol == 9) {
            currentRow += 1;
            currentCol = 0;

            if (currentRow == 9) {
                currentRow = 0;
            }
        }

        int[] emptyCellLocation = {currentRow, currentCol};
        return emptyCellLocation;
    }
}