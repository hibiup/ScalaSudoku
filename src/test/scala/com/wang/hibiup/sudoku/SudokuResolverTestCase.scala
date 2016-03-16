package com.wang.hibiup.sudoku

import org.scalatest._, org.scalatest.junit._;
import org.junit._, runner.RunWith, Assert._

@RunWith(classOf[JUnitRunner])
class SudokuResolverTestCase extends FunSuite {
    test("playEasy") {
        SudokuResolver.play(Array(
            0, 5, 0, 2, 0, 0, 0, 1, 4,
            0, 1, 8, 0, 6, 3, 0, 9, 0,
            0, 2, 0, 0, 0, 4, 8, 5, 0,
            2, 3, 4, 0, 9, 0, 0, 0, 1,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            8, 0, 0, 0, 7, 0, 4, 2, 9,
            0, 8, 5, 1, 0, 0, 0, 3, 0,
            0, 4, 0, 9, 3, 0, 7, 8, 0,
            3, 9, 0, 0, 0, 2, 0, 4, 0));
    }

    test("playEvil1") {
        SudokuResolver.play(Array(
            1, 0, 0, 0, 0, 5, 0, 0, 0,
            0, 0, 0, 8, 0, 0, 1, 0, 0,
            0, 9, 7, 0, 3, 4, 0, 0, 0,
            0, 0, 9, 0, 0, 0, 0, 1, 3,
            6, 0, 4, 0, 0, 0, 9, 0, 7,
            7, 8, 0, 0, 0, 0, 4, 0, 0,
            0, 0, 0, 2, 4, 0, 8, 6, 0,
            0, 0, 6, 0, 0, 1, 0, 0, 0,
            0, 0, 0, 9, 0, 0, 0, 0, 4));
    }

    test("playEvil2") {
        SudokuResolver.play(Array(
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0));
    }

    test("getGroup") {
        val matrix: Array[Int] = Array(
            0, 5, 0, 2, 0, 0, 0, 1, 4,
            0, 1, 8, 0, 6, 3, 0, 9, 0,
            0, 2, 0, 0, 0, 4, 8, 5, 0,
            2, 3, 4, 0, 9, 0, 0, 0, 1,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            8, 0, 0, 0, 7, 0, 4, 2, 9,
            0, 8, 5, 1, 0, 0, 0, 3, 0,
            0, 4, 0, 9, 3, 0, 7, 8, 0,
            3, 9, 0, 0, 0, 2, 0, 4, 0);

        println(SudokuResolver.getCube(matrix.toList, 1));
    }

    test("getLine") {
        val matrix: Array[Int] = Array(
            0, 5, 0, 2, 0, 0, 0, 1, 4,
            0, 1, 8, 0, 6, 3, 0, 9, 0,
            0, 2, 0, 0, 0, 4, 8, 5, 0,
            2, 3, 4, 0, 9, 0, 0, 0, 1,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            8, 0, 0, 0, 7, 0, 4, 2, 9,
            0, 8, 5, 1, 0, 0, 0, 3, 0,
            0, 4, 0, 9, 3, 0, 7, 8, 0,
            3, 9, 0, 0, 0, 2, 0, 4, 0);

        println(SudokuResolver.getLine(matrix.toList, 1, 9));
    }

    test("getColum") {
        val matrix: Array[Int] = Array(
            0, 5, 0, 2, 0, 0, 0, 1, 4,
            0, 1, 8, 0, 6, 3, 0, 9, 0,
            0, 2, 0, 0, 0, 4, 8, 5, 0,
            2, 3, 4, 0, 9, 0, 0, 0, 1,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            8, 0, 0, 0, 7, 0, 4, 2, 9,
            0, 8, 5, 1, 0, 0, 0, 3, 0,
            0, 4, 0, 9, 3, 0, 7, 8, 0,
            3, 9, 0, 0, 0, 2, 0, 4, 0);
        println(SudokuResolver.getColumn(matrix.toList, 8, 9));
    }

    test("checkCube") {
        val matrix: Array[Int] = Array(
            0, 5, 0, 2, 0, 0, 0, 1, 4,
            0, 1, 8, 0, 6, 3, 0, 9, 0,
            0, 2, 3, 0, 1, 4, 8, 5, 0,
            2, 3, 4, 0, 9, 0, 0, 0, 1,
            0, 0, 0, 0, 0, 0, 0, 0, 0,
            8, 0, 0, 0, 7, 0, 4, 2, 9,
            0, 8, 5, 1, 0, 0, 0, 3, 0,
            0, 4, 0, 9, 3, 0, 7, 8, 0,
            3, 9, 0, 0, 0, 2, 1, 4, 0);
        val sudoku = SudokuResolver(matrix)

        val cube = SudokuResolver.getCube(matrix.toList, 0);
        println(cube)
        val mask = sudoku.fillNumber(cube, 4);
        println(mask)
        SudokuResolver.applyMask(matrix, mask, 4);
        SudokuResolver.printSudoku(matrix.toList);
    }

    test("Calcu") {
        val index = 0
        val offset = 7
        //println((index / 3 * 27) + (index % 3 * 3) + (offset / 3 * 9 + (offset % 3)))
        println(SudokuResolver.localToGlobe(index, offset));
    }
}