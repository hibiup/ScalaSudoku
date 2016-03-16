package com.wang.hibiup.sudoku

import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks.{ break, breakable };
import scala.collection.mutable.ArrayBuffer;

object SudokuResolver {
    val unit_size = 3;
    val group_size = unit_size * unit_size;
    val amount = group_size * group_size;

    def play(matrix: Array[Int]): Boolean = {
        SudokuResolver(matrix).play();
    }

    /**
     * A group contains certain numbers. they come from a cube, or a vertical or horizontal line
     *
     *
     */
    def getGroup(list: List[Int], n: Int, x: (Int) => Int): List[Int] = {
        val g = ListBuffer[Int]();
        for (i <- 0 until list.size) {
            //(n == x(i)) match { case true => g += list(i) }
            if (n == x(i)) {
                g += list(i)
            }
        }

        g.toList;
    }

    /**
     * Get a group of numbers belong to same cube from a matrix. the cube size is fixed 9
     *
     * matrix: data set
     * n: the cube index we are looking for.
     */
    def getCube(matrix: List[Int], n: Int) = {
        val cube = getGroup(matrix, n, x => {
            // Calculate abstract positions for numbers in a cube
            (x / (group_size * unit_size) * unit_size) + (x % group_size / unit_size)
        });

        Cube(n, cube);
    }

    /**
     * Get a group of numbers belong to same horizontal line
     *
     * matrix: data set
     * n: the line number we are looking for.
     * size: length of line
     */
    def getLine(matrix: List[Int], n: Int, size: Int) = {
        val line = getGroup(matrix, n, x => {
            // Calculate abstract positions for numbers in a horizontal line
            (x / size)
        });

        Line(n, line);
    }

    /**
     * Get a group of numbers belong to same vertical line
     *
     * matrix: data set
     * n: the column index we are looking for.
     * size: length of column
     */
    def getColumn(matrix: List[Int], n: Int, size: Int) = {
        val column = getGroup(matrix, n, x => {
            // Calculate abstract positions for numbers in a vertical line
            (x % size)
        });

        Column(n, column);
    }

    /**
     * Fill number into matrix base on the mask
     */
    def applyMask(matrix: Array[Int], mask: Cube, n: Int): Cube = {
        var i = 0;
        mask.list.foreach { x =>
            if (0 == x) {
                val p = SudokuResolver.localToGlobe(mask.index, i);
                matrix(p) = n;
            }
            i = i + 1;
        }

        getCube(matrix.toList, mask.index)
    }

    /**
     * Calculate possible positions for a number in a certain cube
     */
    def findOptionsForNumberInTheCube(cube: List[Int], number: Int): Option = {
        val listBuffer = new ListBuffer[Int];
        if (0 == count(cube, number)) {
            var i = 0;
            cube.foreach { x =>
                x match { case 0 => listBuffer += i; }
                i = i + 1;
            };
        }
        Option(number, listBuffer.toList);
    }

    def count(matrix: List[Int], n: Int) = {
        matrix.count { x => x == n }
    }

    def isDone(matrix: List[Int]): Boolean = {
        0 == matrix.count { x => x == 0 }
    }

    /**
     *  Convert offset from cube to matrix
     */
    def localToGlobe(index: Int, offset: Int): Int = {
        (index / unit_size * 27) + (index % unit_size * unit_size) + (offset / unit_size * group_size + (offset % unit_size))
    }

    def printSudoku(matrix: List[Int]) {
        var i = 0;
        matrix.foreach { x =>
            if (0 == i % group_size)
                println()

            print(matrix(i) + ", ");
            i = i + 1;
        }

        println("\n-------------------------------")
    }

    def printHistory(hitOptions: Array[Array[ArrayBuffer[Int]]]) {
        // Loop the number from 1 to 9 in the cube
        for (number <- 1 to SudokuResolver.group_size) {
            // Check cube[0 to 8]
            for (cubeIndex <- 0 until SudokuResolver.group_size) {
                if (null != hitOptions(number - 1)(cubeIndex))
                    println(number, cubeIndex, hitOptions(number - 1)(cubeIndex).toList);
                else
                    println(number, cubeIndex, null);
            }
        }
    }
}

case class SudokuResolver(var matrix: Array[Int]) {
    val cubeShadow: Array[Cube] = new Array[Cube](SudokuResolver.group_size);
    val hitOptions: Array[Array[ArrayBuffer[Int]]] = new Array(SudokuResolver.group_size);
    {
        for (i <- 0 until hitOptions.size) {
            hitOptions(i) = new Array(SudokuResolver.group_size);
        }
    }

    var modified = false;

    /**
     * Entrance
     */
    def play(): Boolean = {
        SudokuResolver.printSudoku(matrix.toList);

        do {
            // Loop the number from 1 to 9 in the cube
            for (number <- 1 to SudokuResolver.group_size) {
                modified = false
                // Check cube[0 to 8]
                for (cubeIndex <- 0 until SudokuResolver.group_size) {
                    breakable {
                        if (SudokuResolver.group_size == SudokuResolver.count(matrix.toList, number)) {
                            //println(number, cubeIndex, " => Done!")
                            break;
                        }

                        val cube = SudokuResolver.getCube(matrix.toList, cubeIndex);
                        val cubeMask = fillNumber(cube, number);

                        cubeShadow(cube.index) = cube;
                        saveOption(cubeMask, number);

                        if (1 == SudokuResolver.count(cubeMask.list, 0)) {
                            val newCube = SudokuResolver.applyMask(matrix, cubeMask, number)
                            modified = true;
                        }
                    }
                }
            }

            //SudokuResolver.printHistory(hitOptions);

            if (!modified) {
                recurse()
            }
        } while (!SudokuResolver.isDone(matrix.toList))

        println(SudokuResolver.isDone(matrix.toList))
        SudokuResolver.printSudoku(matrix.toList);

        SudokuResolver.isDone(matrix.toList)
    }

    /**
     *
     */
    def recurse(): Boolean = {
        var index = 0;

        // If game is not end but no number is obvious, we have to try to fill a number randomly
        var newMatrix = generateNewSudoku(index)
        index = index + 1;

        while (0 != newMatrix.length) {
            // If new Sudoku is able to generated, start recursion.
            val sudoku = SudokuResolver(newMatrix);
            val result = sudoku.play()
            if (result) {
                // If the game is finished without failed.
                matrix = sudoku.matrix;
                return true
            }
            else {
                // try next randomly Sudoku
                newMatrix = generateNewSudoku(index);
                index = index + 1;
            }
        }

        return false
    }

    /**
     *
     */
    def generateNewSudoku(index: Int): Array[Int] = {
        import scala.util.Random;

        var minimal = 9;
        var cubeIndex = 0;
        var number = 0;

        //SudokuResolver.printHistory(hitOptions);

        // Loop the number from 1 to 9 in the cube
        for (n <- 1 to SudokuResolver.group_size) {
            // Check cube[0 to 8]
            for (ci <- 0 until SudokuResolver.group_size) {
                val options: ArrayBuffer[Int] = hitOptions(n - 1)(ci);
                if (null != options) {
                    if (options.size > 0) {
                        if (minimal > options.size) {
                            minimal = options.size;
                            number = n;
                            cubeIndex = ci;
                        }
                    }
                }
            }
        }

        if (0 == number) {
            return Array();
        }

        val options: ArrayBuffer[Int] = hitOptions(number - 1)(cubeIndex);
        if (options.length <= 0)
            return Array();

        //val i = Random.nextInt(1 /*options.length*/ );
        //val option = options.remove(i)
        val option = options(index)

        val newMatrix = matrix.clone();

        println("Old sudoku:")
        SudokuResolver.printSudoku(matrix.toList);
        println("New sudoku:")
        SudokuResolver.printSudoku(newMatrix.toList);

        newMatrix(SudokuResolver.localToGlobe(cubeIndex, option)) = number;
        newMatrix
    }

    /**
     * Store options for number
     */
    def saveOption(cubeMask: Cube, number: Int) = {
        def optionList(list: List[Int]): ArrayBuffer[Int] = {
            val options = ArrayBuffer[Int]();
            var from = cubeMask.list.indexWhere({ p => p == 0 });
            while (-1 != from) {
                options += from;
                from = cubeMask.list.indexWhere({ p => p == 0 }, from + 1);
            }

            options;
        }
        hitOptions(number - 1)(cubeMask.index) = optionList(cubeMask.list);
    }

    /**
     * Check a cube
     *
     * cube: the cube which is going to be check
     * n: the number is interested
     */
    def fillNumber(cube: Cube, n: Int): Cube = {
        val cubeMask: Array[Int] = makeMask(cube.list, n);
        if (cubeMask.contains(0)) {
            // If this cube has place(s) available for this number.
            // Check horizontal
            checkHorizontal(cube, cubeMask, n)

            // Check vertical
            checkVertical(cube, cubeMask, n);
        }

        //println(cubeMask.toList)
        Cube(cube.index, cubeMask.toList)
    }

    /**
     * Check horizontal lines
     */
    def checkHorizontal(cube: Cube, cubeMask: Array[Int], n: Int) = {
        getRowIndexesFromCubeIndex(cube.index).foreach { x =>
            {
                val line: Line = SudokuResolver.getLine(matrix.toList, x, SudokuResolver.group_size);
                Utils.toSdkGroup(line.list) match {
                    case SdkGroup(p0, p1, p2, p3, p4, p5, p6, p7, p8) if (p0 == n || p1 == n || p2 == n || p3 == n || p4 == n || p5 == n || p6 == n || p7 == n || p8 == n) =>
                        cubeMask(x % SudokuResolver.unit_size * SudokuResolver.unit_size) = 1;
                        cubeMask(x % SudokuResolver.unit_size * SudokuResolver.unit_size + 1) = 1;
                        cubeMask(x % SudokuResolver.unit_size * SudokuResolver.unit_size + 2) = 1;
                    case _ =>
                        cubeMask;
                }
            }
        }
    }

    /**
     * Check vertical lines
     */
    def checkVertical(cube: Cube, cubeMask: Array[Int], n: Int) = {
        getColumnIndexesFromCubeIndex(cube.index).foreach { x =>
            {
                val line: Column = SudokuResolver.getColumn(matrix.toList, x, SudokuResolver.group_size);
                Utils.toSdkGroup(line.list) match {
                    case SdkGroup(p0, p1, p2, p3, p4, p5, p6, p7, p8) if (p0 == n || p1 == n || p2 == n || p3 == n || p4 == n || p5 == n || p6 == n || p7 == n || p8 == n) =>
                        cubeMask(x % SudokuResolver.unit_size) = 1;
                        cubeMask(x % SudokuResolver.unit_size + 3) = 1;
                        cubeMask(x % SudokuResolver.unit_size + 6) = 1;
                    case _ =>
                        cubeMask;
                }
            }
        }
    }

    /**
     * Make a mask.
     *
     * value 0 in the mask means it's available for number n, 1 means unavailable or occupied
     *
     * list: data set
     * n: the number we are going to be applied
     */
    def makeMask(list: List[Int], n: Int): Array[Int] = {
        val mask: Array[Int] = Array[Int](1, 1, 1, 1, 1, 1, 1, 1, 1);
        if (!list.contains(n)) {
            var i: Int = 0;
            list.foreach { x =>
                mask(i) = compare(x, n);
                i = i + 1;
            }
        }

        mask;
    }

    /**
     * x: number in the cube
     * n: which number we are looking for
     */
    def compare(x: Int, n: Int) = (x != 0) match {
        case true => 1;
        case _ => 0;
    }

    /**
     * Get row numbers where the cube across
     */
    def getRowIndexesFromCubeIndex(index: Int): List[Int] = (index / 3) match {
        case (0) => List(0, 1, 2)
        case (1) => List(3, 4, 5)
        case (2) => List(6, 7, 8)
    }

    /**
     * Get column numbers where the cube across
     */
    def getColumnIndexesFromCubeIndex(index: Int): List[Int] = (index % 3) match {
        case (0) => List(0, 1, 2)
        case (1) => List(3, 4, 5)
        case (2) => List(6, 7, 8)
    }
}