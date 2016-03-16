package com.wang.hibiup.sudoku

object Direction extends Enumeration {
    val HORIZONTAL = Value;
    val VERTICAL = Value;
    val CUBE = Value;
}

abstract class Group(t: Direction.Value, index: Int, list: List[Int]);
case class Cube(index: Int, list: List[Int]) extends Group(Direction.CUBE, index, list);
case class Line(index: Int, list: List[Int]) extends Group(Direction.HORIZONTAL, index, list);
case class Column(index: Int, list: List[Int]) extends Group(Direction.VERTICAL, index, list);

case class Option(number: Int, options: List[Int])

case class SdkGroup(p0: Int, p1: Int, p2: Int, p3: Int, p4: Int, p5: Int, p6: Int, p7: Int, p8: Int)

object Utils {
    def toSdkGroup(group: List[Int]): SdkGroup = {
        SdkGroup(group(0), group(1), group(2), group(3), group(4), group(5), group(6), group(7), group(8))
    }
}