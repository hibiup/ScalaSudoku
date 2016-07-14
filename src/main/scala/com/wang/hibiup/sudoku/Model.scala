package com.wang.hibiup.sudoku

sealed trait Formation
case object HORIZONTAL extends Formation
case object VERTICAL extends Formation
case object SQUARE extends Formation

abstract class Group(t: Formation, index: Int, list: List[Int]);
case class Cube(index: Int, member: List[Int]) extends Group(SQUARE, index, member);
case class Line(index: Int, member: List[Int]) extends Group(HORIZONTAL, index, member);
case class Column(index: Int, member: List[Int]) extends Group(VERTICAL, index, member);
case class Option(number: Int, options: List[Int])
case class SdkGroup(p0: Int, p1: Int, p2: Int, p3: Int, p4: Int, p5: Int, p6: Int, p7: Int, p8: Int)

object Utils {
    def toSdkGroup(group: List[Int]): SdkGroup = {
        SdkGroup(group(0), group(1), group(2), group(3), group(4), group(5), group(6), group(7), group(8))
    }
}