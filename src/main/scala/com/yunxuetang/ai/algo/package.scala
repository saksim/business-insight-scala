package com.yunxuetang.ai

import scala.annotation.tailrec

package object algo {

  import scala.math.min

  type Slice = (Long, Long)

  def cutIntoSlice(r: Slice, sliceLen: Long): List[Slice] = {

    @tailrec
    def _cutIntoSlice(r: Slice, acc: List[Slice]): List[Slice] = r match {
      case (begin, end) if begin >= end => acc.reverse
      case (begin, end) =>
        val newEnd = min(end, begin + sliceLen)
        val slice = (begin, newEnd)
        val newLeft = (newEnd, end)
        _cutIntoSlice(newLeft, slice :: acc)
    }

    _cutIntoSlice(r, Nil)
  }


}
