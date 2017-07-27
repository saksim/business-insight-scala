package com.yunxuetang.ai.regular

object TrimPriority {
  val tails = Array(
    "者优先考虑",
    "等相关工作",
    "者优先",
    "优先考虑",
    "优先",
    "等等",
    "等"
  )

  val starts = Array(
    "优先考虑",
    "优先",
    "专业:"
  )
}

trait TrimPriority extends Regular {

  import TrimPriority.{tails, starts}

  override def execute(text: String): Seq[String] = {

    val text0 = (text /: tails) {
      case (acc, tail) if acc.endsWith(tail) => acc.substring(0, acc.length - tail.length)
      case (acc, _) => acc
    }

    val text1 = (text0 /: starts) {
      case (acc, start) if acc.startsWith(start) => acc.substring(start.length)
      case (acc, _) => acc
    }

    super.execute(text1)
  }

}
