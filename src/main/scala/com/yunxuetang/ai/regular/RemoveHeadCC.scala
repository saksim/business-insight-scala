package com.yunxuetang.ai.regular

import com.hankcs.hanlp.HanLP

trait RemoveHeadCC extends Regular {

  override def execute(text: String): Seq[String] = {
    import scala.collection.JavaConverters._
    val segs = HanLP.segment(text).asScala
    val newText = segs.dropWhile { x =>
      val postag = x.nature.name()
      postag == "c" || postag == "cc"
    }.map(_.word).mkString
    super.execute(newText)
  }


}
