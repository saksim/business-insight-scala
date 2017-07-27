package com.yunxuetang.ai.hanlp

import com.hankcs.hanlp.HanLP
import com.hankcs.hanlp.corpus.dependency.CoNll.CoNLLWord
import com.hankcs.hanlp.dependency.MaxEntDependencyParser
import com.yunxuetang.ai.dep_analysis.DepNode

import scala.collection.JavaConverters._

object DepParser {

  import com.yunxuetang.ai.dep_analysis.DepNode.{Attr => DepNodeAttr}

  def createAttr(word: CoNLLWord): DepNodeAttr = Map(
    "id" -> word.ID,
    "word" -> word.LEMMA,
    "deprel" -> standardDepRel(word.DEPREL),
    "postag" -> word.POSTAG,
    "parent" -> word.HEAD.ID
  )

  val zhToEnDepRelMap = Map(
    "主谓关系" -> "SBV",
    "介宾关系" -> "POB",
    "兼语" -> "DBL",
    "前置宾语" -> "FOB",
    "动宾关系" -> "VOB",
    "动补结构" -> "CMP",
    "右附加关系" -> "RAD",
    "定中关系" -> "ATT",
    "左附加关系" -> "LAD",
    "并列关系" -> "COO",
    "标点符号" -> "WP",
    "核心关系" -> "HED",
    "状中结构" -> "ADV",
    "独立结构" -> "IS",
    "间宾关系" -> "IOB"
  )

  def standardDepRel(zhDep: String): String = zhToEnDepRelMap(zhDep)

  def parse(text: String): Seq[DepNode] = {
    val sentence = HanLP.parseDependency(text)
//    val sentence = MaxEntDependencyParser.compute(text)
    DepNode.buildDepTrees(sentence.asScala.map(createAttr).toList)
  }

}
