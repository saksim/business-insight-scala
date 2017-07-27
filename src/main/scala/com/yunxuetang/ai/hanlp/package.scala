package com.yunxuetang.ai

import com.hankcs.hanlp.dictionary.CustomDictionary

import scala.io.Source

package object hanlp {

  def loadCustomDictionary(): Unit = {
    val dict = ClassLoader.getSystemResource("yunxuetang.dict")
    for (line <- Source.fromURL(dict).getLines()) {
      line.split(" +") match {
        case Array(word, postag, freq) =>
          CustomDictionary.add(word, s"$postag $freq")
        case Array(word, postag) =>
          CustomDictionary.add(word, s"$postag 1024")
        case Array(word) =>
          CustomDictionary.add(word, "n 1024")
        case _ =>
      }
    }
  }


  def isNounPosTag(tag: String) = tag(0) == 'n'

  def isVerbPosTag(tag: String) = tag(0) == 'v'

}