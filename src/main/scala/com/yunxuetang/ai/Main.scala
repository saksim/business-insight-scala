package com.yunxuetang.ai

import java.util.concurrent.Executors

import scala.annotation.tailrec
import com.hankcs.hanlp.HanLP
import com.hankcs.hanlp.dictionary.CustomDictionary
import com.hankcs.hanlp.seg.Viterbi.ViterbiSegment
import com.yunxuetang.ai.ability.abilitiesOf
import com.yunxuetang.ai.algo.cutIntoSlice
import com.yunxuetang.ai.repo.Slick.getReqItem
import com.yunxuetang.ai.repo.{JobReqItem, db, jobAbilityTable}
import scala.collection.parallel.ParSeq

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scalaz.Scalaz._
import com.yunxuetang.ai.hanlp.loadCustomDictionaries

object Main {

  import slick.jdbc.MySQLProfile.api._

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))


  //  def abilityTableWithId =
  //    (jobAbilityTable returning jobAbilityTable.map(_.id)).into((ability, id) => ability.copy(id = id.some))

  def sinkAbility(items: Seq[JobReqItem]) = db.run {
    jobAbilityTable ++= items.flatMap(abilitiesOf)
  }

  def showAbilities(): Unit = Seq(
    //    "熟练各种办公软件操作",
    //    "具有较强的分析能力和项目管理能力",
    //    "熟练运用主流的移动端JS库和开发框架,例如:JQUERY MOBILE､ANGULAR､REACTJS､BOOTSTRAP等",
    //    "具备一定的数据分析能力,如DATA MINING、TEXT MINING、NATURAL LANGUAGE PROCESSING、SENTENCE SYNTHESIZING 等",
    //    "熟悉AIX､SOLARIS､LINUX任一操作系统",
    //    "熟悉发动机开发流程及工作原理",
    //    "精通HTML5､CSS3､JAVASCRIPT等WEB前端开发技术",
    //    "优秀的沟通与表达能力"
    //    "计划､组织､沟通､协调能力强"
    //    "熟悉HADOOP、HIVE、STORM 等相关环境和工作原理"
    //    "熟练使用至少一种常用统计工具:SAS､R､PYTHON､SQL等"
    //    "责任心强､具备团队合作精神"
    //    "熟知各大搜索引擎的排名原理和技术特性"
    //    "熟悉MICROSOFT OFFICE系列"
    //    "有团队合作精神"
    //        "熟悉大数据处理架构"
    //    "对大数据技术体系､关键技术､发展趋势有深刻的理解和认识"
    //    "能够独立搭建LINUX开发环境,如MYSQL、PHP、APACHE等常用软件"
    //    "少精通一门编程语言,如JAVA、C++或PYTHON"
    //    "团队打造能力､沟通能力､良好的执行力及职业化素养"
    //    "熟练使用SEO优化各种工具和技术"
    //    "阅读和理解能力强"
    //    "熟悉掌握C､C++语言编程"
    //    "具备JAVA WEB､数据库开发基础者优先"
    //    "良好的沟通技巧和团队合作精神"
    //    "具有创意策划力",
    //    "懂得肉食品冷藏存放技术"
    "发现并解决问题的能力"
  ).zipWithIndex.map {
    case (text, index) =>
      val id = index.toLong + 1L
      JobReqItem(id = id.some, job_id = id, item = text)
  }.map(abilitiesOf)

  @tailrec
  def group4(spans: Seq[(Long, Long)], acc: List[Seq[(Long, Long)]] = Nil): List[Seq[(Long, Long)]] = {
    if (spans.size <= 4) {
      val newAcc = spans :: acc
      newAcc.reverse
    } else {
      val segment = spans.take(3)
      group4(spans.drop(3), segment :: acc)
    }
  }

  def procSpan(span: (Long, Long)): Unit = span match {
    case (beg, end) =>
      val f = getReqItem(beg, end).flatMap(sinkAbility)
      //      val f = getReqItem(beg, end).map(_.map(abilitiesOf))
      Await.result(f, Duration.Inf)

  }

  def procDb(): Unit = {
    val slices = cutIntoSlice((1L, 9579541L), 100L)
    val groupSlices = group4(slices)
    //    val parSlices = ParSeq(slices :_*)
    for (group <- groupSlices) {
      val parSpan = ParSeq(group: _*)
      parSpan.map(procSpan)
    }

    //    groupSlices.map { case (beg, end) =>
    //      val f = getReqItem(beg, end).flatMap(sinkAbility)
    //      //      val f = getReqItem(beg, end).map(_.map(abilitiesOf))
    //      Await.result(f, Duration.Inf)
    //    }
  }

  def printItems(): Unit = {
    val slices = cutIntoSlice((11042661L, 11042661L + 1000L), 100L)
    for ((beg, end) <- slices) {
      val f = getReqItem(beg, end).map(_.map(_.item))
      for (item <- Await.result(f, Duration.Inf)) {
        println(item)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    loadCustomDictionaries()
    //    printItems()
    procDb()
    //    showAbilities()
  }

}
