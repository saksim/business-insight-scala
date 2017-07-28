package com.yunxuetang.ai

import java.util.concurrent.Executors

import com.hankcs.hanlp.HanLP
import com.hankcs.hanlp.dictionary.CustomDictionary
import com.yunxuetang.ai.ability.abilitiesOf
import com.yunxuetang.ai.algo.cutIntoSlice
import com.yunxuetang.ai.repo.Slick.getReqItem
import com.yunxuetang.ai.repo.{JobReqItem, db, jobAbilityTable}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scalaz.Scalaz._
import com.yunxuetang.ai.hanlp.loadCustomDictionary

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
    //      "优秀的沟通与表达能力"
    //    "计划､组织､沟通､协调能力强"
    //    "熟悉HADOOP、HIVE、STORM 等相关环境和工作原理"
    "熟练使用至少一种常用统计工具:SAS､R､PYTHON､SQL等"
  ).zipWithIndex.map {
    case (text, index) =>
      val id = index.toLong + 1L
      JobReqItem(id = id.some, job_id = id, item = text)
  }.map(abilitiesOf)

  def procDb(): Unit = {
    val slices = cutIntoSlice((0L, 3000L), 100L)
    slices.map { case (beg, end) =>
      //val f = getReqItem(beg, end).flatMap(sinkAbility)
      val f = getReqItem(beg, end).map(_.map(abilitiesOf))
      Await.result(f, Duration.Inf)
    }
  }

  def printItems(): Unit = {
    val slices = cutIntoSlice((0L, 3000000L), 10000L)
    slices.map { case (beg, end) =>
      val f = getReqItem(beg, end).map(_.map(_.item))
      for (item <- Await.result(f, Duration.Inf)) {
        println(item)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    loadCustomDictionary()
//    printItems()
//    procDb()
    showAbilities()
  }

}
