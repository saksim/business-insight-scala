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

  var basicItems = Seq(
//    "熟练各种办公软件操作",
//    "具有较强的分析能力和项目管理能力"
//    "熟练运用主流的移动端JS库和开发框架,例如:JQUERY MOBILE､ANGULAR､REACTJS､BOOTSTRAP等",
    "具备一定的数据分析能力,如DATA MINING、TEXT MINING、NATURAL LANGUAGE PROCESSING、SENTENCE SYNTHESIZING 等"
//    "熟悉AIX､SOLARIS､LINUX任一操作系统"
//    "熟悉发动机开发流程及工作原理"
  ).zipWithIndex.map {
    case (text, index) =>
      val id = index.toLong + 1L
      JobReqItem(id = id.some, job_id = id, item = text)
  }

  def main(args: Array[String]): Unit = {
    loadCustomDictionary()

//    println(HanLP.extractPhrase("具有较强的分析能力和项目管理能力", 5))

    val slices = cutIntoSlice((100L, 1000L), 100L)
//    slices.map { case (beg, end) =>
//      val f = getReqItem(beg, end).flatMap(sinkAbility)
//      val f = getReqItem(beg, end).map(_.map(abilitiesOf))
      val f = Future { basicItems.map(abilitiesOf) }
      val insertCount = Await.result(f, Duration.Inf)
      println(insertCount)
      insertCount
//    }
  }

}
