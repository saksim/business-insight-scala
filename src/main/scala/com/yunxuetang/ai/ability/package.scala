package com.yunxuetang.ai

import com.yunxuetang.ai.dep_analysis.DepNode
import com.yunxuetang.ai.hanlp.{DepParser => HanLPDepParser}
import com.yunxuetang.ai.regular.{RemoveHeadCC, TrimPriority}
import com.yunxuetang.ai.repo.{JobAbility, JobReqItem}

package object ability {

  val MasterVerbSet = Set(
    "具备", "具有", "掌握"
  )

  val GeneralAbilitiesSet = Set(
    "开发",
    "流程",
    "工作",
    "原理",
    "技术",
    "特性",
    "工具",
    "技术",
    "产品"
  )

  val NonBranchClauseBeginWords = Set(
    "如",
    "例如"
  )

  def isGeneralAbility(node:DepNode) :Boolean = {
    node.collect_nodes().filter(_.deprel == "ATT").forall {n => GeneralAbilitiesSet.contains(n.word) }
  }

  def abilityOf(item: JobReqItem, tree: DepNode): Option[JobAbility] = {
    for {
      entity <- tree.subject
      level <- tree.level
    } yield
      JobAbility(id = None,
        item_id = item.id.get,
        job_id = item.job_id,
        entity = entity,
        level = level
      )
  }

  def abilitiesOf(item: JobReqItem): Seq[JobAbility] = {
    val regular = new TrimPriority with RemoveHeadCC
    val roots = regular.execute(item.item).flatMap(HanLPDepParser.parse)

//    for (elem <- roots) {
//      elem.show()
//    }


    val branches = roots.flatMap(_.split_coo())

//    for (elem <- branches) {
//      elem.show()
//    }

    val abilities = branches.flatMap(abilityOf(item, _)).filterNot(_.entity.endsWith("例如"))
    showAbilities(item, abilities)
    abilities
  }

  def showAbilities(item: JobReqItem, abilities: Seq[JobAbility]): Unit = {
    println(s"[${item.id.get}]${item.item}")
    for (ab <- abilities) {
      println(s"\t\t${ab.entity}:${ab.level}")
    }
    println("-"*120)
  }


}
