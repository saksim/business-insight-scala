package com.yunxuetang.ai.dep_analysis

import com.yunxuetang.ai.ability.MasterVerbSet
import com.yunxuetang.ai.{ability, hanlp}
import com.yunxuetang.ai.hanlp.isNounPosTag

import scalaz.Scalaz._

case class DepNode(var id: Int,
                   var word: String,
                   var deprel: String,
                   var postag: String,
                   var parent: Option[DepNode] = None,
                   var children: List[DepNode] = Nil) {

  import DepNode.choose

  def copyWithChildren(newChildren: List[DepNode]): DepNode = {
    val me = copy(children = newChildren)
    for (c <- children) {
      c.parent = Some(this)
    }
    me
  }

  def addChild(c: DepNode): DepNode = {
    children = c :: children
    c.parent = Some(this)
    this
  }


  def removeRedundantCC: Option[DepNode] = {
    val allIds = collect_nodes().map(_.id).toSet

    def nonExists(id: Int): Boolean = !allIds.contains(id)

    def removeCC(n: DepNode): Seq[DepNode] = {
      val cs = n.children.flatMap(removeCC)
      (n.deprel, n.postag) match {
        case ("LAD", "cc") if nonExists(n.id - 1) =>
          cs
        case ("LAD", "c") if nonExists(n.id - 1) =>
          cs
        case ("RAD", "cc") if nonExists(n.id + 1) =>
          cs
        case (_, "w") if nonExists(n.id - 1) || nonExists(n.id + 1) =>
          cs
        case _ =>
          Seq(n.copy(children = cs))
      }
    }

    removeCC(this).headOption
  }

  def deepCopy: DepNode = copyWithChildren(children.map(_.deepCopy))

  def de: Option[DepNode] = {
    children.find {
      case DepNode(_, _, "ATT", "a", _, List(DepNode(_, _, "RAD", "ude1", _, _))) => true
      case DepNode(_, _, "ATT", "d", _, List(DepNode(_, _, "RAD", "ude1", _, _))) => true
      case _ =>
        false
    }
  }

  def attSeq: Seq[DepNode] = {
    children.filter(_.deprel == "ATT")
  }

  def borrowDe(): DepNode = {
    if (de.isEmpty) {
      for (d <- parent.flatMap(_.de)) {
        addChild(d.deepCopy)
      }
    }
    this
  }

  def borrowAtt(): DepNode = {
    if (ability.isGeneralAbility(this)) {
      for (att <- parent.toSeq.flatMap(_.attSeq)) {
        addChild(att.deepCopy)
      }
    }
    this
  }

  def collect_nodes(): List[DepNode] = {
    this :: children.flatMap(_.collect_nodes())
  }

  override def toString: String = {
    import DepNode.joinWord
    val words = collect_nodes().sortBy(_.id).dropWhile { x =>
      x.deprel == "LAD" || x.postag == "cc"
    }.map(_.word.trim).filter(_.nonEmpty)

    joinWord(words)
  }

  def show(shiftCount: Int = 0): Unit = {
    printf("%s%2d:%3s  [%4s]  %s\n", " " * shiftCount, id, deprel, postag, word)
    for (child <- children) {
      child.show(shiftCount = shiftCount + 8)
    }
  }

  def subject: Option[String] = {
    if (isSbvVob) {
      toString.some
    } else if (isVOB) {
      reform_for_vob_subject.map(_.toString)
    } else if (isNoun) {
      reform_for_noun_subject.map(_.toString)
    } else if (isSBV) {
      reform_for_sbv_subject.map(_.toString)
    } else if (isCMP) {
      reform_for_cmp_subject.map(_.toString)
    } else {
      None
    }
  }

  def isNoun: Boolean = children.all(_.deprel == "ATT")

  def isVOB: Boolean = children.exists(_.deprel == "VOB")

  def isSBV: Boolean = children.exists(_.deprel == "SBV")

  def isCMP: Boolean = children.exists(_.deprel == "CMP")

  def isSbvVob: Boolean = isVOB && isSBV

  def isYou: Boolean = postag == "vyou" || MasterVerbSet.contains(word)


  def reform_for_vob_subject: Option[DepNode] = {
    val nonVOBs = children.filter(x => x.isLevelAtt || x.deprel != "VOB")
    val VOBs = children.filter(_.deprel == "VOB").map { x =>
      x.copy(children = x.children.flatMap(_.trimLevelAtt))
    }
    copy(children = nonVOBs ++ VOBs).some
  }

  def reform_for_cmp_subject: Option[DepNode] = {
    copy(children = children.filter(_.isNonLevelAtt)).some
  }

  def reform_for_sbv_subject: Option[DepNode] = {
    copy(children = children.filter(_.isNonLevelAtt)).some
  }

  def reform_for_noun_subject: Option[DepNode] = {
    copy(children = children.filter(_.isNonLevelAtt)).some
  }

  def reform_for_subject: DepNode = {
    copy(children = children.filter(x => x.deprel != "ATT" || x.postag != "d"))
  }

  def level: Option[String] = {
    if (isSbvVob) {
      "符合".some
    } else if (isVOB) {
      reform_for_vob_level.map(_.toString)
    } else if (isNoun) {
      reform_for_noun_level.map(_.toString) match {
        case None => "具备".some
        case level => level
      }
    } else if (isSBV) {
      reform_for_sbv_level.map(_.toString)
    } else if (isCMP) {
      reform_for_cmp_level.map(_.toString)
    } else {
      None
    }
  }

  def isLevelAtt: Boolean = {
    (deprel == "ATT" && Set("a", "d", "b").contains(postag)) ||
      (deprel == "ADV" && postag != "p")
  }

  def isNonLevelAtt: Boolean = !isLevelAtt


  def trimLevelAtt: Option[DepNode] = {
    if (isLevelAtt) {
      None
    } else {
      this.some
    }
  }


  def reform_for_vob_level: Option[DepNode] = {
    List(
      children.filter(_.isLevelAtt),
      if (isYou) Seq() else Seq(copy(children = Nil)),
      children.filter(_.deprel == "VOB").map(_.children.filter(_.isLevelAtt)).flatten,
      Seq(copy(children = Nil))
    ).flatten.headOption
  }

  def reform_for_noun_level: Option[DepNode] = {
    List(
      children.filter(_.isLevelAtt)
    ).flatten.headOption
  }

  def reform_for_sbv_level: Option[DepNode] = {
    List(
      children.filter(_.isLevelAtt),
      Seq(copy(children = Nil))
    ).flatten.headOption
  }

  def reform_for_cmp_level: Option[DepNode] = {
    Seq(
      children.filter(_.deprel == "CMP"),
      Seq(copy(children = Nil))
    ).flatten.headOption
  }

  def isSubOf(depRelSeq: Seq[String]): Boolean = parent match {
    case None => false
    case Some(p) =>
      if (depRelSeq.contains(p.deprel)) {
        true
      } else {
        p.isSubOf(depRelSeq)
      }
  }

  def isVerb: Boolean = hanlp.isVerbPosTag(postag)

  def isSplitableCoo: Boolean = (parent, deprel) match {
    case (Some(p), "COO") =>
      if (p.deprel == "ATT" && p.isVerb && isVerb) {
        false
        //      } else if (p.deprel) {

      } else if (isSubOf(List("VOB", "SBV"))) {
        true
      } else {
        (p.deprel, p.postag, postag) match {
          case (_, _, postag) if isNounPosTag(postag) => true
          case (_, _, "vyou") => true
          case _ => false
        }
      }
    case _ =>
      false
  }

  def split_coo(): List[DepNode] = _split().flatMap(_.removeRedundantCC)

  private def _split(): List[DepNode] = {
    if (children.isEmpty) {
      List(copy())
    } else {
      val (cooSeq, nonCooChildren) = children.partition(_.isSplitableCoo)
      val childrenSeq = choose(nonCooChildren.map(_._split()))
      var result: List[DepNode] = Nil
      if (cooSeq.nonEmpty) {
        result = cooSeq.flatMap(_._split()).map { x =>
          x.deprel = deprel
          x.borrowDe()
          x.borrowAtt()
          x
        }
      }
      result ++ childrenSeq.map(copyWithChildren)
    }
  }

  def nonValidBranch: Boolean = {
    collect_nodes().size < 2
  }

  def reform: Option[DepNode] = (word, deprel, postag) match {
    case (_, "LAD", "cc") => None
    case (_, "LAD", "c") => None
    case (_, "RAD", "udeng") => None
    case (w, "WP", _) if w != "++" => None
    case _ =>
      Some(copyWithChildren(children.flatMap(_.reform)))
  }


}

object DepNode {
  type Attr = Map[String, Any]

  def createNode(attr: Attr): DepNode = {
    DepNode(
      id = attr("id").asInstanceOf[Int],
      word = attr("word").asInstanceOf[String],
      deprel = attr("deprel").asInstanceOf[String],
      postag = attr("postag").asInstanceOf[String]
    )
  }

  def buildDepTrees(attrSeq: List[Attr]): Seq[DepNode] = {
    val nodes = attrSeq.map(DepNode.createNode)
    val nodes2 = nodes.zip(attrSeq).map { case (node, attrs) =>
      nodes.find(_.id == attrs("parent")) match {
        case Some(p) =>
          node.parent = Some(p)
          p.children = node :: p.children
        case None =>
          node.parent = None
      }
      node
    }
    nodes2.filter(_.parent.isEmpty)
  }

  def isAscii(ch: Char): Boolean = ch.toInt < 128

  def joinWord(words: Seq[String]): String = words match {
    case Nil => ""
    case Seq(word) => word
    case _ =>
      val left = joinWord(words.tail)
      val head = words.head
      if (isAscii(head.last) && isAscii(left.head)) {
        if (left.head >= '0' && left.head <= '9') {
          head + left
        } else {
          head + " " + left
        }
      } else {
        head + left
      }
  }

  def choose(xss: List[List[DepNode]]): List[List[DepNode]] = xss match {
    case Nil => List(Nil)
    case List(xs) => xs.map(List(_))
    case head :: tail =>
      val tails = choose(tail)
      head.flatMap { x =>
        tails.map(x :: _)
      }
  }

}