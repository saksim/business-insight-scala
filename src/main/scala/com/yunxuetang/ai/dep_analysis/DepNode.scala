package com.yunxuetang.ai.dep_analysis

import com.yunxuetang.ai.ability.MasterVerbSet
import com.yunxuetang.ai.ability.NonBranchClauseBeginWords
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

  def root: DepNode = {
    parent.map(_.root).getOrElse(this)
  }

  def ancestors: Seq[DepNode] = parent.toSeq.flatMap(_.ancestors)

  def findById(nodeId: Int): Option[DepNode] = {
    root.collect_nodes().find(_.id == nodeId)
  }

  def compactId(): Unit = {
    val nodes = collect_nodes().sortBy(_.id)
    val dict = nodes.map(_.id).zipWithIndex.toMap
    for (n <- nodes) {
      n.id = dict(n.id)
    }
    this._correctParent()
  }

  def _correctParent(): Unit = {
    children.foreach(_._correctParent())
    for (c <- children) {
      c.parent = this.some
    }
  }

  def trimSpaceWP: Option[DepNode] = (deprel, word) match {
    case ("WP", " ") => None
    case _ => Some(copyWithChildren(children.flatMap(_.trimSpaceWP.toSeq)))
  }

  def copyWithChildren(newChildren: List[DepNode]): DepNode = {
    val me = copy(children = newChildren)
    for (c <- me.children) {
      c.parent = Some(me)
    }
    me
  }

  def addChild(c: DepNode): Boolean = {
    if (ancestors.exists(_.id == c.id)) {
      false
    } else {
      c.detach()
      children = c :: children.filterNot(_.id == c.id)
      c.parent = this.some
      true
    }
  }

  def addChildren(cs: Seq[DepNode]): Boolean = {
    cs.map(addChild).forall(_ == true)
  }

  def removeChild(id: Int): DepNode = {
    children = children.filterNot(_.id == id)
    this
  }

  def trimRedundantCC: Option[DepNode] = {
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
          Seq(n.copyWithChildren(cs))
      }
    }

    removeCC(this).headOption
  }

  def deepCopy: DepNode = {
    val x = copyWithChildren(children.map(_.deepCopy))
    x.parent = None
    x
  }

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

  def show(shiftCount: Int = 0, traceToken: String = ""): Unit = {
    printf(s"%s%2d:%3s  [%4s]  %s%s ${parent.map(_.id)}\n", " " * shiftCount, id, deprel, postag, word, traceToken)
    for (child <- children) {
      val err = if (child.parent.map(_.id).getOrElse(-1) != id) "*" else ""
      child.show(shiftCount = shiftCount + 8, err)
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

  def isFOB: Boolean = children.exists(_.deprel == "FOB")

  def isSBV: Boolean = children.exists(_.deprel == "SBV")

  def isCMP: Boolean = children.exists(_.deprel == "CMP")

  def isSbvVob: Boolean = isVOB && isSBV

  def isClause: Boolean = isVOB || isSBV || isFOB

  def isYou: Boolean = postag == "vyou" || MasterVerbSet.contains(word)


  def reform_for_vob_subject: Option[DepNode] = {
    val nonVOBs = children.filter(x => x.isLevelAtt || x.deprel != "VOB")
    val VOBs = children.filter(_.deprel == "VOB").map { x =>
      x.copyWithChildren(x.children.flatMap(_.trimLevelAtt))
    }
    copyWithChildren(nonVOBs ++ VOBs).some
  }

  def reform_for_cmp_subject: Option[DepNode] = {
    copyWithChildren(children.filter(_.isNonLevelAtt)).some
  }

  def reform_for_sbv_subject: Option[DepNode] = {
    copyWithChildren(children.filter(_.isNonLevelAtt)).some
  }

  def reform_for_noun_subject: Option[DepNode] = {
    copyWithChildren(children.filter(_.isNonLevelAtt)).some
  }

  def reform_for_subject: DepNode = {
    copyWithChildren(children.filter(x => x.deprel != "ATT" || x.postag != "d"))
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
      if (isYou) Seq() else Seq(copyWithChildren(Nil)),
      children.filter(_.deprel == "VOB").flatMap(_.children.filter(_.isLevelAtt)),
      Seq(copyWithChildren(Nil))
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
      Seq(copyWithChildren(Nil))
    ).flatten.headOption
  }

  def reform_for_cmp_level: Option[DepNode] = {
    Seq(
      children.filter(_.deprel == "CMP"),
      Seq(copyWithChildren(Nil))
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
      if (postag == "nx" && p.deprel == "HED") {
        false
      } else if (p.deprel == "HED" && children.empty) {
        false
      } else if (p.deprel == "HED" && NonBranchClauseBeginWords.contains(word)) {
        false
      } else if (p.isClause && isClause) {
        true
      } else if (p.deprel == "ATT" && p.isVerb && isVerb) {
        false
      } else if (isSubOf(List("VOB", "SBV"))) {
        true
      } else {
        (p.deprel, p.postag, postag) match {
          case (_, _, ptag) if isNounPosTag(ptag) => true
          case (_, _, "vyou") => true
          case _ => false
        }
      }
    case _ =>
      false
  }

  def split_coo(): Seq[DepNode] = {
    for {
      branch <- split_multi_vob
      coo <- branch._split()
      x <- coo.trimRedundantCC
    } yield x
  }

  private def split_multi_vob: Seq[DepNode] = {
    val vobs = children.filter(_.deprel == "VOB")
    for (x <- vobs) {
      x.detach()
    }

    for (x <- vobs) yield {
      val cp = deepCopy
      cp.addChild(x.deepCopy)
      cp
    }
  }

  private def _split(): List[DepNode] = {
    if (children.isEmpty) {
      List(copyWithChildren(Nil))
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

  def detach(): Unit = {
    parent.foreach(_.removeChild(id))
    parent = None
  }

  /// 合并相邻的NX(英文单词)
  def mergeNeighbourNX: Option[DepNode] = {

    def _merge(root: DepNode): Unit = {
      val nodeMap = root.collect_nodes().map(x => x.id -> x).toMap
      val nextNode = nodeMap.find {
        case (_, node) if node.postag != "nx" || node.deprel == "HED" => false
        case (nid, _) if !nodeMap.contains(nid - 1) => false
        case (nid, _) => nodeMap(nid - 1).postag == "nx"
      }.map(_._2)

      for (n <- nextNode) {
        for (p <- nodeMap.get(n.id - 1)) {
          n.children = n.children.filterNot(_.id == p.id)
          p.children = p.children.filterNot(_.id == n.id)
          val newWord = p.word + " " + n.word
          if (p.ancestors.exists(_.id == n.id)) {
            p.detach()
            n.word = newWord
            n.addChildren(p.children)
          } else {
            n.detach()
            p.word = newWord
            p.addChildren(n.children)
          }
          root._correctParent()
          root.compactId()
          _merge(root)
        }
      }
    }

    for (n <- trimSpaceWP) yield {
      n.compactId()
      _merge(n)
      n
    }
  }

  def reform_for_nx(): Unit = {
    val dict = collect_nodes().map(x => x.id -> x).toMap
    val subCoo = dict.find {
      case (nid, node) if hanlp.isNounPosTag(node.postag) && node.deprel == "COO" =>
        dict.get(nid - 1).exists(_.word == "､") &&
          dict.get(nid - 2).exists(x => hanlp.isNounPosTag(x.postag) && x.children.find(_.id == nid).empty)
      case _ => false
    }.map(_._2)

    for (n <- subCoo) {
      for (p <- dict.get(n.id - 2)) {
        if (p.ancestors.exists(_.id == n.id)) {
          n.addChild(p)
        } else {
          p.addChild(n)
        }
        reform_for_nx()
      }
    }
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
          p.addChild(node)
        case None =>
          node.parent = None
      }
      node
    }
    val roots = nodes2.filter(_.parent.isEmpty)
    val roots2 = roots.flatMap(_.mergeNeighbourNX)
    roots2.foreach(_.reform_for_nx())
    roots2
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