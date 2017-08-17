package com.yunxuetang.ai.repo

import java.sql.Date

import slick.jdbc.MySQLProfile.api._

import scala.concurrent.Future


/// ==== JOB_REQUIREMENT_ITEM ====
case class JobReqItem(id: Option[Long], job_id: Long, item: String)

class JobReqItems(tag: Tag) extends Table[JobReqItem](tag, "job_requirement_item") {

  def id = column[Long]("id", O.PrimaryKey, O.SqlType("int(11)"), O.AutoInc)

  def job_id = column[Long]("job_id", O.SqlType("int(11)"))

  def item = column[String]("item", O.SqlType("varchar(400)"))

  def * = (id.?, job_id, item) <> (JobReqItem.tupled, JobReqItem.unapply)

}


object jobReqItemTable extends TableQuery(new JobReqItems(_)) {}

/// ==== JOB_ABILITY ====
case class JobAbility(id: Option[Long], item_id: Long, job_id: Long, entity: String, text_level: String, ability: String = "", int_level: Long = -1)

class JobAbilities(tag: Tag) extends Table[JobAbility](tag, "job_ability") {

  def id = column[Long]("id", O.PrimaryKey, O.SqlType("int(11)"), O.AutoInc)

  def item_id = column[Long]("item_id", O.SqlType("int(11)"))

  def job_id = column[Long]("job_id", O.SqlType("int(11)"))

  def entity = column[String]("entity", O.SqlType("varchar(400)"))

  def text_level = column[String]("text_level", O.SqlType("varchar(400)"))

  def ability = column[String]("ability", O.SqlType("varchar(400)"))

  def int_level = column[Long]("int_level", O.SqlType("int(11)"))

  def * = (id.?, item_id, job_id, entity, text_level, ability, int_level) <> (JobAbility.tupled, JobAbility.unapply)

}

object jobAbilityTable extends TableQuery(new JobAbilities(_)) {
}

/// ==== JOB_DETAILS ====
case class JobDetail(id: Option[Long],
                     title: String,
                     location: String,
                     company: String,
                     companyType: String,
                     companyScope: String,
                     numOfEmployee: String,
                     educationRequirement: String,
                     description: String,
                     jobType: String,
                     url: String,
                     publishedAt: String,
                     fetchedAt: Date,
                     requirementExtracted: Int)

class JobDetails(tag: Tag) extends Table[JobDetail](tag, "job_detail") {

  def id = column[Long]("id", O.PrimaryKey, O.SqlType("int(11)"), O.AutoInc)

  def title = column[String]("title")

  def location = column[String]("location")

  def company = column[String]("company")

  def companyType = column[String]("company_type")

  def companyScope = column[String]("company_scope")

  def numOfEmployee = column[String]("num_of_employee")

  def educationRequirement = column[String]("education_requirement")

  def description = column[String]("description")

  def jobType = column[String]("job_type")

  def url = column[String]("url")

  def publishedAt = column[String]("published_at")

  def fetchedAt = column[Date]("fetched_at", O.SqlType("datetime"))

  def requirementExtracted = column[Int]("requirement_extracted", O.SqlType("tinyint(1)"))

  def * = (id.?, title, location, company, companyType, companyScope,
    numOfEmployee, educationRequirement, description,
    jobType, url, publishedAt, fetchedAt,
    requirementExtracted) <> (JobDetail.tupled, JobDetail.unapply)

}

object jobDetailTable extends TableQuery(new JobDetails(_)) {}


object Slick {
  def getReqItem(startId: Long, endId: Long): Future[Seq[JobReqItem]] = {
    val query = jobReqItemTable.filter { x => x.id >= startId && x.id < endId }
    val f = db.run(query.result)
    f
  }
}

object SlickTest {

  import scala.concurrent.Await
  import scala.concurrent.duration.Duration

  def main(args: Array[String]): Unit = {
    val db = Database.forConfig("octopus")

    val insert0 = jobAbilityTable.insertOrUpdate(JobAbility(None, 1L, 1L, "ab", "le"))
    val f0 = db.run(insert0)
    Await.result(f0, Duration.Inf)


    val insert = jobAbilityTable.insertOrUpdate(JobAbility(Some(1), 2L, 2L, "ab", "le"))
    val f = db.run(insert)
    Await.result(f, Duration.Inf)


    println((jobAbilityTable returning jobAbilityTable.map(_.id)).insertStatement)
    println(jobAbilityTable.updateStatement)


  }

}
