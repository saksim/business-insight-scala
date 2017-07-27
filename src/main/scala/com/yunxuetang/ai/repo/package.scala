package com.yunxuetang.ai


package object repo {
  import slick.jdbc.MySQLProfile.api.Database
  val db = Database.forConfig("octopus")
}
