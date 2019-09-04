package collections

import org.scalacheck.{ Arbitrary, Gen }

sealed trait CompanyRole {
  def id: String
  final def roleName: String = getClass.toString
}

final case class Employee(id: String) extends CompanyRole {
  def takeVacation(): Unit = println("taking a vacation")
}

object Employee {
  val gen: Gen[Employee] = Arbitrary.arbitrary[String].map(Employee.apply)

  implicit val arbitrary: Arbitrary[Employee] = Arbitrary(gen)
}

final case class Consultant(id: String, companyName: String) extends CompanyRole {
  def submitInvoice(): Unit = println("here is my invoice")
}

object Consultant {
  val gen: Gen[Consultant] = {
    val g: Gen[String] = Arbitrary.arbitrary[String]

    for {
      id <- g
      companyName <- g
    } yield Consultant(id, companyName)
  }

  implicit val arbitrary: Arbitrary[Consultant] = Arbitrary(gen)
}
