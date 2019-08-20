package collections

sealed trait CompanyRole {
  def id: String
  final def roleName: String = getClass.toString
}

final case class Employee(id: String) extends CompanyRole {
  def takeVacation(): Unit = println("taking a vacation")
}

final case class Consultant(id: String, companyName: String) extends CompanyRole {
  def submitInvoice(): Unit = println("here is my invoice")
}
