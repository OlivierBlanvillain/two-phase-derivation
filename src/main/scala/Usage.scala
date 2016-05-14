package felis.catus

import cats._
import cats.syntax.cartesian._
import felis.catus.ObjectsInstances._
import felis.catus.lib.DisjointCartesianSyntax._
import felis.catus.lib.FreeObjects
import scala.Function.unlift

object Usage extends App {
  sealed trait Pet
  case class Dog(name: String, bones: Int) extends Pet
  case class Cat(name: String, price: Double) extends Pet
  case class Person(name: String, age: Int, pet: Pet)

  val dsl = new BaseAlgebraDsl[BaseAlgebra](); import dsl._

  val dogConfig: FreeObjects[BaseAlgebra, Dog] =
    (string("name") |@| int("bones"))
      .imap(Dog.apply)(unlift(Dog.unapply))

  val catConfig: FreeObjects[BaseAlgebra, Cat] =
    (string("name") |@| double("price"))
      .imap(Cat.apply)(unlift(Cat.unapply))

  val petConfig: FreeObjects[BaseAlgebra, Pet] =
    (dogConfig |#| catConfig).as[Pet]

  val personConfig: FreeObjects[BaseAlgebra, Person] =
    (string("name") |@| int("age") |@| obj("pet")(petConfig))
      .imap(Person.apply)(unlift(Person.unapply))

  val person1 = Person("Olivier", 25, Dog("aslan", 10))
  val person2 = Person("Alicia",  18, Cat("sansan", 1))

  val personShow: Show[Person] = personConfig.foldMap(Compile2Show.naturalTransformation)(showObjects)
  assert(personShow.show(person1) == "name = Olivier, age = 25, pet = { name = aslan, bones = 10 }")
  assert(personShow.show(person2) == "name = Alicia, age = 18, pet = { name = sansan, price = 1.0 }")

  val personMonoid: Monoid[Person] = personConfig.foldMap(Compile2Monoid.naturalTransformation)(monoidObjects)
  assert(personMonoid.empty == Person("", 0, Dog("", 0)))
  assert(personMonoid.combine(person1, person2) == Person("OlivierAlicia", 43, Dog("aslan", 10)))
}
