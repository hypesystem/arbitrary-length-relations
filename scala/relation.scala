import scala.language.implicitConversions

sealed trait RelationBool[+A] {
  def evaluate[B >: A, C >: B] (y: RelationBool[B], comparisonEvaluator: Int=>Boolean, ordering: math.Ordering[C]) : RelationBool[B] = (this, y) match {
    case (PossiblyTrue(x), PossiblyTrue(y)) if comparisonEvaluator(ordering.compare(x,y)) => PossiblyTrue(y)
    case (True, v) => v
    case (v, True) => v
    case _ => False
  }

  def *<* [B >: A, C >: B] (y: RelationBool[B]) (implicit ordering: math.Ordering[C]) = this.evaluate(y, _ <  0, ordering)
  def *<=*[B >: A, C >: B] (y: RelationBool[B]) (implicit ordering: math.Ordering[C]) = this.evaluate(y, _ <= 0, ordering)
  def *>* [B >: A, C >: B] (y: RelationBool[B]) (implicit ordering: math.Ordering[C]) = this.evaluate(y, _ >  0, ordering)
  def *>=*[B >: A, C >: B] (y: RelationBool[B]) (implicit ordering: math.Ordering[C]) = this.evaluate(y, _ >= 0, ordering)
  def *==*[B >: A, C >: B] (y: RelationBool[B]) (implicit ordering: math.Ordering[C]) = this.evaluate(y, _ == 0, ordering)
  def *!=*[B >: A, C >: B] (y: RelationBool[B]) (implicit ordering: math.Ordering[C]) = this.evaluate(y, _ != 0, ordering)
  def *<>*[B >: A, C >: B] (y: RelationBool[B]) (implicit ordering: math.Ordering[C]) = this.evaluate(y, _ != 0, ordering)
}
case object False extends RelationBool[Nothing]
case class PossiblyTrue[A](value: A) extends RelationBool[A]
case object True extends RelationBool[Nothing]

object RelationBool {
  implicit def valueToRelationBool[A] = (value: A) => PossiblyTrue(value)
  implicit def relationBoolToBoolean[A] (x: RelationBool[A]): Boolean = x match {
    case False => false
    case _ => true
  }
}
