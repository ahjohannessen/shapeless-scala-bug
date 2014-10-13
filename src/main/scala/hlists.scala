package demo

import shapeless._
import ops.hlist.{ LeftFolder, Prepend, Replacer, Selector }

object hlists {

  /* Combining */

  trait LowPriorityCombine extends Poly2 {
    implicit def notAlreadySeen[L <: HList, A, Out <: HList](implicit
      p: Prepend.Aux[L, List[A] :: HNil, Out]
    ): Case.Aux[L, List[A], Out] = at[L, List[A]](_ :+ _)
  }

  object combiner extends LowPriorityCombine {
    implicit def alreadySeen[L <: HList, A, Out <: HList](implicit
      s: Selector[L, List[A]],
      r: Replacer.Aux[L, List[A], List[A], (List[A], Out)]
    ): Case.Aux[L, List[A], Out] = at[L, List[A]] {
      case (acc, as) => acc.updatedElem[List[A], Out](acc.select[List[A]] ++ as)
    }
  }

  def combine[L <: HList](l: L)(implicit f: LeftFolder[L, HNil, combiner.type]) =
    l.foldLeft(HNil: HNil)(combiner)


  /* Morphing */

  trait Picker[I <: HList, O <: HList] {
    def apply(i: I): O
  }

  object Picker {
    implicit def hnilPicker[I <: HList]: Picker[I, HNil] = new Picker[I, HNil] {
      def apply(i: I) = HNil
    }

    implicit def hnilHlistPicker[I <: HList, OT <: HList](implicit
      picker: Picker[I, OT]
    ): Picker[I, HNil :: OT] = new Picker[I, HNil :: OT] {
      def apply(i: I) = HNil :: picker(i)
    }

    implicit def hlistPicker[I <: HList, OHH, OHT <: HList, OT <: HList](implicit
      sel: Selector[I, OHH],
      picker: Picker[I, OHT :: OT]
    ): Picker[I, (OHH :: OHT) :: OT] = new Picker[I, (OHH :: OHT) :: OT] {
      def apply(i: I) = picker(i) match {
        case h :: t => (sel(i) :: h) :: t
      }
    }
  }

  def morph[In <: HList, Out <: HList](in: In)(implicit
    picker: Picker[In, Out]
  ): Out = picker(in)


  /* Convenience */

  implicit class HListOps[I <: HList](inner: I) {
    def transformTo[O <: HList](implicit p: Picker[I, O]) : O = morph[I, O](inner)
  }

}