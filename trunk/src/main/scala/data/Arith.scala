/*
 * Copyright (c) 2012 Dame Ningen.
 * All rights reserved.
 *
 * This file is part of Gausel.
 *
 * Gausel is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Gausel is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Gausel.  If not, see <http://www.gnu.org/licenses/>.
 */

package gausel.data

import  scala.collection.mutable.OpenHashMap

/** Case classes for representation of Arithmetic.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
sealed trait Arith {
  override def toString(): String
  def toLatex(): String
  val uid: BigInt
  override def equals(that: Any) = that match {
    case a:Arith => this.getClass == a.getClass && this.uid == a.uid
    case _ => false
  }
}

trait Uid[T] {
  protected var cpt: BigInt = 0
  protected val map: OpenHashMap[T,Arith] = new OpenHashMap
  protected def getId() = {
    cpt += 1
    cpt - 1
  }
}

/** Zero.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object Zero extends Arith {
  val uid = BigInt(0)
  override def toString() = "0"
  def toLatex() = toString()
}

/** Identifiers.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class Ident private(val id: String,
                    val uid: BigInt) extends Arith {
  override def toString() = id
  def toLatex() = toString()
}

/** Companion object for identifiers.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object Ident extends Uid[String] {
  def apply(id: String): Arith = map get id match {
    case Some(ident) => ident
    case None => {
      val ident = new Ident(id,getId())
      map.update(id,ident)
      ident
    }
  }
  def unapply(id: Ident) = Some(id.id)
}

/** Concrete value.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class Value private (val dec: BigDecimal,
                     val uid: BigInt) extends Arith {
  override def toString() = dec.toString()
  def toLatex() = toString()
}

/** Companion object for values.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object Value extends Uid[BigDecimal] {
  def apply(dec: BigDecimal): Arith =
    if(dec == 0) Zero else map get dec match {
      case Some(value) => value
      case None => {
        val value = new Value(dec,getId)
        map.update(dec,value)
        value
      }
    }
  def unapply(v: Value) = Some(v.dec)
}

/** Unary minus.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class UMinus private (val kid: Arith,
                      val uid: BigInt) extends Arith {
  override def toString() = "-" + kid
  def toLatex() = "-" + kid
}

/** Companion object for Unary minus.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object UMinus extends Uid[BigInt] {
  def apply(kid: Arith): Arith = map get kid.uid match {
    case Some(uminus) => uminus
    case None => {
      val uminus = new UMinus(kid,getId)
      map.update(kid.uid,uminus)
      uminus
    }
  }
  def unapply(um: UMinus) = Some(um.kid)
}

/** Plus.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class Plus private (val kid1: Arith,
                    val kid2: Arith,
                    val uid: BigInt) extends Arith {
  override def toString() =
    "(" + kid1.toString() + " + " + kid2.toString() + ")"
  def toLatex() =
    "(" + kid1.toLatex() + " + " + kid2.toLatex() + ")"
}

/** Companion object for Plus.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object Plus extends Uid[(BigInt,BigInt)] {
  def apply(kid1: Arith, kid2: Arith): Arith = (map get ((kid1.uid,kid2.uid)), map get ((kid2.uid,kid2.uid))) match {
    case (Some(_),Some(_)) => throw new Exception("This should not happen.")
    case (Some(plus),_) => plus
    case (_,Some(plus)) => plus
    case (None,None) => (kid1,kid2) match {
      case (Zero,_) => kid2
      case (_,Zero) => kid1
      case (Div(num1,den1),Div(num2,den2)) if den1 == den2 => Div(Plus(num1,num2),den1)
      case (Div(num1,den1),Div(num2,den2)) => Div(Plus(Mult(num1,den2),Mult(num2,den1)),Mult(den1,den2))
      case (Div(num1,den1),_) => Div(Plus(num1,Mult(den1,kid2)),den1)
      case (_,Div(num1,den1)) => Div(Plus(Mult(den1,kid1),num1),den1)
      case (_,UMinus(kid)) if kid1 == kid => Zero
      case (_,_) => {
        val plus = new Plus(kid1,kid2,getId)
        map.update((kid1.uid,kid2.uid),plus)
        plus
      }
    }
  }
  def unapply(p: Plus) = Some(p.kid1,p.kid2)
}

/** Minus.
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class Minus private (val kid1: Arith,
                     val kid2: Arith,
                     val uid: BigInt) extends Arith {
  override def toString() = "(" + kid1.toString() + " - " + kid2.toString() + ")"
  def toLatex() = "(" + kid1.toLatex() + " - " + kid2.toLatex() + ")"
}
/** Companion object for Minus.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object Minus extends Uid[(BigInt,BigInt)] {
  def apply(kid1: Arith, kid2: Arith): Arith = (map get ((kid1.uid,kid2.uid)), map get ((kid2.uid,kid2.uid))) match {
    case (Some(_),Some(_)) => throw new Exception("This should not happen.")
    case (Some(minus),_) => minus
    case (_,Some(minus)) => minus
    case (None,None) => (kid1,kid2) match {
      case (Zero,_) => kid2
      case (_,Zero) => kid1
      case (Div(num1,den1),Div(num2,den2)) if den1 == den2 => Div(Minus(num1,num2),den1)
      case (Div(num1,den1),Div(num2,den2)) => Div(Minus(Mult(num1,den2),Mult(num2,den1)),Mult(den1,den2))
      case (Div(num1,den1),_) => Div(Minus(num1,Mult(den1,kid2)),den1)
      case (_,Div(num1,den1)) => Div(Minus(Mult(den1,kid1),num1),den1)
      case (_,_) if (kid1 == kid2) => Zero
      case (_,_) => {
        val minus = new Minus(kid1,kid2,getId)
        map.update((kid1.uid,kid2.uid),minus)
        minus
      }
    }
  }
  def unapply(m: Minus) = Some(m.kid1,m.kid2)
}

/** N-ary minus, only used when extracting values from the system.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class Minus1N private (val kid1: Arith,
                       val kids: List[Arith],
                       val uid: BigInt) extends Arith {
  override def toString() =
    "(" + kid1.toString() +
    kids.foldLeft("")((string,kid) => string + " - " + kid.toString()) + ")"
  def toLatex() =
    "(" + kid1.toLatex() +
    kids.foldLeft("")((string,kid) => string + " - " + kid.toLatex()) + ")"
}
/** Companion object for N-ary minus.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object Minus1N extends Uid[(BigInt,List[BigInt])] {
  def apply(kid1: Arith, kids: List[Arith]): Arith = map get ((kid1.uid,kids.map(_.uid))) match {
    case Some(minus) => minus
    case None => {
      val minus = new Minus1N(kid1,kids,getId)
      map.update((kid1.uid,kids.map(_.uid)),minus)
      minus
    }
  }
  def unapply(m: Minus1N) = Some(m.kid1,m.kids)
}

/** Multiplication.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class Mult private (val kid1: Arith,
                    val kid2: Arith,
                    val uid: BigInt) extends Arith {
  override def toString() = "(" + kid1.toString() + " * " + kid2.toString() + ")"
  def toLatex() = "(" + kid1.toLatex() + " * " + kid2.toLatex() + ")"
}
/** Companion object for Multiplication.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object Mult extends Uid[(BigInt,BigInt)] {
  // This one is tough.
  // Order in the pattern matching is important.
  def apply(kid1: Arith, kid2: Arith): Arith = (map get ((kid1.uid,kid2.uid)),map get ((kid2.uid,kid1.uid))) match {
    case (Some(_),Some(_)) => throw new Exception("This should not happen.")
    case (Some(mult),_) => mult
    case (_,Some(mult)) => mult
    case (None,None) => (kid1,kid2) match {
      // Values related matches.
      case (Value(v),_) if v == 1.0 => kid2
      case (_,Value(v)) if v == 1.0 => kid1
      case (Zero,_) | (_,Zero) => Zero
      case (Value(v),_) if v == 0.0 => {
        println("Weird, found a zero as a value.")
        Zero
      }
      case (_,Value(v)) if v == 0.0 => {
        println("Weird, found a zero as a value.")
        Zero
      }
      case (Value(v1),Value(v2)) => Value(v1 * v2)
      case (Value(_),Plus(pkid1,pkid2)) => Plus(Mult(kid1,pkid1),Mult(kid1,pkid2))
      case (Plus(pkid1,pkid2),Value(_)) => Plus(Mult(pkid1,kid1),Mult(pkid2,kid1))
      case (Value(_),Minus(pkid1,pkid2)) => Minus(Mult(kid1,pkid1),Mult(kid1,pkid2))
      case (Minus(pkid1,pkid2),Value(_)) => Minus(Mult(pkid1,kid1),Mult(pkid2,kid1))
      case (Value(_),UMinus(kid)) => UMinus(Mult(kid1,kid))
      case (UMinus(kid),Value(_)) => UMinus(Mult(kid,kid1))
      // Div related matches.
      case (Div(num1,den1),Div(num2,den2)) => Div(Mult(num1,num2),Mult(den1,den2))
      case (Div(num1,den1),arith) => Div(Mult(num1,arith),den1)
      case (arith,Div(num1,den1)) => Div(Mult(arith,num1),den1)
      // Wildcard.
      case (_,_) => {
        val mult = new Mult(kid1,kid2,getId)
        map.update((kid1.uid,kid2.uid),mult)
        mult
      }
    }
  }

  def unapply(m: Mult) = Some(m.kid1,m.kid2)
}

/** Division.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class Div private (val kid1: Arith,
                   val kid2: Arith,
                   val uid: BigInt) extends Arith {
  override def toString() = "(" + kid1.toString() + " / " + kid2.toString() + ")"
  def toLatex() = "\\frac{" + kid1.toLatex() + "}{" + kid2.toLatex() + "}"
}
/** Companion object for division.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object Div extends Uid[(BigInt,BigInt)] {
  def apply(kid1: Arith, kid2: Arith): Arith = map get ((kid1.uid,kid2.uid)) match {
    case Some(div) => div
    case None => (kid1,kid2) match {
      case (Div(num1,den1),Div(num2,den2)) => Div(Mult(num1,den2),Mult(num2,den1))
      case (Div(num1,den1),_) => Div(num1,Mult(den1,kid2))
      case (_,Div(num1,den1)) => Div(Mult(kid1,den1),num1)
      case (_,_) => new Div(kid1,kid2,getId)
    }
  }
  def unapply(d: Div) = Some(d.kid1,d.kid2)
}
