package potentializer

import ap.SimpleAPI
import ap.api.SimpleAPI.ProverStatus
import ap.parser.{IExpression, ITerm, IFormula}
import ap.terfor.conjunctions.Conjunction
import automata.sfa.{SFA, SFAInputMove, SFAEpsilon}
import transducers.sft.{SFT, SFTEpsilon, SFTInputMove}
import seqSolver.SeqTheory
import seqSolver.automataIntern.{ParametricAutomaton, ParametricTransducer}
import ap.theories.ADT

import scala.collection.JavaConverters._



object MsgPassing extends App {

  import IExpression._

  /**
   * Stores define the values of the two shared variables, x and y
   */
  val (storeSort, store, Seq(x, y)) =
    ADT.createRecordType("Store",
                         List(("x", Sort.Integer),
                              ("y", Sort.Integer)))

  val storeTheory = storeSort.asInstanceOf[ADT.ADTProxySort].adtTheory

  /**
   * Sequences of stores
   */
  val seqTheory = new SeqTheory(storeSort,
                                List(), // parameters
                                Seq(storeTheory))

  val pt = seqTheory.parameterTheory
  val Seq(c, c1) = seqTheory.parameterTheoryChars

  class RichITerm(underlying : ITerm) {
    def ++(that : ITerm) : ITerm =
      seqTheory.seq_++(underlying, that)
    def ∈(regexId : Int) : IFormula =
      seqTheory.seq_in_re_id(underlying, regexId)
    def ∉(regexId : Int) : IFormula =
      !seqTheory.seq_in_re_id(underlying, regexId)
    def tmap(transducerId : Int) : ITerm =
      seqTheory.seq_in_relation_id(underlying, transducerId)
  }

  implicit def toRichTerm(t : ITerm) : RichITerm = new RichITerm(t)

  /**
   * Definition of the regular languages involved in the assertions.
   */

  def genAut(transitions : Seq[pt.SFAMove],
             accepting : Seq[Int]) : Int = {
    val aut = SFA.MkSFA(transitions.asJava,
                        0, (for (a <- accepting) yield new Integer(a)).asJava,
                        pt, false, false, false)
    val paut = new ParametricAutomaton(aut, pt)
    seqTheory.autDatabase.registerAut(paut)
  }

  // Language of non-empty words
  val nonEmpty =
    genAut(List(
             new SFAInputMove(0, 1, pt.FromFormula(true)),
             new SFAInputMove(1, 1, pt.FromFormula(true))
           ),
           List(1))

  val len1 =
    genAut(List(
             new SFAInputMove(0, 1, pt.FromFormula(true))
           ),
           List(1))

  // [ y != 1 ]
  val aut1 =
    genAut(List(
             new SFAInputMove(0, 1, pt.FromFormula(y(c) =/= 1)),
             new SFAInputMove(1, 1, pt.FromFormula(y(c) =/= 1))
           ),
           List(1))

  // [ x == 1 ]
  val aut2 =
    genAut(List(
             new SFAInputMove(0, 0, pt.FromFormula(x(c) === 1))
           ),
           List(0))

  // Complement of aut2
  val aut2Comp =
    genAut(List(
             new SFAInputMove(0, 0, pt.FromFormula(true)),
             new SFAInputMove(0, 1, pt.FromFormula(x(c) =/= 1)),
             new SFAInputMove(1, 1, pt.FromFormula(true))
           ),
           List(1))

  // [ y != 1 ]; [ x == 1 ]
  val aut3 =
    genAut(List(
             new SFAInputMove(0, 0, pt.FromFormula(y(c) =/= 1)),
             new SFAEpsilon(0, 1),
             new SFAInputMove(1, 1, pt.FromFormula(x(c) === 1))
           ),
           List(1))

  /*

   0 -A-> 0
   0 -!A & B-> 1
   1 -B-> 1

   0 -!A & !B-> 2
   1 -!B-> 2
   2 -true-> 2

   accepting 0, 1;

   */

  // Complement of aut3
  val aut3Comp =
    genAut(List(
             new SFAInputMove(0, 0, pt.FromFormula(y(c) =/= 1)),
             new SFAInputMove(0, 1, pt.FromFormula(y(c) === 1 & x(c) === 1)),
             new SFAInputMove(0, 2, pt.FromFormula(y(c) === 1 & x(c) =/= 1)),
             new SFAInputMove(1, 1, pt.FromFormula(x(c) === 1)),
             new SFAInputMove(1, 2, pt.FromFormula(x(c) =/= 1)),
             new SFAInputMove(2, 2, pt.FromFormula(true))
           ),
           List(2))

  // [ y != 1 ]; [ x == 1 ]; [ x == 1 ]
  val aut4 =
    genAut(List(
             new SFAInputMove(0, 0, pt.FromFormula(y(c) =/= 1)),
             new SFAEpsilon(0, 1),
             new SFAInputMove(1, 1, pt.FromFormula(x(c) === 1)),
             new SFAEpsilon(1, 2),
             new SFAInputMove(2, 2, pt.FromFormula(x(c) === 1))
           ),
           List(2))

  /**
   * Transducers needed in the examples.
   */

  def jlist[A](els : A*) : java.util.List[A] = els.asJava

  def genTrans(transitions : Seq[pt.SFTMove],
               accepting : Seq[Int]) : Int = {
    val aut = SFT.MkSFT(transitions.asJava,
                0, (for (a <- accepting)
                    yield (new Integer(a) ->
                             Set(jlist[ITerm]()).asJava)).toMap.asJava,
                        pt)
    val paut = new ParametricTransducer(aut, pt)
    seqTheory.autDatabase.registerTrans(paut)
  }

  val assignX1 =
    genTrans(List(new SFTInputMove(0, 0, Conjunction.TRUE,
                                   jlist(store(1, y(c))))),
             List(0))

  SimpleAPI.withProver(enableAssert = true) { p =>
    import p._
    import seqTheory.{SeqSort, seq_in_re_id, seq_in_relation_id}

    addTheory(seqTheory)

    val a    = createConstant("a", Sort.Integer)
    val b    = createConstant("b", Sort.Integer)
    val pot  = createConstant("pot", SeqSort)
    val pot2 = createConstant("pot2", SeqSort)
    val l    = createConstant("l", SeqSort)
    val l1   = createConstant("l1", SeqSort)
    val l2   = createConstant("l2", SeqSort)
    val l3   = createConstant("l3", SeqSort)
    val lP   = createConstant("lP", SeqSort)

    // only consider non-empty sequences of stores
    !! (pot ∈ nonEmpty)

    scope {
      // { T0 sees [y != 1] } ... { T2 sees [y != 1]; [ x == 1 ] }
      print("T2 valid Hoare triple 1 ... ");
      !! (pot ∈ aut1);
      ?? (pot ∉ aut3Comp)
      println(???)
    }

    scope {
      // { T2 sees [y != 1]; [ x == 1 ] } ...LOAD... { a==1 => T2 sees [x==1] }
      print("T2 valid Hoare triple 2 ... ")

      // What exactly is the Hoare rule for LOAD in this setting?
      // Which checks are needed for lossiness?

      ?? (true)
      println(???)
    }

    scope {
      // Stability of { T2 sees [y != 1]; [ x == 1 ] } under STORE(x, 1)
      print("T2 stable assertion 1 ... ");
      !! (pot ∈ aut4);
      ?? (pot ∉ aut3Comp)
      println(???)
    }

    scope {
      // Stability of { a==1 => T2 sees [ x == 1 ] } under STORE(x, 1)
      print("T2 stable assertion 2 (version 1) ... ");
      // Probably not the right encoding: let's just say that the effect
      // of STORE is to append some elements to the T2-potential
      !! (a===1 ==> (pot ∈ aut2));
      !! (l ∈ aut2);
      !! (pot2 === pot ++ l);
      ?? (a===1 ==> (pot2 ∉ aut2Comp))
      println(???)
    }

    scope {
      // Stability of { a==1 => T2 sees [ x == 1 ] } under STORE(x, 1)
      print("T2 stable assertion 2 (version 2) ... ")

      // (this does not seem to be working properly yet!)

      !! (a === 1 ==> (pot ∈ aut2))

      !! (pot === l1 ++ l2 ++ l3);
      !! (lP === seq_in_relation_id(l2 ++ l3, assignX1));
      !! (pot2 === l1 ++ l2 ++ lP)
      
      ?? (a === 1 ==> (pot2 ∉ aut2Comp))
      println(???)
    }

  }

}
