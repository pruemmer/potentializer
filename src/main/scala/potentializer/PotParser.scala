package potentializer

import scala.util.parsing.combinator._

/**
 * See Scala parser combinators:
 * https://github.com/scala/scala-parser-combinators/tree/main
 */

object PotParser extends JavaTokenParsers {

  case class VarDecl   (id : String, typ : String)

  abstract class Expr
  case class NumExpr   (num   : BigInt)             extends Expr
  case class IdExpr    (id    : String)             extends Expr
  case class PlusExpr  (e1 : Expr, e2 : Expr)       extends Expr
  case class MulExpr   (e1 : Expr, e2 : Expr)       extends Expr
  case class NegExpr   (e  : Expr)                  extends Expr

  abstract class Formula
  case class DisjFor   (f1 : Formula, f2 : Formula) extends Formula
  case class ConjFor   (f1 : Formula, f2 : Formula) extends Formula
  case class NegFor    (f  : Formula)               extends Formula
  case class EqFor     (e1 : Expr, e2 : Expr)       extends Formula
  case class LitFor    (v  : Boolean)               extends Formula

  abstract class Stmt
  case class AssignStmt(lhs : String, rhs : Expr)   extends Stmt
  case class LoadStmt  (lhs : String, rhs : String) extends Stmt
  case class StoreStmt (lhs : String, rhs : Expr)   extends Stmt
  case class IteStmt   (cond       : Expr,
                        branch1    : Seq[Stmt],
                        branch2    : Seq[Stmt])     extends Stmt
  case class AssertStmt(cond : Formula)             extends Stmt

  case class Thread    (name       : String,
                        localVars  : Seq[VarDecl],
                        stmts      : Seq[Stmt])

  case class Prog      (globalVars : Seq[VarDecl],
                        pre        : Option[Formula],
                        threads    : Seq[Thread],
                        post       : Option[Formula])

  def keywords =
    guard("int" | "bool" | "var" | "STORE" | "LOAD" | "else" | "thread" |
          "true" | "false")

  def identifier =
    not(keywords) ~> ident

  def typ : Parser[String] =
    "int" | "bool"
  def varDecl : Parser[VarDecl] =
    "var " ~> identifier ~ ":" ~ typ ^^ { case n ~ _ ~ t => VarDecl(n, t) }

  def expr  : Parser[Expr] =
    expr1

  def expr1 : Parser[Expr] =
    expr2 ~ ("+" ~> expr2).* ^^ {
      case fst ~ rest => (List(fst) ++ rest).reduceLeft(PlusExpr(_, _)) }
  def expr2 : Parser[Expr] =
    expr3 ~ ("*" ~> expr3).* ^^ {
      case fst ~ rest => (List(fst) ++ rest).reduceLeft(MulExpr(_, _)) }
  def expr3 : Parser[Expr] =
    ("-" ~ expr4 ^^ { case _ ~ r => NegExpr(r) }) | expr4
  def expr4 : Parser[Expr] =
    (wholeNumber ^^ { case n => NumExpr(BigInt(n)) }) |
    (identifier ^^ { case n => IdExpr(n) }) |
    ("(" ~> expr <~ ")")

  def formula : Parser[Formula] =
    formula1

  def formula1 : Parser[Formula] =
    formula2 ~ ("∨" ~> formula2).* ^^ {
      case fst ~ rest => (List(fst) ++ rest).reduceLeft(DisjFor(_, _)) }
  def formula2 : Parser[Formula] =
    formula3 ~ ("∧" ~> formula3).* ^^ {
      case fst ~ rest => (List(fst) ++ rest).reduceLeft(ConjFor(_, _)) }
  def formula3 : Parser[Formula] =
    ("¬" ~ formula4 ^^ { case _ ~ r => NegFor(r) }) | formula4
  def formula4 : Parser[Formula] =
    (expr ~ "=" ~ expr ^^ { case e1 ~ _ ~ e2 => EqFor(e1, e2) }) |
    ("true" ^^ { case _ => LitFor(true) }) |
    ("false" ^^ { case _ => LitFor(false) }) |
    ("(" ~> formula <~ ")")

  def stmt =
    (identifier ~ "=" ~ expr <~ ";" ^^ {
       case lhs ~ _ ~ e => AssignStmt(lhs, e) }) |
    (identifier ~ "=" ~ "LOAD" ~ "(" ~ identifier <~ ")" ~ ";" ^^ {
       case lhs ~ _ ~ _ ~ _ ~ rhs => LoadStmt(lhs, rhs) }) |
    ("STORE" ~ "(" ~> identifier ~ "," ~ expr <~ ")" ~ ";" ^^ {
       case lhs ~ _ ~ rhs => StoreStmt(lhs, rhs) }) |
    ("if" ~ "(" ~> expr ~ ")" ~
       "{" ~ seqProg ~ "}" ~ "else" ~
       "{" ~ seqProg <~ "}" ^^ {
         case cond ~ _ ~ _ ~ b1 ~ _ ~ _ ~ _ ~ b2 => IteStmt(cond, b1, b2) }) |
    assertStmt

  def assertStmt : Parser[AssertStmt] =
    ("{" ~> formula <~ "}" ^^ { case f => AssertStmt(f) })

  def seqProg  : Parser[Seq[Stmt]]    = stmt.*
  def varDecls : Parser[Seq[VarDecl]] = (varDecl <~ ";").*

  def thread : Parser[Thread] =
    "thread " ~ identifier ~ "{" ~ varDecls ~ seqProg ~ "}" ^^ {
      case _ ~ n ~ _ ~ decls ~ stmts ~ _ => Thread(n, decls, stmts) }

  def prog : Parser[Prog] =
    varDecls ~ assertStmt.? ~ thread.* ~ assertStmt.? ^^ {
      case decls ~ pre ~ threads ~ post =>
        Prog(decls, pre.map(_.cond), threads, post.map(_.cond)) }

}
