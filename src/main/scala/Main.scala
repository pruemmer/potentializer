import potentializer.PotParser

@main def hello(): Unit = {
  println("Testing parser:")

  val testprog = """
var x : int;
var y : int;

{ ¬ y = 1 }

thread A {
  { true }
  STORE(x, 1);
  { x = 1 }
  STORE(y, 1);
  { true }
}

thread B {
  var a : int;
  var b : int;
  { ¬ y = 1 ∨ x = 1 }
  a = LOAD(y);
  { ¬ a = 1 ∨ x = 1 }
  b = LOAD(x);
  { ¬ a = 1 ∨ b = 1 }
}

  """

  println(PotParser.parseAll(PotParser.prog, testprog))

}

