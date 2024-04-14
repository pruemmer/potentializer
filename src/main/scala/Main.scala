import potentializer.PotParser

@main def hello(): Unit = {
  println("Testing parser:")

  val testprog = """
var g : int;

thread A {
  var l : int;
  l = LOAD(g);
  l = l + 1;
  STORE(g, l);
}

thread B {
  var l2 : int;
  l2 = LOAD(g);
  STORE(g, 2*l2);
}
  """

  println(PotParser.parseAll(PotParser.prog, testprog))

}

