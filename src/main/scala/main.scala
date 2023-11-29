@main
def main(): Unit = {
  val originalList = LazyList(1, 2, 3, 4, 5)
  val repeatedList = Irepeat(originalList)(2)
  repeatedList.take(10).foreach(println)
  Ifib.take(10).foreach(println)
}

//Zadanie 1: powtarzanie elementów listy
def Irepeat(lista: LazyList[Int])(n: Int): LazyList[Int] = {
  def repeatNTimes(number: Int, n: Int): LazyList[Int] = {
    if (n <= 0) LazyList.empty
    else number #:: repeatNTimes(number, n - 1)
  }

  lista match {
    case LazyList() => LazyList.empty // for empty list
    case head #:: tail => repeatNTimes(head, n) #::: Irepeat(tail)(n)
  }
}

//Zadanie 2: liczby fibonacciego
def Ifib: LazyList[Long] = {
  def fib(a: Long, b: Long): LazyList[Long] = {
    a #:: fib(b, a + b)
  }
  fib(0, 1)
}