import scala.annotation.tailrec

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def isGoodEnough(guess: Double, x: Double) =
  abs(guess * guess - x) < 0.001

def abs(x: Double) =
  if (x < 0) -x else x

def sqrt(x: Double) = sqrtIter(1.0, x)

sqrt(2.0)

//Scoping
def sqrt2(x: Double): Double = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def improve(guess: Double) =
    (guess + x / guess) / 2

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) < 0.001

  def abs(x: Double) =
    if (x < 0) -x else x

  sqrtIter(1)
}

sqrt2(2.0)

//Tail Recursion
@tailrec
def gcd(a: Int, b: Int): Int = {
  if(0 == b) a else gcd(b, b % a)
}

gcd(14, 21)


def factorial(n: Int): Int = {
  if (n == 0) 1 else n * factorial(n - 1)
}


//Tail recursive factorial
def factorial2(n: Int): Int = {

  def interim(a: Int, res: Int): Int = {
    if(a == 1) res else interim(a-1, a*res)
  }
  interim(n, 1)
}

factorial(3)

factorial2(3)