
val l = (0 to 10).toList

def mapWithFold[A, B](a: List[A])(f: A => B): List[B] =
  a.foldRight(List.empty[B])((x, acc) => f(x) :: acc)

def filterWithFold[A](a: List[A])(f: A => Boolean): List[A] =
  a.foldRight(List.empty[A]){(x, acc) => if (f(x)) x::acc else acc}

def flatMapWithFold[A, B](a: List[A])(f: A => List[B]): List[B] =
  a.foldRight(List.empty[B]){(x, acc) => 
    f(x) ::: acc
  }

l.map(_ * 2)

mapWithFold(l)(_ * 2)
filterWithFold(l)(_ % 2 == 0)

flatMapWithFold(l)((x: Int) => (0 to x).toList)

