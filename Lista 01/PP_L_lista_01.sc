//zad 1
val x= (2, "sss", true, 2.0)

def mirror4[A, B, C, D]  (x : (A, B, C, D)) : (B, A, D, C) =
  (x._2, x._1, x._4, x._3)

mirror4((1, 2, 3, 4)) == (2, 1, 4, 3)
mirror4 ((true, 1, "xyz", 4.20)) == (1, true, 4.20, "xyz")

//zad 2
def remove[A] (l: List[A], n: Int) : List[A] =
  (l, n) match
    case (_::tail,0) => tail
    case(Nil,_) => Nil
    case (head::tail,_) => head::remove(tail,n-1)

remove(List(0, 1, 2, 3, 4, 5), 0) == List(1, 2, 3, 4, 5)
remove(List(0, 1, 2, 3, 4, 5), 3) == List(0, 1, 2, 4, 5)
remove(List(0, 1, 2, 3, 4, 5), 10) == List(0, 1, 2, 3, 4, 5)
remove(List(0, 1, 2, 3, 4, 5), -10) == List(0, 1, 2, 3, 4, 5)

//zad 3
def rotations(l: List[Int]): Double =
  l match
    case (Nil) => 0
    case (_::Nil) => 1.0
    case (head::tail) => rotations(tail) * -head / tail.head

rotations(List(10)) == 1
rotations(List(10, 5, 1)) == 10
rotations(List(10, 20)) == -0.5
rotations(List(10, 20, 5)) == 2
rotations(List(10, 20, 5, 10)) == -1


