package fpinscala.exercises.datastructures


enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = {
    def dig(branch: Tree[A], depthSoFar: Int): Int = {
      branch match {
        case Leaf(_) => depthSoFar
        case Branch(left, right) => math.max(dig(left, depthSoFar + 1), dig(right, depthSoFar + 1))
      }
    }

    dig(this, 0)
  }

  def map[B](f: A => B): Tree[B] = {
    def dfs(branch: Tree[A]): Tree[B] = {
      branch match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(dfs(left), dfs(right))
      }
    }

    dfs(this)
  }


  def fold[B](f: A => B, g: (B, B) => B): B = {
    def dfs(branch: Tree[A]): B = {
      branch match {
        case Leaf(value) => f(value)
        case Branch(left, right) => g(dfs(left), dfs(right))
      }
    }

    dfs(this)
  }


  def sizeViaFold: Int = this.fold(_ => 1, 1 + _ + _)

  def depthViaFold: Int = ???

  def mapViaFold[B](f: A => B): Tree[B] = ???

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = ???

  extension (t: Tree[Int]) def maximum: Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => math.max(Tree.maximum(left), Tree.maximum(right))
  }

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
