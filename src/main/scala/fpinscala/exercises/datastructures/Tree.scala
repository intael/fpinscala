package fpinscala.exercises.datastructures


enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = {
    this match {
      case Leaf(_) => 0
      case Branch(left, right) => math.max(left.depth + 1, right.depth + 1)
    }
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

  def depthViaFold: Int = this.fold(_ => 0, (left, right) => math.max(left + 1, right + 1))

  def mapViaFold[B](f: A => B): Tree[B] = this.fold(value => Leaf(f(value)), (left, right) => Branch(left, right))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] =
    t match {
      case Leaf(value) if value > 0 => Some(value)
      case Leaf(value) => None
      case Branch(left, right) => Tree.firstPositive(left).orElse(Tree.firstPositive(right))
    }

  extension (t: Tree[Int]) def maximum: Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => math.max(Tree.maximum(left), Tree.maximum(right))
  }

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
