package Chat

import Data.ProductInformations

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  // Example cases
  case class Thirsty() extends ExprTree
  case class Hungry() extends ExprTree

  // Action cases
  case class Identification(pseudo: String) extends ExprTree
  case class Command(products: ExprTree) extends ExprTree
  case class Balance() extends ExprTree
  case class Price(products: ExprTree) extends ExprTree

  // Items
  case class Product(name: String, brand: String, quantity: Int) extends ExprTree

  // Operators
  case class And(leftExp: ExprTree, rightExp: ExprTree) extends ExprTree
  case class Or(leftExp: ExprTree, rightExp: ExprTree) extends ExprTree

