import scala.annotation.tailrec

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null) return List[Nothing]()

    getResult(List(root, null), false, List[Int](), List[List[Int]]())
  }

  @tailrec
  def getResult(pending: List[TreeNode], goRight: Boolean, step: List[Int], results: List[List[Int]]): List[List[Int]] = {
    if (pending.isEmpty) return results

    val node = pending(0)

    if (node != null) {
      val newPending = pending.drop(1) ::: List(node.left, node.right).filter(n => n != null)
      if (goRight) getResult(newPending, goRight, step :+ node.value, results)
      else getResult(newPending, goRight, node.value :: step, results)
    }
    else {
      val newPending = pending.drop(1)
      if (newPending.isEmpty) getResult(newPending, !goRight, List[Int](), results :+ step)
      else getResult(newPending :+ null, !goRight, List[Int](), results :+ step)
    }
  }
}

//Solution.zigzagLevelOrder(new TreeNode(3, new TreeNode(9, null, null), new TreeNode(20, new TreeNode(15), new TreeNode(7))))
//[3,9,20,null,null,15,7]
