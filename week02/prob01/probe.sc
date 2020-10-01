import scala.annotation.tailrec

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
  @tailrec
  def traverse(node: List[TreeNode], result: List[List[Int]], direction: Boolean): List[List[Int]] = {
    if (node.isEmpty) {
      result
    } else {
      val next = node.foldLeft(List[TreeNode]()) {
        case (list, node) =>
          if (node.left == null && node.right == null) {
            list
          } else if (node.left == null) {
            node.right :: list
          } else if (node.right == null) {
            node.left :: list
          } else if (direction) {
            node.right :: node.left :: list
          } else {
            node.left :: node.right :: list
          }
      }
      traverse(next, result :+ node.map(_.value), !direction)
    }
  }

  traverse(List(root), List(), direction = true)
}

zigzagLevelOrder(new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15), new TreeNode(7))))