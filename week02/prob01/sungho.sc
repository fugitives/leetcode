import scala.annotation.tailrec
import scala.collection.immutable.Queue

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  @tailrec
  private def bfs(queue: Queue[(Int, TreeNode)], visited: List[(Int, Int)]): List[(Int, Int)] = {
    if (queue.isEmpty) visited
    else {
      val ((level, node), poppedQ) = queue.dequeue
      val visitedCur = (level, node.value) :: visited
      val leftPushedQ =
        if (node.left == null)
          poppedQ
        else
          poppedQ :+ (level + 1, node.left)

      val rightPushedQ =
        if (node.right == null)
          leftPushedQ
        else
          leftPushedQ :+ (level + 1, node.right)

      bfs(rightPushedQ, visitedCur)
    }
  }

  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null) Nil
    else {
      val visited = bfs(Queue((0, root)), Nil)

      val groupByLevel = visited.tail.foldLeft(List(List(visited.head))) { (acc, node) =>
        val (headLevel, _) = acc.head.head
        val (curLevel, _) = node

        if (headLevel == curLevel)
          (node :: acc.head) :: acc.tail
        else
          List(node) :: acc
      }

      groupByLevel.zipWithIndex.map {
        case (list: List[(Int, Int)], idx: Int) => {
          val newList =
            if (idx % 2 == 0)
              list
            else
              list.reverse

          newList.map { case (_, num) => num }
        }
      }
    }
  }
}
