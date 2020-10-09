import scala.annotation.tailrec

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    // 1. 만약 TreeNode 가 null 이면 List[Nothing]() 반환
    if (root == null) return List[Nothing]()

    // 2. 레벨별로 BFS 돌아다닐건데, 레벨 처리 List 끝에 null 구분자 값 넣어줘서 해당 레벨 처리가 끝났음을 알린다.
    getResult(List(root, null), true, List[Int](), List[List[Int]]())
  }

  @tailrec
  def getResult(pending: List[TreeNode], goRight: Boolean, step: List[Int], results: List[List[Int]]): List[List[Int]] = {
    // 3. 더이상 레벨 처리할게 없으면 결과 return
    if (pending.isEmpty) return results

    // 4. 레벨 처리 List 첫 번째 값 가져온다.
    val head :: tail = pending

    // 5. 만약 head 가 null 이면 해당 레벨이 끝난것, 아니면 해당 레벨 처리 List 값을 처리한다.
    if (head != null) {
      // 5-1. 레벨 처리 List 에 다음 레벨 처리할거 넣는다.
      val newPending = tail ::: List(head.left, head.right).filter(n => n != null)

      // 5-2. 오른쪽 방향이면 현재 레벨용 step List 뒤에다 현재 TreeNode 값 넣는다.
      if (goRight) getResult(newPending, goRight, step :+ head.value, results)
      // 5-3. 왼쪽 방향이면 현재 레벨용 step List 앞에다 현재 TreeNode 값 넣는다.
      else getResult(newPending, goRight, head.value :: step, results)
    }
    else {
      // 5-4. 더이상 처리할게 없으면 그대로 Nil 값 보낸다.
      if (tail.isEmpty) getResult(tail, !goRight, List[Int](), results :+ step)
      // 5-5. 레벨 처리 List 에 남은게 있으면 null 넣어서 다음 레벨 처리의 종료 지점을 표시한다.
      else getResult(tail :+ null, !goRight, List[Int](), results :+ step)
    }
  }
}

//Solution.zigzagLevelOrder(new TreeNode(3, new TreeNode(9, null, null), new TreeNode(20, new TreeNode(15), new TreeNode(7))))
//[3,9,20,null,null,15,7]
