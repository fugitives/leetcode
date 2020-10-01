import scala.annotation.tailrec

object Solution {
  def partitionLabels(S: String): List[Int] = {
    getBunch(S, 0, 0, S.lastIndexOf(S(0)), List())
  }

  @tailrec
  def getBunch(S: String, idx: Int, anchor: Int, partEnd: Int, ans: List[Int]): List[Int] = {
    if (idx == S.length) return ans.reverse

    val newPartEnd = Math.max(partEnd, S.lastIndexOf(S(idx)))

    if (idx == newPartEnd) getBunch(S, idx + 1, idx + 1, newPartEnd, (idx - anchor + 1) :: ans)
    else getBunch(S, idx + 1, anchor, newPartEnd, ans)
  }
}

//Solution.partitionLabels("qiejxqfnqceocmy")
//"ababcbaca defegde hijhklij"
//[9,7,8]

//"qiejxqfnqceoc m y"
//[13,1,1]
