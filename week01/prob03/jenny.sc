import scala.annotation.tailrec

object Solution {
  def partitionLabels(S: String): List[Int] = {
    val lastList = getLastList(S, 0, List.fill('z' - 'a' + 1)(0))

    getBunch(S, 0, 0, lastList(S(0) - 'a'), lastList, List())
  }

  @tailrec
  def getLastList(S: String, i: Int, lastList: List[Int]): List[Int] = {
    if (i == S.length) lastList
    else getLastList(S, i + 1, lastList.updated(S(i) - 'a', i))
  }

  @tailrec
  def getBunch(S: String, idx: Int, anchor: Int, partEnd: Int, lastList: List[Int], ans: List[Int]): List[Int] = {
    if (idx == S.length) return ans.reverse

    val newPartEnd = Math.max(partEnd, lastList(S(idx) - 'a'))

    if (idx == newPartEnd) getBunch(S, idx + 1, idx + 1, newPartEnd, lastList, (idx - anchor + 1) :: ans)
    else getBunch(S, idx + 1, anchor, newPartEnd, lastList, ans)
  }
}

//Solution.partitionLabels("qiejxqfnqceocmy")
//"ababcbaca defegde hijhklij"
//[9,7,8]

//"qiejxqfnqceoc m y"
//[13,1,1]
