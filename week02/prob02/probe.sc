object Solution {
  def wordBreak(s: String, wordDict: List[String]): Boolean = {
    val result = (1 to s.length).foldLeft(List(0)) { (acc, index) =>
      if (acc.exists(x => wordDict.contains(s.substring(x, index)))) {
        index :: acc
      } else {
        acc
      }
    }
    s.length == result.head
  }
}

println(Solution.wordBreak("leetcode", List("leet", "code")))
//
println(Solution.wordBreak("catsandog", List("cats", "dog", "sand", "and", "cat")))
//
println(Solution.wordBreak("a", List()))

println(Solution.wordBreak("a", List("b")))

println(Solution.wordBreak("bb", List("a", "b", "bbb", "bbbbb")))
