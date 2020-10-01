import scala.annotation.tailrec

object Solution {
  def wordBreak(s: String, wordDict: List[String]): Boolean = {
    if (wordDict.isEmpty) false
    else {
      val words = wordDict.toVector

      @tailrec
      def solve(stack: List[(Int, Int)], memo: Set[(Int, Int)]): Boolean = {
        if (stack.isEmpty) false
        else {
          val (pos, wordIdx) :: restStack = stack
          if (pos == s.length) true
          else {
            val nextWordState = (pos, wordIdx + 1)
            val (stackWithNextWord, memoWithNextWordState) =
              if (wordIdx + 1 < wordDict.length && !memo.contains(nextWordState)) {
                (nextWordState :: restStack, memo + nextWordState)
              }
              else (restStack, memo)

            val word = words(wordIdx)

            val nextPosState = (pos + word.length, 0)
            val (stackWithNextPos, memoWithNextPosState) =
              if (pos + word.length <= s.length &&
                s.substring(pos, pos + word.length) == word &&
                !memo.contains(nextPosState)) {
                (nextPosState :: stackWithNextWord, memoWithNextWordState + nextPosState)
              } else (stackWithNextWord, memoWithNextWordState)

            solve(stackWithNextPos, memoWithNextPosState)
          }
        }
      }

      solve(
        List((0, 0)),
        Set()
      )
    }
  }
}
