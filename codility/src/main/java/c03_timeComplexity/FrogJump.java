package c03_timeComplexity;

/*
  A small frog wants to get to the other side of the road. The frog is currently located
  at position X and wants to get to a position greater than or equal to Y. The small frog
  always jumps a fixed distance, D.

  Count the minimal number of jumps that the small frog must perform to reach its target.

  Write a function:

  class Solution { public int solution(int X, int Y, int D); }

  that, given three integers X, Y and D, returns the minimal number of jumps
  from position X to a position equal to or greater than Y.

  For example, given:
  X = 10
  Y = 85
  D = 30

  the function should return 3, because the frog will be positioned as follows:

  after the first jump, at position 10 + 30 = 40
  after the second jump, at position 10 + 30 + 30 = 70
  after the third jump, at position 10 + 30 + 30 + 30 = 100

  Write an efficient algorithm for the following assumptions:

  X, Y and D are integers within the range [1..1,000,000,000];
  X ≤ Y.
*/

public class FrogJump {

  private static int solution(int start, int target, int jumpDistance) {
    int jumps = 0;
    if (target > start) {
      int distance = target - start;
      int times = distance / jumpDistance;
      if ((times * jumpDistance) < distance) {
        jumps = times + 1;
      } else {
        jumps = times;
      }
    }
    return jumps;
  }

  private static int solutionInEfficient(int start, int target, int jumpDistance) {
    int jumps = 0;
    int position = start;
    while (position < target) {
      position += jumpDistance;
      jumps += 1;
    }
    return jumps;
  }

  public static void main(String[] args) {

    int start = 10;
    int target = 85;
    int jumpDistance = 30;
    System.out.println(String.format("Efficient   : Start %s, Target %s, Distance %s, Nbr of Jumps %s", start, target, jumpDistance, solution(start, target, jumpDistance)));
    System.out.println(String.format("InEfficient : Start %s, Target %s, Distance %s, Nbr of Jumps %s", start, target, jumpDistance, solutionInEfficient(start, target, jumpDistance)));
  }
}