namespace _2025_cs.Solutions;


// --- Day 1: Secret Entrance --- //


public record Day01() : Solver(AnswerOne: "1150", AnswerTwo: "6738")
{
    public override Solution PartOne(IEnumerable<string> input)
    {
        var result = 0;

        _ = input
            .Select(l => int.Parse(l[1..]) * (l[0] == 'R' ? 1 : -1))
            .Aggregate(50, (sum, x) =>
            {
                var next = Mod(sum + x, 100);
                if (next == 0) result++;
                return next;
            });

        return result.ToString();
    }

    public override Solution PartTwo(IEnumerable<string> input)
    {
        var result = 0;

        _ = input
            .Select(l => int.Parse(l[1..]) * (l[0] == 'R' ? 1 : -1))
            .Aggregate(50, (current, step) =>
            {
                if (current < 0) throw new Exception("current is " + current);

                var next = Mod(current + step, 100);

                var diffToZero = (current, step) switch
                {
                    (current: _, step: 0) => throw new ArgumentOutOfRangeException(nameof(step), step, "Step cannot be 0"),

                    (current: 0, step: > 0) => 100,
                    (current: 0, step: < 0) => -100,

                    (current: _, step: > 0) => 100 - current,
                    (current: _, step: < 0) => -current
                };

                var stepAbs = Math.Abs(step);
                var diffToZeroAbs = Math.Abs(diffToZero);

                if (stepAbs == diffToZeroAbs)
                {
                    result++;
                }
                else if (stepAbs > diffToZeroAbs)
                {
                    if (stepAbs > 100)
                    {
                        var stepsLeftAfterReachingZero = stepAbs - diffToZeroAbs;
                        var loopCount = stepsLeftAfterReachingZero / 100 + 1;

                        result += loopCount;
                    }
                    else if (current != 0)
                    {
                        result++;
                    }
                }

                return next;
            });

        return result.ToString();
    }

    private static int Mod(int x, int m)
    {
        // return x - m * (int)Math.Floor(1.0F * x / m);

        var res = x % m;

        return res < 0
            ? m + res
            : res;
    }


    // --- Example Inputs --- ///


    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "3",
            Input: """
            L68
            L30
            R48
            L5
            R60
            L55
            L1
            L99
            R14
            L82
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "10",
            Input: """
            L1000
            """
        ),
        (
            Expected: "11",
            Input: """
            L50
            L1000
            """
        ),
        (
            Expected: "1",
            Input: """
            L50
            """
        ),
        (
            Expected: "1",
            Input: """
            R50
            """
        ),
        (
            Expected: "2",
            Input: """
            R2
            R50
            L100
            """
        ),
        (
            Expected: "6",
            Input: """
            L68
            L30
            R48
            L5
            R60
            L55
            L1
            L99
            R14
            L182
            """
        ),
    ];
}
