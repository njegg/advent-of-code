using _2025_cs.Extension;

namespace _2025_cs.Solutions;


// --- Day 6: Trash Compactor --- //


public record Day06() : Solver(AnswerOne: "5322004718681", AnswerTwo: "9876636978528")
{

    /**
     *
     * clockwise rotate makes this:
     *
     *      123 328
     *       45 64 
     *        6 98 
     *      *   +  
     *
     * into this:
     *
     *      *, 6, 45, 123
     *      +, 98, 64, 328
     *      *, 215, 387, 51
     *      +, 314, 23, 64
     *
     * rest is ez
     *
     */
    public override Solution PartOne(IEnumerable<string> input)
    {
        return input
            .Select(l => l.Split(" ", StringSplitOptions.RemoveEmptyEntries))
            .ToArray()
            .CWRotate()
            .Select(p => p[1..].Select(long.Parse).Aggregate(Operation(p[0])))
            .Sum()
            .ToString();
    }

    /**
     *
     * counter clockwise rotate makes this:
     *
     *       51 64 
     *      387 23 
     *      215 314
     *      *   +
     *
     * into this:
     *
     *       ,  , 4,
     *      4, 3, 1,
     *      6, 2, 3, +
     *       ,  ,  ,
     *      1, 7, 5,
     *      5, 8, 1,
     *       , 3, 2, *
     *
     * rest is ez
     *
     */
    public override Solution PartTwo(IEnumerable<string> input)
    {
        return input
            .Select(l => l.ToCharArray())
            .ToArray()
            .CCWRotate()
            .Select(r => new string(r))
            .SplitOn(string.IsNullOrWhiteSpace)
            .Select(r => r
                .SkipLast(1) // last one has the operation symbol, parse will complain
                .Select(long.Parse)
                .Append(long.Parse(r[^1][..^1])) // without the operation symbol at the end
                .Aggregate(Operation(r[^1][^1].ToString()))
            )
            .Sum()
            .ToString();
    }

    private Func<long, long, long> Operation(string c) => c switch
    {
        "*" => (x, y) => x * y,
        "+" => (x, y) => x + y,

        _ => throw new ArgumentException($"Invalid operation '{c}'")
    };


    // --- Example Inputs --- ///


    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "4277556",
            Input: """
            123 328  51 64 
             45 64  387 23 
              6 98  215 314
            *   +   *   +  
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "3263827",
            Input: PartOneExamples[0].Input
        ),
    ];
}
