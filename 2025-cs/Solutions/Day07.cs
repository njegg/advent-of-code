namespace _2025_cs.Solutions;


// --- Day 7: Laboratories --- //


public record Day07() : Solver(AnswerOne: "1553", AnswerTwo: "15811946526915")
{
    private int SimulateBeam(char[][] manifold, int x, int y)
    {
        if (OutOfBounds(manifold, x, y)) return 0;
        if (manifold[y][x] == '|') return 0;

        if (manifold[y][x] is '.')
        {
            manifold[y][x] = '|';
            return SimulateBeam(manifold, x, y + 1);
        }

        return 1 // count the split
            + SimulateBeam(manifold, x - 1, y)
            + SimulateBeam(manifold, x + 1, y);
    }

    public override Solution PartOne(IEnumerable<string> input)
    {
        var manifold = input.Select(l => l.ToCharArray()).ToArray();

        return SimulateBeam(
            manifold,
            x: input.First().IndexOf('S'),
            y: 0
        );
    }


    private long SimulateBeamManyWorldsInterpretation(
        char[][] manifold,
        long[][] memo,
        int x,
        int y
    )
    {
        // reached the end => 1 path found
        if (OutOfBounds(manifold, x, y)) return y == manifold.Length ? 1 : 0;

        if (manifold[y][x] == '|')
        {
            return memo[y][x];
        }

        else if (manifold[y][x] == '.')
        {
            var pathCount = SimulateBeamManyWorldsInterpretation(manifold, memo, x, y + 1);

            memo[y][x] = pathCount;
            manifold[y][x] = '|';

            return pathCount;
        }
        else // split into 2 timelines
        {
            return
                SimulateBeamManyWorldsInterpretation(manifold, memo, x - 1, y)
                + SimulateBeamManyWorldsInterpretation(manifold, memo, x + 1, y);
        }
    }

    public override Solution PartTwo(IEnumerable<string> input)
    {
        var manifold = input.Select(l => l.ToCharArray()).ToArray();
        var memo = manifold.Select(r => r.Select(_ => 0L).ToArray()).ToArray();

        return SimulateBeamManyWorldsInterpretation(
            manifold,
            memo,
            x: input.First().IndexOf('S'),
            y: 0
        );
    }

    private static bool OutOfBounds(char[][] map, int x, int y)
    {
        return x >= map[0].Length || x < 0 || y >= map.Length || y < 0;
    }


    // --- Example Inputs --- ///


    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "21",
            Input: """
            .......S.......
            ...............
            .......^.......
            ...............
            ......^.^......
            ...............
            .....^.^.^.....
            ...............
            ....^.^...^....
            ...............
            ...^.^...^.^...
            ...............
            ..^...^.....^..
            ...............
            .^.^.^.^.^...^.
            ...............
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "40",
            Input: PartOneExamples[0].Input
        ),
    ];
}
