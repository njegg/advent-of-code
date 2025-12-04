using System.Drawing;
using System.Text;

namespace _2024_cs.Solutions;


// --- Day 4: Printing Department --- //


public record Day04() : Solver(AnswerOne: "1435", AnswerTwo: "8623")
{
    private static readonly int[] XNeighbours = [0, 1, 1, 1, 0, -1, -1, -1]; 
    private static readonly int[] YNeighbours = [-1, -1, 0, 1, 1, 1, 0, -1];    
    
    public override string PartOne(IEnumerable<string> input)
    {
        var paperMap = input.Select(l => l.ToCharArray()).ToArray();

        var movablePaperCount = 0;
        
        var (width, height) = (paperMap[0].Length, paperMap.Length);

        for (var y = 0; y < height; y++)
        {
            for (var x = 0; x < width; x++)
            {
                if (paperMap[y][x] != '@') continue;
                
                var paperCount = 0;
                
                for (var di = 0; di < 8; di++)
                {
                    var nx = x + XNeighbours[di];
                    var ny = y + YNeighbours[di];
                    
                    if (OutOfBounds(paperMap, nx, ny)) continue;
                    if (paperMap[ny][nx] != '.') paperCount++;
                }

                if (paperCount < 4)
                {
                    paperMap[y][x] = 'x';
                    movablePaperCount++;
                }
            }
        }

        if (Simulate)
        {
            PrintMap(paperMap);
        }


        return movablePaperCount.ToString();
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var paperMap = input.Select(l => l.ToCharArray()).ToArray();

        var totalRemoved = 0;

        if (Simulate)
        {
            Console.CursorVisible = false;
            ClearScreen();
            PrintMap(paperMap);
        }

        while (TryRemovePaper(paperMap, out var movablePapers))
        {
            totalRemoved += movablePapers;

            if (Simulate)
            {
                Thread.Sleep(IsExample() ? 300 : 66);
                ClearScreen();
                PrintMap(paperMap);
            }
        }

        Console.CursorVisible = true;
        return totalRemoved.ToString();
    }

    private static bool TryRemovePaper(char[][] paperMap, out int removedCount)
    {
        var (width, height) = (paperMap[0].Length, paperMap.Length);

        var removablePapers = new List<Point>();

        for (var y = 0; y < height; y++)
        {
            for (var x = 0; x < width; x++)
            {
                if (paperMap[y][x] != '@') continue;
                
                var adjacentPapers = 0;
                
                for (var di = 0; di < 8; di++)
                {
                    var nx = x + XNeighbours[di];
                    var ny = y + YNeighbours[di];
                    
                    if (OutOfBounds(paperMap, nx, ny)) continue;
                    if (paperMap[ny][nx] != '.') adjacentPapers++;
                }

                if (adjacentPapers < 4)
                {
                    removablePapers.Add(new(x, y));
                }
            }
        }

        removablePapers.ForEach(p => paperMap[p.Y][p.X] = '.');
        
        removedCount = removablePapers.Count;
        return removedCount > 0;
    }

    private static bool OutOfBounds(char[][] map, int x, int y)
    {
        return x >= map[0].Length || x < 0 || y >= map.Length || y < 0;
    }

    private static void PrintMap(char[][] map)
    {
        var sb = new StringBuilder(map[0].Length);
        
        var prettierMap = map.Select(r =>
        {
            foreach (var c in r)
            {
                sb.Append(c);
                sb.Append(c);
            }

            var res = sb.ToString().ToCharArray();
            sb.Clear();
            return res;
        }).ToArray();
        
        foreach (var row in prettierMap)
        {
            Console.WriteLine(row);
        }
    }
    
    
    // --- Example Inputs --- ///

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "13",
            Input: """
            ..@@.@@@@.
            @@@.@.@.@@
            @@@@@.@.@@
            @.@@@@..@.
            @@.@@@@.@@
            .@@@@@@@.@
            .@.@.@.@@@
            @.@@@.@@@@
            .@@@@@@@@.
            @.@.@@@.@.
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "43",
            Input: PartOneExamples[0].Input
        ),
    ];
}
