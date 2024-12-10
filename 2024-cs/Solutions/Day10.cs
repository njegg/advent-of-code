using Vec2 = _2024_cs.Solutions.Day08.Vec2;
    
namespace _2024_cs.Solutions;


// --- Day 10: Hoof It --- //


public record Day10() : Solver(AnswerOne: "794", AnswerTwo: "1706")
{
    public override string PartOne(IEnumerable<string> input)
    {
        return TrailheadCount(input.ToList(), new HashSet<Vec2>()).ToString();
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        return TrailheadCount(input.ToList(), new List<Vec2>()).ToString();
    }

    private static int TrailheadCount(List<string> map, ICollection<Vec2> paths)
    {
        var trailheads = 0;
        
        for (var y = 0; y < map.Count; y++)
        {
            for (var x = 0; x < map[0].Length; x++)
            {
                if (map[y][x] != '0') continue;
                
                paths.Clear();
                Traverse(map, x, y, paths);

                trailheads += paths.Count;
            }
        }

        return trailheads;
    }
    
    private static readonly (int, int)[] directions = [(0, -1), (1, 0), (0, 1), (-1, 0)];

    private static void Traverse(List<string> map, int x, int y, ICollection<Vec2> paths) 
    {
        if (OutOfBounds(map, x, y)) return;
        
        var currentHeight = map[y][x];
        
        if (currentHeight == '9')
        {
            paths.Add(new(x, y));
            return;
        }

        foreach (var (dx, dy) in directions)
        {
            var (nextX, nextY) = (x + dx, y + dy);
            if (OutOfBounds(map, nextX, nextY) || map[nextY][nextX] - currentHeight != 1) continue;
            
            Traverse(map, nextX, nextY, paths);
        }
    }
    
    private static bool OutOfBounds(List<string> map, int x, int y) 
        => y < 0 || x < 0 || y >= map.Count || x >= map[0].Length;

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
           Expected: "1",
           Input: """
           0123
           1234
           8765
           9876
           """
        ),
        (
            "36",
            """
            89010123
            78121874
            87430965
            96549874
            45678903
            32019012
            01329801
            10456732
            """
        )
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            "81",
            """
            89010123
            78121874
            87430965
            96549874
            45678903
            32019012
            01329801
            10456732
            """
        ),
    ];
}