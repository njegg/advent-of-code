namespace _2024_cs.Solutions;
using Vec2 = Day08.Vec2;


// --- Day 12: Garden Groups --- //


public record Day12() : Solver(AnswerOne: "1549354", AnswerTwo: "937032")
{
    public override string PartOne(IEnumerable<string> input)
    {
        var garden = input.ToList();
        var visited = garden.Select(l => l.Select(_ => false).ToList()).ToList();

        var totalPrice = 0;

        for (var y = 0; y < garden.Count; y++)
        {
            for (var x = 0; x < garden[0].Length; x++)
            {
                var (perimeter, area) = Traverse(garden, visited, x, y, garden[y][x]);
                totalPrice += perimeter * area;
            }
        }

        return totalPrice.ToString();
    }

    private static (int perimeter, int area) Traverse(List<string> garden, List<List<bool>> visited, int x, int y, char region)
    {
        if (OutOfBounds(garden, x, y)) return (1, 0);
        if (visited[y][x] && garden[y][x] == region) return (0, 0);
        if (garden[y][x] != region) return (1, 0);

        visited[y][x] = true;
        var sum = (perimeter: 0, area: 1);

        foreach (var dir in Directions)
        {
            var (dx, dy) = (dir.X(), dir.Y());
            
            var (perimeter, area) = Traverse(garden, visited, x + dx, y + dy, region);
            sum.perimeter += perimeter;
            sum.area += area;
        }

        return sum;
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var garden = input.ToList();
        var visited = garden.Select(l => l.Select(_ => false).ToList()).ToList();

        var totalPrice = 0;

        for (var y = 0; y < garden.Count; y++)
        {
            for (var x = 0; x < garden[0].Length; x++)
            {
                var directionSortedFencePoints = new Dictionary<Direction, List<Vec2>>();
                
                var area = GetAreaAndFindFencePoints(
                    garden,
                    visited,
                    directionSortedFencePoints,
                    x,
                    y,
                    Direction.Right,
                    garden[y][x]
                );

                if (directionSortedFencePoints.Count > 0)
                    totalPrice += area * CalculateSides(directionSortedFencePoints);
            }
        }

        return totalPrice.ToString();
    }
    
    private static int GetAreaAndFindFencePoints(
        List<string> garden,
        List<List<bool>> visited,
        Dictionary<Direction, List<Vec2>> fences,
        int x,
        int y,
        Direction direction,
        char region
    ) {
        if (OutOfBounds(garden, x, y))
        {
            fences.TryAdd(direction, []);
            fences[direction].Add(new Vec2(x, y));
            
            return 0;
        }

        if (visited[y][x] && garden[y][x] == region) return 0;

        if (garden[y][x] != region)
        {
            fences.TryAdd(direction, []);
            fences[direction].Add(new Vec2(x, y));
            
            return 0;
        }

        visited[y][x] = true;

        return 1 + Directions.Sum(
            dir => GetAreaAndFindFencePoints(garden, visited, fences, x + dir.X(), y + dir.Y(), dir, region)
        );
    }
    
    private static int CalculateSides(Dictionary<Direction, List<Vec2>> directionSortedFences)
    {
        var totalSides = 0;
        
        foreach (var (dir, points) in directionSortedFences.ToList())
        {
            var alignByX = dir is Direction.Right or Direction.Left;

            totalSides += points
                .GroupBy(point => alignByX ? point.X : point.Y)
                .Select(groupedByAlignment => groupedByAlignment.ToList())
                .Select(alignedPoints => alignedPoints
                    .Select(p => alignByX ? p.Y : p.X)
                    .Order()
                    .ToList()
                )
                .Select(sorted =>
                {
                    var sides = 1;
                    
                    for (var i = 1; i < sorted.Count; i++)
                        if (sorted[i] - sorted[i - 1] > 1) sides++;

                    return sides;
                })
                .Sum();
        }

        return totalSides;
    }

    private static List<Direction> Directions => [Direction.Right, Direction.Down, Direction.Left, Direction.Up];

    private static bool OutOfBounds(List<string> map, int x, int y) 
        => y < 0 || x < 0 || y >= map.Count || x >= map[0].Length;

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "140",
            Input: """
            AAAA
            BBCD
            BBCC
            EEEC
            """
        ),
        (
            Expected: "772",
            Input: """
            OOOOO
            OXOXO
            OOOOO
            OXOXO
            OOOOO
            """
        ),
        (
            Expected: "1930",
            Input: """
            RRRRIICCFF
            RRRRIICCCF
            VVRRRCCFFF
            VVRCCCJFFF
            VVVVCJJCFE
            VVIVCCJJEE
            VVIIICJJEE
            MIIIIIJJEE
            MIIISIJEEE
            MMMISSJEEE
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "80",
            Input: """
            AAAA
            BBCD
            BBCC
            EEEC
            """
        ),
        (
            Expected: "368",
            Input: """
            AAAAAA
            AAABBA
            AAABBA
            ABBAAA
            ABBAAA
            AAAAAA
            """
        ),
    ];
}