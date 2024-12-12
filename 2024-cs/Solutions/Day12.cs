namespace _2024_cs.Solutions;


// --- Day 12: Garden Groups --- //


public record Day12() : Solver(AnswerOne: "1549354", AnswerTwo: null)
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
    
    public override string PartTwo(IEnumerable<string> input)
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
    
    private static readonly (int X, int Y)[] Directions = [(1, 0), (0, 1), (-1, 0), (0, -1)];

    private static (int perimeter, int area) Traverse(List<string> garden, List<List<bool>> visited, int x, int y, char region)
    {
        if (OutOfBounds(garden, x, y)) return (1, 0);
        if (visited[y][x] && garden[y][x] == region) return (0, 0);
        if (garden[y][x] != region) return (1, 0);

        visited[y][x] = true;
        var sum = (perimeter: 0, area: 1);

        foreach (var (dx, dy) in Directions)
        {
            var (perimeter, area) = Traverse(garden, visited, x + dx, y + dy, region);
            sum.perimeter += perimeter;
            sum.area += area;
        }

        return sum;
    }

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
    ];
}