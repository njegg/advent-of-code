using _2024_cs.Extension;

namespace _2024_cs.Solutions;
using Vec2 = Day08.Vec2;


//  //


public record Day16() : Solver(AnswerOne: "114476", AnswerTwo: "508")
{
    private static readonly Direction[] Dirs = [Direction.Up, Direction.Right, Direction.Down, Direction.Left];
    
    private record BestPath
    {
        public int Cost = int.MaxValue;
    }
    
    public override string PartOne(IEnumerable<string> input)
    {
        var map = input.Select(l => l.ToCharArray()).ToArray();
        var visited = map.Select(row => row.Select(_ => int.MaxValue).ToArray()).ToArray();

        var start = FindStart(map);

        var bestPath = new BestPath();
        
        FindBestPath(map, visited, start.X, start.Y, 0, Direction.Right, ref bestPath);

        return bestPath.Cost.ToString();
    }

    private static void FindBestPath(
        char[][] map,
        int[][] visited,
        int x,
        int y,
        int currentCost,
        Direction currentDir,
        ref BestPath bestPath)
    {
        if (map[y][x] == 'E')
        {
            if (currentCost < bestPath.Cost) bestPath.Cost = currentCost;
            return;
        }
        
        if (
            map[y][x] == '#' ||
            currentCost >= bestPath.Cost ||
            visited[y][x] < currentCost
        ) return;

        visited[y][x] = currentCost;
        
        foreach (var dir in Dirs)
        {
            var (dx, dy) = (x + dir.X(), y + dir.Y());
            var cost = dir.IsVertical() == currentDir.IsVertical() ? 1 : 1001;
            
            FindBestPath(map, visited, dx, dy, currentCost + cost, dir, ref bestPath);
        }
    }
    
    
    // ----0-----o-------- Part 2 ----*--------------------- //

    
    public override string PartTwo(IEnumerable<string> input)
    {
        var map = input.Select(l => l.ToCharArray()).ToArray();
        var visited = map.Select(row => row.Select(_ => new Visit()).ToArray()).ToArray();
        var start = FindStart(map);

        var bro = new Bro();
        
        FindBestSpots(map, visited, start.X, start.Y, 0, Direction.Right, bro);

        return bro.BestSpots.Count.ToString();
    }

    private record Bro
    {
        public readonly Stack<int> CurrentSpots = new();
        public readonly HashSet<int> BestSpots = new();
        public int Cost = int.MaxValue;
    }

    private class Visit
    {
        public Direction Dir = Direction.None;
        public int Cost = int.MaxValue;
    }

    private static void FindBestSpots(
        char[][] map,
        Visit[][] visited,
        int x,
        int y,
        int currentCost,
        Direction currentDir,
        Bro bro
    ) {
        if (map[y][x] == 'E')
        {
            bro.CurrentSpots.Push(y * map.Length + x);
            
            if (currentCost < bro.Cost)
            {
                bro.Cost = currentCost;
                bro.BestSpots.Clear();
            }

            foreach (var spot in bro.CurrentSpots)
            {
                bro.BestSpots.Add(spot);
            }
            
            bro.CurrentSpots.Pop();
            return;
        }
        
        if (
            currentCost >= bro.Cost ||
            map[y][x] == '#' ||
            visited[y][x].Cost < currentCost && 
            visited[y][x].Dir.IsVertical() == currentDir.IsVertical() // try from a different dir
        ) return;

        visited[y][x] = new Visit { Cost = currentCost, Dir = currentDir };
        bro.CurrentSpots.Push(y * map.Length + x);
        
        foreach (var dir in Dirs)
        {
            var (dx, dy) = (x + dir.X(), y + dir.Y());
            var cost = dir.IsVertical() == currentDir.IsVertical() ? 1 : 1001;
            
            FindBestSpots(map, visited, dx, dy, currentCost + cost, dir, bro);
        }
        
        bro.CurrentSpots.Pop();
    }

    private static Vec2 FindStart(char[][] map)
    {
        var start = new Vec2(-1, -1);
        for (var y = 0; y < map.Length; y++)
            for (var x = 0; x < map[0].Length; x++)
                if (map[y][x] == 'S') start = new(x, y);
        return start;
    }
    
    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "7036",
            Input: """
            ###############
            #.......#....E#
            #.#.###.#.###.#
            #.....#.#...#.#
            #.###.#####.#.#
            #.#.#.......#.#
            #.#.#####.###.#
            #...........#.#
            ###.#.#####.#.#
            #...#.....#.#.#
            #.#.#.###.#.#.#
            #.....#...#.#.#
            #.###.#.#.#.#.#
            #S..#.....#...#
            ###############
            """
        ),
        (
            Expected: "11048",
            Input: """
            #################
            #...#...#...#..E#
            #.#.#.#.#.#.#.#.#
            #.#.#.#...#...#.#
            #.#.#.#.###.#.#.#
            #...#.#.#.....#.#
            #.#.#.#.#.#####.#
            #.#...#.#.#.....#
            #.#.#####.#.###.#
            #.#.#.......#...#
            #.#.###.#####.###
            #.#.#...#.....#.#
            #.#.#.#####.###.#
            #.#.#.........#.#
            #.#.#.#########.#
            #S#.............#
            #################
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "45",
            Input: """
            ###############
            #.......#....E#
            #.#.###.#.###.#
            #.....#.#...#.#
            #.###.#####.#.#
            #.#.#.......#.#
            #.#.#####.###.#
            #...........#.#
            ###.#.#####.#.#
            #...#.....#.#.#
            #.#.#.###.#.#.#
            #.....#...#.#.#
            #.###.#.#.#.#.#
            #S..#.....#...#
            ###############
            """
        ),
        (
            Expected: "64",
            Input: """
            #################
            #...#...#...#..E#
            #.#.#.#.#.#.#.#.#
            #.#.#.#...#...#.#
            #.#.#.#.###.#.#.#
            #...#.#.#.....#.#
            #.#.#.#.#.#####.#
            #.#...#.#.#.....#
            #.#.#####.#.###.#
            #.#.#.......#...#
            #.#.###.#####.###
            #.#.#...#.....#.#
            #.#.#.#####.###.#
            #.#.#.........#.#
            #.#.#.#########.#
            #S#.............#
            #################
            """
        ),
    ];
}