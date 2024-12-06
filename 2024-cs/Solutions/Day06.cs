using _2024_cs.Extension;

namespace _2024_cs.Solutions;


// --- Day 5: Print Queue --- //


public record Day06() : Solver(AnswerOne: null, AnswerTwo: null)
{
    private static readonly int[] DirectionsX = [0, 1, 0, -1];
    private static readonly int[] DirectionsY = [-1, 0, 1, 0];
    public override string PartOne(IEnumerable<string> input)
    {
        var map = input.Select(row => row.ToList()).ToList();
        var (x, y) = map
            .WithIndex()
            .Select((row, y) => (x: row.IndexOf('^'), y))
            .First(coord => coord.x != -1);

        var visitedMap = map.Select(row => row.Select(_ => 0).ToList()).ToList();

        var dirIndex = 0;
        
        var infiniteLoopProtec = 1000000;
        while (infiniteLoopProtec --> 0)
        {
            if (OutOfBounds(map, x, y)) break;
            if (map[y][x] == '#')
            {
                x -= DirectionsX[dirIndex];
                y -= DirectionsY[dirIndex];
                
                dirIndex = (dirIndex + 1) % DirectionsX.Length;
                continue;
            }

            visitedMap[y][x] = 1;

            x += DirectionsX[dirIndex];
            y += DirectionsY[dirIndex];
        }

        return visitedMap.Select(l => l.Sum()).Sum().ToString();
    }

    public override string PartTwo(IEnumerable<string> input)
    {
        var map = input.Select(l => l.ToList()).ToList();
        var (startX, startY) = map
            .WithIndex()
            .Select((row, y) => (x: row.IndexOf('^'), y))
            .First(coord => coord.x != -1);

        var visitedMap = map.Select(row => row.Select(_ => (visited: false, direction: Direction.Up)).ToList()).ToList();
        var loopCount = 0;

        for (var y = 0; y < map.Count; y++)
        {
            for (var x = 0; x < map[0].Count; x++)
            {
                if ((x == startX && y == startY) || map[y][x] == '#') continue;

                map[y][x] = '#';
                ClearVisitedMap(visitedMap);
                loopCount += Traverse(map, visitedMap, startX, startY, Direction.Up) == TraverseResult.Loop ? 1 : 0;

                map[y][x] = '.';
            }
        }

        return loopCount.ToString();
    }

    private enum TraverseResult { Wall, OutOfBounds, Loop }

    private static TraverseResult Traverse(List<List<char>> map, List<List<(bool visited, Direction direction)>> visitedMap, int x, int y, Direction direction)
    {
        if (OutOfBounds(map, x, y)) return TraverseResult.OutOfBounds;
        if (visitedMap[y][x].visited && visitedMap[y][x].direction == direction) return TraverseResult.Loop;
        if (map[y][x] == '#') return TraverseResult.Wall;

        visitedMap[y][x] = (true, direction);

        while (true)
        {
            var res = Traverse(map, visitedMap, x + direction.XY().x, y + direction.XY().y, direction);

            if (res == TraverseResult.Wall)
            {
                direction = direction.RotateClockwise();
            }
            else
            {
                return res;
            }
        }
    }

    private static void ClearVisitedMap(List<List<(bool Visited, Direction Direction)>> visitedMap)
    {
        for (var i = 0; i < visitedMap.Count; i++)
        {
            for (var j = 0; j < visitedMap[0].Count; j++)
            {
                visitedMap[i][j] = (false, Direction.Up);
            }
        }
    }
    
    private static bool OutOfBounds(List<List<char>> map, int x, int y) 
        => y < 0 || x < 0 || y >= map.Count || x >= map[0].Count;

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "41",
            Input: 
            """
            ....#.....
            .........#
            ..........
            ..#.......
            .......#..
            ..........
            .#..^.....
            ........#.
            #.........
            ......#...
            """
        )
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "6",
            Input: 
            """
            ....#.....
            .........#
            ..........
            ..#.......
            .......#..
            ..........
            .#..^.....
            ........#.
            #.........
            ......#...
            """
        )
    ];
}

public enum Direction { Up, Down, Right, Left }
    
public static class Extension
{
    public static (int x, int y) XY(this Direction dir)
    {
        return dir switch
        {
            Direction.Up => (0, -1),
            Direction.Down => (0, 1),
            Direction.Right => (1, 0),
            Direction.Left => (-1, 0),
            _ => throw new ArgumentOutOfRangeException(nameof(dir), dir, null)
        };
    }
    
    public static Direction RotateClockwise(this Direction dir)
    {
        return dir switch
        {
            Direction.Up => Direction.Right,
            Direction.Right => Direction.Down,
            Direction.Down => Direction.Left,
            Direction.Left => Direction.Up,
            _ => throw new ArgumentOutOfRangeException(nameof(dir), dir, null)
        };
    }
}