using _2024_cs.Extension;

namespace _2024_cs.Solutions;


// --- Day 6: Guard Gallivant --- //


public record Day06() : Solver(AnswerOne: "4973", AnswerTwo: "1482")
{
    private static readonly int[] DirectionsX = [0, 1, 0, -1];
    private static readonly int[] DirectionsY = [-1, 0, 1, 0];
    
    public override string PartOne(IEnumerable<string> input)
    {
        var map = input.Select(row => row.ToCharArray()).ToArray();
        var (x, y) = map
            .WithIndex()
            .Select((row, y) => (x: Array.IndexOf(row, '^'), y))
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
        var map = input.Select(l => l.ToCharArray()).ToArray();
        var (startX, startY) = map
            .WithIndex()
            .Select((row, y) => (x: Array.IndexOf(row, '^'), y))
            .First(coord => coord.x != -1);
        
        var visitedMap = new Direction[map.Length * map[0].Length];
        
        Traverse(map, visitedMap, startX, startY, Direction.Up);
        
        var possibleWalls = visitedMap
            .WithIndex()
            .Where(d => d.e != Direction.None && d.i != startY * map[0].Length + startX)
            .Select(d => (y: d.i / map[0].Length, x: d.i % map[0].Length))
            .ToList();
        
        Array.Fill(visitedMap, Direction.None);
        
        var loopCount = 0;
        foreach (var wall in possibleWalls)
        {
            var (y, x) = wall;
            map[y][x] = '#';
            Array.Fill(visitedMap, Direction.None);

            var traverseResult = Traverse(map, visitedMap, startX, startY, Direction.Up);
            if (traverseResult == TraverseResult.Loop) loopCount++;

            map[y][x] = '.';
        }

        return loopCount.ToString();
    }

    private enum TraverseResult { Wall, OutOfBounds, Loop }

    private static TraverseResult Traverse(char[][] map, Direction[] visitedMap, int x, int y, Direction direction)
    {
        var visitedIndex = y * map[0].Length + x;
        
        if (OutOfBounds(map, x, y)) return TraverseResult.OutOfBounds;
        if (visitedMap[visitedIndex] == direction) return TraverseResult.Loop;
        if (map[y][x] == '#') return TraverseResult.Wall;

        visitedMap[visitedIndex] = direction;

        while (true)
        {
            var res = Traverse(map, visitedMap, x + direction.X(), y + direction.Y(), direction);

            if (res != TraverseResult.Wall) return res;

            direction = direction.RotateClockwise();
        }
    }

    private static bool OutOfBounds(char[][] map, int x, int y) 
        => y < 0 || x < 0 || y >= map.Length || x >= map[0].Length;

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

public enum Direction { None, Up, Down, Right, Left }
    
public static partial class Extension
{
    public static int X(this Direction dir) => dir switch
    {
        Direction.Right => 1,
        Direction.Left => -1,
        _ => 0
    };
    
    public static int Y(this Direction dir) => dir switch
    {
        Direction.Up => -1,
        Direction.Down => 1,
        _ => 0
    };
    
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