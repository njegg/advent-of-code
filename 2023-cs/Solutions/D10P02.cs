using AoC_2023.Extension;
using static AoC_2023.Solutions.D10P02.Pipe;

namespace AoC_2023.Solutions;

// --- Day 10: Pipe Maze - Part 1 --- //

public class D10P02 : Solution
{
    public override string? Answer => "353";
    public override string? ExampleAnswer => "10";

    private record struct V2 (int x, int y);

    [Flags]
    public enum Pipe
    {
        // Directions
        Down = 1,
        Up = 2,
        Right = 4,
        Left = 8,
    
        // Pipes
        LeftRight = Left | Right,
        UpDown = Up | Down,
    
        UpRight = Up | Right,
        UpLeft = Up | Left,
    
        DownLeft = Down | Left,
        DownRight = Down | Right,
    
        Start = Up | Down | Left | Right,
        
    
        Ground = 0,
        
        MainLoopFlag = 16,
        CheckedFlag = 32,
        Hole = 64,
    }

    public override string Solve(IEnumerable<string> lines)
    {
        var smallMap = lines
            .Select(l => l.Select(ToPipe).ToArray())
            .ToArray();
        
        var map = Enhance(smallMap);

        MarkMainLoop(map);

        var answer = 0;

        for (var y = 0; y < map.Length; y++)
        {
            for (var x = 0; x < map[0].Length; x++)
            {
                var tile = map[y][x];

                if ((tile & CheckedFlag) == 0)
                {
                    var tilesInside = CheckGround(map, x, y, false);
                    if (tilesInside > 0) answer += tilesInside;
                }
            }
        }

        return answer.ToString();
    }

    private static Pipe[][] Enhance(Pipe[][] map)
    {
        var bigMap = new Pipe[map.Length * 2][];
        for (var y = 0; y < bigMap.Length; y++)
            bigMap[y] = new Pipe[map[0].Length * 2];
        
        // Fill
        for (var y = 0; y < bigMap.Length; y++)
        {
            for (var x = 0; x < bigMap[0].Length; x++)
            {
                bigMap[y][x] = x % 2 == 0 && y % 2 == 0 ?
                    map[y / 2][x / 2] :
                    Hole;
            }
        }

        // Connect the holes
        for (var y = 0; y < bigMap.Length; y++)
        {
            var yOdd = y % 2 == 1;
            for (var x = 0; x < bigMap[0].Length; x++)
            {
                var xOdd = x % 2 == 1;
                
                if (xOdd == yOdd) continue;

                if (yOdd) // Connect up and down
                {
                    if (y == bigMap.Length - 1) continue;
                    
                    bigMap[y][x] = PipesConnect(bigMap[y + 1][x], bigMap[y - 1][x], Up) ?
                        UpDown | Hole : Hole;
                }
                else // Connect left and right
                {
                    if (x == bigMap[0].Length - 1) continue;
                    
                    bigMap[y][x] = PipesConnect(bigMap[y][x + 1], bigMap[y][x - 1], Left) ?
                         LeftRight | Hole : Hole;
                }
            }
        }


        return bigMap;
    }

    private static void MarkMainLoop(Pipe[][] map)
    {
        var start = FindStart(map);

        var prev = start;
        var current = FindNextPipe(map, prev, prev);
        
        while (current != start)
        {
            var oldCurrent = current;
            current = FindNextPipe(map, current, prev);
            prev = oldCurrent;
            
            map[prev.y][prev.x] |= MainLoopFlag;
        }

        map[current.y][current.x] |= MainLoopFlag;
    }

    private static int[] dy = { 0, 0, 1, -1 };
    private static int[] dx = { 1, -1, 0, 0 };
    private static Pipe[] ddir = { Right, Left, Down, Up };
    
    private static int CheckGround(Pipe[][] map, int px, int py, bool dirty)
    {
        if (px < 0 || py < 0 || py >= map.Length || px >= map[0].Length) return -1;
        
        var tile = map[py][px];
        if ((tile & CheckedFlag) == CheckedFlag) return 0;
        if ((tile & MainLoopFlag) == MainLoopFlag) return 0;
        
        map[py][px] |= CheckedFlag;
        
        var isInside = !dirty;
        var nestTiles = 0;

            
        for (var i = 0; i < 4; i++)
        {
            var x = px + dx[i];
            var y = py + dy[i];
            

            var tilesInside = CheckGround(map, x, y, !isInside);
            
            if (!isInside || tilesInside == -1)
            {
                isInside = false;
            }
            else if (isInside)
            {
                nestTiles += tilesInside;
            }
        }

        if (isInside)
        {
            return nestTiles + ((tile & Hole) == Hole ? 0 : 1);
        }

        return -1;
    }
    

    private static V2 FindNextPipe(Pipe[][] map, V2 current, V2 prev)
    {
        for (var i = 0; i < 4; i++)
        {
            var x = current.x + dx[i];
            var y = current.y + dy[i];

            if (
                x == prev.x && y == prev.y ||
                x < 0 || y < 0 || y >= map.Length || x >= map[0].Length
            ) continue;
            
            var nextPipe = map[y][x];
            var currentPipe = map[current.y][current.x];
            
            if (PipesConnect(currentPipe, nextPipe, ddir[i]))
                return new V2(x, y);
        }

        throw new ArgumentException("Failed to find next pipe");
    }

    private static bool PipesConnect(Pipe from, Pipe to, Pipe dir)
    {
        return (from & dir) > 0 && (to & dir.Opposite()) > 0;
    }

    private static V2 FindStart(IEnumerable<Pipe[]> map)
    {
        foreach (var (row, y) in map.WithIndex())
        {
            var x = Array.IndexOf(row, Start);
            if (x >= 0) return new V2{ x = x, y = y };
        }

        throw new Exception("Start position was not found");
    }
    
    
    private static Pipe ToPipe(char c)
    {
        return c switch
        {
            '|' => UpDown,
            '-' => LeftRight,
            'L' => UpRight,
            'J' => UpLeft,
            '7' => DownLeft,
            'F' => DownRight,
            '.' => Ground,
            'S' => Start,
            
            _ => throw new ArgumentException($"'{c}' cannot be converted to a Pipe"),
        };
    }
}

internal static partial class Extensions
{
    public static D10P02.Pipe Opposite(this D10P02.Pipe pipe) => pipe switch
    {
        Down => Up,
        Up => Down,
        Right => Left,
        Left => Right,
        
        _ => throw new ArgumentException($"Cannot reverse {pipe}")
    };
}