using System.Resources;
using AoC_2023.Extension;
using static AoC_2023.Solutions.D10P01.Pipe;

namespace AoC_2023.Solutions;

// --- Day 10: Pipe Maze - Part 1 --- //

public class D10P01 : Solution
{
    public override string? Answer => "6682";
    public override string? ExampleAnswer => "8";
    
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
        
        NotAPipe = 0,
    }

    public override string Solve(IEnumerable<string> lines)
    {
        var map = lines
            .Select(l => l.Select(ToPipe).ToArray())
            .ToArray();
        
        var prev = FindStart(map);
        var current = FindNextPipe(map, prev, prev);
        var steps = 1;
        
        while (map[current.y][current.x] != Pipe.Start)
        {
            var oldCurrent = current;
            current = FindNextPipe(map, current, prev);
            prev = oldCurrent;

            steps++;
        }

        return (steps / 2).ToString();
    }


    private static int[] dy = { 0, 0, 1, -1 };
    private static int[] dx = { 1, -1, 0, 0 };
    private static Pipe[] ddir = { Right, Left, Down, Up };
    
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
            var x = Array.IndexOf(row, Pipe.Start);
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
            '.' => NotAPipe,
            'S' => Start,
            
            _ => throw new ArgumentException($"'{c}' cannot be converted to a Pipe"),
        };
    }
}

internal static partial class Extensions
{
    public static D10P01.Pipe Opposite(this D10P01.Pipe pipe) => pipe switch
    {
        Down => Up,
        Up => Down,
        Right => Left,
        Left => Right,
        
        _ => throw new ArgumentException($"Cannot reverse {pipe}")
    };
}
