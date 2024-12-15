using System.Text;
using _2024_cs.Extension;

namespace _2024_cs.Solutions;


// --- Day 15: Warehouse Woes --- //


public record Day15() : Solver(AnswerOne: "1349898", AnswerTwo: "1376686")
{
    private const char Wall = '#';
    private const char Barrel = 'O';
    private const char Robot = '@';
    private const char Nothing = '.';
    private const char BoxLeft = '[';
    private const char BoxRight = ']';

    public override string PartOne(IEnumerable<string> input)
    {
        var simulate = Environment.GetEnvironmentVariable("AOC") is "sim";
        
        var mapAndSteps = input.ToList();
        var splitAt = mapAndSteps.IndexOf("");
        var map = mapAndSteps.Take(splitAt).Select(s => s.ToCharArray()).ToArray();
        var (robotX, robotY) = RobotWhere(map);

        var steps = mapAndSteps
            .Skip(splitAt)
            .SelectMany(l => l.Select(ToDirection));

        if (simulate)
        {
            Console.Clear();
            Console.CursorVisible = false;
        }

        foreach (var dir in steps)
        {
            (robotX, robotY) = MoveRobot(map, robotX, robotY, dir);

            if (simulate) PrintMap(map);
        }

        var boxGPSSum = 0;
        
        for (var y = 0; y < map.Length; y++)
            for (var x = 0; x < map[0].Length; x++)
                if (map[y][x] == Barrel) boxGPSSum += 100 * y + x;
        
        return boxGPSSum.ToString();
    }

    private static (int x, int y) MoveRobot(char[][] map, int rx, int ry, Direction dir)
    {
        var (x, y) = (rx + dir.X(), ry + dir.Y());

        if (map[y][x] == Wall) return (rx, ry);
        if (map[y][x] == Barrel && !TryMove(map, x, y, dir)) return (rx, ry);

        map[ry][rx] = Nothing;
        map[y][x] = Robot;

        return (x, y);
    }
    
    private static (int x, int y) MoveRobotInBigMap(char[][] map, int rx, int ry, Direction dir)
    {
        var (x, y) = (rx + dir.X(), ry + dir.Y());

        if (map[y][x] is Wall) return (rx, ry);
        if (
            map[y][x] is BoxLeft or BoxRight 
            && !TryMoveBox(map, x, y, dir)
        ) return (rx, ry);

        map[ry][rx] = Nothing;
        map[y][x] = Robot;

        return (x, y);
    }

    private static bool TryMove(char[][] map, int x, int y, Direction dir)
    {
        while (true)
        {
            switch (map[y][x])
            {
                case Wall: return false;
                
                case Nothing:
                    map[y][x] = Barrel;
                    return true;
                
                default:
                    y += dir.Y();
                    x += dir.X();
                    break;
            }
        }
    }
    
    // ----#-----o---- Part 2 -^---0-----*--- //
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var simulate = Environment.GetEnvironmentVariable("AOC") is "sim";
        
        var mapAndSteps = input.ToList();
        var splitAt = mapAndSteps.IndexOf("");
        var map = mapAndSteps
            .Take(splitAt)
            .Select( s => 
                s.Select(c => c switch
                    {
                        Wall => "##",
                        Nothing => "..",
                        Robot => "@.",
                        Barrel => "[]",
                        _ => throw new ArgumentOutOfRangeException(nameof(c), c, null)
                    }
                )
                .SelectMany(x => x)
                .ToArray()
            )
            .ToArray();

        var (robotX, robotY) = RobotWhere(map);

        var steps = mapAndSteps
            .Skip(splitAt)
            .SelectMany(l => l.Select(ToDirection));

        if (simulate)
        {
            Console.Clear();
            Console.CursorVisible = false;
        }

        foreach (var dir in steps)
        {
            (robotX, robotY) = MoveRobotInBigMap(map, robotX, robotY, dir);

            if (simulate) PrintMap(map);
        }

        Console.CursorVisible = true;

        var boxGPSSum = 0;
        
        for (var y = 0; y < map.Length; y++)
            for (var x = 0; x < map[0].Length; x++)
                if (map[y][x] == BoxLeft) boxGPSSum += 100 * y + x;
        
        
        return boxGPSSum.ToString();
    }

    private static void PrintMap(char[][] map)
    {
        Console.SetCursorPosition(0, 0);
        Console.WriteLine(map
            .Select(s => new string(s))
            .Stringify("\n")
            .Aggregate(
                seed: new StringBuilder(), 
                (sb, c) => sb.Append(c switch
                {
                    Robot => $"{R}{c}",
                    Wall => $"{G}{c}",
                    Nothing => $"{L}{c}",
                    Barrel or BoxRight or BoxLeft => $"{Y}{c}",
                    _ => c
            
                }).Append(X)
            )
        );
    }

    private static bool TryMoveBox(char[][] map, int x, int y, Direction dir)
    {
        if (dir is Direction.Left or Direction.Right)
        {
            return MoveBoxHorizontal(map, x, y, dir);
        }

        if (CanMoveBoxVertical(map, x, y, dir, true))
        {
            MoveBoxVertical(map, x, y, dir, replaceWith: Robot, checkTheOtherSide: true);
            map[y][x] = Nothing;
            return true;
        }
            
        return false;
    }

    private static bool MoveBoxHorizontal(char[][] map, int x, int y, Direction dir)
    {
        var tile = map[y][x];

        if (tile is Wall) return false;
        if (tile is Nothing) 
        {
            map[y][x] = dir is Direction.Right ? BoxRight : BoxLeft;
            return true;
        }

        if (!MoveBoxHorizontal(map, x + dir.X(), y, dir)) return false;
        
        map[y][x] = tile is BoxRight ? BoxLeft : BoxRight; // simulate shift by flipping
        return true;
    }
    
    private static void MoveBoxVertical(char[][] map, int x, int y, Direction dir, char replaceWith, bool checkTheOtherSide)
    {
        var currentBoxPart = map[y][x];
        map[y][x] = replaceWith;

        if (currentBoxPart == Nothing) return;

        MoveBoxVertical(map, x, y + dir.Y(), dir, currentBoxPart, true);
        
        if (checkTheOtherSide)
        {
            var xDir = currentBoxPart == BoxRight ? -1 : 1;
            MoveBoxVertical(map, x + xDir, y, dir, replaceWith: Nothing, false);
        }
    }

    private static bool CanMoveBoxVertical(char[][] map, int x, int y, Direction dir, bool checkOtherPart)
    {
        return map[y][x] switch
        {
            Wall => false,
            Nothing => true,
            
            BoxLeft => CanMoveBoxVertical(map, x, y + dir.Y(), dir, true) 
                       && (!checkOtherPart || CanMoveBoxVertical(map, x + 1, y, dir, false)),
            
            BoxRight => CanMoveBoxVertical(map, x, y + dir.Y(), dir, true) 
                        && (!checkOtherPart || CanMoveBoxVertical(map, x - 1, y, dir, false)),
            
            _ => throw new ArgumentException($"wtf: arr[{y}][{x}] == {map[y][x]}")
        };
    }

    private static Direction ToDirection(char step)
    {
        return step switch
        {
            '>' => Direction.Right,
            '<' => Direction.Left,
            'v' => Direction.Down,
            '^' => Direction.Up,

            _ => throw new ArgumentOutOfRangeException(nameof(step), step, null)
        };
    }

    private static (int, int) RobotWhere(char[][] map)
    {
        for (var y = 0; y < map.Length; y++)
            for (var x = 0; x < map[0].Length; x++)
                if (map[y][x] == Robot) return (x, y);
        return (-1, -1);
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "2028",
            Input: """
            ########
            #..O.O.#
            ##@.O..#
            #...O..#
            #.#.O..#
            #...O..#
            #......#
            ########
            
            <^^>>>vv<v>>v<<
            """
        ),
        (
            Expected: "10092",
            Input: """
            ##########
            #..O..O.O#
            #......O.#
            #.OO..O.O#
            #..O@..O.#
            #O#..O...#
            #O..O..O.#
            #.OO.O.OO#
            #....O...#
            ##########
            
            <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
            vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
            ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
            <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
            ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
            ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
            >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
            <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
            ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
            v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "9021",
            Input: """
            ##########
            #..O..O.O#
            #......O.#
            #.OO..O.O#
            #..O@..O.#
            #O#..O...#
            #O..O..O.#
            #.OO.O.OO#
            #....O...#
            ##########
            
            <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
            vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
            ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
            <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
            ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
            ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
            >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
            <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
            ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
            v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
            """
        ),
    ];
    
    private const string G = "\u001b[32m";
    private const string R = "\u001b[31m";
    private const string X = "\u001b[0m";
    private const string L = "\u001b[90m";
    private const string Y = "\u001b[33m";
}