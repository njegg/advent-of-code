using _2024_cs.Extension;
using Vec2 = _2024_cs.Solutions.Day08.Vec2;

namespace _2024_cs.Solutions;


// --- Day 14: Restroom Redoubt --- //

///
/// run with env var AOC set to
///     =sim1 - show last few moments before tree is formed
///             slowed down 600 times
///     =sim2 - same as sim1 but continue the sim after the tree
///     =xxx  - just print the tree to admire it
/// 
public record Day14() : Solver(AnswerOne: "214109808", AnswerTwo: "7687")
{
    private const int Time = 100;
    private static int _width = 101;
    private static int _height = 103;
    
    public override string PartOne(IEnumerable<string> input)
    {
        if (Environment.GetEnvironmentVariable("example") is not null)
        {
            _width = 11;
            _height = 7;
        }
        var robots = ParseRobots(input).ToList();
        
        return robots
            .Select(r => r.Pos + r.Vel * Time)
            .Select(p => new Vec2(Wrap(p.X, _width), Wrap(p.Y, _height)))
            .GroupBy(p => GetRegion(p, _width, _height), p => p)
            .Where(g => g.Key != -1)
            .Select(g => g.Count())
            .Aggregate(seed: 1, (a, x) => a * x)
            .ToString();
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var specialFlag = Environment.GetEnvironmentVariable("AOC");

        return specialFlag is null
            ? JustSolve(input)
            : DrawSimulation(input, specialFlag);
    }

    private static int GetRegion(Vec2 p, int w, int h)
    {
        if (p.X < w / 2 && p.Y < h / 2) return 0;
        if (p.X < w / 2 && p.Y > h / 2) return 2;
        if (p.X > w / 2 && p.Y < h / 2) return 1;
        if (p.X > w / 2 && p.Y > h / 2) return 3;
        return -1; // mid
    }

    private static int Wrap(int n, int m) => (n % m + m) % m;

    private record struct Robot()
    {
        public Vec2 Pos = new(0, 0);
        public Vec2 Vel = new(0, 0);
        public Vec2 RenderPos = new(0, 0);
    }

    private static IEnumerable<Robot> ParseRobots(IEnumerable<string> input)
    {
        return input.Select(l =>
            {
                var pv = l
                    .Split(" ")
                    .Select(x => x[2..].Split(",").Select(int.Parse).ToArray())
                    .Select(a => new Vec2(a[0], a[1]))
                    .ToArray();

                return new Robot { Pos = pv[0], Vel = pv[1], RenderPos = pv[0]};
            })
            .ToList();
    }
    
    private static string JustSolve(IEnumerable<string> input)
    {
        var robots = ParseRobots(input).ToArray();

        var center = new Vec2(_width / 2, _height / 2);
        var second = 0;
        while (true)
        {
            var treeDetected = robots
                .Select(r => r.Pos)
                .GroupBy(p => p.DistSquared(center) / 25, p => p)
                .Select(g => g.ToList())
                .Any(g => g.Count > 25);
            
            if (treeDetected) return second.ToString();

            for (var i = 0; i < robots.Length; i++)
            {
                ref var r = ref robots[i];
                r.Pos += r.Vel;
                r.Pos.X = Wrap(r.Pos.X, _width);
                r.Pos.Y = Wrap(r.Pos.Y, _height);
            }

            second++;
        }
    }

    private string DrawSimulation(IEnumerable<string> input, string specialFlag)
    {
        var simulate = specialFlag is "sim1" or "sim2";
        var stopWhenTreeDetected = specialFlag is "sim1";
        
        Console.ForegroundColor = ConsoleColor.Green;
        Console.Clear();
        Console.CursorVisible = false;

        var magicChristmasTreeToleranceAmount = stopWhenTreeDetected ? 25 : 1000;
        
        var robots = ParseRobots(input).ToArray();
        var map = Enumerable.Range(0, _height).Select(_ => Enumerable.Repeat('.', _width * 2).ToList()).ToList();

        var second = 1;
        var frame = 0;
        var fps = 600;
        while (true) // Maybe loops at 10402
        {
            var isCloseToSolution = simulate && second > int.Parse(AnswerTwo!) - 2;
            var secondPassed = frame % fps == 0;
            
            if (SentientAiChristmasTreeDetectorAi(robots, magicChristmasTreeToleranceAmount))
            {
                // Redraw with actual pos to avoid artifacts
                map = Enumerable.Range(0, _height).Select(_ => Enumerable.Repeat('.', _width * 2).ToList()).ToList();
                for (var i = 0; i < robots.Length; i++)
                {
                    ref var r = ref robots[i];
                    map[r.Pos.Y][r.Pos.X * 2] = '#';
                    map[r.Pos.Y][r.Pos.X * 2 + 1] = '#';
                }
                
                PrintMap(map);
                
                Console.CursorVisible = true;
                return (second-1).ToString();
            }
            
            
            if (isCloseToSolution) 
            {
                for (var i = 0; i < robots.Length; i++)
                {
                    ref var r = ref robots[i];
                    map[r.RenderPos.Y][r.RenderPos.X * 2] = '#';
                    map[r.RenderPos.Y][r.RenderPos.X * 2 + 1] = '#';
                }
                PrintMap(map);
                Thread.Sleep(5);
            }

            for (var i = 0; i < robots.Length; i++)
            {
                ref var r = ref robots[i];

                if (isCloseToSolution)
                {
                    map[r.RenderPos.Y][r.RenderPos.X * 2] = '.';
                    map[r.RenderPos.Y][r.RenderPos.X * 2 + 1] = '.';
                }

                var k = frame % fps / (float)fps;
                r.RenderPos = r.Pos + r.Vel * k;
                
                if (secondPassed)
                {
                    r.Pos += r.Vel;
                    r.Pos.X = Wrap(r.Pos.X, _width);
                    r.Pos.Y = Wrap(r.Pos.Y, _height);
                }
                
                r.RenderPos.X = Wrap(r.RenderPos.X, _width);
                r.RenderPos.Y = Wrap(r.RenderPos.Y, _height);
            }

            frame += isCloseToSolution ? 1 : fps;
            if (secondPassed) second++;
        }
    }

    private static void PrintMap(List<List<char>> map)
    {
        Console.SetCursorPosition(0,0);
        foreach (var (row, ri) in map.WithIndex())
        {
            Console.SetCursorPosition(0,ri + 1);
            Console.WriteLine(row.Stringify(""));
        }
    }

    private static bool SentientAiChristmasTreeDetectorAi(Robot[] robots, int magicChristmasTreeToleranceAmount)
    {
        var center = new Vec2(_width / 2, _height / 2);

        return robots
            .Select(r => r.Pos)
            .GroupBy(p => p.DistSquared(center) / magicChristmasTreeToleranceAmount, p => p)
            .Select(g => g.ToList())
            .Any(g => g.Count > magicChristmasTreeToleranceAmount);
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "12",
            Input: """
            p=0,4 v=3,-3
            p=6,3 v=-1,-3
            p=10,3 v=-1,2
            p=2,0 v=2,-1
            p=0,0 v=1,3
            p=3,0 v=-2,-2
            p=7,6 v=-1,-3
            p=3,0 v=-1,-2
            p=9,3 v=2,3
            p=7,3 v=-1,2
            p=2,4 v=2,-3
            p=9,5 v=-3,-3
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "0",
            Input: """
            p=0,4 v=3,-3
            p=6,3 v=-1,-3
            p=10,3 v=-1,2
            p=2,0 v=2,-1
            p=0,0 v=1,3
            p=3,0 v=-2,-2
            p=7,6 v=-1,-3
            p=3,0 v=-1,-2
            p=9,3 v=2,3
            p=7,3 v=-1,2
            p=2,4 v=2,-3
            p=9,5 v=-3,-3
            """
        ),
    ];
}