#pragma warning disable CS8509

namespace AoC_2023.Solutions;

// --- Day 2: Cube Conundrum --- Part 1 --- //


public class D02P01 : SolutionBase
{
    protected override string ExampleSolution => "";
    private const int INVALID_FLAG = -1;


    public override string Solve(string inputPath)
    {
        return File.ReadLines(inputPath)
            .ToList()
            .Select(GetValidIds)
            .Sum()
            .ToString();
    }

    private static int GetValidIds(string l)
    {
        var split = l.Split(' ');
        var id = int.Parse(split[1][..(split[1].Length - 1)]);
        
        var cubes = l[(l.IndexOf(':') + 1)..].Split(",;".ToArray());

        var color = cubes
            .Select(s => s.TrimStart())
            .Select(s => s.Split(' '))
            .Aggregate(new { r = 0, g = 0, b = 0 }, (c, s) =>
                {
                    if (c.r == INVALID_FLAG) return c;
                    
                    var cubeCount = int.Parse(s[0]);
                    var channel = s[1][0];

                    if (IsInvalid(channel, cubeCount)) return c with { r = INVALID_FLAG };
                    
                    return channel switch
                    {
                        'r' => c with { r = c.r + cubeCount },
                        'g' => c with { g = c.g + cubeCount },
                        'b' => c with { b = c.b + cubeCount },
                    };
                }
            );

        return color.r == INVALID_FLAG ? 0 : id;
    }

    private static bool IsInvalid(char channel, int cubeCount) =>
        channel == 'r' && cubeCount > 12 ||
        channel == 'g' && cubeCount > 13 ||
        channel == 'b' && cubeCount > 14;
}