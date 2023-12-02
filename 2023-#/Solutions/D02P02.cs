#pragma warning disable CS8509

namespace AoC_2023.Solutions;

// --- Day 2: Cube Conundrum --- Part 2 --- //


public class D02P02 : SolutionBase
{
    public override string ExampleAnswer => "2286";
    public override string Answer => "66363";

    public override string Solve(string inputPath)
    {
        return File.ReadLines(inputPath)
            .ToList()
            .Select(GetPower)
            .Sum()
            .ToString();
    }

    private static int GetPower(string l)
    {
        var color = l[(l.IndexOf(':') + 1)..]
            .Split(",;".ToArray())
            .Select(s => s.TrimStart())
            .Select(s => s.Split(' '))
            .Aggregate(new { r = 0, g = 0, b = 0 }, (c, s) =>
                {
                    var cubeCount = int.Parse(s[0]);
                    var channel = s[1][0];
                    
                    return channel switch
                    {
                        'r' => c with { r = Math.Max(c.r, cubeCount) },
                        'g' => c with { g = Math.Max(c.g, cubeCount) },
                        'b' => c with { b = Math.Max(c.b, cubeCount) },
                    };
                }
            );

        return color.r * color.g * color.b;
    }
}