using System.Data;

namespace AoC_2023.Solutions;

// --- Day 1: Trebuchet?! Part 1 --- //

public class D01P01 : SolutionBase
{
    protected override string ExampleSolution => "142";

    public override string Solve(string inputPath)
    {
        return File.ReadLines(inputPath)
            .ToList()
            .Select(l =>
                (l.SkipWhile(IsNotDigit).First() - '0') * 10
                + (l.Reverse().SkipWhile(IsNotDigit).First() - '0')
            )
            .Sum()
            .ToString();
    }

    private static bool IsNotDigit(char c) => c is < '0' or > '9';
}