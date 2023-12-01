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
            .Select(FindDigits)
            .Sum()
            .ToString();
    }

    private static int FindDigits(string l)
    {
        var firstDigit = l.SkipWhile(IsNotDigit).First() - '0';
        var lastDigit = l.Reverse().SkipWhile(IsNotDigit).First() - '0';

        return firstDigit * 10 + lastDigit;
    }

    private static bool IsNotDigit(char c) => c is < '0' or > '9';
}