using System.Data;

namespace AoC_2023.Solutions;

// --- Day 1: Trebuchet?! Part 1 --- //

public class D01P01 : Solution
{
    public override string ExampleAnswer => "142";
    public override string Answer => "54597";

    public override string Solve(IEnumerable<string> lines)
    {
        return lines
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
