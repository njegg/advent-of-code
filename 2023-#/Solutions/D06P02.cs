using System.Data;

namespace AoC_2023.Solutions;

// ---  Part 2 --- //

public class D06P02 : Solution
{
    public override string ExampleAnswer => "71503";
    public override string Answer => "27563421";

    public override string Solve(IEnumerable<string> lines)
    {
        var input = lines
            .Select(l => long.Parse(l.Where(char.IsDigit).ToArray()))
            .ToArray();

        var time = input[0];
        var record = input[1];

        int min = (int) ((time - Math.Sqrt(time*time - 4*record)) / 2) + 1;

        return $"{time - min + 1 - min}";
    }
}
