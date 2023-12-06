using System.Data;

namespace AoC_2023.Solutions;

// --- Day 1: Trebuchet?! Part 1 --- //

public class D06P01 : Solution
{
    public override string ExampleAnswer => "288";
    public override string Answer => "74698";

    public override string Solve(IEnumerable<string> lines)
    {
        var input = lines
            .Select(l => l
                    .Split(':')[1]
                    .Split()
                    .Where(s => s.Length > 0)
                    .Select(int.Parse)
                    .ToArray()
            )
            .ToArray();

        var times = input[0];
        var distances = input[1];

        var res = 1;

        foreach(var i in Enumerable.Range(0, times.Length))
        {
            var record = distances[i];
            var time = times[i];

            var min = (int) ((time - Math.Sqrt(time*time - 4*record)) / 2) + 1;

            var waysToBeatWife = time - min + 1 - min;

            if (waysToBeatWife > 0) res *= waysToBeatWife;
        }

        return res.ToString();
    }
}
