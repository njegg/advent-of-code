using System.Data;

namespace AoC_2023.Solutions;

// --- Day 1: Trebuchet?! Part 1 --- //

public class D06P01 : Solution
{
    public override string ExampleAnswer => "288";

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

            var waysToBeat = Enumerable.Range(0, times[i])
                .Select(hold => hold * (time - hold))
                .Where(dist => dist > record)
                .Count();

            if (waysToBeat > 0) res *= waysToBeat;
        }

        return res.ToString();
    }
}
