namespace AoC_2023.Solutions;

// --- --- Day 4: Scratchcards - Part 1 --- //

public class D04P01 : SolutionBase
{
    public override string ExampleAnswer => "142";

    public override string Solve(string inputPath)
    {
        return File.ReadLines(inputPath)
            .Select(l => l.Split(':')[1])
            .Select(l => l
                .Split(" | ")
                .Select(s => s.Split(' ').Where(s => s != "").ToHashSet())
                .ToArray())
            .Select(s => s[1].Intersect(s[0]).Count())
            .Where(x => x > 0)
            .Select(x => 1 << (x - 1))
            .Sum()
            .ToString();
    }
}