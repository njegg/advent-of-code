namespace AoC_2023.Solutions;

// --- --- Day 4: Scratchcards - Part 1 --- //

public class D04P01 : Solution
{
    public override string ExampleAnswer => "13";
    public override string Answer => "24848";
    
    public override string Solve(IEnumerable<string> lines)
    {
        return lines
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
