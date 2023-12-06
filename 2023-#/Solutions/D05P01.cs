using System.Data;
using AoC_2023.Extension;

namespace AoC_2023.Solutions;

// --- Day 5: If You Give A Seed A Fertilizer - Part 1 ---

public class D05P01 : Solution
{
    public override string ExampleAnswer => "35";
    public override string Answer => "403695602";

    public override string Solve(IEnumerable<string> lines)
    {
        var categories = lines
            .First()
            .Split(": ")[1]
            .Split()
            .Select(long.Parse)
            .Select(x => ((long current, long next))(x, x))
            .ToArray();
        
        lines
            .Skip(1)
            .Where(l => l.Length == 0 || char.IsDigit(l[0]))
            .Select(r => string.IsNullOrWhiteSpace(r) ? null : r.Split().Select(long.Parse).ToArray())
            .Select(r => r is null ? null : ((long start, long end, long offset)?)(r[1], r[1] + r[2] - 1, r[0] - r[1]))
            .ToList()
            .ForEach(r =>
            {
                if (r is null) categories.Mutate(UpdateCurrentWithNext); // empty line, update
                else  categories.Mutate(c => (c.current, FindNext(c, r.Value)));
            });
        
        return categories
            .Select(c => c.next)
            .Min()
            .ToString();
    }

    private static (long current, long next) UpdateCurrentWithNext((long current, long next) category)
        => category with { current = category.next };

    private static long FindNext(
        (long current, long next) category,
        (long start, long end, long offset) range
    )
    {
        if (category.next != category.current) return category.next;
        
        return category.current >= range.start && category.current <= range.end
            ? category.current + range.offset
            : category.next;
    }
}
