namespace AoC_2023.Solutions;

// --- Day 5: If You Give A Seed A Fertilizer - Part 2 ---

public class D05P02 : Solution
{
    public override string ExampleAnswer => "46";
    public override string Answer => "219529182";

    private class Range
    {
        public long Start;
        public long End;
        public bool Updated;

        public Range(long start, long end, bool updated)
        {
            Start = start;
            End = end;
            Updated = updated;
        }
    }
    
    private record struct Transform(long Start, long End, long Offset);

    public override string Solve(IEnumerable<string> lines)
    {
        var seeds = lines
            .First()
            .Split(": ")[1]
            .Split()
            .Select(long.Parse)
            .ToArray();

        List<Range> categoryRanges = new();

        for (var i = 0; i < seeds.Length; i += 2)
            categoryRanges.Add(new Range (seeds[i],  seeds[i] + seeds[i + 1] - 1, false));

        lines
            .Skip(1)
            .Where(l => l.Length == 0 || char.IsDigit(l[0]))
            .Select(l => l.Length == 0 ? null : l.Split().Select(long.Parse).ToArray())
            .Select(r => r is null ? null : (Transform?)new Transform(r[1], r[1] + r[2] - 1, r[0] - r[1]))
            .ToList()
            .ForEach(t =>
            {
                if (t is null) categoryRanges.ForEach(r => r.Updated = false);
                else
                {
                    var newRanges = categoryRanges
                        .Select(r => TransformRange(r, t.Value))
                        .ToList();
                    
                    foreach (var (left, right) in newRanges)
                    {
                        if (left is not null) categoryRanges.Add(left);
                        if (right is not null) categoryRanges.Add(right);
                    }
                }
            });

        return categoryRanges
            .Select(r => r.Start)
            .Min()
            .ToString();
    }

    private static (Range?, Range?) TransformRange(Range range, Transform transform)
    {
        if (range.Updated) return (null, null);
        if (range.End < transform.Start || range.Start > transform.End) return (null, null); // Not in transform range

        if (range.Start >= transform.Start && range.End <= transform.End) // Whole range is inside
        {
            range.Start += transform.Offset;
            range.End += transform.Offset;
            range.Updated = true;
            return (null, null);
        }

        if (range.Start < transform.Start && range.End > transform.End) // Transform is inside
        {
            var left = new Range(range.Start, transform.Start - 1, false);
            var right = new Range(transform.End + 1, range.End , false);

            range.Start = transform.Start + transform.Offset;
            range.End = transform.End + transform.Offset;
            range.Updated = true;

            return (left, right);
        }

        if (range.End >= transform.Start && range.Start < transform.Start)
        {
            var newRange = new Range(transform.Start + transform.Offset, range.End + transform.Offset, true);
            range.End = transform.Start - 1;

            return (newRange, null);
        }


        if (range.Start <= transform.End)
        {
            var newRange = new Range(range.Start + transform.Offset, transform.End + transform.Offset, true);
            range.Start = transform.End + 1;
            return (newRange, null);
        }

        throw new Exception("rip");
    }
 
}
