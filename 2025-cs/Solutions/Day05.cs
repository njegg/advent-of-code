using _2025_cs.Extension;

namespace _2025_cs.Solutions;


// --- Day 5: Cafeteria --- //


public record Day05() : Solver(AnswerOne: "735", AnswerTwo: "344306344403172")
{
    record Range(long From, long To)
    {
        public static bool Has(Range r, long x) => x >= r.From && x <= r.To;

        public static bool Intersect(Range x, Range y) =>
            x.From >= y.From && x.From <= y.To ||
            x.To >= y.From && x.To <= y.To ||
            y.From >= x.From && y.From <= x.To ||
            y.To >= x.From && y.To <= x.To;

        public static Range Merge(Range x, Range y) => new(Math.Min(x.From, y.From), Math.Max(x.To, y.To));
    }

    public override Solution PartOne(IEnumerable<string> input)
    {
        List<Range> ranges = [];
        List<long> ids = [];

        var readingRanges = true;
        foreach (var line in input)
        {
            if (line == "")
            {
                readingRanges = false;
                continue;
            }

            if (readingRanges)
            {
                var range = line.Split("-").Select(long.Parse).ToArray();
                ranges.Add(new(From: range[0], To: range[1]));
            }
            else
            {
                ids.Add(long.Parse(line));
            }
        }

        return ids
            .Count(id => ranges.Any(r => Range.Has(r, id)))
            .ToString();
    }

    public override Solution PartTwo(IEnumerable<string> input)
    {
        LinkedList<Range> ranges = [];

        foreach (var line in input)
        {
            if (line == "") continue;
            if (!line.Contains('-')) break;

            var rangeElements = line.Split("-").Select(long.Parse).ToArray();
            var newRange = new Range(rangeElements[0], rangeElements[1]);

            while (true)
            {
                Range? conflictingRange = ranges.FirstOrDefault(range => Range.Intersect(range, newRange));

                if (conflictingRange is default(Range))
                {
                    ranges.AddFirst(newRange);
                    break;
                }

                newRange = Range.Merge(newRange, conflictingRange);
                ranges.Remove(conflictingRange);
            }
        }

        return ranges.Aggregate(0L, (sum, r) => sum + r.To - r.From + 1).ToString();
    }

    // --- Example Inputs --- ///


    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "3",
            Input: """
            3-5
            10-14
            16-20
            12-18

            1
            5
            8
            11
            17
            32
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "14",
            Input: PartOneExamples[0].Input
        ),
    ];
}

