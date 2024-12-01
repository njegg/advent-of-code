using _2024_cs.Extension;

namespace _2024_cs.Solutions;


/// --- Day 1: Historian Hysteria --- ///


public record Day01() : Solver(AnswerOne: "2378066", AnswerTwo: "18934359")
{
    public override string PartOne(IEnumerable<string> input)
    {
        var (left, right) = input
            .Select(l => l.Split("   "))
            .Select(l => (Left: int.Parse(l[0]), Right: int.Parse(l[1])))
            .Aggregate(
                seed: (Left: new List<int>(), Right: new List<int>()),
                (lists, items) => (lists.Left.With(items.Left), lists.Right.With(items.Right))
            );

        left.Sort();
        right.Sort();

        return left.Zip(right)
            .Select((l, r) => int.Abs(l - r))
            .Sum()
            .ToString();
    }

    public override string PartTwo(IEnumerable<string> input)
    {
        var (ids, counter) = input
            .Select(l => l.Split("   "))
            .Select(l => (Left: int.Parse(l[0]), Right: int.Parse(l[1])))
            .Aggregate(
                seed: (Ids: new List<int>(), Counter: new Dictionary<int, int>()),
                (ag, x) => (ag.Ids.With(x.Left), ag.Counter.CountUp(x.Right))
            );
        
        return ids
            .Select(x => x * counter.GetValueOrDefault(x, 0))
            .Sum()
            .ToString();
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "11",
            Input: 
            """
            3   4
            4   3
            2   5
            1   3
            3   9
            3   3
            """
        )
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "3", 
            Input:
            """
            3   4
            4   3
            2   5
            1   3
            3   9
            3   3
            """
        )
    ];
}

public static class Extensions
{
    public static Dictionary<T, int> CountUp<T>(this Dictionary<T, int> counter, T key) where T : notnull
    {
        if (counter.TryAdd(key, 1)) return counter;

        counter[key] += 1;

        return counter;
    }
}
