namespace _2024_cs.Solutions;


// --- Day 5: Print Queue --- //


public record Day05() : Solver(AnswerOne: "6384", AnswerTwo: "5353")
{
    public override string PartOne(IEnumerable<string> input)
    {
        var lines = input.ToList();
        
        var rules = lines
            .TakeWhile(l => l != "")
            .Select(l => l.Split("|").Select(int.Parse).ToList())
            .Select(l => (Left: l[0], Right: l[1]))
            .ToList();

        return lines
            .SkipWhile(l => l != "")
            .Skip(1)
            .Select(l => l.Split(",").Select(int.Parse).ToList())
            .Select(updates =>
            {
                var sorted = FuckingQuickSort(updates, rules);
                return (Sorted: sorted.SequenceEqual(updates), Updates: updates);
            })
            .Select(t => t.Sorted ? t.Updates[t.Updates.Count / 2] : 0)
            .Sum()
            .ToString();
    }

    public override string PartTwo(IEnumerable<string> input)
    {
        var lines = input.ToList();
        
        var rules = lines
            .TakeWhile(l => l != "")
            .Select(l => l.Split("|").Select(int.Parse).ToList())
            .Select(l => (Left: l[0], Right: l[1]))
            .ToList();

        return lines
            .SkipWhile(l => l != "")
            .Skip(1)
            .Select(l => l.Split(",").Select(int.Parse).ToList())
            .Select(updates =>
            {
                var sorted = FuckingQuickSort(updates, rules);
                return (Sorted: sorted.SequenceEqual(updates), Updates: sorted);
            })
            .Select(t => !t.Sorted ? t.Updates[t.Updates.Count / 2] : 0)
            .Sum()
            .ToString();
    }
    
    private static List<int> FuckingQuickSort(List<int> pages, List<(int Left, int Right)> rules)
    {
        return pages switch
        {
            [] => pages,
            [_] => pages,
            [var x, ..] =>
            [
                ..FuckingQuickSort(pages[1..].Where(rest => rules.Contains((Left: rest, Right: x))).ToList(), rules),
                x,
                ..FuckingQuickSort(pages[1..].Where(rest => rules.Contains((Left: x, Right: rest))).ToList(), rules),
            ]
        };
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "143",
            Input: 
            """
            47|53
            97|13
            97|61
            97|47
            75|29
            61|13
            75|53
            29|13
            97|29
            53|29
            61|53
            97|53
            61|29
            47|13
            75|47
            97|75
            47|61
            75|61
            47|29
            75|13
            53|13
            
            75,47,61,53,29
            97,61,53,29,13
            75,29,13
            75,97,47,61,53
            61,13,29
            97,13,75,29,47
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "123",
            Input: 
            """
            47|53
            97|13
            97|61
            97|47
            75|29
            61|13
            75|53
            29|13
            97|29
            53|29
            61|53
            97|53
            61|29
            47|13
            75|47
            97|75
            47|61
            75|61
            47|29
            75|13
            53|13
            
            75,47,61,53,29
            97,61,53,29,13
            75,29,13
            75,97,47,61,53
            61,13,29
            97,13,75,29,47
            """
        )
    ];
}
