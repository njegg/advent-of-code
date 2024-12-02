namespace _2024_cs.Solutions;


/// --- Day 2: Red-Nosed Reports --- ///


public record Day02() : Solver(AnswerOne: "591", AnswerTwo: "621")
{
    public override string PartOne(IEnumerable<string> input)
    {
        return input
            .Select(l => l.Split(" ").Select(int.Parse))
            .Select(l => l.ToArray().AsMemory())
            .Count(l => IsGradual(l, asc: true) || IsGradual(l, asc: false))
            .ToString();
    }

    public override string PartTwo(IEnumerable<string> input)
    {
        return input
            .Select(l => l.Split(" ").Select(int.Parse))
            .Select(l => l.ToList())
            .Count(l => BruteForce2(l, asc: true) || BruteForce2(l, asc: false))
            .ToString();
    }
    
    private static bool IsGradual(Memory<int> l, bool asc)
    {
        return l.Span switch
        {
            [] => true,
            [_] => true,
            [var x, var y, ..] => AreCloseEnough(x, y, asc) && IsGradual(l[1..], asc)
        };
    }
    
    private static bool BruteForce2(List<int> l, bool asc)
    {
        if (IsGradual(l, asc)) return true;
        
        for (var indexToRemove = 0; indexToRemove < l.Count; indexToRemove++)
        {
            var fixedList = l.Where((_, i) => i != indexToRemove).ToList();
            
            if (IsGradual(fixedList, asc))
            {
                return true;
            }
        }
        
        return false;
    }

    private static bool IsGradual(List<int> l, bool asc)
    {
        for (var i = 0; i < l.Count - 1; i++)
        {
            if (!AreCloseEnough(l[i], l[i + 1], asc))
            {
                return false;
            }
        }

        return true;
    }

    private static bool AreCloseEnough(int x, int y, bool asc) 
        => (x - y) * (asc ? -1 : 1) is >= 1 and <= 3;

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "2", 
            Input:
            """
            7 6 4 2 1
            1 2 7 8 9
            9 7 6 2 1
            1 3 2 4 5
            8 6 4 4 1
            1 3 6 7 9
            """
        )
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "4", 
            Input:
            """
            7 6 4 2 1
            1 2 7 8 9
            9 7 6 2 1
            1 3 2 4 5
            8 6 4 4 1
            1 3 6 7 9
            """
        )
    ];
}

