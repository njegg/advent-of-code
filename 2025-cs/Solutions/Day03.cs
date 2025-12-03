using _2024_cs.Extension;

namespace _2024_cs.Solutions;


// --- Day 3: Lobby --- //


public record Day03() : Solver(AnswerOne: "17311", AnswerTwo: "171419245422055")
{
    private static string FindBestBatteries(char[] line, int len)
    {
        if (len == 0) return "";
        
        var (max, maxIndex) = line[..^(len - 1)].Max();

        return max + FindBestBatteries(line[(maxIndex + 1)..], len - 1);
    }
    
    public override string PartOne(IEnumerable<string> input)
    {
        return input
            .Select(line => FindBestBatteries(line.ToCharArray(), 2))
            .Select(long.Parse)
            .Sum()
            .ToString();
    }

    public override string PartTwo(IEnumerable<string> input)
    {
        return input
            .Select(line => FindBestBatteries(line.ToCharArray(), 12))
            .Select(long.Parse)
            .Sum()
            .ToString();
    }
    

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "357",
            Input: """
            987654321111111
            811111111111119
            234234234234278
            818181911112111
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "3121910778619",
            Input: PartOneExamples[0].Input
        ),
    ];
}
