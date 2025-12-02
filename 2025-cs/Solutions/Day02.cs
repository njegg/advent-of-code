namespace _2024_cs.Solutions;


// --- Day 2: Gift Shop --- //


public record Day02() : Solver(AnswerOne: "44487518055", AnswerTwo: "53481866137")
{
    public override string PartOne(IEnumerable<string> input)
    {
        var result = 0UL;

        input.First()
            .Split(",")
            .ToList()
            .ForEach(s =>
            {
                var range = s.Split("-").Select(ulong.Parse).ToArray();

                for (var x = range[0]; x <= range[1]; x++)
                {
                    var id = x.ToString();
                    if (id.Length % 2 != 0) continue;

                    var mid = id.Length / 2;

                    if (id[..mid] == id[mid..]) result += x;
                }
            });


        return result.ToString();
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var result = 0UL;

        input.First()
            .Split(",")
            .ToList()
            .ForEach(s =>
            {
                var range = s.Split("-").Select(ulong.Parse).ToArray();

                for (var x = range[0]; x <= range[1]; x++)
                {
                    var id = x.ToString();
                    var mid = id.Length / 2;

                    // Find any sequence len that is repeated
                    for (var seqLen = 1; seqLen <= mid; seqLen++)
                    {
                        if (Repeats(id, seqLen))
                        {
                            result += x;
                            break;
                        }
                    }
                }
            });


        return result.ToString();
    }

    public static bool Repeats(string s, int len)
    {
        if (s.Length % len != 0) return false;

        var seq = s[..len];
        
        // Check each len sized block
        for (var blockStart = len; blockStart <= s.Length - len; blockStart += len)
        {
            var blockEnd = blockStart + len;

            if (s[blockStart..blockEnd] != seq) return false;
        }

        // All blocks same
        return true;
    }
    
    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "1227775554",
            Input: "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "4174379265",
            Input: "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
        ),
    ];
}
