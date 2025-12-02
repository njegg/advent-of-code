namespace _2024_cs.Solutions;

// --- Day 2: Gift Shop --- //

public record Day02() : Solver(AnswerOne: "44487518055", AnswerTwo: "53481866137")
{
    public override string PartOne(IEnumerable<string> input)
    {
        var result = 0UL;

        var ranges = input.First()
            .Split(",")
            .ToList();
        
        Parallel.ForEach(ranges, s =>
            {
                var range = s.Split("-").Select(ulong.Parse).ToArray();
                518055         56.296 ms *
                    53481866137        143.149 ms *
                for (var x = range[0]; x <= range[1]; x++)
                {
                    var id = x.ToString();
                    if (id.Length % 2 != 0) continue;
                    
                    var mid = id.Length / 2;

                    if (HasRepeatingSequence(id, mid))
                    {
                        Interlocked.Add(ref result, x);
                    }
                }
            });


        return result.ToString();
    }

    public override string PartTwo(IEnumerable<string> input)
    {
        var result = 0UL;

        var ranges = input.First()
            .Split(",")
            .Select(s => s.Split("-").Select(ulong.Parse).ToArray())
            .ToList();
            
        Parallel.ForEach(ranges, range =>
        {
            for (var x = range[0]; x <= range[1]; x++)
            {
                var id = x.ToString();
                var mid = id.Length / 2;

                for (var seqLen = 1; seqLen <= mid; seqLen++)
                {
                    if (HasRepeatingSequence(id, seqLen))
                    {
                        Interlocked.Add(ref result, x);
                        break;
                    }
                }
            }
        });

        return result.ToString();
    }
    
    private static bool HasRepeatingSequence(string s, int len)
    {
        if (len == 0) return false;
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
    
    // This one is slower :(
    [Obsolete]
    private static bool Block(ulong number, int n)
    {
        var digitCount = 1 + (int) Math.Floor(Math.Log10(number));

        if (digitCount % n != 0) return false;

        var blockCount = digitCount / n;
        var nPow10 = (int)Math.Floor(Math.Pow(10, n));
        
        if (blockCount == 0) return false;

        double current = number;
        var targetSequence = (int)Math.Floor(current % nPow10);

        for (var i = 0; i < blockCount; i++)
        {
            var lastNDigits = (int)Math.Floor(current % nPow10);

            if (lastNDigits != targetSequence)
            {
                return false;
            }
            
            current /= nPow10; // shift right by N digits
        }
        
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
