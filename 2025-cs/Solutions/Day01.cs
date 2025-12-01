namespace _2024_cs.Solutions;


// --- Day 1: Secret Entrance --- //


public record Day01() : Solver(AnswerOne: "1150", AnswerTwo: "6738")
{
    public override string PartOne(IEnumerable<string> input)
    {
        var result = 0;

        _ = input
            .Select(l => int.Parse(l[1..]) * (l[0] == 'R' ? 1 : -1))
            .Aggregate(50, (sum, x) =>
            {
                var next = Mod(sum + x, 100);
                if (next == 0) result++;
                return next;
            });

        return result.ToString();
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var result = 0;
        var current = 50;

        input
            .Select(l => int.Parse(l[1..]) * (l[0] == 'R' ? 1 : -1))
            .ToList()
            .ForEach(x =>
            {
                var sign = x > 0 ? 1 : -1;
                
                for (var i = 0; i < Math.Abs(x); i++)
                {
                    current += sign;

                    current = current switch
                    {
                        -1 => 99,
                        100 => 0,
                        _ => current
                    };
                    
                    if (current == 0) result++;
                }
            });

        return result.ToString();
    }
 
    
    // 5360 - TOO LOW
    // 6605 - NOT IT
    // 6146 - NOT IT
    // 6266 - NOT IT
    // 6734 - NOT IT
    // 6634 - NOT IT
    // 7744 - TOO HIGH
    // AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    public string PartTwo2(IEnumerable<string> input)
    {
        var result = 0;

        _ = input
            .Select(l => int.Parse(l[1..]) * (l[0] == 'R' ? 1 : -1))
            .Aggregate(50, (current, x) =>
            {
                if (current < 0) throw new Exception("current is " + current);
                
                var next = Mod(current + x, 100);
                
                var diffToZero = x > 0 ? 100 - current : -current;

                if (Math.Abs(x) < Math.Abs(diffToZero))
                {
                    // nothing
                } 
                else if (Math.Abs(x) == Math.Abs(diffToZero))
                {
                    result++;
                }
                else if (Math.Abs(x) > Math.Abs(diffToZero))
                {
                    // Oveerflow

                    if (Math.Abs(x) - Math.Abs(diffToZero) >= 100)
                    {
                        var add = (next == 0 ? 0 : 1) + (Math.Abs(x) - Math.Abs(diffToZero)) / 100;
                        // var add = Math.Abs(x) / 100;
                        // help
                        result += add;

                        _ = add;
                    }
                    else if (current != 0)
                    {
                        result++;
                    }
                    
                }

                return next;
            });

        return result.ToString();
    }

    private static int Mod(int x, int m)
    {
        // return x - m * (int)Math.Floor(1.0F * x / m);

        var res = x % m;
        
        return res < 0 
            ? m + res 
            : res;
    }
    


    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "3",
            Input: """
            L68
            L30
            R48
            L5
            R60
            L55
            L1
            L99
            R14
            L82
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "10",
            Input: """
            L1000
            """
        ),
        (
            Expected: "11",
            Input: """
            L50
            L1000
            """
        ),
        (
            Expected: "1",
            Input: """
            L50
            """
        ),
        (
            Expected: "1",
            Input: """
            R50
            """
        ),
        (
            Expected: "2",
            Input: """
            R2
            R50
            L100
            """
        ),
        (
            Expected: "6",
            Input: """
            L68
            L30
            R48
            L5
            R60
            L55
            L1
            L99
            R14
            L182
            """
        ),
    ];
}
