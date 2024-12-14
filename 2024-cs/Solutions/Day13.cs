namespace _2024_cs.Solutions;
using Vec2 = Day08.Vec2;


// --- Day 13: Claw Contraption --- //


public record Day13() : Solver(AnswerOne: "37297", AnswerTwo: "83197086729371")
{
    public override string PartOne(IEnumerable<string> input)
    {
        var configurations = input.ToList();
        var totalPrice = 0L;

        var i = 0;
        while (i < configurations.Count)
        {
            var a = ParseButtonVec2(configurations[i]);
            var b = ParseButtonVec2(configurations[i + 1]);
            var target = ParseButtonVec2(configurations[i + 2]);

            for (var y = 0; y <= 100; y++)
                for (var x = 0; x <= 100; x++)
                    if (a * y + b * x == target)
                        totalPrice += y * 3 + x;
            
            i += 4;
        }

        return totalPrice.ToString();
    }
    
    private static Vec2 ParseButtonVec2(string l)
    {
        var points = l.Split(": ")[1].Split(", ").Select(n => int.Parse(n[2..])).ToArray();
        return new Vec2(points[0], points[1]);
    } 
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var configurations = input.ToList();
        var totalPrice = 0L;

        var conversionError = 10000000000000L;
        
        var i = 0;
        while (i < configurations.Count)
        {
            var (a1, a2) = ParseButton(configurations[i]);
            var (b1, b2) = ParseButton(configurations[i + 1]);
            var (c1, c2) = ParseButton(configurations[i + 2]);

            var (x, y) = Solve(a1, b1, a2, b2, c1 + conversionError, c2 + conversionError);

            if (IsInteger(x, out var roundX) && IsInteger(y, out var roundY))
            {
                totalPrice += roundX * 3 + roundY;
            }
            
            i += 4;
        }

        return totalPrice.ToString();
    }

    private static (double x, double y) Solve(double a1, double b1, double a2, double b2, double c1, double c2) {
        var det = a1 * b2 - b1 * a2;
        var x = (b2 * c1 - b1 * c2) / det;
        var y = (a1 * c2 - a2 * c1) / det;
        return (x, y);
    }

    private static (long, long) ParseButton(string l)
    {
        var points = l.Split(": ")[1].Split(", ").Select(n => long.Parse(n[2..])).ToArray();
        return (points[0], points[1]);
    } 
    
    private static bool IsInteger(double x, out long roundX)
    {
        roundX = (long)x; 
        return x - double.Floor(x) == 0;
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "480",
            Input: """
            Button A: X+94, Y+34
            Button B: X+22, Y+67
            Prize: X=8400, Y=5400
            
            Button A: X+26, Y+66
            Button B: X+67, Y+21
            Prize: X=12748, Y=12176
            
            Button A: X+17, Y+86
            Button B: X+84, Y+37
            Prize: X=7870, Y=6450
            
            Button A: X+69, Y+23
            Button B: X+27, Y+71
            Prize: X=18641, Y=10279
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "875318608908",
            Input: """
            Button A: X+94, Y+34
            Button B: X+22, Y+67
            Prize: X=8400, Y=5400
            
            Button A: X+26, Y+66
            Button B: X+67, Y+21
            Prize: X=12748, Y=12176
            
            Button A: X+17, Y+86
            Button B: X+84, Y+37
            Prize: X=7870, Y=6450
            
            Button A: X+69, Y+23
            Button B: X+27, Y+71
            Prize: X=18641, Y=10279
            """
        ),
    ];
}