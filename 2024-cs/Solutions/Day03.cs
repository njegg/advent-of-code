using System.Text;
using System.Text.RegularExpressions;

namespace _2024_cs.Solutions;


/// --- Day 3: Mull It Over --- ///


public partial record Day03() : Solver(AnswerOne: "174561379", AnswerTwo: "106921067")
{
    [GeneratedRegex(@"(?<mul>mul\((?<x>\d{1,3}),(?<y>\d{1,3})\))")]
    private static partial Regex MulRegex();
    
    private static int MulXY(Match m) => int.Parse(m.Groups["x"].ToString()) * int.Parse(m.Groups["y"].ToString());
    
    public override string PartOne(IEnumerable<string> input)
    {
        var code = input.Aggregate(seed: new StringBuilder(), (sb, s) => sb.Append(s)).ToString();

        return MulRegex()
            .Matches(code)
            .Select(MulXY)
            .Sum()
            .ToString();
    }
    
    [GeneratedRegex(@"((?<mul>mul\((?<x>\d{1,3}),(?<y>\d{1,3})\))|(?<do>do\(\))|(?<dont>don't\(\)))")]
    private static partial Regex MulRegex2();
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var code = input.Aggregate(seed: new StringBuilder(), (sb, s) => sb.Append(s)).ToString();

        return MulRegex2().Matches(code)
            .Aggregate(
                seed: (Enabled: true, Sum: 0),
                (acc, m) => m.ToString() switch
                {
                    "do()" => (true, acc.Sum),
                    "don't()" => (false, acc.Sum),
                    _ when acc.Enabled => (true, acc.Sum + MulXY(m)),
                    _ => acc
                })
            .Sum
            .ToString();
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "161",
            Input: 
            """
            xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
            """
        )
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [

        (
            Expected: "48",
            Input: 
            """
            xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
            """
        )
    ];
}
