using System.Text;
using System.Text.RegularExpressions;

namespace _2024_cs.Solutions;


/// --- Day 3: Mull It Over --- ///


public partial record Day03() : Solver(AnswerOne: "174561379", AnswerTwo: "106921067")
{
    private const string Mul = "mul(";
    private const string Do = "do()";
    private const string Dont = "don't()";
    
    [GeneratedRegex(@"(?<mul>mul\((?<x>\d{1,3}),(?<y>\d{1,3})\))")]
    private static partial Regex MulRegex();
    
    public override string PartOne(IEnumerable<string> input)
    {
        var code = input.Aggregate(seed: new StringBuilder(), (sb, s) => sb.Append(s)).ToString();

        return MulRegex()
            .Matches(code)
            .Select(m => int.Parse(m.Groups["x"].ToString()) * int.Parse(m.Groups["y"].ToString()))
            .Sum()
            .ToString();
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var code = input.Aggregate(seed: new StringBuilder(), (sb, s) => sb.Append(s)).ToString();
        var mulator = 0;

        while (code != "")
        {
            var dontIndex = code.IndexOf(Dont);
            var mulIndex = code.IndexOf(Mul);

            if (mulIndex == -1) break;
            
            var dontCloserThanMul = dontIndex != -1 && dontIndex < mulIndex;

            var (codeSlice, mul) = dontCloserThanMul
                ? JumpToDo(code[dontIndex..])
                : TryReadNumbers(code[(mulIndex + 4)..]);
                
            code = codeSlice;
            mulator += mul;
        }

        return mulator.ToString();
    }

    private static (string codeSlice, int mul) JumpToDo(string code)
    {
        var doIndex = code.IndexOf(Do);
        var codeSlice = doIndex != -1 ? code[(doIndex + 3)..] : string.Empty;

        return (codeSlice, 0);
    }

    private static (string codeSlice, int mul) TryReadNumbers(string code)
    {
        var mulator = 0;
        var digitBuffer = "";
        var commaEncountered = false;

        for (var i = 0; i < code.Length; i++)
        {
            var c = code[i];

            switch (c)
            {
                case var _ when char.IsDigit(c): 
                {
                    if (digitBuffer.Length == 3) return (code[i..], 0);
                    
                    digitBuffer += c;
                    break;
                }
                
                case ',' when int.TryParse(digitBuffer, out var x):
                {
                    if (commaEncountered) return (code[i..], 0);
                
                    commaEncountered = true;
                    mulator = x;
                    digitBuffer = "";
                    break;
                }
                
                case ')' when commaEncountered && int.TryParse(digitBuffer, out var y):
                    return (code[i..], mulator * y);
                
                default:
                    return (code[i..], 0);
            }
        }

        return (string.Empty, 0);
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
