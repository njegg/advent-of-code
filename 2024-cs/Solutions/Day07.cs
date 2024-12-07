using System.ComponentModel.DataAnnotations;
using System.Runtime.CompilerServices;

namespace _2024_cs.Solutions;


// --- Day 5: Print Queue --- //


public record Day07() : Solver(AnswerOne: "882304362421", AnswerTwo: "145149066755184")
{
    public override string PartOne(IEnumerable<string> input)
    {
        return input
            .Select(l => l.Split(": "))
            .Select(l => (
                Target: long.Parse(l[0]),
                Values: l[1].Split(" ").Select(int.Parse).ToList()
            ))
            .Where(l => CanSolve(l.Target, l.Values[0], l.Values))
            .Select(l => l.Target)
            .Sum()
            .ToString();
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        return input
            .Select(l => l.Split(": "))
            .Select(l => (
                Target: long.Parse(l[0]),
                Values: l[1].Split(" ").Select(int.Parse).ToList()
            ))
            .Where(l => CanSolve(l.Target, l.Values[0], l.Values, canConcat: true))
            .Select(l => l.Target)
            .Sum()
            .ToString();
    }

    private static bool CanSolve(long target, long accumulator, List<int> values, int i = 1, bool canConcat = false)
    {
        if (i == values.Count) return accumulator == target;
        if (accumulator > target) return false;

        var x = values[i];

        return CanSolve(target, x + accumulator, values, i + 1, canConcat)
               | CanSolve(target, x * accumulator, values, i + 1, canConcat)
               | (canConcat && CanSolve(target, Concat(accumulator, x), values, i + 1, canConcat));
    }

    private static long Concat(long x, long y) 
        => x * (long)Math.Pow(10, double.Floor(Math.Log10(y)) + 1) + y;   
    
    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "3749",
            Input: 
            """
            190: 10 19
            3267: 81 40 27
            83: 17 5
            156: 15 6
            7290: 6 8 6 15
            161011: 16 10 13
            192: 17 8 14
            21037: 9 7 18 13
            292: 11 6 16 20
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "11387",
            Input: 
            """
            190: 10 19
            3267: 81 40 27
            83: 17 5
            156: 15 6
            7290: 6 8 6 15
            161011: 16 10 13
            192: 17 8 14
            21037: 9 7 18 13
            292: 11 6 16 20
            """
        ),
    ];
}
