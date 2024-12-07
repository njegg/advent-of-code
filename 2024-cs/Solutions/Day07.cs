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
                Values: l[1].Split(" ").Select(long.Parse).ToArray().AsMemory()
            ))
            .Where(l => CanSolve(l.Target, l.Values.Span[0], l.Values.Span[1..]))
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
                Values: l[1].Split(" ").Select(long.Parse).ToArray().AsMemory()
            ))
            .Where(l => CanSolveWithConcat(l.Target, l.Values.Span[0], l.Values.Span[1..]))
            .Select(l => l.Target)
            .Sum()
            .ToString();
    }
    
    private static bool CanSolve(long target, long accumulator, Span<long> values)
    {
        if (values.Length == 0) return accumulator == target;
        if (accumulator > target) return false;
        
        return CanSolve(target, values[0] + accumulator, values[1..])
               | CanSolve(target, values[0] * accumulator, values[1..]);
    }
    
    private static bool CanSolveWithConcat(long target, long accumulator, Span<long> values)
    {
        if (values.Length == 0) return accumulator == target;
        if (accumulator > target) return false;
        
        return CanSolveWithConcat(target, values[0] + accumulator, values[1..])
               | CanSolveWithConcat(target, values[0] * accumulator, values[1..])
               | CanSolveWithConcat(target, Concat(accumulator, values[0]), values[1..]);
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
