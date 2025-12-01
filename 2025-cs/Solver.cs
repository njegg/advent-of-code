namespace _2024_cs;

public record Solver(string? AnswerOne = null, string? AnswerTwo = null)
{
    protected virtual List<(string Expected, string Input)> PartOneExamples => [];
    protected virtual List<(string Expected, string Input)> PartTwoExamples => [];
    
    public virtual string PartOne(IEnumerable<string> input) => "-";
    public virtual string PartTwo(IEnumerable<string> input) => "-";

    public void TestPartOne() => Test(PartOne, PartOneExamples);
    public void TestPartTwo() => Test(PartTwo, PartTwoExamples);
    
    protected bool IsExample() => Environment.GetEnvironmentVariable("example") is not null;
    
    private static void Test(
        Func<IEnumerable<string>, string> solver,
        List<(string Expected, string Input)> examples
    ) {
        Environment.SetEnvironmentVariable("example", "1");
        
        foreach (var (expected, input) in examples)
        {
            var result = solver(input.Split("\n"));
            var isCorrect = result == expected;

            Console.ForegroundColor = isCorrect
                ? ConsoleColor.Green
                : ConsoleColor.Red;

            Console.Write($"{result,-16}");

            if (!isCorrect)
            {
                Console.ForegroundColor = ConsoleColor.DarkGray;
                Console.Write($" ~ expected {expected}");
            }

            Console.WriteLine();
            Console.ResetColor();
        }
    }
}

public record EmptySolver : Solver;
