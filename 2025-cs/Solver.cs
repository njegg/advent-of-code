namespace _2025_cs;

public record Solver(string? AnswerOne = null, string? AnswerTwo = null)
{
    protected virtual List<(string Expected, string Input)> PartOneExamples => [];
    protected virtual List<(string Expected, string Input)> PartTwoExamples => [];

    public virtual Solution PartOne(IEnumerable<string> input) => "-";
    public virtual Solution PartTwo(IEnumerable<string> input) => "-";

    public void TestPartOne() => Test(PartOne, PartOneExamples);
    public void TestPartTwo() => Test(PartTwo, PartTwoExamples);

    protected bool IsExample() => Environment.GetEnvironmentVariable("example") is not null;
    public bool Simulate { get; set; }

    private static void Test(
        Func<IEnumerable<string>, Solution> solver,
        List<(string Expected, string Input)> examples
    )
    {
        Environment.SetEnvironmentVariable("example", "1");

        foreach (var (expected, input) in examples)
        {
            var result = solver(input.Split(Environment.NewLine));
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

    protected void ClearScreen()
    {
        Console.Write("\x1b[2J");   // clear screen
        Console.Write("\x1b[H");    // move cursor to 0,0
    }
}

public record EmptySolver : Solver;

public record Solution(string Value)
{
    public static implicit operator Solution(string s) => new(s);
    public static implicit operator Solution(int i) => new(i.ToString());
    public static implicit operator Solution(long i) => new(i.ToString());
    public static implicit operator Solution(uint i) => new(i.ToString());
    public static implicit operator Solution(ulong i) => new(i.ToString());

    public static implicit operator string(Solution s) => s.Value;

    public override string ToString() => Value;
}
