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
    public bool Simulate { get; set; }
    
    private static void Test(
        Func<IEnumerable<string>, string> solver,
        List<(string Expected, string Input)> examples
    ) {
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
        // var width = Console.WindowWidth;
        //
        // var emptyRow = new string(' ', width);
        //
        // for (int y = 0; y < Console.WindowHeight; y++)
        // {
        //     Console.SetCursorPosition(0, y);
        //     Console.Write(emptyRow);
        // }
        //
        // Console.SetCursorPosition(0, 0);
        
        
        Console.Write("\x1b[2J");   // clear screen
        Console.Write("\x1b[H");    // move cursor to 0,0
    }
}

public record EmptySolver : Solver;
