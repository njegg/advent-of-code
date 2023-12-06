using System.Diagnostics;
using AoC_2023.Solutions;
using CommandLine;

namespace AoC_2023;

internal static class Program
{
    public static void Main(string[] args)
    {
        Parser.Default.ParseArguments<Options>(args)
            .WithParsed(o =>
            {
                if (o.Day is < 0 or > 25)
                {
                    Console.Error.WriteLine("There is only 25 days :(");
                    Environment.Exit(1);
                }

                if (o.Part is < 0 or > 2)
                {
                    Console.Error.WriteLine("There is only 2 parts ):");
                    Environment.Exit(1);
                }

                if (o.Day == 0) SolveAllDays(o);
                else if (o.Part == 0) SolveOneDay(o);
                else SolvePuzzle(o.WithSingle(true));
            });
    }

    private static void SolvePuzzle(Options o)
    {
        var inputPath = $"Input/d{o.Day}_" + (o.Example ? "example" : "input");
        
        if (o.Example && o is { Day: 1, Part: 2 })
            inputPath += "_2";

        if (GetSolutionInstance(o.Day, o.Part) is { } s) PrintSolution(s, o, inputPath);
    }

    private static void SolveAllDays(Options o) =>
        Enumerable
            .Range(1, 25)
            .ToList()
            .ForEach(day => SolveOneDay(o.WithDay(day)));

    private static void SolveOneDay(Options o)
    {
        SolvePuzzle(o.WithPart(1));
        SolvePuzzle(o.WithPart(2));
    }

    private static Solution? GetSolutionInstance(int day, int part)
    {
        var solutionName = $"{nameof(AoC_2023)}.{nameof(Solutions)}.D{day:00}P{part:00}";
        
        var type = Type.GetType(solutionName);
        if (type is null) return null;
        
        var solution = Activator.CreateInstance(type) ?? throw new IOException($"Could not load {solutionName}"); ;

        return (Solution)solution;
    }

    private static void PrintSolution(
        Solution solution,
        Options o,
        string inputPath
    ) {
        Stopwatch? time = null;

        if (o.Time) time = Stopwatch.StartNew();
        var result = solution.Solve(File.ReadLines(inputPath));
        if (o.Time) time?.Stop();
        
        var paddedTimeMs = time is null ? 
            "" : $"{time.ElapsedMilliseconds / 1000.0f:F3}s".PadLeft(24 - result.Length, ' ');
        
        // Print day numbers
        Console.ForegroundColor = ConsoleColor.DarkGray;

        if (o is { Single: false, Part: 1 } ) Console.Write($"{o.Day,-4}");
        if (o is { Single: false, Part: 2 } ) Console.Write($"{' ',-4}");

        // Print result
        var answer = o.Example ? solution.ExampleAnswer : solution.Answer;

        if (answer is not null) // No color if answer is not defined
        {
            Console.ForegroundColor = answer == result ?
                ConsoleColor.Green :
                ConsoleColor.Red;
        }
        
        Console.Write($"{result}");

        
        Console.ForegroundColor = ConsoleColor.DarkGray;

        if (time is not null)
        {
            Console.Write(paddedTimeMs);
            
            if (answer == result && !o.Example)
            {
                Console.ForegroundColor = ConsoleColor.Yellow;
                Console.Write(" *");
            }
        }
        
        Console.ResetColor();
        Console.WriteLine();
    }
}

// ReSharper disable once ClassNeverInstantiated.Global ArrangeTypeModifiers
class Options
{
    [Option(
        shortName: 'd',
        longName: "day",
        Required = false,
        HelpText = "Run a day"
    )]
    [Value(0)]
    public int Day { get; set; }

    [Option(
        shortName: 'p',
        longName: "part",
        Required = false,
        HelpText = "Which part"
    )]
    public int Part { get; set; }

    [Option(
        shortName: 't',
        longName: "time",
        Required = false,
        HelpText = "Time it"
    )]
    public bool Time { get; set; }

    [Option(
        shortName: 'e',
        longName: "example-input",
        Required = false,
        HelpText = "Use example input"
    )]
    public bool Example { get; set; }

    public bool Single { get; set; }

    public Options WithDay(int day)
    {
        Day = day;
        return this;
    }
    
    public Options WithPart(int part)
    {
        Part = part;
        return this;
    }
    
    public Options WithSingle(bool single)
    {
        Single = single;
        return this;
    }
}
