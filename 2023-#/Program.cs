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

                if (o.Day == 0) RunAllDays(o);
                else SolvePuzzle(o);
            });
    }

    private static void SolvePuzzle(Options o)
    {
        var inputPath = $"Input/d{o.Day}_" + (o.Example ? "example.txt" : "input.txt");
        
        var part1 = GetSolutionInstance(o.Day, 1);
        var part2 = GetSolutionInstance(o.Day, 2);

        if (o.Part is 0 or 1) PrintSolution(part1, o, inputPath);
        if (o.Part is 0 or 2) PrintSolution(part2, o, inputPath);
    }

    private static void RunAllDays(Options o)
    {
        foreach (var day in Enumerable.Range(1, 25))
        {
            SolvePuzzle(new Options { Day = day, Part = 0, Example = o.Example, Time = o.Time});
        }
    }

    private static SolutionBase GetSolutionInstance(int day, int part)
    {
        var solutionName = $"{nameof(AoC_2023)}.{nameof(Solutions)}.D{day:00}P{part:00}";
        var type = Type.GetType(solutionName) ?? typeof(SolutionBase);
        var solution = Activator.CreateInstance(type);

        if (solution is null) throw new IOException($"Could not load {solutionName}");

        return (SolutionBase)solution;
    }

    private static void PrintSolution(
        SolutionBase solution,
        Options o,
        string inputPath
    )
    {
        if (!o.Time)
        {
            Console.WriteLine(solution.Solve(inputPath));
            return;
        }

        var watch = System.Diagnostics.Stopwatch.StartNew();
        var result = solution.Solve(inputPath);
        watch.Stop();

        var paddedTimeMs = $"{watch.ElapsedMilliseconds}ms".PadLeft(40 - result.Length, ' ');
        
        if (o.Example)
        {
            var correct = solution.Test(result);

            Console.ForegroundColor = correct ? ConsoleColor.Green : ConsoleColor.Red;
        }

        Console.WriteLine($"{result}{paddedTimeMs}");

        Console.ForegroundColor = ConsoleColor.White;
    }
}

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

    [Option(
        shortName: 'a',
        longName: "all",
        Required = false,
        HelpText = "Run em all"
    )]
    public bool All { get; set; }
}