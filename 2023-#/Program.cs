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
                else            RunDay(o);
            });
    }

    private static void RunDay(Options o)
    {
        var solution = GetSolutionInstance(o.Day);
        var inputPath = $"Input/d{o.Day}_" + (o.Example ? "example.txt" : "input.txt");
        
        if (o.Part is 0 or 1) PrintSolution(solution.Part1, o.Time, inputPath, o.Day);
        if (o.Part is 0 or 2) PrintSolution(solution.Part2, o.Time, inputPath, o.Day);
    }

    private static void RunAllDays(Options o)
    {
        var solutions = Enumerable
            .Range(1, 25)
            .Select(GetSolutionInstance);

        foreach (var (solution, day) in solutions.Select((s, i) => (s, i + 1)))
        {
            var inputPath = $"Input/d{day}_" + (o.Example ? "example.txt" : "input.txt");
            
            Console.WriteLine($"---- Day {day:00} ----------------------------");
            PrintSolution(solution.Part1, o.Time, inputPath, day);
            PrintSolution(solution.Part2, o.Time, inputPath, day);
        }
        
        Console.WriteLine($"----------------------------------------");
    }

    private static SolutionBase GetSolutionInstance(int day)
    {
        var solutionName = $"{nameof(AoC_2023)}.{nameof(Solutions)}.Day{day:00}";
        var type = Type.GetType(solutionName) ?? typeof(SolutionBase);
        var solution = Activator.CreateInstance(type);

        if (solution is null) throw new IOException($"Could not load {solutionName}");

        return (SolutionBase) solution;
    }

    private static void PrintSolution(
        Func<string, string> solutionFunction,
        bool time,
        string inputPath,
        int day
    ) {
        if (!time)
        {
            Console.WriteLine(solutionFunction(inputPath));
            return;
        }
        
        var watch = System.Diagnostics.Stopwatch.StartNew(); 
        var result = solutionFunction(inputPath);
        watch.Stop();

        var paddedTimeMs = $"{watch.ElapsedMilliseconds}ms".PadLeft(40 - result.Length, ' ');
        
        Console.WriteLine($"{result}{paddedTimeMs}");
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
}
