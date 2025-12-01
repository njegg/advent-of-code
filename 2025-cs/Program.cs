using System.Diagnostics;
using CommandLine;

namespace _2024_cs;

internal static class Program
{
    private static double _totalTime;
    
    public static void Main(string[] args)
    {
        Console.CancelKeyPress += (_, _) => Console.CursorVisible = true;
        Console.WriteLine(Tree);
        
        Parser.Default.ParseArguments<Options>(args)
            .WithParsed(o =>
            {
                if (o is { Day: 0, All: false }) TrySetToday(ref o);
                
                switch (o)
                {
                    case { Day: 0 } or { All: true }: SolveAllDays(o); break;
                    
                    case { Day: < 0 or > 25 }: Panic("There is only 25 days :("); break;
                    case { Part: < 0 or > 2 }: Panic("There is only 2 parts ):"); break;
                    
                    case { Example: true }: SolveOnExamples(o); break;
                        
                    case { Part: 0 }:
                        Solve(o.WithPart(1));
                        Solve(o.WithPart(2));
                        break;
                    
                    default: Solve(o.IsSingle(true)); break;
                }
            });
        
        Console.CursorVisible = true;
    }

    private static void TrySetToday(ref Options o)
    {
        var (_, month, day) = DateTime.Now;

        if (month != 12 || day > 25) return;

        o.Day = day;
        o.Example = !o.Example;
    }

    private static void SolveAllDays(Options o)
    {
        Enumerable
            .Range(1, 25)
            .ToList()
            .ForEach(day =>
            {
                o = o.WithDay(day);
                Solve(o.WithPart(1));
                Solve(o.WithPart(2));
            });

        if (!o.TimeOff)
        {
            Console.ForegroundColor = ConsoleColor.DarkGray;
            Console.WriteLine("--------- Total Time ----------");
            var time = $"{_totalTime:F3}";
            Console.WriteLine($"{time,26} ms");
        }
    }
    private static void SolveOnExamples(Options o)
    {
        var solver = GetSolverInstance(o);

        if (o is { Part: 0 or 1 }) solver.TestPartOne();
        if (o is { Part: 0 or 2 }) solver.TestPartTwo();
    }

    private static List<string> ReadInput(Options o)
    {
        var inputPath = $"Input/input{o.Day:00}";

        if (!File.Exists(inputPath)) Panic($"File {inputPath} not found");
        
        return File.ReadLines(inputPath).ToList();
    }

    private static void Solve(Options o) {
        var solver = GetSolverInstance(o);

        if (solver is EmptySolver) return;
        
        var input = ReadInput(o);
        var answer = o.Part == 1 ? solver.AnswerOne : solver.AnswerTwo;
        
        Func<List<string>, string> partSolver = o.Part == 1 
            ? solver.PartOne 
            : solver.PartTwo;

        var stopwatch = Stopwatch.StartNew();
        var result = partSolver(input);
        stopwatch.Stop();

        var timeInMs = TimeSpan
            .FromTicks(stopwatch.ElapsedTicks / 100)
            .Ticks * 1d / TimeSpan.TicksPerMillisecond;

        _totalTime += timeInMs;
        
        var isCorrect = answer == result;

        if (o is { Part: 1 } or { Part: 2, Single: true })
        {
            Console.ForegroundColor = ConsoleColor.DarkGray;
            Console.WriteLine($"----------- Day {o.Day:00} ------------");
        }
        
        if (answer != null) // No color if answer is unknown
        {
            Console.ForegroundColor = isCorrect 
                ? ConsoleColor.Green
                : ConsoleColor.Red;
        }
        
        Console.Write($"{result,-17}");
        
        Console.ForegroundColor = ConsoleColor.DarkGray;
        var displayTime = $"{timeInMs:F3} ms";
        if (!o.TimeOff) Console.Write($"{displayTime,12}");
        
        if (isCorrect && !o.TimeOff) Console.Write($" {Y}*");
        
        Console.ResetColor();
        Console.WriteLine();
    }
    
    private static Solver GetSolverInstance(Options o)
    {
        var solutionName = $"{nameof(_2024_cs)}.{nameof(Solutions)}.Day{o.Day:00}";

        var type = Type.GetType(solutionName);
        if (type == null)
        {
            if (o.Single || o.Example) Panic($"Could not read {solutionName}");
            
            return new EmptySolver();
        }
        
        var solver = Activator.CreateInstance(type) ?? throw new IOException($"Could not create instance {solutionName}");

        return (Solver)solver;
    }

    private static void Panic(string message)
    {
        Console.ForegroundColor = ConsoleColor.Red;
        Console.WriteLine(message);
        Environment.Exit(1);
    }
    
    private const string G = "\u001b[32m";
    private const string R = "\u001b[31m";
    private const string B = "\u001b[34m";
    private const string O = "\u001b[38;2;255;165;0m";
    private const string Y = "\u001b[38;2;255;255;102m";
    private const string X = "\u001b[0m";

    private const string Tree =
        $"""
        
               {Y}\|/
              --*--{G}    
               >{R}o{G}<   {G}Advent Of Code{G}
              >>{B}O{G}<<      {Y}*2025*{G}
             >{O}o{G}<<<{B}O{G}<
            >{R}@{G}>>>{Y}*{G}<<<{X}
            
        """;
}

class Options
{
    [Option(shortName: 'd', Required = false, HelpText = "Day")]
    public int Day { get; set; }

    [Option(shortName: 'p', Required = false, HelpText = "Part")]
    public int Part { get; set; }

    [Option(shortName: 't', Required = false, HelpText = "Time it! Will invert when running for all cases")]
    public bool TimeOff { get; set; }
    
    [Option(shortName: 'a', Required = false, HelpText = "Run 'em all")]
    public bool All { get; set; }

    [Option(shortName: 'e', Required = false, HelpText = "Use example input")]
    public bool Example { get; set; }

    public bool Single { get; set; }
    
    public Options WithDay(int day) { Day = day; return this; } 
    public Options WithPart(int part) { Part = part; return this; }
    public Options IsSingle(bool single) { Single = single; return this; }
    public Options WithTimeOff(bool time) { TimeOff = time; return this; }
}
