using System.Diagnostics;
using CommandLine;
using static _2025_cs.Colors;

namespace _2025_cs;

internal static class Program
{
    private static double _totalTime;

    public static void Main(string[] args)
    {
        Parser.Default.ParseArguments<Options>(args)
            .WithParsed(o =>
            {
                if (o is { DownlaodInput: true })
                {
                    DownlaodInput(o);
                    return;
                }

                Console.CancelKeyPress += (_, _) => Console.CursorVisible = true;
                Console.WriteLine(Tree);

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

    private static void Solve(Options o)
    {
        var solver = GetSolverInstance(o);

        if (solver is EmptySolver) return;

        var input = ReadInput(o);
        var answer = o.Part == 1 ? solver.AnswerOne : solver.AnswerTwo;

        if (solver.Simulate)
        {
            Console.CursorVisible = false;
        }

        Func<List<string>, Solution> partSolver = o.Part == 1
            ? solver.PartOne
            : solver.PartTwo;

        var stopwatch = Stopwatch.StartNew();
        var result = partSolver(input);
        stopwatch.Stop();

        var timeInMs = stopwatch.Elapsed.TotalMilliseconds;

        _totalTime += timeInMs;

        var isCorrect = answer == result.Value;

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
        Console.CursorVisible = true;
    }

    private static Solver GetSolverInstance(Options o)
    {
        var solutionName = $"{nameof(_2025_cs)}.{nameof(_2025_cs.Solutions)}.Day{o.Day:00}";

        var type = Type.GetType(solutionName);
        if (type == null)
        {
            if (o.Single || o.Example) Panic($"Could not read {solutionName}");

            return new EmptySolver();
        }

        var instance = Activator.CreateInstance(type) ?? throw new IOException($"Could not create instance {solutionName}");

        var solver = (Solver)instance;
        solver.Simulate = o.Simulate;

        return solver;
    }

    private static void DownlaodInput(Options o)
    {
        if (string.IsNullOrWhiteSpace(o.Session))
        {
            Panic($"When downloading input you must pass the session for auth with --session arg");
        }

        if (o is { All: false, Day: <= 0 or > 25 })
        {
            Panic($"{o.Day} is not a valid day!");
        }

        var handler = new HttpClientHandler { UseCookies = false };
        using var client = new HttpClient(handler);

        var daysToDownload = DateTime.Now switch
        {
            { Year: > 2025 } or { Year: 2025, Month: 12, Day: >= 12 } => 25,
            { Year: 2025, Month: 12, Day: var day } when day <= 12 => day,
            _ => 0
        };

        if (daysToDownload == 0)
        {
            Console.WriteLine("No days to download yet");
            return;
        }


        Enumerable
            .Range(o.All ? 1 : o.Day, o.All ? daysToDownload : 1)
            .ToList()
            .ForEach(day =>
            {
                Console.ForegroundColor = ConsoleColor.Gray;
                Console.Write($"Downloading input for day {day} ...");

                var req = new HttpRequestMessage(HttpMethod.Get, $"https://adventofcode.com/2025/day/{day}/input");
                req.Headers.Add("cookie", $"session={o.Session}");

                var res = client.Send(req);
                var text = res.Content.ReadAsStringAsync().GetAwaiter().GetResult();

                if (res.StatusCode is not System.Net.HttpStatusCode.OK)
                {
                    Panic($"Status {res.StatusCode}: {text}");
                    return;
                }

                Directory.CreateDirectory("Input");

                File.WriteAllText(Path.Combine("Input", $"input{day:D2}"), text);

                Console.ForegroundColor = ConsoleColor.Green;
                Console.WriteLine(" Done!");
            });
    }

    private static void Panic(string message)
    {
        Console.ForegroundColor = ConsoleColor.Red;
        Console.Error.WriteLine(message);
        Environment.Exit(1);
    }

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

internal class Options
{
    [Option(shortName: 'd', Required = false, HelpText = "Day")]
    public int Day { get; set; }

    [Option(shortName: 'p', Required = false, HelpText = "Part")]
    public int Part { get; set; }

    [Option(shortName: 't', Required = false, HelpText = "Time it! Will invert when running for all cases")]
    public bool TimeOff { get; set; }

    [Option(shortName: 'a', Required = false, HelpText = "Run 'em all")]
    public bool All { get; set; }

    [Option(shortName: 'e', Required = false, HelpText = "Use example input (Or real one if working on today's day)")]
    public bool Example { get; set; }

    [Option(shortName: 's', Required = false, HelpText = "Simulate")]
    public bool Simulate { get; set; }

    [Option(shortName: 'i', Required = false, HelpText = "Download Input")]
    public bool DownlaodInput { get; set; }

    [Option(longName: "session", Required = false, HelpText = "Session for Auth when downloading input")]
    public string Session { get; set; } = string.Empty;

    public bool Single { get; set; }

    public Options WithDay(int day) { Day = day; return this; }
    public Options WithPart(int part) { Part = part; return this; }
    public Options IsSingle(bool single) { Single = single; return this; }
    public Options WithTimeOff(bool time) { TimeOff = time; return this; }
}
