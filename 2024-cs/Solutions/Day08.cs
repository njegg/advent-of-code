namespace _2024_cs.Solutions;


// --- Day 8: Resonant Collinearity --- //


public record Day08() : Solver(AnswerOne: "400", AnswerTwo: "1280")
{
    public record struct Vec2
    {
        public Vec2(int x, int y)
        {
            X = x;
            Y = y;
        }
        
        public int X;
        public int Y;
        public static Vec2 operator +(Vec2 p, Vec2 q) => new(p.X + q.X, p.Y + q.Y);
        public static Vec2 operator -(Vec2 p, Vec2 q) => new(p.X - q.X, p.Y - q.Y);
        public static Vec2 operator *(Vec2 p, int d) => new(p.X * d, p.Y * d);
        public static Vec2 operator *(Vec2 p, float d) => new((int)(p.X * d), (int)(p.Y * d));
        public int DistSquared(Vec2 q) => (X - q.X) * (X - q.X) + (Y - q.Y) * (Y - q.Y);

        public override string ToString() => $"({X}, {Y})";
    }

    public override string PartOne(IEnumerable<string> input)
    {
        var map = input.ToList();
        Dictionary<char, List<Vec2>> antennaCoordsByFrequency = CreateAntennaMap(map);

        return antennaCoordsByFrequency.Values
            .SelectMany(GenerateAntennaPairs)
            .SelectMany(antennaPair => ClosestTwoAntinodes(antennaPair, map))
            .ToHashSet()
            .Count
            .ToString();
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var map = input.ToList();
        Dictionary<char, List<Vec2>> antennas = CreateAntennaMap(map);

        return antennas.Values
            .SelectMany(GenerateAntennaPairs)
            .SelectMany(antennaPair => AllAntinodes(antennaPair, map))
            .ToHashSet()
            .Count
            .ToString();
    }

    private static IEnumerable<Vec2> ClosestTwoAntinodes((Vec2, Vec2) antennaPair, List<string> map)
    {
        var (antennaA, antennaB) = antennaPair;
        var step = antennaA - antennaB;

        var antinodeA = antennaA + step;
        var antinodeB = antennaB - step;
        
        if (InBound(map, antinodeA)) yield return antinodeA;
        if (InBound(map, antinodeB)) yield return antinodeB;
    }

    private static IEnumerable<Vec2> AllAntinodes((Vec2, Vec2) antennaPair, List<string> map)
    {
        var (antennaA, antennaB) = antennaPair;
        var step = antennaA - antennaB;

        var current = antennaA;
        while (InBound(map, current))
        {
            yield return current;
            current += step;
        }
        
        current = antennaB;
        while (InBound(map, current))
        {
            yield return current;
            current -= step;
        }
    }
    
    private static IEnumerable<(Vec2, Vec2)> GenerateAntennaPairs(List<Vec2> source)
    {
        for (var i = 0; i < source.Count - 1; i++)
            for (var j = i + 1; j < source.Count; j++)
                yield return (source[i], source[j]);
    }

    private static Dictionary<char, List<Vec2>> CreateAntennaMap(List<string> map)
    {
        var antennas = new Dictionary<char, List<Vec2>>();

        for (var y = 0; y < map.Count; y++)
        {
            for (var x = 0; x < map[0].Length; x++)
            {
                var c = map[y][x];
                if (c == '.') continue;
                
                if (!antennas.ContainsKey(c)) antennas[c] = new();
                antennas[c].Add(new(x, y));
            }
        }

        return antennas;
    }

    private static bool InBound(List<string> map, Vec2 p) 
        => p.Y >= 0 && p.X >= 0 && p.Y < map.Count && p.X < map[0].Length;

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "14",
            Input: """
            ............
            ........0...
            .....0......
            .......0....
            ....0.......
            ......A.....
            ............
            ............
            ........A...
            .........A..
            ............
            ............
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "9",
            Input: """
            T.........
            ...T......
            .T........
            ..........
            ..........
            ..........
            ..........
            ..........
            ..........
            ..........
            """
        ),

        (
            Expected: "34",
            Input: """
            ............
            ........0...
            .....0......
            .......0....
            ....0.......
            ......A.....
            ............
            ............
            ........A...
            .........A..
            ............
            ............
            """
        ),
    ];
}