using System.Text;

namespace _2024_cs.Solutions;


// --- Day 20: Race Condition --- //


public record Day20() : Solver(AnswerOne: "1448", AnswerTwo: "1017615")
{
    public override string PartOne(IEnumerable<string> input)
    {
        return CountGoodCheats(input, allowedCheats: 2);
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        return CountGoodCheats(input, allowedCheats: 20);
    }

    private string CountGoodCheats(IEnumerable<string> input, int allowedCheats)
    {
        var map = input.Select(l => l.ToCharArray()).ToArray();
        var mapH = map.Length;

        var (startNode, _, nodes) = CreateNodes(map);

        var dist = Enumerable
            .Range(0, map.Length * map[0].Length)
            .Select(_ => int.MaxValue)
            .ToArray();
        
        dist[startNode] = 0;

        Djikstra(nodes, dist);

        if (IsExample()) PrintMap(map, printDst: true, dist);

        var allCheats = FindPossibleCheats(nodes, mapH, dist, allowedCheats);

        return IsExample()
            ? allCheats.Max().ToString()
            : allCheats.Count(s => s >= 100).ToString();
    }

    private static void Djikstra(Dictionary<int, HashSet<int>> nodes, int[] dist)
    {
        var unvisited = new PriorityQueue<int, int>(nodes.Keys.Select(i => (i, dist[i])));

        while (unvisited.Count != 0)
        {
            var node = unvisited.Dequeue();

            foreach (var bro in nodes[node])
            {
                var costToBro = dist[node] + 1;
                if (dist[bro] <= costToBro) continue;

                dist[bro] = costToBro;
                unvisited.Enqueue(bro, costToBro);
            }
        }
    }

    private static List<int> FindPossibleCheats(Dictionary<int, HashSet<int>> nodes, int mapH, int[] dist, int cheats)
    {
        var cheatSaves = new List<int>();

        foreach (var node in nodes.Keys)
        {
            foreach (var farNode in Neighbours(cheats, node, mapH).Where(nodes.ContainsKey))
            {
                var farDist = dist[farNode];

                var cheatSteps = Distance(node, farNode, mapH);
                
                if (farDist != int.MaxValue && farDist > dist[node] + cheatSteps)
                {
                    cheatSaves.Add(farDist - dist[node] - cheatSteps);
                }
            }
        }

        return cheatSaves;
    }

    private static int Distance(int p, int q, int h)
    {
        var (px, py) = (p / h, p % h);
        var (qx, qy) = (q / h, q % h);
        return Math.Abs(px - qx) + Math.Abs(py - qy);
    }

    private static IEnumerable<int> Neighbours(int manhattanDist, int cellId, int mapH)
    {
        var originX = cellId % mapH;
        var originY = cellId / mapH;
        var dist = manhattanDist;

        for (var dx = -dist; dx <= dist; dx++)
        {
            for (int dy = -dist + Math.Abs(dx); dy <= dist - Math.Abs(dx); dy++)
            {
                if (dx == 0 && dy == 0) continue;

                var (nx, ny) = (originX + dx, originY + dy);
                
                if (OutOfBounds(nx, ny, mapH)) continue;

                yield return ny * mapH + nx;
            }
        }
    }
    
    private static readonly (int, int)[] Dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)];

    private static (int StartNode, int EndNode, Dictionary<int, HashSet<int>>) CreateNodes(char[][] map)
    {
        var nodes = new Dictionary<int, HashSet<int>>();
        var startNode = -1;
        var endNode = -1;

        var h = map.Length;

        for (var y = 0; y < h; y++)
        {
            for (var x = 0; x < map[0].Length; x++)
            {
                if (map[y][x] == '#') continue;

                var node = y * h + x;
                nodes.Add(node, []);

                if (map[y][x] == 'S') startNode = node;
                if (map[y][x] == 'E') endNode = node;

                foreach (var (dx, dy) in Dirs)
                {
                    var bx = dx + x;
                    var by = dy + y;
                    var bro = by * h + bx;
                    if (map[by][bx] == '#' || !nodes.ContainsKey(bro)) continue;

                    nodes.TryAdd(bro, []);

                    nodes[bro].Add(node);
                    nodes[node].Add(bro);
                }
            }
        }

        return (startNode, endNode, nodes);
    }
    
    private static bool OutOfBounds(int x, int y, int mapSize)
        => y < 0 || x < 0 || y >= mapSize || x >= mapSize;
    
    private static void PrintMap(char[][] map, bool printDst, int[] dist)
    {
        var sb = new StringBuilder();
        
        const string gray = "\u001b[90m";
        const string res = "\u001b[0m";
        const string green = "\u001b[32m";
        
        var h = map.Length;
        for (var y = 0; y < h; y++)
        {
            for (var x = 0; x < map[0].Length; x++)
            {
                if (map[y][x] is '#')
                {
                    sb.Append($"{gray} ## {res}");
                    continue;
                }
                
                var id = y * h + x;
                
                var tile = printDst
                    ? dist[id] == int.MaxValue ? "inf" : dist[id].ToString()
                    : id.ToString();
                
                if (map[y][x] is '.') sb.Append($"{tile,-4}");
                else if (map[y][x] is 'S') sb.Append($"{green}{tile,-4}{res}");
                else if (map[y][x] is 'E') sb.Append($"{green}{tile,-4}{res}");
            }

            sb.Append("\n\n");
        }

        Console.WriteLine(sb);
    }


    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "64",
            Input: """
            ###############
            #...#...#.....#
            #.#.#.#.#.###.#
            #S#...#.#.#...#
            #######.#.#.###
            #######.#.#...#
            #######.#.###.#
            ###..E#...#...#
            ###.#######.###
            #...###...#...#
            #.#####.#.###.#
            #.#...#.#.#...#
            #.#.#.#.#.#.###
            #...#...#...###
            ###############
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "76",
            Input: """
            ###############
            #...#...#.....#
            #.#.#.#.#.###.#
            #S#...#.#.#...#
            #######.#.#.###
            #######.#.#...#
            #######.#.###.#
            ###..E#...#...#
            ###.#######.###
            #...###...#...#
            #.#####.#.###.#
            #.#...#.#.#...#
            #.#.#.#.#.#.###
            #...#...#...###
            ###############
            """
        ),
    ];
}