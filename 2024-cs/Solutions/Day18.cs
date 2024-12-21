using System.Text;

namespace _2024_cs.Solutions;


// --- Day 18: RAM Run --- //


public record Day18() : Solver(AnswerOne: "436", AnswerTwo: "61,50")
{
    private readonly Direction[] dirs = [Direction.Up, Direction.Right, Direction.Down, Direction.Left];
    
    public override string PartOne(IEnumerable<string> input)
    {
        var mapSize = IsExample() ? 7 : 71;
        var byteCount = IsExample() ? 12 : 1 << 10;

        var bytes = input
            .Take(byteCount)
            .Select(l => l.Split(",").Select(int.Parse).ToArray())
            .Select(l => (X: l[0], Y: l[1]))
            .Select(b => b.Y * mapSize + b.X)
            .ToHashSet();
            
        var nodes = CreateNodes(mapSize, bytes);

        var dists = Enumerable.Range(0, mapSize * mapSize)
            .Select(_ => int.MaxValue)
            .ToArray();
        
        dists[0] = 0;

        var unvisited = new PriorityQueue<int, int>(nodes.Keys.Select(i => (i, dists[i])));

        while (unvisited.Count != 0)
        {
            var node = unvisited.Dequeue();

            foreach (var ni in nodes[node])
            {
                var costToBro = dists[node] + 1;
                if (dists[ni] <= costToBro) continue;

                dists[ni] = costToBro;
                unvisited.Enqueue(ni, costToBro);
            }
        }

        return dists[mapSize * mapSize - 1].ToString();
    }
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var mapSize = IsExample() ? 7 : 71;

        var bytes = input
            .Select(l => l.Split(",").Select(int.Parse).ToArray())
            .Select(l => (X: l[0], Y: l[1]))
            .Select(b => b.Y * mapSize + b.X)
            .Reverse()
            .ToList();

        var removedNodes = bytes.ToHashSet();
        var nodes = CreateNodes(mapSize, ignore: []);
        var end = mapSize * mapSize - 1;

        foreach (var b in bytes)
        {
            removedNodes.Remove(b);
            if (HasPath(nodes, removedNodes, [], 0, end))
            {
                return $"{b % mapSize},{b / mapSize}";
            }
        }

        return "uh";
    }

    private static bool HasPath(
        Dictionary<int, HashSet<int>> nodes,
        HashSet<int> removedNodes,
        HashSet<int> visited,
        int n,
        int end
    )
    {
        if (removedNodes.Contains(n)) return false;
        if (n == end) return true;
        if (!visited.Add(n)) return false;

        return nodes[n]
            .Where(x => !removedNodes.Contains(x))
            .Any(next => HasPath(nodes, removedNodes, visited, next, end));
    }
    
    private Dictionary<int, HashSet<int>> CreateNodes(int mapSize, HashSet<int> ignore)
    {
        var nodeCount = mapSize * mapSize;
        var nodes = new Dictionary<int, HashSet<int>>();

        Enumerable.Range(0, nodeCount)
            .Where(id => !ignore.Contains(id))
            .ToList()
            .ForEach(id => nodes.TryAdd(id, []));

        for (var y = 0; y < mapSize; y++)
            for (var x = 0; x < mapSize; x++)
            {
                var id = y * mapSize + x;
                if (!nodes.ContainsKey(id)) continue;

                foreach (var dir in dirs)
                {
                    var (dx, dy) = dir;
                    var nx = x + dx;
                    var ny = y + dy;
                    if (OutOfBounds(nx, ny, mapSize)) continue;
                    
                    var nid = (y + dy) * mapSize + (x + dx);
                    if (!nodes.TryGetValue(nid, out var neighbour)) continue;

                    neighbour.Add(id);
                    nodes[id].Add(nid);
                }
            }

        return nodes;
    }

    private static bool OutOfBounds(int nx, int ny, int mapSize)
        => nx < 0 || ny < 0 || nx >= mapSize || ny >= mapSize;

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "22",
            Input: """
            5,4
            4,2
            4,5
            3,0
            2,1
            6,3
            2,4
            1,5
            0,6
            3,3
            2,6
            5,1
            1,2
            5,5
            2,5
            6,5
            1,4
            0,4
            6,4
            1,1
            6,1
            1,0
            0,5
            1,6
            2,0
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "6,1",
            Input: """
            5,4
            4,2
            4,5
            3,0
            2,1
            6,3
            2,4
            1,5
            0,6
            3,3
            2,6
            5,1
            1,2
            5,5
            2,5
            6,5
            1,4
            0,4
            6,4
            1,1
            6,1
            1,0
            0,5
            1,6
            2,0
            """
        ),
    ];
}

public static partial class Extension
{
    public static IEnumerator<int> GetEnumerator(this Range range)
    {
        var start = range.Start.Value;
        var end = range.End.Value;
        if (end > start)
            for (var i = start; i < end; i++)
                yield return i;
        else
            for (var i = start; i > end; i--)
                yield return i;
    }
}

