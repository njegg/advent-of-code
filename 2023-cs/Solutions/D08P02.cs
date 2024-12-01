using AoC_2023.Extension;

namespace AoC_2023.Solutions;


// --- Day 8: Haunted Wasteland - Part 2 --- //


public class D08P02 : Solution
{
    public override string ExampleAnswer => "6";
    public override string Answer => "15726453850399";

    private readonly Dictionary<int, (int L, int R)> network = new();
    private readonly List<(int Id, long StepsToFinish)> startingNodes = new();

    public override string Solve(IEnumerable<string> lines)
    {
        var linesList = lines.ToList();
        var instructions = linesList.First();
        
        linesList
            .Skip(2)
            .ToList()
            .ForEach(l =>
                {
                    var node = NodeId(l[..3]);
                    var next = (L: NodeId(l[7..10]), R: NodeId(l[12..15]));
                    network.Add(node, next);
                    
                    if ((node & 0b11111) == 0) startingNodes.Add((node, 0));
                }
            );

        uint steps = 0;
        var nodesFinished = 0;
        
        foreach (var instruction in instructions.RepeatForever())
        {
            for (var i = 0; i < startingNodes.Count; i++)
            {
                var currentNode = startingNodes[i];

                if (currentNode.StepsToFinish > 0) continue;
                
                var next = network[currentNode.Id];
                currentNode.Id = instruction == 'R' ? next.R : next.L;

                if ((currentNode.Id & 0b11111) == 'Z' - 'A')
                {
                    nodesFinished++;
                    currentNode.StepsToFinish = steps + 1;
                }

                startingNodes[i] = currentNode;
            }

            steps++;

            if (nodesFinished == startingNodes.Count) break;
        }


        return startingNodes
            .Select(n => n.StepsToFinish)
            .Aggregate(1L, Lcm)
            .ToString();
    }

    private static int NodeId(string node)
    {
        return node[2] - 'A' + ((node[1] - 'A') << 5) + ((node[0] - 'A') << 10);
    }

    long Gcd(long a, long b)
    {
        if (b == 0) return a;
        return Gcd(b, a % b);
    }

    private long Lcm(long x, long y)
    {
        return x*y / Gcd(x, y);
    }
}
