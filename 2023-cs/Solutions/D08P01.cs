using AoC_2023.Extension;

namespace AoC_2023.Solutions;

// --- Day 8: Haunted Wasteland - Part 1 --- //

public class D08P01 : Solution
{
    public override string ExampleAnswer => "6";
    public override string Answer => "16043";

    private readonly Dictionary<int, (int L, int R)> network = new();

    public override string Solve(IEnumerable<string> lines)
    {
        var linesList = lines.ToList();
        var instructions = linesList.First();
        
        var currentNode = NodeId("AAA");
        var finish = NodeId("ZZZ");

        linesList
            .Skip(2)
            .ToList()
            .ForEach(l =>
                network.Add(
                    NodeId(l[..3]),
                    (
                        L: NodeId(l[7..10]),
                        R: NodeId(l[12..15])
                    )
                )
            );

        long steps = 0;
        
        foreach (var instruction in instructions.RepeatForever())
        {
            if (currentNode == finish) break;
            
            var lr = network[currentNode];
            currentNode = instruction == 'R' ? lr.R : lr.L;
            steps++;
        }
        
        return steps.ToString();
    }

    private static int NodeId(string node)
    {
        return (node[2] - 'A') + ((node[1] - 'A') << 5) + ((node[0] - 'A') << 10);
    }
}