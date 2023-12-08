using System.Collections;
using AoC_2023.Extension;

namespace AoC_2023.Solutions;

// --- Day 8: Haunted Wasteland - Part 2 --- //

public class D08P02 : Solution
{
    public override string ExampleAnswer => "6";
    public override string Answer => "16043";

    private readonly Dictionary<int, (int L, int R)> network = new();
    private readonly List<int> startingNodes = new();
    

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
                    
                    if ((node & 0b11111) == 0) startingNodes.Add(node);
                }
            );

        long steps = 0;
        
        foreach (var instruction in instructions.RepeatForever())
        {
            var nodesFinished = 0;
            
            for (var i = 0; i < startingNodes.Count; i++)
            {
                var currentNode = startingNodes[i];
                
                var lr = network[currentNode];
                currentNode = instruction == 'R' ? lr.R : lr.L;

                startingNodes[i] = currentNode;

                if ((currentNode & 0b11111) == 'Z' - 'A') nodesFinished++;
            }

            steps++;
            Console.WriteLine(steps);
            
            if (nodesFinished == startingNodes.Count) break;
        }
        
        return steps.ToString();
    }

    private static int NodeId(string node)
    {
        return node[2] - 'A' + ((node[1] - 'A') << 5) + ((node[0] - 'A') << 10);
    }

}