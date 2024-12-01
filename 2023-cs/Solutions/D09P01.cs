namespace AoC_2023.Solutions;

// --- Day 9: Mirage Maintenance - Part 1 --- //

public class D09P01 : Solution
{
    public override string ExampleAnswer => "114";
    public override string Answer => "2038472161";

    public override string Solve(IEnumerable<string> lines)
    {
        return lines
            .Select(l => l.Split().Select(int.Parse).ToArray())
            .Select(seq => GetNextInSequence(seq, seq.Length))
            .Sum()
            .ToString();
    }

    private static int GetNextInSequence(int[] seq, int len)
    {
        var allZeros = true;
        
        for (var i = 0; i < len - 1; i++)
        {
            seq[i] = seq[i + 1] - seq[i];
            
            if (seq[i] != 0) allZeros = false;
        }
        
        return seq[len - 1] + (allZeros ? 0 : GetNextInSequence(seq, len - 1));
    }
}
