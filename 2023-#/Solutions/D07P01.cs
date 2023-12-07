namespace AoC_2023.Solutions;

// --- Day T: - Part 1 --- //

public class D07P01 : Solution
{
    public override string Solve(IEnumerable<string> lines)
    {
        var hands = lines
            .Select(l => l.Split())
            .Select(l => new { Hand = HandStrength(ToValues(l[0])), Bid = int.Parse(l[1]) })
            .ToList();
        
        hands.Sort((x, y) => y.Hand - x.Hand);

        return hands
            .Select((h, i) => h.Bid * (i + 1))
            .Sum()
            .ToString();
    }

    private static int[] ToValues(string hand)
    {
        return hand
            .Select(c => c switch
            {
                'A' => 14,
                'K' => 13,
                'Q' => 12,
                'J' => 11,
                'T' => 10,
                _ => c - '0'
            })
            .ToArray();
    }

    private static byte[] set = new byte[15];

    private static int HandStrength(int[] hand)
    {
        Array.Clear(set);

        foreach (var card in hand)
        {
            set[card]++;
        }

        var unique = 5;
        var strength = 0;

        foreach (var count in set)
        {
            unique -= count;
            if (count < 2) continue;

            strength += 1 << count;
        }
        
        return unique + strength;
    }
    
    /*
     *  hc => 1 1 1 1 1     0
     *  op => 2 1 1 1       1 4
     *  tp => 2 2 1 1       1 8
     *  tk => 3 1 1         2 8
     *  fh => 3 2           3 8 4
     *  4k => 4 1           3 16
     *  5  => 5             4 32
     */ 
}
