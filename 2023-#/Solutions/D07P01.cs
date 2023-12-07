namespace AoC_2023.Solutions;

// --- Day 7: - Part 1 --- //

public class D07P01 : Solution
{
    public string ExampleAnswer = "6440";

    private record struct Hand(int[] Values, int Strength, long Bid);
    
    public override string Solve(IEnumerable<string> lines)
    {
        var hands = lines
            .Select(l => l.Split())
            .Select(l =>
            {
                var values = ToValues(l[0]);
                return new Hand(values, HandStrength(values), long.Parse(l[1]));
            })
            .ToList();
        
        hands.Sort(HandCompare);
        
        // foreach (var hand in hands)
        // {
        //     Console.WriteLine($"{string.Join(' ', hand.Values)}: {hand.Strength}");
        // }

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
        var strengthSum = 0;

        foreach (var count in set)
        {
            if (count == 0) continue;
            var strength = count switch
            {
                1 => 0,
                2 => 1,
                3 => 8,
                4 => 16,
                5 => 32
            };
            
            unique--;

            strengthSum += strength;
        }
        
        return (1 << unique) + strengthSum;
    }

    private static int HandCompare(Hand handA, Hand handB)
    {
        var cmp = handA.Strength - handB.Strength;
        
        if (cmp != 0) return cmp;

        for (var i = 0; i < 5; i++)
        {
            if (handA.Values[i] != handB.Values[i]) return handA.Values[i] - handB.Values[i];
        }

        return 0;
    }
    
    /*
     *  hc => 1 1 1 1 1     0
     *  op => 2 1 1 1       2 1
     *  tp => 2 2 1         4 2
     *  tk => 3 1 1         4 8
     *  fh => 3 2           8 8 1
     *  4k => 4 1           8 16
     *  5  => 5             16 32
     */ 
}
