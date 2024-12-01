namespace AoC_2023.Solutions;

// --- Day 7: - Part 1 --- //

public class D07P01 : Solution
{
    public override string ExampleAnswer => "6440";
    public override string Answer => "251806792";
    
    private enum Cards
    {
        One = 0,
        Pair = 1,
        Three = 8,
        Four = 16,
        Five = 32,
    }

    /*                      p^2
     *  hc => 1 1 1 1 1      0
     *  op => 2 1 1 1 p      2  +  1
     *  tp => 2 2 1 p p      4  +  1+1
     *  tk => 3 1 1 p p      4  +  8
     *  fh => 3 2 p p p      8  +  8+1
     *  4k => 4 1 p p p      8  +  16
     *  5  => 5 p p p p      16 +  32
     */ 
    private enum HandType
    {
        HighCard = 0,
        OnePair = Cards.Pair + 2,
        TwoPairs = Cards.Pair + Cards.Pair + 4,
        ThreeOfAKind = Cards.Three + 4,
        FullHouse = Cards.Three + Cards.Pair + 8,
        FourOfAKind = Cards.Four + 8,
        FiveOfAKind = Cards.Five + 16,
    }

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

    private static byte[] Set = new byte[15];

    private static int[] CardsValues = Enum.GetValues(typeof(Cards)).Cast<int>().ToArray();

    private static int HandStrength(int[] hand)
    {
        Array.Clear(Set);

        foreach (var card in hand) Set[card]++;

        var magicPadding = 5;
        var strengthSum = 0;

        foreach (var count in Set.Skip(1))
        {
            if (count == 0) continue;
            magicPadding--;

            strengthSum += CardsValues[count - 1];
        }

        magicPadding = magicPadding == 0 ? 0 : (1 << magicPadding);

        return magicPadding + strengthSum;
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
}
