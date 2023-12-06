namespace AoC_2023.Solutions;

// --- --- Day 4: Scratchcards - Part 2 --- //

public class D04P02 : Solution
{
    public override string ExampleAnswer => "30";
    public override string Answer => "7258152";

    private Dictionary<int, (int hits, int cards)> map = new();

    public override string Solve(IEnumerable<string> lines)
    {
        lines
            .Select(l => l.Split(':')[1])
            .Select(l => l
                .Split(" | ")
                .Select(s => s.Split(' ').Where(s => s != "").ToHashSet())
                .ToArray())
            .Select((s, i) => new { id = i + 1, cardData = (s[1].Intersect(s[0]).Count(), 1) })
            .ToList()
            .ForEach(e => map.Add(e.id, e.cardData));

        foreach (var (id, (hits, cards)) in map)
        {
            foreach (var i in Enumerable.Range(1, hits))
            {
                map[id + i] = (map[id + i].hits, map[id + i].cards + cards);
            }
        }

        return map.Values
            .Select(x => x.cards)
            .Sum()
            .ToString();
    }
}
