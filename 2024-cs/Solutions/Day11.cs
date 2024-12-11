using _2024_cs.Extension;

namespace _2024_cs.Solutions;


// --- Day 11: Plutonian Pebbles --- //


// 189128 - too low
// 189128
public record Day11() : Solver(AnswerOne: "189167", AnswerTwo: "225253278506288")
{
    public override string PartOne(IEnumerable<string> input)
    {
        var ogStones = input
            .First()
            .Split(" ")
            .Select(long.Parse)
            .ToList();

        const int blinks = 25;
        
        return CountStones(ogStones, blinks).ToString();
    }

    private static long CountStones(List<long> stones, int blinks)
    {
        var stoneCounter = new Dictionary<long, long>();
        foreach (var stone in stones) stoneCounter.CountUp(stone);

        for (var bi = 0; bi < blinks; bi++)
        {
            foreach (var (stone, amount) in stoneCounter.ToList())
            {
                var digits = stone.ToString().Length;
                
                switch (stone) {
                    case 0:
                        stoneCounter
                            .CountDown(0, amount)
                            .CountUp(1, amount);
                        break;
                    
                    case var _ when stone == 1 || digits % 2 != 0:
                        stoneCounter
                            .CountDown(stone, amount)
                            .CountUp(stone * 2024, amount);
                        break;
                    
                    default:
                        var mid = digits / 2;
                        stoneCounter
                            .CountDown(stone, amount)
                            .CountUp(long.Parse(stone.ToString()[..mid]), amount)
                            .CountUp(long.Parse(stone.ToString()[mid..]), amount);
                        break;
                }
            }

            // if (bi == blinks - 1) 
                // Console.WriteLine($"stones for {ogStone} after blink {bi + 1}:\n{stoneCounter.Stringify()}");
        }

        var res = stoneCounter.Values.Sum();
        // Console.WriteLine(res);
        return res;
    }

    public override string PartTwo(IEnumerable<string> input)
    {
        var ogStones = input
            .First()
            .Split(" ")
            .Select(long.Parse)
            .ToList();

        const int blinks = 75;
        
        return CountStones(ogStones, blinks).ToString();
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "55312",
            Input: """
            125 17
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "",
            Input: """
            
            """
        ),
    ];
}