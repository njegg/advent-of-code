namespace _2024_cs.Solutions;


// --- Day 17: Chronospatial Computer --- //


public record Day17() : Solver(AnswerOne: "7,6,1,5,3,1,4,2,6", AnswerTwo: "164541017976509")
{
    private const int ADV = 0;
    private const int BXL = 1;
    private const int BST = 2;
    private const int JNZ = 3;
    private const int BXC = 4;
    private const int OUT = 5;
    private const int BDV = 6;
    private const int CDV = 7;
    

    public override string PartOne(IEnumerable<string> input)
    {
        var (a, b, c, program) = ParseInput(input);

        var output = Execute(program, a, b, c);
        
        return string.Join(",", output);
    }

    private static List<int> Execute(List<long> program, long A, long B = 0, long C = 0)
    {
        var ip = 0;
        var output = new List<int>();

        while (ip <= program.Count - 2)
        {
            var operand = (long)program[ip + 1];
            var jump = false;

            switch (program[ip])
            {
                case ADV: A >>= (int)Combo(operand); break;
                case OUT: output.Add((int)(Combo(operand) % 8)); break;
                case BXL: B ^= operand; break;
                case BST: B = Combo(operand) % 8; break;
                case BXC: B ^= C; break;
                case BDV: B = A >> (int)Combo(operand); break;
                case CDV: C = A >> (int)Combo(operand); break;

                case JNZ:
                    jump = A != 0;
                    if (jump) ip = (int)operand;
                    break;

                default: throw new Exception($"Unknown instruction {program[ip]}");
            }

            if (!jump) ip += 2;
        }

        return output;

        long Combo(long x)
        {
            return x switch
            {
                >= 0 and <= 3 => (int)x,
                4 => A,
                5 => B,
                6 => C,
                _ => throw new InvalidOperationException($"Combo of {x}")
            };
        }
    }
    
    
    // ------^--------*------ Part Two ---o-------------0------- //
    
    
    public override string PartTwo(IEnumerable<string> input)
    {
        var (_, _, _, program) = ParseInput(input);

        return Find(program, program.Count - 1, 0).Min().ToString();
    }

    private static IEnumerable<long> Find(List<long> program, int outputIdx, long a)
    {
        if (outputIdx < 0) return [a];
        
        List<long> goodA = [];
        
        for (var i = 0; i < 8; i++)
        {
            var maybe = (a << 3) + i;
            if (Execute(program, maybe)[0] == program[outputIdx])
            {
                goodA.AddRange(Find(program, outputIdx - 1, maybe));
            }
        }

        return goodA;
    }

    (long, long, long, List<long>) ParseInput(IEnumerable<string> input)
    {
        var d = input.Where(l => l != "").Select(l => l.Split(": ")[1]).ToList();

        var a = long.Parse(d[0]);
        var b = long.Parse(d[1]);
        var c = long.Parse(d[2]);
        var program = d[3].Split(",").Select(long.Parse).ToList();

        return (a, b, c, program);
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "4,6,3,5,6,3,5,2,1,0",
            Input: """
            Register A: 729
            Register B: 0
            Register C: 0
            
            Program: 0,1,5,4,3,0
            """
        ),
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "117440",
            Input: """
            Register A: 2024
            Register B: 0
            Register C: 0
            
            Program: 0,3,5,4,3,0
            """
        ),
    ];
}