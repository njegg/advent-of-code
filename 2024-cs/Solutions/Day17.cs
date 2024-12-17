namespace _2024_cs.Solutions;


// --- Day 17: Chronospatial Computer --- //


public record Day17() : Solver(AnswerOne: null, AnswerTwo: null)
{
    private const int ADV = 0;
    private const int BXL = 1;
    private const int BST = 2;
    private const int JNZ = 3;
    private const int BXC = 4;
    private const int OUT = 5;
    private const int BDV = 6;
    private const int CDV = 7;
    
    private record CPU
    {
        public int A;
        public int B;
        public int C;
        private int ip;

        public List<int> Execute(List<int> program)
        {
            var output = new List<int>();
            
            while (ip <= program.Count - 2)
            {
                var operand = program[ip + 1];
                var jump = false;
            
                switch (program[ip])
                {
                    case ADV: A /= 1 << Combo(operand); break;
                    case OUT: output.Add(Combo(operand) % 8); break;
                    case BXL: B ^= operand; break;
                    case BST: B = Combo(operand) % 8; break;
                    case BXC: B ^= C; break;
                    case BDV: B = A / (1 << Combo(operand)); break;
                    case CDV: C = A / (1 << Combo(operand)); break;
                    
                    case JNZ:
                        jump = A != 0;
                        if (jump) ip = operand;
                        break;
                
                    default: throw new Exception($"Unknown instruction {program[ip]}");
                }

                if (!jump) ip += 2;
            }

            return output;
        }

        private int Combo(int x)
        {
            return x switch
            {
                >= 0 and <= 3 => x,
                4 => A,
                5 => B,
                6 => C,
                _ => throw new InvalidOperationException($"Combo of {x}")
            };
        }
    }
    
    public override string PartOne(IEnumerable<string> input)
    {
        var (a, b, c, program) = ParseInput(input);

        var cpu = new CPU { A = a, B = b, C = c };
        var output = cpu.Execute(program);
        
        return string.Join("", output);
    }
    
    
    // ------^--------*------ Part Two ---o-------------0------- //
    
    
    public override string PartTwo(IEnumerable<string> input)
    {
        return "";
    }

    (int, int, int, List<int>) ParseInput(IEnumerable<string> input)
    {
        var d = input.Where(l => l != "").Select(l => l.Split(": ")[1]).ToList();

        var a = int.Parse(d[0]);
        var b = int.Parse(d[1]);
        var c = int.Parse(d[2]);
        var program = d[3].Split(",").Select(int.Parse).ToList();

        return (a, b, c, program);
    }

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "4635635210",
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