namespace _2024_cs.Solutions;


// --- Day 4: Ceres Search --- //


public record Day04() : Solver(AnswerOne: "2603", AnswerTwo: "1965")
{
    private const string XMAS = "XMAS";
    
    public override string PartOne(IEnumerable<string> input)
    {
        var words = input.ToList();
        var XMASCount = 0;

        for (var y = 0; y < words.Count; y++)
        {
            for (var x = 0; x < words[0].Length; x++)
            {
                XMASCount += SearchXMAS(words, x, y);
            }
        }

        return XMASCount.ToString();
    }

    private static readonly int[] xNeighbours = [0, 1, 1, 1, 0, -1, -1, -1];
    private static readonly int[] yNeighbours = [-1, -1, 0, 1, 1, 1, 0, -1];

    private static int SearchXMAS(List<string> words, int x, int y)
    {
        var XMASCount = 0;
        
        for (var direction = 0; direction < 8; direction++)
        {
            var badDirection = false;
            
            for (var xmasLetter = 0; !badDirection && xmasLetter < XMAS.Length; xmasLetter++)
            {
                var dx = x + xNeighbours[direction] * xmasLetter;
                var dy = y + yNeighbours[direction] * xmasLetter;
                
                if (OutOfBounds(words, dx, dy) || words[dy][dx] != XMAS[xmasLetter]) badDirection = true;
            }

            if (!badDirection) XMASCount++;
        }

        return XMASCount;
    }

    public override string PartTwo(IEnumerable<string> input)
    {
        var words = input.ToList();
        var XMASCount = 0;

        for (var y = 0; y < words.Count; y++)
        {
            for (var x = 0; x < words[0].Length; x++)
            {
                XMASCount += SearchX_MAS(words, x, y);
            }
        }

        return XMASCount.ToString();
    }

    private static readonly int[] xCornerNeighbours = [-1, 1, -1, 1];
    private static readonly int[] yCornerNeighbours = [-1, -1, 1, 1];
    private static readonly List<string> validCombination = ["MMSS", "MSMS", "SSMM", "SMSM"];
    
    private static int SearchX_MAS(List<string> words, int x, int y)
    {
        if (words[y][x] != 'A') return 0;

        var cornerBuffer = new char[4];

        for (var direction = 0; direction < xCornerNeighbours.Length; direction++)
        {
            var dx = x + xCornerNeighbours[direction];
            var dy = y + yCornerNeighbours[direction];
            
            if (OutOfBounds(words, dx, dy)) return 0;

            cornerBuffer[direction] = words[dy][dx];
        }

        return validCombination.Contains(new string(cornerBuffer)) ? 1 : 0;
    }
    
    private static bool OutOfBounds(List<string> words, int x, int y) 
        => y < 0 || x < 0 || y >= words.Count || x >= words[0].Length;

    protected override List<(string Expected, string Input)> PartOneExamples => [
        (
            Expected: "18",
            Input: 
            """
            MMMSXXMASM
            MSAMXMSMSA
            AMXSXMAAMM
            MSAMASMSMX
            XMASAMXAMM
            XXAMMXXAMA
            SMSMSASXSS
            SAXAMASAAA
            MAMMMXMMMM
            MXMXAXMASX
            """
        )
    ];

    protected override List<(string Expected, string Input)> PartTwoExamples => [
        (
            Expected: "9",
            Input: 
            """
            .M.S......
            ..A..MSMS.
            .M.S.MAA..
            ..A.ASMSM.
            .M.S.M....
            ..........
            S.S.S.S.S.
            .A.A.A.A..
            M.M.M.M.M.
            ..........
            """
        )
    ];
}
