namespace AoC_2023.Solutions;


// --- Day 3: Gear Ratios - Part 1 ---


public class D03P02 : Solution
{
    public override string ExampleAnswer => "467835";
    public override string Answer => "80253814";

    private static Dictionary<int, Stack<int>> gearParts = new();
    private static HashSet<int> connectedGearsBuffer = new();

    public override string Solve(IEnumerable<string> lines)
    {
        string[] map = lines.ToArray();

        foreach (var (l, y) in map.Select((l, y) => (l, y)))
        {
            int number = 0;

            foreach (var (c, x) in l.Select((c, x) => (c, x)))
            {
                if (char.IsDigit(c))   
                {
                    number = number * 10 + (c - '0');
                    AddNearGearsToBuffer(x, y, map);
                }
                else // End of number
                {
                    if (number > 0) AddPart(number);

                    number = 0;
                    connectedGearsBuffer.Clear();
                }
            }

            if (number > 0) AddPart(number); // End of line

            connectedGearsBuffer.Clear();
        }

        return gearParts
            .Values
            .Where(s => s.Count == 2)
            .Select(s => s.Pop() * s.Pop())
            .Sum()
            .ToString();
    }

    private static void AddPart(int part)
    {
        foreach (var id in connectedGearsBuffer)
        {
            if (!gearParts.ContainsKey(id)) gearParts.Add(id, new ());
            gearParts[id].Push(part);
        };
    }


    static int[] DX = { 1, 1,  1, 0,  0, -1, -1, -1 };
    static int[] DY = { 1, 0, -1, 1, -1,  1,  0, -1 };

    private static void AddNearGearsToBuffer(int originX, int originY, string[] map)
    {
        for (var i = 0; i < 8; i++)
        {
            var x = originX + DX[i];
            var y = originY + DY[i];

            if (
                x >= 0 && y >= 0 && y < map.Length && x < map[0].Length &&
                map[y][x] == '*'
            )
            {
                connectedGearsBuffer.Add(y * map[0].Length + x);
            }
        }
    }
}
