namespace AoC_2023.Solutions;


// --- Day 3: Gear Ratios - Part 1 ---


public class D03P01 : Solution
{
    public override string ExampleAnswer => "4361";
    public override string Answer => "530495";

    public override string Solve(IEnumerable<string> lines)
    {
        string[] map = lines.ToArray();
        long answer = 0;

        foreach (var (l, y) in map.Select((l, y) => (l, y)))
        {
            int number = 0;
            bool connected = false;

            foreach (var (c, x) in l.Select((c, x) => (c, x)))
            {
                if (char.IsDigit(c))   
                {
                    number = number * 10 + (c - '0');
                    connected = connected || IsConnected(x, y, map);
                }
                else // End of number
                {
                    if (connected) answer += number;

                    number = 0;
                    connected = false;
                }
            }

            if (connected) answer += number; // End of line
        }

        return answer.ToString();
    }

    static int[] DX = { 1, 1,  1, 0,  0, -1, -1, -1 };
    static int[] DY = { 1, 0, -1, 1, -1,  1,  0, -1 };

    private static bool IsConnected(int originX, int originY, string[] map)
    {
        for (var i = 0; i < 8; i++)
        {
            var x = originX + DX[i];
            var y = originY + DY[i];

            if (
                x >= 0 && y >= 0 && y < map.Length && x < map[0].Length &&
                IsAConnection(map[y][x])
            ) return true;
        }

        return false;
    }

    private static bool IsAConnection(char c) => !char.IsDigit(c) && c != '.';
}
