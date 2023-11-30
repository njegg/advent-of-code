namespace AoC_2023.Solutions;

public class Day01 : SolutionBase
{
    public override string Part1(string inputPath)
    {
        var lines = File.ReadLines(inputPath)
            .TakeLast(1);

        return lines.First();
    }
}